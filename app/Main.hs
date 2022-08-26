{-# LANGUAGE OverloadedStrings, FlexibleContexts, TemplateHaskell #-}
module Main where
  
import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow
import Data.Maybe
import Control.Monad.State.Class
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as M
import Control.Monad
import Data.Char
import Control.Lens
import Data.Char (ord, chr)

data PrettyPrintStyle = PrettyPrintStyle { formats :: [Text] }

data Expression = EVar Variable | EString Text | ENum Int
  deriving (Show, Eq, Ord)
  
data VType = VString | VInt
  deriving (Show, Eq, Ord)

type Variable = Text

data Command = Print Variable
             | Set Variable Expression
             | Input Variable
  deriving (Show, Eq, Ord)

type Program = [Command]

data ProgState = ProgState
  { _bfOutput :: Text
  , _astInput :: Program
  , _pointerLoc :: Int
  , _vars :: M.Map Variable (VType, Int)
  , _allocPtr :: Int
  }
$(makeLenses ''ProgState)
  
data ParseState = ParseState
  { _tInput :: Text
  , _astOutput :: Program
  , _iLevel :: Int
  }
$(makeLenses ''ParseState)
  
tmpVar :: Int -> Variable
tmpVar n = "__tmp__" <> T.pack (show n)

main :: IO ()
main = putStrLn "Hello, Haskell!"

{----------------------
 - Pretty prent bf
 ---------------------}

{----------------------
 - Parse AST to brainf
 ---------------------}

compileAST :: Program -> Text
compileAST p = view bfOutput . execState compileASTM $  ProgState
  { _bfOutput = ""
  , _astInput = p
  , _pointerLoc = 0
  , _vars = M.empty
  , _allocPtr = 0
  }

compileASTM :: MonadState ProgState m => m ()
compileASTM = do
  p <- popCmd
  case p of
    Just p' -> do
      case p' of
        Input var -> do
          alloc VString var
          shiftToVar var
          writeBf $ (T.pack . replicate (ord '\n') $ '-')
          bfLoop $ do
            writeBf (T.pack . replicate (ord '\n') $ '+')
            writeBf ">,"
            writeBf (T.pack . replicate (ord '\n') $ '-')
          writeBf "<"
          bfLoop $ writeBf "<"
        Print var -> do
          shiftToVar var
          writeBf "[>.]<[<]"
        Set var expr -> do
          handle <- calculateExpr expr
          renameVar handle var
    Nothing -> return ()
    
-- calculate the value of an expession, and return a pointer to the result
calculateExpr :: MonadState ProgState m => Expression -> m Variable
calculateExpr = do
  error "todo"

shiftToVar :: MonadState ProgState m => Variable -> m ()
shiftToVar = getVarPointer >=> shiftTo

getVarInfo :: MonadState ProgState m => Variable -> m (VType, Int)
getVarInfo var = do
  st <- get
  case st^.vars.at var of
    Just i -> return i
    Nothing -> error $ "Undefined variable: " <> T.unpack var

getVarPointer :: MonadState ProgState m => Variable -> m Int
getVarPointer = fmap snd . getVarInfo

getVarType :: MonadState ProgState m => Variable -> m VType
getVarType = fmap fst . getVarInfo

shiftTo :: MonadState ProgState m => Int -> m ()
shiftTo n = do
  currentLoc <- view pointerLoc <$> get
  modify $ set pointerLoc n
  let dist = abs $ currentLoc - n
      c = if currentLoc < n then '+' else '-'
  writeBf (T.pack $ replicate dist c)

popCmd :: MonadState ProgState m => m (Maybe Command)
popCmd = do
  (p, ps) <- fromMaybe (Nothing, []) . fmap (first Just) . uncons . view astInput <$> get
  modify $ set astInput ps
  return p

alloc :: MonadState ProgState m => VType -> Variable -> m ()
alloc typ var = do
  st <- get
  case st^.vars.at var of
    Nothing -> do
      modify $ set (vars.at var) $ Just (typ, st^.allocPtr)
      modify $ over allocPtr (+sizeOf typ)
    Just (oldtyp, _) -> 
      when (oldtyp /= typ) $ do
        free var
        alloc typ var

copy :: MonadState ProgState m => Variable -> Variable -> m ()
copy src tgt = do
  typ <- getVarType src 
  alloc typ (tmpVar 0)
  ps <- getVarPointer src
  pt <- getVarPointer tgt
  p0 <- getVarPointer (tmpVar 0)
  forM_ [0..(sizeOf typ) - 1] $ \n -> do
    shiftTo (ps + n)
    bfLoop $ do
      writeBf "-"
      shiftTo (p0 + n)
      writeBf "+"
      shiftTo (pt + n)
      writeBf "+"
      shiftTo (ps + n)
  move (tmpVar 0) src
  free (tmpVar 0)
  
-- Precondition: m does not shift pointerLoc
bfLoop :: MonadState ProgState m => m () -> m ()
bfLoop m = do
  writeBf "["
  m
  writeBf "]"

renameVar :: MonadState ProgState m => Variable -> Variable -> m ()
renameVar src tgt = do
  v <- getVarInfo src
  modify $ set (vars.at tgt) $ Just v
  modify $ set (vars.at src) $ Nothing

move :: MonadState ProgState m => Variable -> Variable -> m ()
move src tgt = do
  typ <- getVarType src 
  ps <- getVarPointer src
  pt <- getVarPointer tgt
  forM_ [0..(sizeOf typ) - 1] $ \n -> do
    shiftTo (ps + n)
    bfLoop $ do
      writeBf "-"
      shiftTo (pt + n)
      writeBf "+"
      shiftTo (ps + n)
  shiftToVar src

sizeOf :: VType -> Int
sizeOf VString = 32
sizeOf VInt = 1

-- does nothing for now
-- later on can optimise
free :: MonadState ProgState m => Variable -> m ()
free _ = return ()

writeBf :: MonadState ProgState m => Text -> m ()
writeBf cs = modify $ over bfOutput (<> cs)

{----------------------
 - Parse file to AST
 ---------------------}

parseSource :: Text -> Program
parseSource cs = view astOutput . execState parseSourceM $ ParseState
  { _tInput = cs
  , _astOutput = []
  , _iLevel = 0
  }
  
parseSourceM :: MonadState ParseState m => m ()
parseSourceM = do
  st <- get
  let (line, ls) = second (fromMaybe "" . fmap snd . T.uncons) . T.breakOn "\n" $ st^.tInput
  modify $ over astOutput (<> parseLine line)
  if T.strip ls == ""
    then return ()
    else do
      modify $ set tInput ls
      parseSourceM

parseLine :: Text -> Program
parseLine cs =
  let (lcs', mrcs') = T.strip *** fmap (T.strip . snd) . T.uncons <<< T.breakOn "=" $ cs
  in case mrcs' of
      Just rcs' -> parseLet (T.strip lcs') (T.strip rcs')
      Nothing -> parseExec (T.strip lcs')

parseExec :: Text -> Program
parseExec cmd | isFunCall "print" cmd = fromMaybe [] $ do
  pExpr <- getFunCall cmd
  return $
    [ Set (tmpVar 0) pExpr
    , Print (tmpVar 0)
    ]
parseExec cmd = error $ "Invalid syntax: " <> show cmd
  
isFunCall :: Text -> Text -> Bool
isFunCall fun expr = T.take (1 + (T.length fun)) expr == fun <> "("
                      && (snd <$> T.unsnoc expr) == Just ')'

-- partial
getFunCall :: Text -> Maybe Expression
getFunCall = parseExpr <=< fmap fst . T.unsnoc . snd <=< T.uncons . snd . T.breakOn "("

parseLet :: Text -> Text -> Program
parseLet var expr | expr == "input()" = [Input var]
parseLet var expr
  | isFunCall "input" expr = fromMaybe [] $ do
      pExpr <- getFunCall expr
      return $
        [ Set (tmpVar 0) pExpr
        , Print (tmpVar 0)
        , Input var
        ]
parseLet var expr = mapMaybe id $ [Set var <$> parseExpr expr]

parseExpr :: Text -> Maybe Expression
parseExpr "" = Nothing
parseExpr expr | isVString expr = Just . EString . T.tail . T.init $ expr
parseExpr num | T.foldr ((&&) . isDigit) True num = Just . ENum . read . T.unpack $ num
parseExpr var = Just $ EVar var

isVString :: Text -> Bool
isVString cs = fromMaybe False $ do
  (a, _) <- T.uncons cs
  (_, b) <- T.unsnoc cs
  return $ a == '"' && b == '"'
  
{--------------------
 - Generic functions
 --------------------}

changeText :: (String -> String) -> Text -> Text
changeText f = T.pack . f . T.unpack
