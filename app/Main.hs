{-# LANGUAGE OverloadedStrings, FlexibleContexts, TemplateHaskell #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Arrow
import Data.Maybe
import Control.Monad.State.Class
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as M
import Control.Monad
import Data.Char
import Control.Lens hiding (uncons)
import Data.Char (ord, chr)
import System.Environment
import Control.Concurrent (threadDelay)
import Safe (readMay, headMay)
import Data.List
import Control.Applicative

data PrettyPrintStyle = BlockStyle Int
                      | CircleStyle Int
                      | DiscStyle Int
                      | TemplateStyle Text
                      | Unknown
  deriving Show

data Expression = EVar Variable
                | EString Text
                | ENum Int
                | EInput (Maybe Expression)
                | EAdd Expression Expression
                | ESub Expression Expression
                | EDiv Expression Expression
                | EMod Expression Expression
                | EStr Expression
                | EOrd Expression
                | EChr Expression
                | EMul Expression Expression
                | ENot Expression
  deriving (Show, Eq, Ord)

data VType = VString | VInt
  deriving (Show, Eq, Ord)

type Variable = Text

data Command = Print Expression
             | Set Variable Expression
             | Input Variable
             | While Expression [Command]
             | If Expression [Command]
  deriving (Show, Eq, Ord)

type Program = [Command]

data ProgState = ProgState
  { _bfOutput :: Text
  , _astInput :: Program
  , _pointerLoc :: Int
  , _vars :: M.Map Variable (VType, Int)
  , _allocPtr :: Int
  , _tempVarPtr :: Int
  }
$(makeLenses ''ProgState)

data ParseState = ParseState
  { _tInput :: Text
  , _astOutput :: Program
  , _iStack :: [Program -> Program]
  }
$(makeLenses ''ParseState)

tmpVar :: Int -> Variable
tmpVar n = "__tmp__" <> T.pack (show n)

main :: IO ()
main = do
  args <- getArgs
  progname <- getProgName
  case getFileTarget args of
    Nothing -> putStrLn $ helpMessage progname
    Just fp -> do
      let blockStyle = BlockStyle $ getNumOpt "block-width" 40 args
      src <- T.readFile fp
      templateStyle <- sequence . fmap T.readFile $ getStrOpt "template" args
      let style = case getStrOpt "style" args of
                    Nothing -> blockStyle
                    Just "block" -> blockStyle
                    Just "circles" -> CircleStyle $ getNumOpt "radius" 10 args
                    Just "template" -> TemplateStyle $ fromMaybe defTemplate templateStyle
                    Just "discs" -> DiscStyle $ getNumOpt "radius" 10 args
                    Just "dna" -> TemplateStyle defTemplate
                    Just "dna-curtains" -> TemplateStyle dnaCurtains
                    Just _ -> Unknown
      let delay = getNumOpt "delay" 0 args
      let compiled = prettyPrint style $ compileBf src
      when (not (getBoolOpt "silent" False args)) $
        forM_ (T.lines compiled) $ \line -> do
          threadDelay delay
          T.putStrLn line
      case getStrOpt "outfile" args of
        Just ofp -> T.writeFile ofp compiled
        Nothing -> return ()

defTemplate :: Text
defTemplate = T.unlines
  [ "....        ...."
  , "....        ...."
  , " ....      .... "
  , "  ....    ....  "
  , "   ....  ....   "
  , "     ......     "
  , "      ....      "
  , "     ......     "
  , "   ....  ....   "
  , "  ....    ....  "
  , " ....      .... "
  , "....        ...."
  , "....        ...."
  ]

dnaCurtains :: Text
dnaCurtains = T.unlines
  [ "....        ....    ....        ....      ....  ....   "
  , "....        ....    ....        ....        ......     "
  , " ....      ....     ....        ....         ....      "
  , "  ....    ....      ....        ....        ......     "
  , "   ....  ....        ....      ....       ....  ....   "
  , "     ......           ....    ....       ....    ....  "
  , "      ....             ....  ....       ....      .... "
  , "     ......              ......        ....        ...."
  , "   ....  ....             ....         ....        ...."
  , "  ....    ....           ......        ....        ...."
  , " ....      ....        ....  ....      ....        ...."
  , "....        ....      ....    ....      ....      .... "
  , "....        ....     ....      ....      ....    ....  "
  ]

helpMessage :: String -> String
helpMessage progname = unlines
  [ "USAGE: " <> progname <> " [OPTIONS] SOURCEFILE [> TARGETFILE]"
  , ""
  , "OPTIONS:"
  , "  --style=STYLE: pretty print the code in the chosen style (default: block)"
  , "  --block-width: width of the block when printing with block style"
  , "  --radius: the radius of each circle when using circles or discs style"
  , "  --silent: supress output (default: off)"
  , ""
  , "STYLES:"
  , "  block (default): just print it as a chunk of text"
  , "  circles: print the code in circles"
  , "  discs: like circles, but filled in"
  , "  dna[-curtains]: DNA strands"
  ]

getNumOpt :: String -> Int -> [String] -> Int
getNumOpt str n args = fromMaybe n $ readMay =<< getStrOpt str args

getStrOpt :: String -> [String] -> Maybe String
getStrOpt str (arg:args)
  | take 2 arg == "--" && str == var = Just . fromMaybe expr $ getStrOpt str args
    where (var, expr) = second tail $ break (=='=') $ drop 2 arg
getStrOpt str (_:args) = getStrOpt str args
getStrOpt _ [] = Nothing

getBoolOpt :: String -> Bool -> [String] -> Bool
getBoolOpt cs _ (arg:args) | arg == "--" <> cs = getBoolOpt cs True args 
getBoolOpt cs _ (arg:args) | arg == "--no-" <> cs = getBoolOpt cs False args 
getBoolOpt cs b (_:args) = getBoolOpt cs b args
getBoolOpt _ b [] = b

getFileTarget :: [String] -> Maybe String
getFileTarget (('-':'-':_):css) = getFileTarget css
getFileTarget (cs:_) = Just cs
getFileTarget [] = Nothing

{----------------------
 - Pretty print bf
 ---------------------}

prettyPrint :: PrettyPrintStyle -> Text -> Text
prettyPrint _ "" = ""
prettyPrint (DiscStyle radius) cs =
  let xA = [-2*radius .. 2*radius]
      yA = [-radius .. radius]
      coords = fmap (sequence $ fmap (,) xA) yA :: [[(Int, Int)]]
      two = 2 :: Int
      p (x, y) =
        if x^two + (2*y)^two <= (2*radius)^two
          then '.' else ' '
      style = TemplateStyle $ (T.unlines . fmap (T.pack . fmap p) $ coords) <> "\n"
  in prettyPrint style cs
prettyPrint (CircleStyle radius) cs =
  let xA = [-2*radius .. 2*radius]
      yA = [-radius .. radius] :: [Int]
      two = 2 :: Int
      coords = fmap (sequence $ fmap (,) xA) yA :: [[(Int, Int)]]
      p (x, y) =
        if x^two + (2*y)^two <= (2*radius)^two && 2*(x^two + (2*y)^two) >= (2*radius)^two
          then '.' else ' '
      style = TemplateStyle $ (T.unlines . fmap (T.pack . fmap p) $ coords) <> "\n"
  in prettyPrint style cs
prettyPrint (BlockStyle width) cs =
  prettyPrint (TemplateStyle $ (T.pack $ replicate width '.') <> "\n") cs
prettyPrint (TemplateStyle template) cs' = changeText (_templatePrint []) cs'
  where stemplate = T.unpack template
        _templatePrint _ "" = ""
        _templatePrint (' ':ts) cs = ' ':_templatePrint ts cs
        _templatePrint ('\n':ts) cs = '\n' : _templatePrint ts cs
        _templatePrint (_:ts) (c:cs) = c:_templatePrint ts cs
        _templatePrint [] cs = _templatePrint stemplate cs
prettyPrint _ _ = error $ "unknown style"

compileBf :: Text -> Text
compileBf = compileAST . parseSource

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
  , _tempVarPtr = 1
  }

compileASTM :: MonadState ProgState m => m ()
compileASTM = do
  -- errorTmp <- view astInput <$> get
  -- error $ show errorTmp
  p <- popCmd
  case p of
    Just p' -> do
      case p' of
        Input var -> do
          alloc VString var
          shiftToVar var
          newline '-'
          bfLoop $ do
            newline '+'
            writeBf ">,"
            newline '-'
          writeBf "<"
          bfLoop $ writeBf "<"
        Print expr -> do
          var <- calculateExpr expr
          printVar var
        While expr prog -> do
          cmds <- view astInput <$> get
          modify $ set astInput prog
          res <- calculateExpr expr
          shiftToVar res
          bfLoop $ do
            compileASTM
            res' <- calculateExpr expr
            shiftToVar res'
          modify $ set astInput cmds
        If expr prog -> do
          cmds <- view astInput <$> get
          modify $ set astInput prog
          res <- calculateExpr expr
          ifVar res $ do
            compileASTM
          modify $ set astInput cmds
        Set var expr -> do
          handle <- calculateExpr expr
          renameVar handle var
      compileASTM
    Nothing -> return ()
  where newline = writeBf . T.pack . replicate (ord '\n')

printVar :: MonadState ProgState m => Variable -> m ()
printVar var = do
    typ <- getVarType var
    case typ of
      VString -> do
        shiftToVar var
        writeBf ">[.>]"
        newline '+'
        writeBf ".[-]"
        writeBf "<[<]"
      VInt -> do
        svar <- calculateExpr $ EStr (EVar var)
        printVar svar
  where newline = writeBf . T.pack . replicate (ord '\n')

-- calculate the value of an expession, and return a pointer to the result
calculateExpr :: MonadState ProgState m => Expression -> m Variable
calculateExpr (EVar var) = return var

calculateExpr (ENot a) = do
  a' <- calculateExpre a
  typ <- getVarType a'
  case typ of
    VInt -> do
      inv <- calculateExpr $ ENum 1
      ifVar a' $ do
        decr inv
      return inv
    _ -> error $ show typ <> " is not boolean!"

calculateExpr (EAdd a b) = do
  a' <- calculateExpr a
  b' <- calculateExpr b
  typ <- getVarType a'
  typ' <- getVarType b'
  tgt <- allocTmp typ
  copy a' tgt
  case (typ, typ') of
    (VInt, VInt) -> do
      repeatVar b' $ do
        shiftToVar tgt
        writeBf "+"
    (_, _) -> error $ "Cannot add " <> show typ <> " and " <> show typ'
  return tgt

calculateExpr (ESub a b) = do
  a' <- calculateExpr a
  b' <- calculateExpr b
  typ <- getVarType a'
  typ' <- getVarType b'
  tgt <- allocTmp typ
  copy a' tgt
  case (typ, typ') of
    (VInt, VInt) -> do
      repeatVar b' $ do
        shiftToVar tgt
        writeBf "-"
    (_, _) -> error $ "Cannot subtract " <> show typ <> " and " <> show typ'
  return tgt

calculateExpr (EMul a b) = do
  a' <- calculateExpr a
  b' <- calculateExpr b
  typ <- getVarType a'
  typ' <- getVarType b'
  case (typ, typ') of
    (VInt, VInt) -> do
      tgt <- allocTmp VInt
      -- nullify tgt
      repeatVar b' $
        repeatVar a' $ do
          shiftToVar tgt
          writeBf "+"
      return tgt
    (_, _) -> error $ "Cannot multiply " <> show typ <> " by " <> show typ'

calculateExpr (EDiv a b) = do
  a' <- calculateExpr a
  b' <- calculateExpr b
  typ <- getVarType a'
  typ' <- getVarType b'
  case (typ, typ') of
    (VInt, VInt) -> do
      acpy <- allocTmp VInt
      tgt <- allocTmp VInt
      nullify tgt
      copy a' acpy
      shiftToVar acpy
      writeBf "+"
      bfLoop $ do -- loop on a
        repeatVar b' $ do
          decr acpy
        ifVar acpy $ do
          shiftToVar tgt
          writeBf "+"
        shiftToVar acpy
      return tgt
    (_, _) -> error $ "Cannot divide " <> show typ <> " by " <> show typ'

calculateExpr (EMod a b) = do
  a' <- calculateExpr a
  b' <- calculateExpr b
  typ <- getVarType a'
  typ' <- getVarType b'
  case (typ, typ') of
    (VInt, VInt) -> do
      calculateExpr $ ESub (EVar a') (EMul (EVar b') (EDiv (EVar a') (EVar b')))
    (_, _) -> error $ "Cannot modulo " <> show typ <> " by " <> show typ'

calculateExpr (EChr expr) = do
  var <- calculateExpr expr
  typ <- getVarType var
  tgt <-
    case typ of
      VString -> error "Cannot cast string to character"
      VInt -> do
        tgt <- allocTmp VString
        tgtptr <- getVarPointer tgt
        varcpy <- allocTmp VInt
        copy var varcpy
        shiftToVar varcpy
        bfLoop $ do
          writeBf "-"
          shiftTo $ tgtptr + 1
          writeBf "+"
          shiftToVar varcpy
        free varcpy
        return tgt
  return tgt

calculateExpr (EOrd expr) = do
  var <- calculateExpr expr
  typ <- getVarType var
  tgt <-
    case typ of
      VInt -> error "Can only cast character to character code"
      VString -> do
        tgt <- allocTmp VInt
        varcpy <- allocTmp VString
        copy var varcpy
        varptr <- getVarPointer varcpy
        shiftTo $ varptr + 1
        bfLoop $ do
          shiftToVar tgt
          writeBf "+"
          shiftTo $ varptr + 1
          writeBf "-"
        return tgt
  return tgt

calculateExpr (EStr expr) = do
  var <- calculateExpr expr
  typ <- getVarType var
  tgt <-
    case typ of
      VString -> return var
      VInt -> do
        tgt <- allocTmp VString
        tgtptr <- getVarPointer tgt
        shiftToVar var
        ten <- calculateExpr $ ENum 10
        bfLoop $ do
          nextChar <- calculateExpr $ EAdd (ENum $ ord '0') (EMod (EVar var) (EVar ten))
          shiftStrRight tgt
          repeatVar nextChar $ do
            shiftTo $ tgtptr + 1
            writeBf "+"
          remainder <- calculateExpr $ EDiv (EVar var) (EVar ten)
          nullify var
          move remainder var
          free remainder
          free nextChar
        free ten
        return tgt
  free var
  return tgt
  

calculateExpr (ENum num) = do
  tgt <- allocTmp VInt
  shiftToVar tgt
  sequence_ $ replicate num $ writeBf "+"
  return tgt

calculateExpr (EInput Nothing) = do
  let newline = writeBf . T.pack . replicate (ord '\n')
  var <- allocTmp VString
  shiftToVar var
  newline '-'
  bfLoop $ do
    newline '+'
    writeBf ">,"
    newline '-'
  writeBf "<"
  bfLoop $ writeBf "<"
  return var
calculateExpr (EInput (Just expr)) = do
  v0 <- calculateExpr expr
  printVar v0
  calculateExpr (EInput Nothing)
calculateExpr (EString cs) = do
  tgt <- allocTmp VString
  tgtPtr <- getVarPointer tgt
  forM_ (zip [1..] $ T.unpack cs) $ \(n, c) -> do
    shiftTo $ tgtPtr + n
    writeBf "[-]"
    sequence_ $ replicate (ord c) $ writeBf "+"
  return tgt

-- int only
ifVar :: MonadState ProgState m => Variable -> m () -> m ()
ifVar var m = do
  tmp <- allocTmp VInt
  shiftToVar var
  bfLoop $ do
    m
    move var tmp
  move tmp var
  free tmp

setVar :: MonadState ProgState m => Variable -> Expression -> m ()
setVar var expr = do
  pExpr <- calculateExpr expr
  move pExpr var
  free pExpr

nullify :: MonadState ProgState m => Variable -> m ()
nullify var = do
  shiftToVar var
  typ <- getVarType var
  case typ of
    VInt -> writeBf "[-]"
    VString -> writeBf "[>]<[[-]<]"

repeatVar :: MonadState ProgState m => Variable -> m () -> m ()
repeatVar var m = do
  typ <- getVarType var
  case typ of
    VInt -> do
      varcpy <- allocTmp typ
      copy var varcpy
      shiftToVar varcpy
      bfLoop $ do
        m
        shiftToVar varcpy
        writeBf "-"
    t -> error $ show t <> " is not currently iterable"

shiftStrRight :: MonadState ProgState m => Variable -> m ()
shiftStrRight var = do
  shiftToVar var
  writeBf "[>]<" -- move to end of string
  bfLoop $ do
    writeBf "[->+<]"
    writeBf "<" -- move to last unmoved character
  -- expect: when done, will be at the null char at start of string

decr :: MonadState ProgState m => Variable -> m ()
decr var = do
  ifVar var $ do
    shiftToVar var
    writeBf "-"

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
      c = if currentLoc < n then '>' else '<'
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
  -- nullify var

copy :: MonadState ProgState m => Variable -> Variable -> m ()
copy src tgt = do
  typ <- getVarType src
  tmp <- allocTmp typ
  ps <- getVarPointer src
  pt <- getVarPointer tgt
  p0 <- getVarPointer tmp
  forM_ [0..(sizeOf typ) - 1] $ \n -> do
    shiftTo (ps + n)
    bfLoop $ do
      writeBf "-"
      shiftTo (p0 + n)
      writeBf "+"
      shiftTo (pt + n)
      writeBf "+"
      shiftTo (ps + n)
  move tmp src
  free tmp

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
sizeOf VString = 64
sizeOf VInt = 1

allocTmp :: MonadState ProgState m => VType -> m Variable
allocTmp typ = do
  st <- get
  let var = tmpVar (st^.tempVarPtr)
  alloc typ var
  modify $ over tempVarPtr (+1)
  return var

-- does nothing for now
-- later on can optimise
free :: MonadState ProgState m => Variable -> m ()
free var = return ()
  -- nullify var

writeBf :: MonadState ProgState m => Text -> m ()
writeBf cs = modify $ over bfOutput (<> cs)

{----------------------
 - Parse file to AST
 ---------------------}

parseSource :: Text -> Program
parseSource cs = view astOutput . execState parseSourceM $ ParseState
  { _tInput = cs
  , _astOutput = []
  , _iStack = []
  }

parseSourceM :: MonadState ParseState m => m ()
parseSourceM = do
  st <- get
  let (line, ls) = second (fromMaybe "" . fmap snd . T.uncons) . T.breakOn "\n" $ st^.tInput
  let expectedIndent = 4 * length (st^.iStack)
  if T.take expectedIndent line == T.pack (replicate expectedIndent ' ')
    then do
      parseLineM (T.strip line)
      if T.strip ls == ""
        then do
          -- Return out of the recursion: force flattening
          out <- view astOutput <$> get
          modify $ set astOutput $ foldl (flip ($)) out (st^.iStack)
        else do
          modify $ set tInput ls
          parseSourceM
    else do
      let (f:fs) = st^.iStack
      modify $ over astOutput f
      modify $ over iStack $ drop 1
      parseSourceM

parseLineM :: MonadState ParseState m => Text -> m ()
parseLineM line | T.take 6 line == "while " = do
  st <- get
  let expr = parseExpr $ T.dropEnd 1 . T.drop 6 . T.strip $ line
  case expr of
    Nothing -> error "Could not parse expression in while statement"
    Just pExpr -> do
      modify $ over iStack $ (:) $ (st^.astOutput <>) . return . (While pExpr)
      modify $ set astOutput []
parseLineM line | T.take 3 line == "if " = do
  st <- get
  let expr = parseExpr $ T.dropEnd 1 . T.drop 3 . T.strip $ line
  case expr of
    Nothing -> error "Could not parse expression in if statement"
    Just pExpr -> do
      modify $ over iStack $ (:) $ (st^.astOutput <>) . return . (If pExpr)
      modify $ set astOutput []
parseLineM line | T.take 3 line == "if " = error "todo"
parseLineM line = modify $ over astOutput (<> parseLine line)

parseLine :: Text -> Program
parseLine cs =
  let (lcs', mrcs') = T.strip *** fmap (T.strip . snd) . T.uncons <<< T.breakOn "=" $ cs
  in case mrcs' of
      Just rcs' -> parseLet (T.strip lcs') (T.strip rcs')
      Nothing -> parseExec (T.strip lcs')

parseExec :: Text -> Program
parseExec cmd | isFunCall "print" cmd = fromMaybe [] $ do
  pExpr <- headMay $ getFunArgs cmd
  return $
    [ Print pExpr ]
parseExec cmd | T.strip cmd == "" = []
parseExec cmd | T.head (T.strip cmd) == '#' = []
parseExec cmd = error $ "Invalid syntax: " <> show cmd

isFunCall :: Text -> Text -> Bool
isFunCall fun expr = T.take (1 + (T.length fun)) expr == fun <> "("
                      && (snd <$> T.unsnoc expr) == Just ')'

-- partial
getFunCall :: Text -> Maybe Expression
getFunCall = parseExpr <=< fmap fst . T.unsnoc . snd <=< T.uncons . snd . T.breakOn "("

getFunArgs :: Text -> [Expression]
getFunArgs cs = do
  (noClosingBracket, _) <- maybeToList . T.unsnoc . snd . T.breakOn "(" $ cs
  (_, joinedArgs) <- maybeToList . T.uncons $ noClosingBracket
  let breakComma '(' (n, acc, accs) = (n + 1, '(':acc, accs)
      breakComma ')' (n, acc, accs) = (n - 1, ')':acc, accs)
      breakComma ',' (0, acc, accs) = (0, [], acc:accs)
      breakComma c (n, acc, accs) = (n, c:acc, accs)
  arg <- fmap (T.pack)
          <<< uncurry (:)
          <<< view _2 &&& view _3
          <<< foldr breakComma (0 :: Int, [], []) . T.unpack $ joinedArgs
  maybeToList $ parseExpr (T.strip arg)

parseLet :: Text -> Text -> Program
parseLet var expr = mapMaybe id $ [Set var <$> parseExpr expr]

parseExpr :: Text -> Maybe Expression
parseExpr "" = Nothing
parseExpr "True" = Just $ ENum 1
parseExpr "False" = Just $ ENum 0
parseExpr expr | isFunCall "input" expr =
  let pExpr = getFunArgs expr
  in Just $ EInput $ headMay pExpr
parseExpr expr | isFunCall "add" expr = do
  (a, r1) <- uncons $ getFunArgs expr
  (b, _) <- uncons r1
  return $ EAdd a b
parseExpr expr | isFunCall "subtract" expr = do
  (a, r1) <- uncons $ getFunArgs expr
  (b, _) <- uncons r1
  return $ ESub a b
parseExpr expr | isFunCall "mul" expr = do
  (a, r1) <- uncons $ getFunArgs expr
  (b, _) <- uncons r1
  return $ EMul a b
parseExpr expr | isFunCall "div" expr = do
  (a, r1) <- uncons $ getFunArgs expr
  (b, _) <- uncons r1
  return $ EDiv a b
parseExpr expr | isFunCall "mod" expr = do
  (a, r1) <- uncons $ getFunArgs expr
  (b, _) <- uncons r1
  return $ EMod a b
parseExpr expr | isFunCall "chr" expr = do
  (a, _) <- uncons $ getFunArgs expr
  return $ EChr a
parseExpr expr | isFunCall "ord" expr = do
  (a, _) <- uncons $ getFunArgs expr
  return $ EOrd a
parseExpr expr | isFunCall "not" expr = do
  (a, _) <- uncons $ getFunArgs expr
  return $ ENot a
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
