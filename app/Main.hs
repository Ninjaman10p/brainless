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
  , _vars :: M.Map Variable Int
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
 - Parse AST to brainf
 ---------------------}



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
