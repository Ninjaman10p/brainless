{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Python (Instruction (..), Program, Expression (..), Variable, expr, variable, file, parsePython, instruction) where

import           Control.Applicative
import           Control.Lens              hiding (noneOf)
import           Control.Monad
import           Control.Monad.State
import           Data.Foldable             (asum)
import           Data.Functor
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Text.Parsec               hiding (State, many, optional, space,
                                            spaces, (<|>))

type PythonParser a = ParsecT Text () (State ParserState) a

data ParserState = PS
  { _indentLevel  :: Int
  , _isIndenting  :: Bool
  , _inExpression :: Bool
  }
$(makeLenses 'PS)

parsePython :: PythonParser a -> SourceName -> Text -> Either ParseError a
parsePython p n s = evalState (runParserT p () n s) $ PS 0 False False

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
                | EGeq Expression Expression
                | ELeq Expression Expression
                | EGt Expression Expression
                | ELt Expression Expression
                | EEq Expression Expression
                | EAnd Expression Expression
                | EOr Expression Expression
                | EBool Bool
  deriving (Show, Eq, Ord)

data Instruction = Print Expression
             | Set Variable Expression
             | While Expression [Instruction]
             | If Expression [Instruction]
  deriving (Show, Eq, Ord)

type Variable = Text

type Program = [Instruction]

variable :: PythonParser Variable
variable = T.pack <$> ((:) <$> (char '_' <|> letter) <*> many (char '_' <|> letter <|> digit))

int :: Stream s m Char => ParsecT s u m Int
int = read <$> some digit

space :: PythonParser Char
space = oneOf " " <|> (string "\\\n" $> '\n')

comment :: PythonParser String
comment = string "#" *> manyTill anyChar endOfLine

spaces :: PythonParser ()
spaces = void $ many (void comment <|> void space)

factor :: PythonParser Expression
factor = asum $ try <$>
  [ char '(' *> spaces *> expr <* spaces <* char ')'
  , EOrd <$> (string "ord(" *> spaces *> expr <* spaces <* string ")")
  , EChr <$> (string "chr(" *> spaces *> expr <* spaces <* string ")")
  , ENot <$> (string "not" *> spaces *> factor)
  , EBool <$> (try (string "True" $> True) <|> (string "False" $> False))
  , ENum <$> int
  , EInput <$> (string "input(" *> optional expr <* string ")")
  , EString <$> stringLiteral
  , EVar <$> variable
  ]

expr :: PythonParser Expression
expr = do
  a <- factor <* spaces
  asum $ try <$>
    [ operator (EMul a) "*"
    , operator (EDiv a) "//"
    , operator (EAdd a) "+"
    , operator (ESub a) "-"
    , operator (EMod a) "%"
    , operator (EEq a) "=="
    , operator (EGt a) ">"
    , operator (ELt a) "<"
    , operator (EGeq a) ">="
    , operator (ELeq a) "<="
    , operator (EAnd a) "and"
    , operator (EOr a) "or"
    , return a
    ]

stringLiteral :: PythonParser Text
stringLiteral = fmap T.pack $
  (try (string "\"\"\"") *> many ((string "\\\"" $> '"') <|> noneOf "\"") <* string "\"\"\"")
  <|> (char '\'' *> many ((string "'" $> '\'') <|> noneOf "\"\n") <* char '\'')
  <|> (char '"' *> many ((string "\\\"" $> '"') <|> noneOf "\"\n") <* char '"')

file :: PythonParser [Instruction]
file = many (many (void comment  <|> void endOfLine) *> instruction) <* spaces <* eof

indentation :: PythonParser ()
indentation = do
  PS {_indentLevel = n, _isIndenting = changing} <- get
  forM_ [1..n] (const space)
  when changing $ do
    newSpaces <- try $ length <$> some space
    modify $ over indentLevel (+newSpaces)
    modify $ set isIndenting False

lock :: MonadState s m => m a -> m a
lock p = do
  a <- get
  p <* put a

indented :: PythonParser a -> PythonParser a
indented p = do
  _ <- endOfLine
  lock $ do
    modify $ set isIndenting True
    p

instruction :: PythonParser Instruction
instruction = (indentation *>) . asum $ try <$>
    [ Set <$> variable <* spaces <* char '=' <* spaces <*> expr <* eol
    , block "while" While
    , block "if" If
    , Print <$> (string "print(" *> spaces *> expr <* spaces <* char ')') <* eol
    ] where eol = spaces *> (void (some endOfLine) <|> eof)

block :: String -> (Expression -> [Instruction] -> b) -> PythonParser b
block s f = f <$> (string s *> spaces *> expr <* spaces <* char ':' <* spaces) <*> indented (some $ try instruction)

operator :: (Expression -> b) -> String -> PythonParser b
operator f op = try $ f <$> (string op *> spaces *> expr)
