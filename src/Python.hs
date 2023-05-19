{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Python (Instruction (..), Program, Expression (..), Variable, expr, variable, file, parsePython, instruction) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.State.Class
import           Data.Foldable             (asum)
import           Data.Functor
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Text.Parsec               hiding (State, many, space, spaces,
                                            (<|>), optional)

type PythonParser a = ParsecT Text () (State (Int, Bool)) a

parsePython :: PythonParser a -> SourceName -> Text -> Either ParseError a
parsePython p n s = evalState (runParserT p () n s) (0, False)

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
space = oneOf " "

spaces :: PythonParser ()
spaces = void $ many space

factor :: PythonParser Expression
factor = asum $ try <$>
  [ char '(' *> spaces *> expr <* spaces <* char ')'
  , ENot <$> (string "not" *> spaces *> factor)
  , EBool <$> ((string "True" $> True) <|> (string "False" $> False))
  , ENum <$> int
  , EVar <$> variable
  , EInput <$> (string "input(" *> optional expr <* string ")")
  , EOrd <$> (string "ord(" *> expr <* string ")")
  , EChr <$> (string "chr(" *> expr <* string ")")
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

file :: PythonParser [Instruction]
file = many instruction <* spaces <* eof

indentation :: PythonParser ()
indentation = do
  (n, changing) <- get
  forM_ [1..n] (const space)
  when changing $ do
    newSpaces <- try $ length <$> some space
    put (n + newSpaces, False)

indented :: PythonParser a -> PythonParser a
indented p = do
  _ <- endOfLine
  n <- gets fst
  modify (second (const True))
  p <* put (n, False)

instruction :: PythonParser Instruction
instruction = (indentation *>) . asum $ try <$>
    [ Set <$> variable <* spaces <* char '=' <* spaces <*> expr <* (void (some endOfLine) <|> eof)
    , block "while" While
    , block "if" If
    , Print <$> (string "print(" *> spaces *> expr <* spaces <* char ')')
    ]

block :: String -> (Expression -> [Instruction] -> b) -> PythonParser b
block s f = f <$> (string s *> spaces *> expr <* spaces <* char ':' <* spaces) <*> indented (some $ try instruction)

operator :: (Expression -> b) -> String -> PythonParser b
operator f op = try $ f <$> (string op *> spaces *> expr)
