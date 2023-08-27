{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter (prettyPrint, PrettyPrintStyle (..)) where

import           Data.Text (Text)
import qualified Data.Text as T

data PrettyPrintStyle = BlockStyle Int
                      | CircleStyle Int
                      | DiscStyle Int
                      | TemplateStyle Text
                      | NoStyle
  deriving Show

prettyPrint :: PrettyPrintStyle -> Text -> Text
prettyPrint _ "" = ""
prettyPrint (DiscStyle radius) cs = printDisc radius cs
prettyPrint (CircleStyle radius) cs = printCircle radius cs
prettyPrint (BlockStyle width) cs = printTemplate (T.pack (replicate width '.') <> "\n") cs
prettyPrint (TemplateStyle template) cs' = printTemplate template cs'
prettyPrint NoStyle cs = cs

printDisc :: Int -> Text -> Text
printDisc radius cs = 
  let xA = [-2*radius .. 2*radius]
      yA = [-radius .. radius]
      coords = fmap (mapM (,) xA) yA :: [[(Int, Int)]]
      two = 2 :: Int
      p (x, y) =
        if x^two + (2*y)^two <= (2*radius)^two
          then '.' else ' '
      template = (T.unlines . fmap (T.pack . fmap p) $ coords) <> "\n"
  in printTemplate template cs

printTemplate :: Text -> (Text -> Text)
printTemplate template = changeText (_templatePrint [])
  where stemplate = T.unpack template
        _templatePrint _ ""          = ""
        _templatePrint (' ':ts) cs   = ' ':_templatePrint ts cs
        _templatePrint ('\n':ts) cs  = '\n' : _templatePrint ts cs
        _templatePrint (_:ts) (c:cs) = c:_templatePrint ts cs
        _templatePrint [] cs         = _templatePrint stemplate cs

printCircle :: Int -> Text -> Text
printCircle radius cs =
  let xA = [-2*radius .. 2*radius]
      yA = [-radius .. radius] :: [Int]
      two = 2 :: Int
      coords = fmap (mapM (,) xA) yA :: [[(Int, Int)]]
      p (x, y) =
        if x^two + (2*y)^two <= (2*radius)^two && 2*(x^two + (2*y)^two) >= (2*radius)^two
          then '.' else ' '
      template = (T.unlines . fmap (T.pack . fmap p) $ coords) <> "\n"
  in printTemplate template cs

changeText :: (String -> String) -> Text -> Text
changeText f = T.pack . f . T.unpack
