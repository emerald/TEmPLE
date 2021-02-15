module Parser.Classic.Megaparsec.Base
  ( symbol
  , word
  , fullParse
  ) where

import Control.Applicative ( empty )
import Data.Void ( Void )
import Text.Megaparsec.Error ( ParseErrorBundle )
import Text.Megaparsec ( eof, parse )
import qualified Text.Megaparsec.Char.Lexer as L
  ( space, symbol' )
import qualified Text.Megaparsec.Char as C
  ( space1 )
import Data.Foldable ( asum )

import Parser.Classic.Megaparsec.Types ( Parser )

space :: Parser ()
space = L.space C.space1 empty empty

symbol :: String -> Parser String
symbol = L.symbol' space

word :: [(String, a)] -> Parser a
word = asum . map (\(w, a) -> symbol w *> return a)

fullParse :: Parser a -> String -> Either (ParseErrorBundle String Void) a
fullParse p s = parse (p <* eof) "" s
