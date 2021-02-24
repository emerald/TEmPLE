module Parser.Classic.Megaparsec.Base
  ( commaList
  , symbol, symbol1
  , stoken1
  , token, token1
  , word, word1
  , fullParse
  ) where

import Control.Monad ( void )
import Control.Applicative ( empty, liftA2 )
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Data.Void ( Void )
import Text.Megaparsec.Error ( ParseErrorBundle )
import Text.Megaparsec ( eof, many, parse )
import qualified Text.Megaparsec.Char.Lexer as L
  ( lexeme, space, symbol' )
import qualified Text.Megaparsec.Char as C
  ( space, space1 )
import Data.Foldable ( asum )

import Parser.Classic.Megaparsec.Types ( Parser )

space1 :: Parser ()
space1 = L.space C.space1 empty empty

space :: Parser ()
space = L.space C.space empty empty

symbol :: String -> Parser String
symbol = L.symbol' space

symbol1 :: String -> Parser String
symbol1 = L.symbol' space1

token :: Parser a -> Parser a
token = L.lexeme space

token1 :: Parser a -> Parser a
token1 = L.lexeme space1

stoken1 :: String -> Parser ()
stoken1 = void . symbol1

word :: [(String, a)] -> Parser a
word = asum . map (\(w, a) -> symbol w *> return a)

word1 :: [(String, a)] -> Parser a
word1 = asum . map (\(w, a) -> symbol1 w *> return a)

commaList :: Parser a -> Parser (NonEmpty a)
commaList p = liftA2 (:|) p (many (symbol "," *> p))

fullParse :: Parser a -> String -> Either (ParseErrorBundle String Void) a
fullParse p s = parse (p <* eof) "" s
