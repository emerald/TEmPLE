module Parser.Classic.Megaparsec.Base
  ( symbol
  , word
  ) where

import Control.Applicative ( empty )
import qualified Text.Megaparsec.Char.Lexer as L
  ( space, symbol )
import qualified Text.Megaparsec.Char as C
  ( space1 )
import Data.Foldable ( asum )

import Parser.Classic.Megaparsec.Types ( Parser )

space :: Parser ()
space = L.space C.space1 empty empty

symbol :: String -> Parser String
symbol = L.symbol space

word :: [(String, a)] -> Parser a
word = asum . map (\(w, a) -> symbol w *> return a)
