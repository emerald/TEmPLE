module Parser.Classic.Megaparsec.Lits
  ( parseLit
  ) where

import Control.Applicative ((<|>), empty)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Ast (Lit(LNil, LSelf))
import Parser.Classic.Megaparsec.Types
import qualified Parser.Classic.Words as W

space :: Parser ()
space = L.space C.space1 empty empty

symbol :: String -> Parser String
symbol = L.symbol space

parseNil :: Parser Lit
parseNil = symbol (show W.Nil) *> return LNil

parseSelf :: Parser Lit
parseSelf = symbol (show W.Self) *> return LSelf

parseLit :: Parser Lit
parseLit = parseNil <|> parseSelf
