module Parser.Classic.Megaparsec.Lits
  ( parseLit
  ) where

import Control.Applicative ( (<|>) )

import Ast ( Lit( LNil, LSelf ) )
import Parser.Classic.Megaparsec.Base ( symbol )
import Parser.Classic.Megaparsec.Types ( Parser )
import qualified Parser.Classic.Words as W
  ( Literals( Nil, Self ) )

parseNil :: Parser Lit
parseNil = symbol (show W.Nil) *> return LNil

parseSelf :: Parser Lit
parseSelf = symbol (show W.Self) *> return LSelf

parseLit :: Parser Lit
parseLit = parseNil <|> parseSelf
