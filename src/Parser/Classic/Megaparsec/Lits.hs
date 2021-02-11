module Parser.Classic.Megaparsec.Lits
  ( parseLit
  ) where

import Control.Applicative ( (<|>) )

import Ast ( Lit( LNil, LSelf ) )
import Parser.Classic.Megaparsec.Base ( word )
import Parser.Classic.Megaparsec.Types ( Parser )
import qualified Parser.Classic.Words as W
  ( Literals( Nil, Self ) )

parseNil :: Parser Lit
parseNil = word [(show W.Nil, LNil)]

parseSelf :: Parser Lit
parseSelf = word [(show W.Self, LSelf)]

parseLit :: Parser Lit
parseLit = parseNil <|> parseSelf
