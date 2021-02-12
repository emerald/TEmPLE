module Parser.Classic.Megaparsec.Lits
  ( parseLit
  ) where

import Data.Foldable ( asum )

import Ast ( Lit( LNil, LSelf, LBool ) )

import Parser.Classic.Megaparsec.Base ( word )
import Parser.Classic.Megaparsec.Types ( Parser )
import Parser.Classic.Megaparsec.NumLits ( parseNumLit )
import Parser.Classic.Megaparsec.TextLits ( parseTextLit )

import qualified Parser.Classic.Words as W
  ( Literals( Nil, Self, True, False ) )

parseNil :: Parser Lit
parseNil = word [(show W.Nil, LNil)]

parseSelf :: Parser Lit
parseSelf = word [(show W.Self, LSelf)]

parseBool :: Parser Lit
parseBool = word
  [ (show W.True,  LBool True )
  , (show W.False, LBool False )
  ]

parseLit :: Parser Lit
parseLit = asum
  [ parseNil
  , parseSelf
  , parseBool
  , parseNumLit
  , parseTextLit
  ]
