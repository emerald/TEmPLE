module Parser.Classic.Lits
  ( parseLit
  ) where

import Ast (Lit(LNil, LBool, LSelf))

import Parser.Common (word)
import Parser.Classic.NumLits (parseNumLit)
import Parser.Classic.TextLits (parseTextLit)

import qualified Parser.Classic.Words as W
  ( Literals(..) )

import Text.ParserCombinators.ReadP (ReadP, choice)

parseNil :: ReadP Lit
parseNil = word [(show W.Nil, LNil)]

parseSelf :: ReadP Lit
parseSelf = word [(show W.Self, LSelf)]

parseBool :: ReadP Lit
parseBool = word
  [ (show W.True,  LBool True  )
  , (show W.False, LBool False )
  ]

parseLit :: ReadP Lit
parseLit = choice
  [ parseNil
  , parseSelf
  , parseBool
  , parseNumLit
  , parseTextLit
  ]
