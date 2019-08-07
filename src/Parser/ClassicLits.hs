module Parser.ClassicLits
  ( parseLit
  ) where

import Ast (Lit(LNil, LBool))

import Parser.Common (word)
import Parser.ClassicWords (WLits(..))
import Parser.ClassicNumLits (parseNumLit)
import Parser.ClassicTextLits (parseTextLit)

import Text.ParserCombinators.ReadP (ReadP, choice)

parseNil :: ReadP Lit
parseNil = word [(show WNil, LNil)]

parseBool :: ReadP Lit
parseBool = word
  [ (show WTrue,  LBool True  )
  , (show WFalse, LBool False )
  ]

parseLit :: ReadP Lit
parseLit = choice
  [ parseNil
  , parseBool
  , parseNumLit
  , parseTextLit
  ]
