module Parser.ClassicLits
  ( parseLit
  ) where

import Ast (Lit(LNil, LBool))

import Parser.Common (token, word)
import Parser.ClassicNumLits (parseNumLit)
import Parser.ClassicTextLits (parseTextLit)

import Text.ParserCombinators.ReadP (ReadP, choice)

parseNil :: ReadP Lit
parseNil = word [("nil", LNil)]

parseBool :: ReadP Lit
parseBool = word
  [ ("true", LBool True)
  , ("false", LBool False)
  ]

parseLit :: ReadP Lit
parseLit = token $ choice
  [ parseNil
  , parseBool
  , parseNumLit
  , parseTextLit
  ]
