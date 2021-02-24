-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.TextLits
  ( escSeqAny_to_C
  , escSeqOct_to_C
  , escSeqUp_to_C
  , isAnyChar
  , isOctChar
  , isSimpleCChar
  , isSimpleSChar
  , parseTextLit
  ) where

import Ast (Lit(LChar, LString))

import Parser.Utils.ReadP (token)
import Parser.Classic.TextLits
  ( escSeqAny_to_C
  , escSeqOct_to_C
  , escSeqUp_to_C
  , isAnyChar
  , isOctChar
  , isSimpleCChar
  , isSimpleSChar
  )

import Util ( liftMaybe )

import Text.ParserCombinators.ReadP
  ( ReadP
  , (<++)
  , between, choice, count, get, many, satisfy, string
  )

parseOctChar :: ReadP Char
parseOctChar = satisfy (`elem` ['0'..'7'])

parseEscSeq :: ReadP Char
parseEscSeq = string "\\" *> choice
  [ satisfy isAnyChar >>= liftMaybe . escSeqAny_to_C
  , string "^" *> fmap escSeqUp_to_C get
  , ((count 3 parseOctChar)
    <++ (count 2 parseOctChar)
    <++ (count 1 parseOctChar)
    ) >>= liftMaybe . escSeqOct_to_C
  ]

parseChar :: ReadP Lit
parseChar =
  between (string "'") (string "'") $
    fmap LChar $ choice
      [ satisfy isSimpleCChar
      , parseEscSeq
      ]

parseString :: ReadP Lit
parseString =
  between (string "\"") (string "\"") $
    fmap LString $ many $ choice
      [ satisfy isSimpleSChar
      , parseEscSeq
      ]

parseTextLit :: ReadP Lit
parseTextLit = token $ choice
  [ parseChar
  , parseString
  ]
