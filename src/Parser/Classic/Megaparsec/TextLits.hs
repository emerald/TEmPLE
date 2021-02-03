{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module       : TEmPLE.Parser.Classic.Megaparsec.TextLits
-- Description  : Parsing character and string literals
-- Copyright    : (c) Oleks Shturmov, 2020
-- License      : BSD 3-Clause (see the file LICENSE)
--
-- Maintainer   : oleks@oleks.info

module Parser.Classic.Megaparsec.TextLits
  ( parseTextLit
  ) where

import Ast ( Lit ( LChar, LString ) )

import Parser.Classic.ReadP.TextLits
  ( escSeqAny_to_C  , escSeqOct_to_C  , escSeqUp_to_C
  , isAnyChar       , isOctChar
  , isSimpleCChar   , isSimpleSChar
  )
import Util ( liftMaybe )

import Data.Text ( Text )
import Data.Void ( Void )
import Text.Megaparsec
  ( Parsec    , (<?>)
  , anySingle , between , choice
  , count     , many    , satisfy
  , try
  )
import Text.Megaparsec.Char ( string )

type Parser = Parsec Void Text

parseOctChar :: Parser Char
parseOctChar = satisfy isOctChar

parseEscSeq :: Parser Char
parseEscSeq = string "\\" *> choice
  [ try (satisfy isAnyChar >>= liftMaybe . escSeqAny_to_C)
      <?> "a valid escape sequence (e.g., \\n, \\t)"
  , string "^" *> fmap escSeqUp_to_C anySingle
  , choice
    [ try $ count 3 parseOctChar
    , try $ count 2 parseOctChar
    ,       count 1 parseOctChar
    ] >>= liftMaybe . escSeqOct_to_C <?> "1-3 octal digits"
  ] <?> "a valid escape sequence (e.g., \\n, \\^J, \\012)"

parseChar :: Parser Lit
parseChar =
  between (string "'") (string "'") $
    fmap LChar $ choice
      [ satisfy isSimpleCChar
      , parseEscSeq
      ]

parseString :: Parser Lit
parseString =
  between (string "\"") (string "\"") $
    fmap LString $ many $ choice
      [ satisfy isSimpleSChar
      , parseEscSeq
      ]

parseTextLit :: Parser Lit
parseTextLit = choice [ parseChar, parseString ]
