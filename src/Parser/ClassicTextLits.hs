module Parser.ClassicTextLits
  ( escSeqAny_to_C
  , escSeqOct_to_C
  , escSeqUp_to_C
  , isAnyChar
  , isSimpleCChar
  , isSimpleSChar
  , parseTextLit
  ) where

import Ast (Lit(LChar, LString))

import Control.Applicative ((*>))
import Data.Bits (clearBit)
import Data.Char (chr, ord, readLitChar)
import Numeric (readOct)
import Text.ParserCombinators.ReadP
  ( ReadP
  , between, choice, get, many, munch1, satisfy, string
  , pfail
  )

isAnyChar :: Char -> Bool
isAnyChar = not . (`elem` ('^':['0'..'7']))

escSeqAny_to_C :: Char -> ReadP Char
escSeqAny_to_C c =
  case (readLitChar . ('\\':)) [c] of
    [(c', "")] -> return c
    _ -> pfail

escSeqUp_to_C :: Char -> Char
escSeqUp_to_C =
  chr . (`clearBit` 7) . (`clearBit` 6) . ord

escSeqOct_to_C :: String -> ReadP Char
escSeqOct_to_C s =
  case readOct s of
    [(c, "")] -> return $ chr c
    _ -> pfail

parseEscSeq :: ReadP Char
parseEscSeq = string "\\" *> choice
  [ satisfy isAnyChar >>= escSeqAny_to_C
  , string "^" *> fmap escSeqUp_to_C get
  , munch1 (`elem` ['0'..'7']) >>= \ s ->
      if length s < 4
      then escSeqOct_to_C s
      else pfail
  ]

isSimpleCChar :: Char -> Bool
isSimpleCChar = not . flip elem ['\\', '\'']

isSimpleSChar :: Char -> Bool
isSimpleSChar = not . flip elem ['\\', '\"']

parseChar :: ReadP Lit
parseChar =
  between (string "'") (string "'") $
    fmap LChar $ choice
      [ satisfy isSimpleCChar
      , parseEscSeq
      ]

parseSChar :: ReadP Char
parseSChar = choice
  [ satisfy isSimpleSChar
  , parseEscSeq
  ]

parseString :: ReadP Lit
parseString =
  between (string "\"") (string "\"") $
    fmap LString $ many parseSChar

parseTextLit :: ReadP Lit
parseTextLit = choice
  [ parseChar
  , parseString
  ]
