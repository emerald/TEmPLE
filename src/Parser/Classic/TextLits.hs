module Parser.Classic.TextLits
  ( escSeqAny_to_C
  , escSeqOct_to_C
  , escSeqUp_to_C
  , isAnyChar
  , isSimpleCChar
  , isSimpleSChar
  , parseTextLit
  ) where

import Ast (Lit(LChar, LString))

import Parser.Common (token)

import Control.Applicative ((*>))
import Data.Bits (clearBit)
import Data.Char (chr, ord, readLitChar)
import Numeric (readOct)
import Text.ParserCombinators.ReadP
  ( ReadP
  , (<++)
  , between, choice, count, get, many, satisfy, string
  , pfail
  )

isAnyChar :: Char -> Bool
isAnyChar = not . (`elem` ('^':['0'..'7']))

escSeqAny_to_C :: Char -> ReadP Char
escSeqAny_to_C c =
  case (readLitChar . ('\\':)) [c] of
    [(_, "")] -> return c
    _ -> pfail

escSeqUp_to_C :: Char -> Char
escSeqUp_to_C =
  chr . (`clearBit` 7) . (`clearBit` 6) . ord

escSeqOct_to_C :: String -> ReadP Char
escSeqOct_to_C s =
  case readOct s of
    [(c, "")] -> return $ chr c
    _ -> pfail

parseOctChar :: ReadP Char
parseOctChar = satisfy (`elem` ['0'..'7'])

parseEscSeq :: ReadP Char
parseEscSeq = string "\\" *> choice
  [ satisfy isAnyChar >>= escSeqAny_to_C
  , string "^" *> fmap escSeqUp_to_C get
  , ((count 3 parseOctChar)
    <++ (count 2 parseOctChar)
    <++ (count 1 parseOctChar)
    ) >>= escSeqOct_to_C
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
