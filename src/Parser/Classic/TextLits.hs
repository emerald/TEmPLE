-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.TextLits
  ( escSeqAny_to_C
  , escSeqOct_to_C
  , escSeqUp_to_C
  , isAnyChar
  , isOctChar
  , isSimpleCChar
  , isSimpleSChar
  ) where

import Data.Bits (clearBit)
import Data.Char (chr, ord, readLitChar)
import Numeric (readOct)

isAnyChar :: Char -> Bool
isAnyChar = not . (`elem` ('^':['0'..'7']))

isOctChar :: Char -> Bool
isOctChar = (`elem` ['0'..'7'])

escSeqAny_to_C :: Char -> Maybe Char
escSeqAny_to_C c =
  case (readLitChar . ('\\':)) [c] of
    [(c', "")] -> Just c'
    _ -> Nothing

escSeqUp_to_C :: Char -> Char
escSeqUp_to_C =
  chr . (`clearBit` 7) . (`clearBit` 6) . ord

escSeqOct_to_C :: String -> Maybe Char
escSeqOct_to_C s =
  case readOct s of
    [(c, "")] -> Just $ chr c
    _ -> Nothing

isSimpleCChar :: Char -> Bool
isSimpleCChar = not . flip elem ['\\', '\'']

isSimpleSChar :: Char -> Bool
isSimpleSChar = not . flip elem ['\\', '\"']
