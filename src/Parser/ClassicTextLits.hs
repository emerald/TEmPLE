module Parser.ClassicTextLits
  ( parseTextLit
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

parseEscSeq :: ReadP Char
parseEscSeq = string "\\" *> choice
  [ satisfy (not . flip elem ('^':['0'..'7'])) >>= \ c -> do
      let [(c', "")] = (readLitChar . reverse . (:"\\")) c
      return c'
  , string "^" *> (flip fmap get $
      chr . (`clearBit` 7) . (`clearBit` 6). ord)
  , munch1 (`elem` ['0'..'7']) >>= \ s ->
      if length s < 4
      then do
        let [(i, "")] = readOct s
        return $ chr i
      else pfail
  ]

parseChar :: ReadP Lit
parseChar =
  between (string "'") (string "'") $
    fmap LChar $ choice
      [ satisfy (not . flip elem ['\\', '\''])
      , parseEscSeq
      ]

parseSChar :: ReadP Char
parseSChar = choice
  [ satisfy (not . flip elem ['\\', '"'])
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
