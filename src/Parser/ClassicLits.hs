module Parser.ClassicLits
  ( parseLit
  ) where

import Ast (Lit(..))

import Parser.Common (token, word)
import Parser.ClassicNumLits (parseNumLit)

import Control.Applicative ((*>), (<*))
import Data.Bits (clearBit)
import Data.Char (chr, ord, readLitChar)
import Numeric (readOct)
import Text.ParserCombinators.ReadP
  ( ReadP
  , between, choice, get, many, munch1, satisfy, string
  , pfail
  )

parseNil :: ReadP Lit
parseNil = word [("nil", LNil)]

parseBool :: ReadP Lit
parseBool = word
  [ ("true", LBool True)
  , ("false", LBool False)
  ]

parseOChar :: ReadP Char
parseOChar = satisfy (`elem` ['0'..'7'])

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

parseLit :: ReadP Lit
parseLit = token $ choice
  [ parseNil
  , parseBool
  , parseNumLit
  , parseChar
  , parseString
  ]
