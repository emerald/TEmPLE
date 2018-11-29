module Parser.ClassicLits
  ( parseLit
  ) where

import Ast (Lit(..))

import Parser.Common (token, word)

import Control.Applicative ((*>), (<*))
import Data.Bits (clearBit)
import Data.Char (chr, isDigit, ord, readLitChar)
import Numeric (readDec, readFloat, readHex, readOct)
import Text.ParserCombinators.ReadP
  ( ReadP
  , between, choice, get, many, munch, munch1, satisfy, string
  , pfail, readS_to_P
  )

parseNil :: ReadP Lit
parseNil = word [("nil", LNil)]

parseBool :: ReadP Lit
parseBool = word
  [ ("true", LBool True)
  , ("false", LBool False)
  ]

parseOctHex :: ReadP Lit
parseOctHex
  = string "0" *> choice [parseOct, parseHex]
    where
      parseHex :: ReadP Lit
      parseHex = fmap LInt $
        string "x" *> readS_to_P readHex

      parseOct :: ReadP Lit
      parseOct = fmap LInt $
        readS_to_P readOct

parseFractional :: String -> ReadP Lit
parseFractional integral = do
  string "."
  fractional <- munch1 isDigit
  let float = integral ++ "." ++ fractional
  return $ (LDouble . fst . head) $ readFloat float

parseStartingWithZero :: ReadP Lit
parseStartingWithZero = do
  string "0" >> choice
    [ return (LInt 0)
    , parseFractional "0"
    ]

parseStartingWithNonZero :: ReadP Lit
parseStartingWithNonZero = do
  first <- satisfy (`elem` ['1'..'9'])
  rest <- munch isDigit
  let integral = first:rest
  choice
    [ return $ (LInt . fst . head) $ readDec integral
    , do
      string "."
      fractional <- munch isDigit
      let float = integral ++ "." ++ fractional
      return $ (LDouble . fst . head) $ readFloat float
    ]

parseIntFloat :: ReadP Lit
parseIntFloat = choice
  [ parseStartingWithZero
  , parseStartingWithNonZero
  ]

parseNum :: ReadP Lit
parseNum = choice
  [ parseIntFloat
  , parseOctHex
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
  , parseNum
  , parseChar
  , parseString
  ]
