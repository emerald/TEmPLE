module Parser.Classic.ReadP.NumLits
  ( parseIntLit
  , parseNumLit
  ) where

import Ast (Lit(LInt, LDouble))

import Parser.Common (token)

import Control.Monad (void)
import Data.Char (isDigit)
import Numeric (readDec, readFloat, readHex, readOct)
import Text.ParserCombinators.ReadP
  ( ReadP, (<++)
  , choice, munch, munch1, satisfy, string
  , readS_to_P
  )

parseOctHex :: ReadP Int
parseOctHex = choice [parseOct, parseHex]
  where
    parseHex :: ReadP Int
    parseHex = string "x" *> readS_to_P readHex

    parseOct :: ReadP Int
    parseOct = readS_to_P readOct

parseFractional :: String -> ReadP Lit
parseFractional integral = do
  void $ string "."
  fractional <- munch1 isDigit
  let float = integral ++ "." ++ fractional
  return $ (LDouble . fst . head) $ readFloat float

parseStartingWithZero :: ReadP Lit
parseStartingWithZero = do
  string "0" >> choice
    [ fmap LInt parseOctHex
    , parseFractional "0"
    ] <++ return (LInt 0)

parseStartingWithNonZero :: ReadP Lit
parseStartingWithNonZero = do
  integral <- parseIntegral
  choice
    [ return $ (LInt . integralToInt) integral
    , parseFractional integral
    ]

parseIntegral :: ReadP String
parseIntegral = do
  first <- satisfy (`elem` ['1'..'9'])
  rest <- munch isDigit
  return $ first:rest

integralToInt :: String -> Int
integralToInt = fst . head . readDec

parseIntLit :: ReadP Int
parseIntLit = token $ choice
  [ string "0" >> parseOctHex <++ return 0
  , fmap integralToInt parseIntegral
  ]

parseNumLit :: ReadP Lit
parseNumLit = token $ choice
  [ parseStartingWithZero
  , parseStartingWithNonZero
  ]
