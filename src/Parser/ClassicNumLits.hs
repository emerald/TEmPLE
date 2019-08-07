module Parser.ClassicNumLits
  ( parseNumLit
  ) where

import Ast (Lit(LInt, LDouble))

import Parser.Common (token)

import Control.Applicative ((*>))
import Control.Monad (void)
import Data.Char (isDigit)
import Numeric (readDec, readFloat, readHex, readOct)
import Text.ParserCombinators.ReadP
  ( ReadP
  , choice, munch, munch1, satisfy, string
  , readS_to_P
  )

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
  void $ string "."
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
    , parseFractional integral
    ]

parseIntFloat :: ReadP Lit
parseIntFloat = choice
  [ parseStartingWithZero
  , parseStartingWithNonZero
  ]

parseNumLit :: ReadP Lit
parseNumLit = token $ choice
  [ parseIntFloat
  , parseOctHex
  ]
