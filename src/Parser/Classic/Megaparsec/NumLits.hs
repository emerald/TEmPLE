module Parser.Classic.Megaparsec.NumLits
  ( parseIntLit
  , parseNumLit
  ) where

import Ast ( Lit ( LInt, LDouble ) )

import Control.Monad ( void )

import Data.Char ( isDigit, isHexDigit, isOctDigit )
import Data.Foldable ( asum )

import Numeric ( readDec, readFloat, readHex, readOct )

import Parser.Classic.Megaparsec.Types ( Parser )

import Text.Megaparsec ( (<?>), many, satisfy, some )
import Text.Megaparsec.Char ( char )

parseHexDigit :: Parser Char
parseHexDigit = satisfy isHexDigit <?>
  "a hexadecimal digit (0|1|..|9|a|b|..|f|A|B|..|F)"

parseOctDigit :: Parser Char
parseOctDigit = satisfy isOctDigit <?>
  "an octal digit (0|1|..|7|)"

parseIntegralDigit :: Parser Char
parseIntegralDigit = satisfy isDigit <?>
  "an integral digit (0|1|..|9)"

parseOctHex :: Parser Int
parseOctHex = asum
  [ char 'x' >> some parseHexDigit >>= hexToInt
  , some parseOctDigit >>= octToInt
  ]

parseFractional :: String -> Parser Lit
parseFractional integral = do
  void $ char '.'
  fractional <- some parseIntegralDigit
  let float = integral ++ "." ++ fractional
  case readFloat float of
    [(value, "")] ->
      let shown = show value in
      if shown == float
      then return $ LDouble value
      else fail $ "invalid float literal; " ++
        "the best approximate representation of this value is " ++
        shown
    _ -> fail "invalid float literal"

parseIntegral :: Parser String
parseIntegral = do
  first <- satisfy (`elem` ['1'..'9']) <?>
        "an integral digit between 1 and 9"
  rest <- many parseIntegralDigit
  return $ first:rest

parseIntLit :: Parser Int
parseIntLit = asum
  [ char '0' >> asum [ parseOctHex, return 0 ]
  , parseIntegral >>= integralToInt
  ]

integralToInt :: String -> Parser Int
integralToInt s = fmap (fst . head . readDec) $
  if length s < 19
  then return s
  else fail "integer literal is too long"

octToInt :: String -> Parser Int
octToInt s = fmap (fst . head . readOct) $
  if length s < 22
  then return s
  else fail "octal literal is too long"

hexToInt :: String -> Parser Int
hexToInt s = fmap (fst . head . readHex) $
  if length s < 15
  then return s
  else fail "hex literal is too long"

parseStartingWithZero :: Parser Lit
parseStartingWithZero = do
  char '0' >> asum
    [ asum
      [ fmap LInt parseOctHex
      , parseFractional "0"
      ]
    , return $ LInt 0
    ]

parseStartingWithNonZero :: Parser Lit
parseStartingWithNonZero = do
  integral <- parseIntegral
  asum
    [ parseFractional integral
    , fmap LInt $ integralToInt integral
    ]

parseNumLit :: Parser Lit
parseNumLit = asum
  [ parseStartingWithZero
  , parseStartingWithNonZero
  ]
