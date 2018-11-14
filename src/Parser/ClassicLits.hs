module Parser.ClassicLits
  ( parseLit
  ) where

import Ast (Lit(..))

import Parser.Common (token, word)

import Control.Applicative ((*>))
import Data.Char (isDigit)
import Numeric (readDec, readFloat, readHex, readOct)
import Text.ParserCombinators.ReadP
  ( ReadP
  , choice, munch, satisfy, string
  , readS_to_P
  )

parseNil :: ReadP Lit
parseNil = word [("nil", LNil)]

parseBool :: ReadP Lit
parseBool = word
  [ ("true", LBool True)
  , ("false", LBool False)
  ]

parseHex :: ReadP Lit
parseHex = fmap LInt $
  string "0x" *> readS_to_P readHex

parseOct :: ReadP Lit
parseOct = fmap LInt $
  string "0" *> readS_to_P readOct

parseIntFloat :: ReadP Lit
parseIntFloat = do
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

parseNum :: ReadP Lit
parseNum = choice
  [ parseIntFloat
  , parseHex
  , parseOct
  ]

parseLit :: ReadP Lit
parseLit = token $ choice
  [ parseNil
  , parseBool
  , parseNum
  ]
