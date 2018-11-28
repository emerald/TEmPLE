module Parser.GenCommon
  ( allToken
  , invalidOp, invalidOp1
  , spaces, spaces1
  , token, token1
  , validOp, validOp1
  ) where

import Data.Char (isSpace)
import Test.Tasty.QuickCheck
  ( Gen
  , arbitrary, elements, listOf, listOf1, oneof, suchThat
  )

spaceChar :: Gen Char
spaceChar = elements [' ', '\t', '\n', '\r', '\f', '\v']

spaces :: Gen String
spaces = listOf spaceChar

spaces1 :: Gen String
spaces1 = listOf1 spaceChar

token :: String -> Gen String
token s = fmap (s ++) spaces

token1 :: String -> Gen String
token1 s = fmap (s ++) spaces1

validOp :: String -> Gen String
validOp = token

invalidOp :: String -> Gen String
invalidOp s = suchThat arbitrary (/= s) >>= token

validOp1 :: String -> Gen String
validOp1 = token1

invalidOp1 :: String -> Gen String
invalidOp1 s = oneof
  [ return s
  , suchThat arbitrary (/= s) >>= token1
  ]

allToken :: (Char -> Bool) -> String -> Bool
allToken _ "" = True
allToken f s @ (c:cs) = (f c && allToken f cs) || all isSpace s
