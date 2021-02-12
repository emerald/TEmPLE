module Parser.GenCommon
  ( genValidInvalid
  , invalidOp, invalidOp1
  , spaces, spaces1
  , token, token1
  , validOp, validOp1
  ) where

import Test.Tasty.QuickCheck
  ( Gen
  , arbitrary, choose, elements
  , listOf, listOf1, oneof
  , shuffle, suchThat
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

genValidInvalid :: [(Gen String, Gen String)] -> Gen String
genValidInvalid validInvalid = do
  let n = length validInvalid
  n_fst <- choose (1, n)
  let n_snd = n - n_fst
  fs <- shuffle $ replicate n_fst fst ++ replicate n_snd snd
  let gens = fs <*> validInvalid
  fmap concat $ sequence gens
