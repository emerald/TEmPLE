module Parser.Classic.Gen.NumLits
  ( ValidNumLit(..)
  , InvalidNumLit(..)
  ) where

import Ast (Lit(LInt, LDouble))

import Numeric (showFFloat, showHex, showInt, showOct)
import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , getNonNegative
  , arbitrary, oneof
  )

newtype ValidNumLit
  = ValidNumLit { validNumLit :: (String, Lit) }
  deriving (Eq, Show)

validFloat :: Gen (String, Lit)
validFloat = do
  x <- fmap getNonNegative arbitrary
  return (showFFloat Nothing x "", LDouble x)

validHex :: Gen (String, Lit)
validHex = do
  x <- fmap getNonNegative arbitrary
  return ("0x" ++ showHex x "", LInt x)

validInt :: Gen (String, Lit)
validInt = do
  x <- fmap getNonNegative arbitrary
  return (showInt x "", LInt x)

validOct :: Gen (String, Lit)
validOct = do
  x <- fmap getNonNegative arbitrary
  return ("0" ++ showOct x "", LInt x)

instance Arbitrary ValidNumLit where
  arbitrary = fmap ValidNumLit $ oneof
    [ validFloat
    , validHex
    , validInt
    , validOct
    ]

newtype InvalidNumLit
  = InvalidNumLit { invalidNumLit :: String }
  deriving (Eq, Show)

instance Arbitrary InvalidNumLit where
  arbitrary = fmap InvalidNumLit $ oneof
    [ return "09"
    , return "0xx"
    ]
