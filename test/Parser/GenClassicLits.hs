module Parser.GenClassicLits
  ( ValidLit(..)
  , InvalidLit(..)
  ) where

import Ast (Lit(..))

import Parser.GenCommon (token)

import Control.Applicative (liftA2)
import Data.Char (isDigit, isOctDigit)
import Numeric (showFFloat, showHex, showInt, showOct)
import Test.Tasty.QuickCheck
  ( Arbitrary, Gen, getNonNegative
  , arbitrary, choose, elements, listOf, oneof
  , shuffle, sized, suchThat
  )

newtype ValidLit
  = ValidLit { validLit :: (String, Lit) }
  deriving (Eq, Show)

validNil :: Gen (String, Lit)
validNil = return ("nil", LNil)

validBool :: Gen (String, Lit)
validBool = elements
  [ ("true", LBool True)
  , ("false", LBool False)
  ]

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

validNum :: Gen (String, Lit)
validNum = oneof
  [ validFloat
  , validHex
  , validInt
  , validOct
  ]

instance Arbitrary ValidLit where
  arbitrary = do
    (sl, l) <- oneof
      [ validNil
      , validBool
      , validNum
      ]
    s <- token sl
    return $ ValidLit (s, l)

newtype InvalidLit
  = InvalidLit { invalidLit :: String }
  deriving (Eq, Show)

instance Arbitrary InvalidLit where
  arbitrary = fmap InvalidLit $ oneof
    [ return "09"
    , return "0xx"
    ]