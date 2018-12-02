module Parser.GenClassicLits
  ( ValidLit(..)
  , InvalidLit(..)
  ) where

import Ast (Lit(..))

import Parser.GenCommon (token)
import Parser.GenClassicNumLits

import Control.Applicative (liftA2)
import Data.Char (isDigit, isOctDigit)
import Numeric (showFFloat, showHex, showInt, showOct)
import Test.Tasty.QuickCheck
  ( Arbitrary, Gen, getNonNegative
  , arbitrary, choose, elements, frequency
  , listOf, oneof, shuffle, sized, suchThat
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

instance Arbitrary ValidLit where
  arbitrary = do
    (sl, l) <- frequency
      [ (20, validNil)
      , (20, validBool)
      , (60, fmap validNumLit arbitrary)
      ]
    s <- token sl
    return $ ValidLit (s, l)

newtype InvalidLit
  = InvalidLit { invalidLit :: String }
  deriving (Eq, Show)

instance Arbitrary InvalidLit where
  arbitrary = do
    s <- fmap invalidNumLit arbitrary
    return $ InvalidLit s
