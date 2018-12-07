module Parser.GenClassicLits
  ( ValidLit(..)
  , InvalidLit(..)
  ) where

import Ast (Lit(..))

import Parser.GenCommon (token)
import Parser.GenClassicNumLits
import Parser.GenClassicTextLits

import Control.Applicative (liftA2)
import Data.Char (isDigit, isOctDigit)
import Numeric (showFFloat, showHex, showInt, showOct)
import Test.Tasty.QuickCheck
  ( Arbitrary, Gen, getNonNegative
  , arbitrary, shrink
  , choose, elements, frequency
  , listOf, oneof, shuffle, sized, suchThat
  )

newtype ValidLit
  = ValidLit
    { validLit :: (String, Lit, [ValidLit])
    }
  deriving (Eq, Show)

genValidNil :: Gen (String, Lit, [ValidLit])
genValidNil = return ("nil", LNil, [])

genValidBool :: Gen (String, Lit, [ValidLit])
genValidBool = elements
  [ ("true", LBool True, [])
  , ("false", LBool False, [])
  ]

genValidNumLit :: Gen (String, Lit, [ValidLit])
genValidNumLit = do
  (s, l) <- fmap validNumLit arbitrary
  return (s, l, [])

textLit_to_Lit :: ValidTextLit -> ValidLit
textLit_to_Lit (ValidTextLit (s, l, sls))
  = ValidLit (s, l, error $ show $ length sls)--map textLit_to_Lit sls)

genValidTextLit :: Gen (String, Lit, [ValidLit])
genValidTextLit = fmap (validLit . textLit_to_Lit) arbitrary

instance Arbitrary ValidLit where
  arbitrary = do
    (sl, l, sls) <- frequency
      [ (10, genValidNil)
      , (10, genValidBool)
      , (40, genValidNumLit)
      , (40, genValidTextLit)
      ]
    s <- token sl
    return $ ValidLit (s, l, sls)

newtype InvalidLit
  = InvalidLit { invalidLit :: String }
  deriving (Eq, Show)

instance Arbitrary InvalidLit where
  arbitrary =
    fmap (InvalidLit . invalidNumLit) arbitrary
