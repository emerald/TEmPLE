module Parser.GenClassicLits
  ( ValidLit(..)
  , InvalidLit(..)
  , isValidLit
  ) where

import Ast (Lit(..))

import Parser.GenCommon (allToken, token)

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

isValidIntegral :: String -> Bool
isValidIntegral "" = False
isValidIntegral cs = allToken isDigit cs

isValidHex :: String -> Bool
isValidHex "" = False
isValidHex cs =
  allToken (`elem` ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']) cs

isValidOctal :: String -> Bool
isValidOctal "" = False
isValidOctal cs = allToken (`elem` ['0'..'8']) cs

isValidCharLit :: Char -> String -> (Bool, String)
isValidCharLit _ "" = (False, "")
isValidCharLit e [c] = (c == e, "")
isValidCharLit _ ('\\':'^':_:cs) = (True, cs)
isValidCharLit _ ('\\':c0:c1:c2:cs)
  | isOctDigit c0 && isOctDigit c1 && isOctDigit c2 =
    (True, cs)
isValidCharLit _ ('\\':c0:c1:cs)
  | isOctDigit c0 && isOctDigit c1 =
    (True, cs)
isValidCharLit _ ('\\':_:cs) = (True, cs)
isValidCharLit _ (_:cs) = (True, cs)

isValidStringLits :: String -> Bool
isValidStringLits ('"':cs) = cs == ""
isValidStringLits s =
  case isValidCharLit '"' s of
    (True, s') -> isValidStringLits s'
    _ -> False

isValidLit :: String -> Bool
isValidLit "" = False
isValidLit ('\'':cs) = isValidCharLit '\'' cs == (True, "'")
isValidLit ('"':cs) = isValidStringLits cs
isValidLit ('0':'.':cs) = isValidIntegral cs
isValidLit ('0':'x':cs) = isValidHex cs
isValidLit ('0':[]) = True
isValidLit ('0':cs) = isValidOctal cs
isValidLit cs = isValidIntegral cs
