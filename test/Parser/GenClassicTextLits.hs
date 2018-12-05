module Parser.GenClassicTextLits
  ( ValidTextLit(..)
  , InvalidTextLit(..)
  ) where

import Ast (Lit(LChar, LString))

import Parser.ClassicTextLits
  ( isSimpleCChar
  , isSimpleSChar
  )

import Numeric (showOct)
import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , getNonNegative
  , arbitrary, oneof, listOf, suchThat
  )

newtype ValidTextLit
  = ValidTextLit { validTextLit :: (String, Lit) }
  deriving (Eq, Show)

genCharChar :: Gen (String, Char)
genCharChar = oneof
  [ do
      c <- suchThat arbitrary isSimpleCChar
      return ([c], c)
  ]

genStringChar :: Gen (String, Char)
genStringChar = oneof
  [ do
      c <- suchThat arbitrary isSimpleSChar
      return ([c], c)
  ]

validChar :: Gen (String, Lit)
validChar = do
  (s, c) <- genCharChar
  return ("'" ++ s ++ "'", LChar c)

validString :: Gen (String, Lit)
validString = do
  ts <- listOf genStringChar
  let ss = concat $ map fst ts
  let cs = map snd ts
  return ("\"" ++ ss ++ "\"", LString cs)

instance Arbitrary ValidTextLit where
  arbitrary = fmap ValidTextLit $ oneof
    [ validChar
    , validString
    ]

newtype InvalidTextLit
  = InvalidTextLit { invalidTextLit :: String }
  deriving (Eq, Show)

instance Arbitrary InvalidTextLit where
  arbitrary = fmap InvalidTextLit $ oneof
    [ return "'"
    , return "\""
    ]
