-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.Gen.TextLits
  ( ValidTextLit(..)
  , InvalidTextLit(..)
  ) where

import Ast (Lit(LChar, LString))

import Parser.Classic.TextLits
  ( escSeqAny_to_C
  , escSeqOct_to_C
  , escSeqUp_to_C
  , isSimpleCChar
  , isSimpleSChar
  )

import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary, shrink
  , choose, elements, oneof
  , listOf, suchThat, vectorOf
  )

newtype ValidTextLit
  = ValidTextLit {
      validTextLit :: (String, Lit, [ValidTextLit])
    }

instance Show ValidTextLit where
  show (ValidTextLit (s, l, _)) = s ++ " ~/~> " ++ show l

instance Eq ValidTextLit where
  (ValidTextLit (s1, l1, _)) == (ValidTextLit (s2, l2, _))
    = (s1, l1) == (s2, l2)

escChars :: [Char]
escChars = ['\'', '"', '\\', 'a', 'b', 'f', 'n', 'r', 't', 'v']

genAnySeq :: Gen (String, Char)
genAnySeq = do
  c <- elements escChars
  let Just c' = escSeqAny_to_C c
  return ('\\':[c], c')

genUpSeq :: Gen (String, Char)
genUpSeq = do
  sc <- arbitrary
  let c = escSeqUp_to_C sc
  return (['\\', '^', sc], c)

genOctSeq :: (String -> String) -> Gen (String, Char)
genOctSeq normalize = do
  n <- choose(1, 3)
  s <- vectorOf n (elements ['0'..'7'])
  let Just c' = escSeqOct_to_C s
  return ('\\' : normalize s, c')

genEscSeq :: (String -> String) -> Gen (String, Char)
genEscSeq octNormalize = oneof
  [ genAnySeq
  , genUpSeq
  , genOctSeq octNormalize
  ]

genChar :: (Char -> Bool) -> Gen (String, Char)
genChar cond = do
  c <- suchThat arbitrary cond
  return ([c], c)

genCharChar :: Gen (String, Char)
genCharChar = oneof
  [ genChar isSimpleCChar
  , genEscSeq id
  ]

octZeroPad :: String -> String
octZeroPad [] = "000"
octZeroPad (s @ [_]) = "00" ++ s
octZeroPad (s @ [_, _]) = "0" ++ s
octZeroPad s = s

genStringChar :: Gen (String, Char)
genStringChar = oneof
  [ genChar isSimpleSChar
  , genEscSeq octZeroPad
  ]

validChar :: Gen ValidTextLit
validChar = do
  (s, c) <- genCharChar
  return $ ValidTextLit
    ("'" ++ s ++ "'", LChar c, [])

shrinkChars :: [(String, Char)] -> [ValidTextLit]
shrinkChars [] = []
shrinkChars [t] = [charsToTextLit [t]]
shrinkChars ts =
  let (tsl, tsr) = splitAt ((length ts) `div` 2) ts in
  fmap charsToTextLit [tsl, tsr] ++ shrinkChars tsl ++ shrinkChars tsr

stringToTextLit :: String -> String -> [ValidTextLit] -> ValidTextLit
stringToTextLit ss cs ls =
  ValidTextLit
    ("\"" ++ ss ++ "\"", LString cs, ls)

charsToTextLit :: [(String, Char)] -> ValidTextLit
charsToTextLit [(s, c)] = stringToTextLit s [c] []
charsToTextLit ts =
  let ss = concat $ map fst ts in
  let cs = map snd ts in
  stringToTextLit ss cs (shrinkChars ts)

validString :: Gen ValidTextLit
validString = listOf genStringChar >>= return . charsToTextLit

instance Arbitrary ValidTextLit where
  arbitrary = oneof
    [ validChar
    , validString
    ]

  shrink (ValidTextLit (_, _, ts)) = ts


newtype InvalidTextLit
  = InvalidTextLit { invalidTextLit :: String }
  deriving (Eq, Show)

instance Arbitrary InvalidTextLit where
  arbitrary = fmap InvalidTextLit $ oneof
    [ return "'"
    , return "\""
    ]
