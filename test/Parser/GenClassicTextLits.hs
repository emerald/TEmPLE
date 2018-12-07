module Parser.GenClassicTextLits
  ( ValidTextLit(..)
  , InvalidTextLit(..)
  ) where

import Ast (Lit(LChar, LString))

import Parser.ClassicTextLits
  ( escSeqAny_to_C
  , escSeqOct_to_C
  , escSeqUp_to_C
  , isAnyChar
  , isSimpleCChar
  , isSimpleSChar
  )
import Parser.Common (fullParse)

import Control.Monad (liftM)
import Data.Char (showLitChar)
import Numeric (showOct)
import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , getNonNegative
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
  case fullParse (escSeqAny_to_C c) "" of
    [c'] -> return ('\\':[c], c')
    _ -> fail $ show c

genUpSeq :: Gen (String, Char)
genUpSeq = do
  sc <- arbitrary
  let c = escSeqUp_to_C sc
  return (['\\', '^', sc], c)

genOctSeq :: Gen (String, Char)
genOctSeq = do
  n <- choose(1, 3)
  s <- vectorOf n (elements ['0'..'7'])
  let [c'] = fullParse (escSeqOct_to_C s) ""
  return ('\\' : s, c')

genEscSeq :: Gen (String, Char)
genEscSeq = oneof
  [ genAnySeq
  , genUpSeq
  , genOctSeq
  ]

genChar :: (Char -> Bool) -> Gen (String, Char)
genChar cond = do
  c <- suchThat arbitrary cond
  return ([c], c)

genCharChar :: Gen (String, Char)
genCharChar = oneof
  [ genChar isSimpleCChar
  , genEscSeq
  ]

genStringChar :: Gen (String, Char)
genStringChar = oneof
  [ genChar isSimpleSChar
  , genEscSeq
  ]

validChar :: Gen ValidTextLit
validChar = do
  (s, c) <- genCharChar
  return $ ValidTextLit
    ("'" ++ s ++ "'", LChar c, [])

shrink' :: [(String, Char)] -> [ValidTextLit]
shrink' [] = []
shrink' [t] = [validString' [t]]
shrink' ts =
  let (tsl, tsr) = splitAt ((length ts) `div` 2) ts in
  fmap validString' [tsl, tsr] ++ shrink' tsl ++ shrink' tsr

validString'' :: String -> String -> [ValidTextLit] -> ValidTextLit
validString'' ss cs ls =
  ValidTextLit
    ("\"" ++ ss ++ "\"", LString cs, ls)

validString' :: [(String, Char)] -> ValidTextLit
validString' [(s, c)] = validString'' s [c] []
validString' ts =
  let ss = concat $ map fst ts in
  let cs = map snd ts in
  validString'' ss cs (shrink' ts)

validString :: Gen ValidTextLit
validString = listOf genStringChar >>= return . validString'

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
