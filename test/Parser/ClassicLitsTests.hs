module Parser.ClassicLitsTests (testTree) where

import Parser.Common (fullParse, parse)
import Parser.ClassicLits (parseLit)

import Parser.GenClassicLits (ValidLit(..), InvalidLit(..))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, (===), testProperty)

prop_validLit :: ValidLit -> Property
prop_validLit (ValidLit (s, e))
  = fullParse parseLit s === [e]

prop_invalidLit :: InvalidLit -> Property
prop_invalidLit (InvalidLit s)
  = fullParse parseLit s === []

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicLitsTests") $ sequence
  [ return $ testProperty
      "Valid lits parse"
      prop_validLit
  , return $ testProperty
      "Invalid lits don't parse"
      prop_invalidLit
  ]
