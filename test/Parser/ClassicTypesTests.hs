module Parser.ClassicTypesTests (testTree) where

import Parser.Common (parse)
import Parser.ClassicTypes (parseType)

import Parser.GenClassicTypes (ValidType(..), InvalidType(..))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, (===), testProperty)

prop_validType :: ValidType -> Property
prop_validType (ValidType (s, e)) = parse parseType s === [(e, "")]

prop_invalidType :: InvalidType -> Property
prop_invalidType (InvalidType s) = parse parseType s === []

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicTypesTests") $ sequence
  [ return $ testProperty
      "Valid types parse"
      prop_validType
  , return $ testProperty
      "Invalid types don't parse"
      prop_invalidType
  ]
