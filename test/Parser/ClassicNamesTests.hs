module Parser.ClassicNamesTests where

import Parser.Common (parse)
import Parser.ClassicNames (keywords, parseName)

import Parser.GenClassicNames (ValidName(..), InvalidName(..))

import Control.Monad (forM_)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (Spec, it, shouldBe, testSpec)
import Test.Tasty.QuickCheck (Property, (===), testProperty)
import Text.Printf (printf)

spec_keywords :: Spec
spec_keywords = do
  forM_ keywords $ \ keyword ->
    it (printf "%s is a keyword" keyword) $
      parse parseName keyword `shouldBe` []

prop_validName :: ValidName -> Property
prop_validName (ValidName s n) = parse parseName s === [(n, "")]

prop_invalidName :: InvalidName -> Property
prop_invalidName (InvalidName s) = parse parseName s === []

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicNamesTests") $ sequence
  [ testSpec "Keywords" spec_keywords
  , return $ testProperty
      "Valid names parse"
      prop_validName
  , return $ testProperty
      "Invalid names don't parse"
      prop_invalidName
  ]
