module Parser.ClassicIdentsTests where

import Parser.Common (fullParse, parse)
import Parser.ClassicIdents (keywords, parseIdent)

import Parser.GenClassicIdents (ValidIdent(..), InvalidIdent(..))

import Control.Monad (forM_)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (Spec, it, shouldBe, testSpec)
import Test.Tasty.QuickCheck (Property, (===), testProperty)
import Text.Printf (printf)

spec_keywords :: Spec
spec_keywords = do
  forM_ keywords $ \ keyword ->
    it (printf "%s is a keyword" keyword) $
      parse parseIdent keyword `shouldBe` []

prop_validIdent :: ValidIdent -> Property
prop_validIdent (ValidIdent (s, n, _)) = parse parseIdent s === [(n, "")]

prop_invalidIdent :: InvalidIdent -> Property
prop_invalidIdent (InvalidIdent s)
  = fullParse parseIdent s === []

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicIdentsTests") $ sequence
  [ testSpec "Keywords" spec_keywords
  , return $ testProperty
      "Valid names parse"
      prop_validIdent
  , return $ testProperty
      "Invalid names don't parse"
      prop_invalidIdent
  ]
