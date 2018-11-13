module Parser.ClassicNamesTests where

import Control.Monad (forM_)
import Text.Printf (printf)

import Parser.Common (parse)
import Parser.ClassicNames (keywords, parseName)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (Spec, it, shouldBe, testSpec)

spec_keywords :: Spec
spec_keywords = do
  forM_ keywords $ \ keyword ->
    it (printf "%s is a keyword" keyword) $
      parse parseName keyword `shouldBe` []

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicNamesTests") $ sequence
  [ testSpec "Keywords" spec_keywords
  ]
