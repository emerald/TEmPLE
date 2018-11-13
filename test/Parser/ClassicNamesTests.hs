module Parser.ClassicNamesTests where

import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Text.Printf (printf)

import Parser.Common (parse)
import Parser.ClassicNames (firstChars, restChars, keywords, parseName)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (Spec, it, shouldBe, testSpec)
import Test.Tasty.QuickCheck

spec_keywords :: Spec
spec_keywords = do
  forM_ keywords $ \ keyword ->
    it (printf "%s is a keyword" keyword) $
      parse parseName keyword `shouldBe` []

data GenName = GenName String
  deriving (Eq, Show)

instance Arbitrary GenName where
  arbitrary = fmap GenName $
    flip suchThat (not . (`elem` keywords)) $
      liftA2 (:) (elements firstChars) (listOf (elements restChars))

prop_name :: GenName -> Bool
prop_name (GenName s) = parse parseName s == [(s, "")]

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicNamesTests") $ sequence
  [ testSpec "Keywords" spec_keywords
  , return $ testProperty "Name" prop_name
  ]
