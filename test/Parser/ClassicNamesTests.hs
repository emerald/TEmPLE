module Parser.ClassicNamesTests where

import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Text.Printf (printf)

import Ast (Name)

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

data ValidName = ValidName Name
  deriving (Eq, Show)

instance Arbitrary ValidName where
  arbitrary = fmap ValidName $
    flip suchThat (not . (`elem` keywords)) $
      liftA2 (:) (elements firstChars) (listOf (elements restChars))

data InvalidName = InvalidName Name
  deriving (Eq, Show)

instance Arbitrary InvalidName where
  arbitrary = fmap InvalidName $
    liftA2 (:)
      (suchThat arbitrary (not . (`elem` firstChars)))
      (listOf arbitrary)

prop_validName :: ValidName -> Property
prop_validName (ValidName s) = parse parseName s === [(s, "")]

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
