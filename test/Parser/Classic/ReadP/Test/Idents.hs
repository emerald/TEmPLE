module Parser.Classic.ReadP.Test.Idents
  ( testTree
  ) where

import Parser.Utils.ReadP (fullParse, parse)
import Parser.Classic.ReadP.Idents (reserved, parseIdent)

import Parser.TestCommon ( goldenTestAll )

import Parser.Classic.Gen.Idents (ValidIdent(..), InvalidIdent(..))

import Control.Monad (forM_)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (Spec, it, shouldBe, testSpec)
import Test.Tasty.QuickCheck (Property, (===), testProperty)
import Text.ParserCombinators.ReadP ( many )
import Text.Printf (printf)

spec_reserved :: Spec
spec_reserved = do
  forM_ reserved $ \ word ->
    it (printf "%s is a reserved word" word) $
      parse parseIdent word `shouldBe` []

prop_validIdent :: ValidIdent -> Property
prop_validIdent (ValidIdent (s, n, _)) = parse parseIdent s === [(n, "")]

prop_invalidIdent :: InvalidIdent -> Property
prop_invalidIdent (InvalidIdent s)
  = fullParse parseIdent s === []

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicIdentsTests") $ sequence
  [ testSpec "Keywords" spec_reserved
  , return $ testProperty
      "Valid names parse"
      prop_validIdent
  , return $ testProperty
      "Invalid names don't parse"
      prop_invalidIdent
  , goldenTests
  ]

goldenTests :: IO TestTree
goldenTests = goldenTestAll p ["Idents"]
  where p = many $ parseIdent
