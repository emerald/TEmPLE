module Parser.Classic.Test.Lits (testTree) where

import Parser.Common (fullParse)
import Parser.Classic (parser)
import Parser.Classic.Lits (parseLit)

import Parser.Classic.Gen.Lits (ValidLit(..), InvalidLit(..))

import Control.Monad (forM_)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (Spec, it, shouldBe, testSpec)
import Test.Tasty.QuickCheck (Property, (===), testProperty)
import Text.Printf (printf)

invalidLits :: [String]
invalidLits =
  [ "09"
  , "0xx"
  ]

spec_invalidLits :: Spec
spec_invalidLits = do
  forM_ invalidLits $ \ lit ->
    it (printf "%s is an invalid literal" lit) $
      fullParse (parseLit parser) lit `shouldBe` []

prop_validLit :: ValidLit -> Property
prop_validLit (ValidLit (s, e, _))
  = fullParse (parseLit parser) s === [e]

prop_invalidLit :: InvalidLit -> Property
prop_invalidLit (InvalidLit s)
  = fullParse (parseLit parser) s === []

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicLitsTests") $ sequence
  [ testSpec "InvalidLits" spec_invalidLits
  , return $ testProperty
      "Valid lits parse"
      prop_validLit
  , return $ testProperty
      "Invalid lits don't parse"
      prop_invalidLit
  ]
