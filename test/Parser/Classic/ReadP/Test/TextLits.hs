-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.Test.TextLits (testTree) where

import Parser.Utils.ReadP (fullParse)
import Parser.Classic.ReadP.TextLits (parseTextLit)

import Parser.Classic.Gen.TextLits (ValidTextLit(..), InvalidTextLit(..))

import Control.Monad (forM_)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (Spec, it, shouldBe, testSpec)
import Test.Tasty.QuickCheck (Property, (===), testProperty)
import Text.Printf (printf)

invalidTextLits :: [String]
invalidTextLits =
  [ "'"
  , "\""
  ]

spec_invalidTextLits :: Spec
spec_invalidTextLits = do
  forM_ invalidTextLits $ \ lit ->
    it (printf "%s is an invalid text literal" lit) $
      fullParse parseTextLit lit `shouldBe` []

prop_validTextLit :: ValidTextLit -> Property
prop_validTextLit (ValidTextLit (s, e, _))
  = fullParse parseTextLit s === [e]

prop_invalidTextLit :: InvalidTextLit -> Property
prop_invalidTextLit (InvalidTextLit s)
  = fullParse parseTextLit s === []

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicTextLitsTests") $ sequence
  [ testSpec "InvalidTextLits" spec_invalidTextLits
  , return $ testProperty
      "Valid text lits parse"
      prop_validTextLit
  , return $ testProperty
      "Invalid text lits don't parse"
      prop_invalidTextLit
  ]
