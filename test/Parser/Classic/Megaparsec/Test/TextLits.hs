module Parser.Classic.Megaparsec.Test.TextLits (testTree) where

import Control.Monad ( forM_ )
import Data.Either ( isLeft )

import Parser.Classic.Megaparsec.Base ( fullParse )
import Parser.Classic.Megaparsec.TextLits ( parseTextLit )

import Parser.Classic.Gen.TextLits ( ValidTextLit(..), InvalidTextLit(..) )

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.Hspec ( Spec, it, shouldSatisfy, testSpec )
import Test.Tasty.QuickCheck ( Property, (===), testProperty )
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
      fullParse parseTextLit lit `shouldSatisfy` isLeft

prop_validTextLit :: ValidTextLit -> Property
prop_validTextLit (ValidTextLit (s, e, _))
  = fullParse parseTextLit s === Right e

prop_invalidTextLit :: InvalidTextLit -> Property
prop_invalidTextLit (InvalidTextLit s)
  = isLeft (fullParse parseTextLit s) === True

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
