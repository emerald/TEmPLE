module Parser.Classic.Megaparsec.Test.Idents
  ( testTree
  ) where

import Control.Monad ( forM_ )

import Data.Either ( isLeft )

import Parser.Classic.Megaparsec.Base ( fullParse )
import Parser.Classic.Megaparsec.Idents ( parseIdent )

import Parser.Classic.Gen.Idents ( ValidIdent(..), InvalidIdent(..) )

import Parser.Classic.Words ( reserved )

-- import Parser.TestCommon ( goldenTestAll )

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.Hspec ( Spec, it, shouldSatisfy, testSpec )
import Test.Tasty.QuickCheck ( Property, (===), testProperty )

import Text.Megaparsec ( many )
import Text.Printf ( printf )

spec_reserved :: Spec
spec_reserved = do
  forM_ reserved $ \ word ->
    it (printf "%s is a reserved word" word) $
      fullParse parseIdent word `shouldSatisfy` isLeft

prop_validIdent :: ValidIdent -> Property
prop_validIdent (ValidIdent (s, n, _))
  = fullParse parseIdent s === Right n

prop_invalidIdent :: InvalidIdent -> Property
prop_invalidIdent (InvalidIdent s)
  = isLeft (fullParse parseIdent s) === True

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicIdentsTests") $ sequence
  [ testSpec "Keywords" spec_reserved
  , return $ testProperty
      "Valid names parse"
      prop_validIdent
  , return $ testProperty
      "Invalid names don't parse"
      prop_invalidIdent
--  , goldenTests
  ]
{-
goldenTests :: IO TestTree
goldenTests = goldenTestAll p ["Idents"]
  where p = many $ parseIdent
-}
