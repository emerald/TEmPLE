module Parser.ClassicExprsTests (testTree) where

import Parser.Common (fullParse, parse)
import Parser.ClassicExprs (parseExpr)

import Parser.GenClassicExprs (ValidExpr(..), InvalidExpr(..))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, (===), testProperty)

prop_validExpr :: ValidExpr -> Property
prop_validExpr (ValidExpr (s, e))
  = parse parseExpr s === [(e, "")]

prop_invalidExpr :: InvalidExpr -> Property
prop_invalidExpr (InvalidExpr s)
  = fullParse parseExpr s === []

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicExprsTests") $ sequence
  [ return $ testProperty
      "Valid expressions parse"
      prop_validExpr
  , return $ testProperty
      "Invalid expressions don't parse"
      prop_invalidExpr
  ]
