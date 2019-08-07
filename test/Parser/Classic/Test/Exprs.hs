module Parser.Classic.Test.Exprs (testTree) where

import Parser.Common (fullParse, parse)
import Parser.Classic.Parser (parser)
import Parser.Classic.Exprs (parseExpr)

import Parser.Classic.Gen.Exprs (ValidExpr(..), InvalidExpr(..))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, (===), property, testProperty)

prop_validExpr :: ValidExpr -> Property
prop_validExpr (ValidExpr (s, e, _))
  = property $
      (e, "") `elem` (parse (parseExpr parser) s)

prop_invalidExpr :: InvalidExpr -> Property
prop_invalidExpr (InvalidExpr s)
  = fullParse (parseExpr parser) s === []

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicExprsTests") $ sequence
  [ return $ testProperty
      "Valid expressions parse"
      prop_validExpr
  , return $ testProperty
      "Invalid expressions don't parse"
      prop_invalidExpr
  ]
