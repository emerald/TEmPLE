module Parser.Classic.ReadP.Test.Exprs (testTree) where

import Parser.Utils.ReadP (fullParse, parse)
import Parser.Classic.ReadP (parser)
import Parser.Classic.ReadP.Exprs (parseExpr)

import Parser.Classic.Gen.Exprs (ValidExpr(..), InvalidExpr(..))

import Parser.TestCommon ( goldenTestAll )

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, (===), property, testProperty)
import Text.ParserCombinators.ReadP ( many )

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
  , goldenTests
  ]

goldenTests :: IO TestTree
goldenTests = goldenTestAll p ["Expr"]
  where p = many $ parseExpr parser
