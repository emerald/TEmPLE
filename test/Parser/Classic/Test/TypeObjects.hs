module Parser.Classic.Test.TypeObjects (testTree) where

import Parser.Classic ( parser )
import Parser.Classic.Lits ( parseLit )
import Parser.TestCommon ( goldenTestAll )

import Test.Tasty (TestTree, testGroup)
import Text.ParserCombinators.ReadP ( many )

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicTypeObjectsTests") $ sequence
  [ goldenTests
  ]

goldenTests :: IO TestTree
goldenTests = goldenTestAll p ["TypeObjects"]
  where p = many $ parseLit parser
