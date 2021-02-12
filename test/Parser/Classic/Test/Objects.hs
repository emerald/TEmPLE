module Parser.Classic.Test.Objects (testTree) where

import Parser.Classic.ReadP ( parser )
import Parser.Classic.ReadP.Lits ( parseLit )
import Parser.TestCommon ( goldenTestAll )

import Test.Tasty (TestTree, testGroup)

import Text.ParserCombinators.ReadP ( many )

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicObjectsTests") $ sequence
  [ goldenTests
  ]

goldenTests :: IO TestTree
goldenTests = goldenTestAll p ["Objects"]
  where p = many $ parseLit parser
