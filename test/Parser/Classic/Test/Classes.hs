module Parser.Classic.Test.Classes (testTree) where

import Parser.Classic ( parser )
import Parser.Classic.Lits ( parseLit )
import Parser.TestCommon ( goldenTestAll )

import Test.Tasty (TestTree, testGroup)

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicClassesTests") $ sequence
  [ goldenTests
  ]

goldenTests :: IO TestTree
goldenTests = goldenTestAll p ["Classes"]
  where p = parseLit parser
