module Parser.Classic.Test.DeclStats (testTree) where

import Parser.Classic ( parser )
import Parser.Classic.DeclStats ( parseDeclStats )
import Parser.TestCommon ( goldenTestAll )

import Test.Tasty (TestTree, testGroup)

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicDeclStatsTests") $ sequence
  [ goldenTests
  ]

goldenTests :: IO TestTree
goldenTests = goldenTestAll p ["DeclStats"]
  where p = parseDeclStats parser
