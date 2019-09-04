module Parser.Classic.Test.Assign (testTree) where

import Parser.TestCommon ( goldenTest )

import Test.Tasty (TestTree, testGroup)

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicAssignTests") $ sequence
  [ return $ goldenTests
  ]

goldenTests :: TestTree
goldenTests = testGroup "Golden tests"
  [ goldenTest "golden/Parser/Classic/Assign/assign"
  ]
