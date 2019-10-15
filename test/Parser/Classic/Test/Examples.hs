module Parser.Classic.Test.Examples (testTree) where

import Parser.Classic ( parseProgram )
import Parser.TestCommon ( goldenTestAll )

import Test.Tasty (TestTree, testGroup)

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicExamplesTests") $ sequence
  [ goldenTests
  ]

goldenTests :: IO TestTree
goldenTests = goldenTestAll p ["Examples"]
  where p = parseProgram
