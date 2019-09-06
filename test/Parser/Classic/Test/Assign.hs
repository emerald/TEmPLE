module Parser.Classic.Test.Assign (testTree) where

import Parser.Classic ( parser )
import Parser.Classic.Assign ( parseAssign )
import Parser.TestCommon ( goldenTest )

import Text.ParserCombinators.ReadP ( many )
import Test.Tasty (TestTree, testGroup)

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicAssignTests") $ sequence
  [ return $ goldenTests
  ]

goldenTests :: TestTree
goldenTests = testGroup "Golden tests"
  [ goldenTest p ["Assign", "assign"]
  ]
  where p = many $ parseAssign parser
