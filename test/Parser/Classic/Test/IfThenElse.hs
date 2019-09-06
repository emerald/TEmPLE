module Parser.Classic.Test.IfThenElse (testTree) where

import Parser.Classic ( parser )
import Parser.Classic.IfThenElse ( parseIfThenElse )
import Parser.TestCommon ( goldenTest )

import Test.Tasty (TestTree, testGroup)

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicAssignTests") $ sequence
  [ return $ goldenTests
  ]

goldenTests :: TestTree
goldenTests = testGroup "Golden tests"
  [ goldenTest p ["IfThenElse", "ifthen"]
  , goldenTest p ["IfThenElse", "elseif"]
  ]
  where p = parseIfThenElse parser
