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
  [ goldenTest p "golden/Parser/Classic/IfThenElse/ifthen"
  , goldenTest p "golden/Parser/Classic/IfThenElse/elseif"
  ]
  where p = parseIfThenElse parser
