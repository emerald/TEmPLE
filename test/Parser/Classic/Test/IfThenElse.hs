module Parser.Classic.Test.IfThenElse (testTree) where

import Parser.Classic ( parser )
import Parser.Classic.IfThenElse ( parseIfThenElse )
import Parser.TestCommon ( goldenTestAll )

import Test.Tasty (TestTree, testGroup)

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicIfThenElseTests") $ sequence
  [ goldenTests
  ]

goldenTests :: IO TestTree
goldenTests = goldenTestAll p ["IfThenElse"]
  where p = parseIfThenElse parser
