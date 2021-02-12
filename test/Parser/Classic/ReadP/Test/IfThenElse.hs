module Parser.Classic.ReadP.Test.IfThenElse (testTree) where

import Parser.Classic.ReadP ( parser )
import Parser.Classic.ReadP.IfThenElse ( parseIfThenElse )
import Parser.TestCommon ( goldenTestAll )

import Test.Tasty (TestTree, testGroup)

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicIfThenElseTests") $ sequence
  [ goldenTests
  ]

goldenTests :: IO TestTree
goldenTests = goldenTestAll p ["IfThenElse"]
  where p = parseIfThenElse parser
