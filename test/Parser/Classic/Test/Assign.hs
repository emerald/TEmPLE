module Parser.Classic.Test.Assign (testTree) where

import Parser.Classic ( parser )
import Parser.Classic.Assign ( parseAssign )
import Parser.TestCommon ( goldenTestAll )

import Text.ParserCombinators.ReadP ( many )
import Test.Tasty (TestTree, testGroup)

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicAssignTests") $ sequence
  [ goldenTests
  ]

goldenTests :: IO TestTree
goldenTests = goldenTestAll p ["Assign"]
  where p = many $ parseAssign parser
