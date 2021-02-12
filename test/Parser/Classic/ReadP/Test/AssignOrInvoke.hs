module Parser.Classic.ReadP.Test.AssignOrInvoke (testTree) where

import Parser.Classic.ReadP ( parser )
import Parser.Classic.ReadP.AssignOrInvoke ( parseAssignOrInvoke )
import Parser.TestCommon ( goldenTestAll )

import Text.ParserCombinators.ReadP ( many )
import Test.Tasty (TestTree, testGroup)

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicAssignOrInvokeTests") $ sequence
  [ goldenTests
  ]

goldenTests :: IO TestTree
goldenTests = goldenTestAll p ["AssignOrInvoke"]
  where p = many $ parseAssignOrInvoke parser
