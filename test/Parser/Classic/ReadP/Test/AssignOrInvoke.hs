-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.Test.AssignOrInvoke (testTree) where

import Parser.Classic.ReadP ( parser )
import Parser.Classic.ReadP.AssignOrInvoke ( parseAssignOrInvoke )
import Parser.Classic.ReadP.Test.Golden ( goldenTestAll )

import Text.ParserCombinators.ReadP ( many )
import Test.Tasty (TestTree, testGroup)

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicAssignOrInvokeTests") $ sequence
  [ goldenTests
  ]

goldenTests :: IO TestTree
goldenTests = goldenTestAll p ["AssignOrInvoke"]
  where p = many $ parseAssignOrInvoke parser
