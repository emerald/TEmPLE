-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.Test.Decls (testTree) where

import Parser.Classic.ReadP ( parser )
import Parser.Classic.ReadP.Decls ( parseObjConstrDecl )
import Parser.Classic.ReadP.Test.Golden ( goldenTestAll )

import Test.Tasty (TestTree, testGroup)
import Text.ParserCombinators.ReadP ( many )

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicDeclTests") $ sequence
  [ goldenTests
  ]

goldenTests :: IO TestTree
goldenTests = goldenTestAll p ["Decls", "ObjConstr"]
  where p = many $ parseObjConstrDecl parser
