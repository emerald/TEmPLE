module Parser.Classic.Test.Decls (testTree) where

import Parser.Classic ( parser )
import Parser.Classic.Decls ( parseAttDecl )
import Parser.TestCommon ( goldenTestAll )

import Test.Tasty (TestTree, testGroup)
import Text.ParserCombinators.ReadP ( many )

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicDeclTests") $ sequence
  [ goldenTests
  ]

goldenTests :: IO TestTree
goldenTests = goldenTestAll p ["Decls", "Att"]
  where p = many $ parseAttDecl parser
