module Parser.ClassicVarDeclsTests (testTree) where

import Parser.Common (fullParse, parse)
import Parser.Classic (parser)
import Parser.ClassicDecls (parseVarDecl)

import Parser.GenClassicVarDecls
  ( ValidVarDecl(..)
  , InvalidVarDecl(..)
  )

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, (===), property, testProperty)

prop_validVarDecl :: ValidVarDecl -> Property
prop_validVarDecl (ValidVarDecl (s, e))
  = property $
      (e, "") `elem` (parse (parseVarDecl parser) s)

prop_invalidVarDecl :: InvalidVarDecl -> Property
prop_invalidVarDecl (InvalidVarDecl s)
  = fullParse (parseVarDecl parser) s === []

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicVarDeclsTests") $ sequence
  [ return $ testProperty
      "Valid variable declarations parse"
      prop_validVarDecl
  , return $ testProperty
      "Invalid variable declarations don't parse"
      prop_invalidVarDecl
  ]
