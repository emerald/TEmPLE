module Parser.ClassicConstDeclsTests (testTree) where

import Parser.Common (fullParse, parse)
import Parser.ClassicConstDecls (parseConstDecl)

import Parser.GenClassicConstDecls
  ( ValidConstDecl(..)
  , InvalidConstDecl(..)
  )

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, (===), testProperty)

prop_validConstDecl :: ValidConstDecl -> Property
prop_validConstDecl (ValidConstDecl (s, e))
  = parse parseConstDecl s === [(e, "")]

prop_invalidConstDecl :: InvalidConstDecl -> Property
prop_invalidConstDecl (InvalidConstDecl s)
  = fullParse parseConstDecl s === []

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicConstDeclsTests") $ sequence
  [ return $ testProperty
      "Valid constant declarations parse"
      prop_validConstDecl
  , return $ testProperty
      "Invalid constant declarations don't parse"
      prop_invalidConstDecl
  ]
