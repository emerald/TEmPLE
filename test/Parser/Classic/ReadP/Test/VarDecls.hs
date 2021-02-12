module Parser.Classic.ReadP.Test.VarDecls (testTree) where

import Parser.Utils.ReadP (fullParse, parse)
import Parser.Classic.ReadP (parser)
import Parser.Classic.ReadP.Decls (parseVarDecl)

import Parser.Classic.Gen.VarDecls
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
