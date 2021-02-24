-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.Test.ConstDecls (testTree) where

import Parser.Utils.ReadP (fullParse, parse)
import Parser.Classic.ReadP (parser)
import Parser.Classic.ReadP.Decls (parseConstDecl)

import Parser.Classic.Gen.ConstDecls
  ( ValidConstDecl(..)
  , InvalidConstDecl(..)
  )
import Parser.Classic.ReadP.Test.Golden ( goldenTestAll )

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, (===), property, testProperty)

import Text.ParserCombinators.ReadP ( many )

prop_validConstDecl :: ValidConstDecl -> Property
prop_validConstDecl (ValidConstDecl (s, e))
  = property $
      (e, "") `elem` (parse (parseConstDecl parser) s)

prop_invalidConstDecl :: InvalidConstDecl -> Property
prop_invalidConstDecl (InvalidConstDecl s)
  = fullParse (parseConstDecl parser) s === []

testTree :: IO TestTree
testTree = fmap (testGroup "ClassicConstDeclsTests") $ sequence
  [ return $ testProperty
      "Valid constant declarations parse"
      prop_validConstDecl
  , return $ testProperty
      "Invalid constant declarations don't parse"
      prop_invalidConstDecl
  , goldenTests
  ]

goldenTests :: IO TestTree
goldenTests = goldenTestAll p ["ConstDecls"]
  where p = many $ parseConstDecl parser
