module Parser.Classic.Test.ConstDecls (testTree) where

import Parser ( parseFile )
import Parser.Common (fullParse, parse)
import Parser.Classic (parser)
import Parser.Classic.Decls (parseConstDecl)

import Parser.Classic.Gen.ConstDecls
  ( ValidConstDecl(..)
  , InvalidConstDecl(..)
  )

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, (===), property, testProperty)
import Test.Tasty.Golden ( goldenVsFile )
import Text.PrettyPrint.GenericPretty ( pretty )

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
  , return $ goldenTests
  ]

goldenTests :: TestTree
goldenTests = testGroup "Golden tests"
  [ golden' "const"
  ]

golden' :: String -> TestTree
golden' basename =
  goldenVsFile name fref fout $ do
    p <- parseFile fin
    let ps = case p of
              Left e -> show e
              Right p' -> pretty p'
    writeFile fout $ ps ++ "\n"
  where
    prefix = "golden/Parser/Classic/Decls/"
    name = basename ++ ".m"
    fin = prefix ++ name
    fref = prefix ++ basename ++ ".ref"
    fout = prefix ++ basename ++ ".out"
