module Parser.TestCommon
  ( goldenTest
  ) where

import Parser.Common ( parseFile' )

import Test.Tasty ( TestTree )
import Test.Tasty.Golden ( goldenVsFile )
import Text.PrettyPrint.GenericPretty ( Out, pretty )

import Text.ParserCombinators.ReadP ( ReadP )

goldenTest :: (Out a, Show a) => ReadP a -> String -> TestTree
goldenTest p basename =
  goldenVsFile basename fref fout $ do
    pres <- parseFile' p fin
    let ps = case pres of
              Left e -> show e
              Right p' -> pretty p'
    writeFile fout $ ps ++ "\n"
  where
    fin = basename ++ ".m"
    fref = basename ++ ".ref"
    fout = basename ++ ".out"
