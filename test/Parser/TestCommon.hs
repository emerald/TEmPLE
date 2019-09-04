module Parser.TestCommon
  ( goldenTest
  ) where

import Parser ( parseFile )

import Test.Tasty ( TestTree )
import Test.Tasty.Golden ( goldenVsFile )
import Text.PrettyPrint.GenericPretty ( pretty )

goldenTest :: String -> TestTree
goldenTest basename =
  goldenVsFile basename fref fout $ do
    p <- parseFile fin
    let ps = case p of
              Left e -> show e
              Right p' -> pretty p'
    writeFile fout $ ps ++ "\n"
  where
    fin = basename ++ ".m"
    fref = basename ++ ".ref"
    fout = basename ++ ".out"
