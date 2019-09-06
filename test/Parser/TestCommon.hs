module Parser.TestCommon
  ( goldenTest
  ) where

import Parser.Common ( parseFile' )

import System.FilePath ( (</>) )

import Test.Tasty ( TestTree )
import Test.Tasty.Golden ( goldenVsFile )
import Text.PrettyPrint.GenericPretty ( Out, pretty )

import Text.ParserCombinators.ReadP ( ReadP )

goldenTest :: (Out a, Show a) => ReadP a -> [String] -> TestTree
goldenTest p path =
  goldenVsFile basepath fref fout $ do
    pres <- parseFile' p fin
    let ps = case pres of
              Left e -> show e
              Right p' -> pretty p'
    writeFile fout $ ps ++ "\n"
  where
    basepath = foldl (</>) "golden" $ ["Parser", "Classic"] ++ path
    fin = basepath ++ ".m"
    fref = basepath ++ ".ref"
    fout =  basepath ++ ".out"
