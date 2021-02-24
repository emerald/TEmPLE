-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.Test.Golden
  ( goldenTest, goldenTestAll
  ) where

import Parser.Utils.ReadP ( parseFile' )

import System.Directory ( listDirectory )
import System.FilePath ( (</>) )

import Data.Maybe ( mapMaybe )

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.Golden ( goldenVsFile )
import Text.PrettyPrint.GenericPretty ( Out, pretty )

import Text.ParserCombinators.ReadP ( ReadP )

fullpath :: [String] -> FilePath
fullpath = foldl (</>) "golden" . (++) ["Parser", "Classic"]

chooseDotM :: FilePath -> Maybe FilePath
chooseDotM path =
  case reverse path of
    ('m':'.':revbase) -> Just $ reverse revbase
    _ -> Nothing

goldenTestAll :: (Out a, Show a) => ReadP a -> [String] -> IO TestTree
goldenTestAll p path = do
  fs <- listDirectory $ fullpath path
  let ms = mapMaybe chooseDotM fs
  let ps = map (\m -> path ++ [m]) ms
  return $ testGroup "Golden tests" $ map (goldenTest p) ps

goldenTest :: (Out a, Show a) => ReadP a -> [String] -> TestTree
goldenTest p path =
  goldenVsFile basepath fref fout $ do
    pres <- parseFile' p fin
    let ps = case pres of
              Left e -> pretty e
              Right p' -> pretty p'
    writeFile fout $ ps ++ "\n"
  where
    basepath = fullpath path
    fin = basepath ++ ".m"
    fref = basepath ++ ".ref"
    fout =  basepath ++ ".out"
