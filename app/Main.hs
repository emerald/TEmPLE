module Main (main) where

import Ast (Compilation)
import qualified Parser.Classic.ReadP as ClassicReadPParser
import qualified Parser.ClassicMegaparsec as ClassicMegaparsecParser

import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.IO (hPutStrLn, stderr)
import Text.PrettyPrint.GenericPretty (pp, pretty)

errReport :: String -> IO ()
errReport = hPutStrLn stderr

failReport :: String -> IO a
failReport s = errReport s >> exitWith (ExitFailure 1)

parseClassicReadP :: FilePath -> IO Compilation
parseClassicReadP path = do
  result <- ClassicReadPParser.parseFile path
  case result of
    Right compilation -> pure compilation
    Left e -> failReport $ pretty e

parseClassicMegaparsec :: FilePath -> IO Compilation
parseClassicMegaparsec path = do
  result <- ClassicMegaparsecParser.parseFile path
  case result of
    Right compilation -> pure compilation
    Left e -> failReport $ show e

noCommand :: IO a
noCommand = failReport "Tell me what to do!"

invalidCommand :: [String] -> IO a
invalidCommand s = do
  case s of
    [] -> return ()
    [x] -> failReport $
            x ++ " is an invalid command-line argument!"
    xs -> failReport $
            (intercalate " " xs) ++
              " are invalid command-line arguments!"
  exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> noCommand
    ["parse", "--readp", path] ->
      parseClassicReadP path >>= pp
    ["parse", path] ->
      parseClassicMegaparsec path >>= pp
    l -> invalidCommand l
