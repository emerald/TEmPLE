module Main where

import Ast (Compilation)
import Parser (parseFile, ParseError)

import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.IO (hPutStrLn, stderr)

errReport :: String -> IO ()
errReport = hPutStrLn stderr

failReport :: String -> IO a
failReport s = errReport s >> exitWith (ExitFailure 1)

parseError :: ParseError -> IO a
parseError e = failReport $ show e

parseOrShowError :: FilePath -> IO Compilation
parseOrShowError path = do
  result <- parseFile path
  case result of
    Right compilation -> pure compilation
    Left e -> parseError e

parseFiles :: [FilePath] -> IO [Compilation]
parseFiles = mapM parseOrShowError

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
    ("parse" : paths) ->
      with paths $ putStrLn . show
    l -> invalidCommand l
  where
    with :: [FilePath] -> ([Compilation] -> IO ()) -> IO ()
    with paths f = parseFiles paths >>= f
