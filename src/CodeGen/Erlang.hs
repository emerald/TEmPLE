module CodeGen.Erlang where

import Ast ( Compilation )

import CodeGen.Erlang.Compilations ( genCompilation )
import CodeGen.Erlang.PrettyAbstractFormat ( prettyModuleDecl )

import Prelude hiding ( putStrLn )
import Data.Text ( pack )
import Data.Text.IO ( hPutStrLn, putStrLn )
import Data.Text.Prettyprint.Doc ( Doc )
import Data.Text.Prettyprint.Doc.Render.Text ( hPutDoc )

import System.FilePath ( dropFileName, takeBaseName )
import System.IO ( hFlush, stdout, stderr )
import System.Process
  ( StdStream ( CreatePipe, UseHandle )
  , createProcess, proc
  , cwd, std_in, std_out, std_err
  , waitForProcess
  )

genCode :: FilePath -> Compilation -> Doc ann
genCode path = prettyModuleDecl . (genCompilation path)

genErl :: FilePath -> Compilation -> IO ()
genErl = genViaErl ".erl"
  "Contents = lists:flatmap(fun(Form) -> erl_pp:form(Form) end, "

genBEAM :: FilePath -> Compilation -> IO ()
genBEAM = genViaErl ".beam"
  "{ok, _, Contents} = compile:forms("

genViaErl :: String -> String -> FilePath -> Compilation -> IO ()
genViaErl ext cmdline path c = do
  let dir = dropFileName path
  let filename = takeBaseName path ++ ext
  let code = genCode path c
  (Just hin, _, _, hproc) <-
    createProcess (proc "erl" []){
      cwd = Just dir,
      std_in = CreatePipe,
      std_out = UseHandle stdout,
      std_err = UseHandle stderr }
  let putCmd = hPutStrLn hin . pack

  putCmd cmdline
  hPutDoc hin code
  putCmd ")."
  hFlush hin

  putCmd $ "FileName = \"" ++ filename ++ "\"."
  putCmd "{ok, File} = file:open(FileName, [write])."
  putCmd "io:fwrite(File, \"~s\",[Contents])."

  putCmd "q()."
  exitCode <- waitForProcess hproc
  putStrLn $ pack $ "Exit code: " ++ show exitCode

  return ()
