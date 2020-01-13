module Parser.ClassicMegaparsec
  ( ParseError
  , parseFile
  , parseString
  ) where

import Ast ( Compilation )

type ParseError = ()

parseFile :: FilePath -> IO (Either String Compilation)
parseFile = undefined

parseString :: String -> Either String Compilation
parseString = undefined
