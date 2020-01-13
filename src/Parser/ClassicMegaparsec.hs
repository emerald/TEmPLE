-- |
-- Module       : TEmPLE.Parser.ClassicMegaparsec
-- Description  : An efficient, user-friendly parser for classical Emerald
-- Copyright    : (c) Oleks Shturmov, 2020
-- License      : BSD 3-Clause (see the file LICENSE)
--
-- Maintainer   : oleks@oleks.info

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
