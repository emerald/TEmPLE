-----------------------------------------------------------------------------
-- |
-- Module      :  TEmPLE.Parser.Classic
-- Copyright   :  (c) Oleks Shturmov
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
--
-- A parser for classical Emerald; no funny business.

module Parser.Classic
  ( ParseError
  , parseString
  , parseFile
  , parser
  ) where

import Ast (ConstDecl)
import Parser.Common (ParseErrorImpl, skipFilling, parseFile', parseString')
import Parser.Classic.Objects (parseObject)
import Parser.Classic.VecLits (parseVecLit)
import Parser.Classic.Decls
  ( parseAttDecl
  , parseConstDecl
  , parseVarDecl
  , parseDecl
  )
import Parser.Classic.DeclStats
  ( parseDeclStat
  , parseDeclStats
  )
import Parser.Classic.BlockBody (parseBlockBody)
import Parser.Types (Parser(Parser))

import Control.Applicative ((*>))
import Text.ParserCombinators.ReadP (ReadP, many)

type ParseError = ParseErrorImpl [ConstDecl]

parser :: Parser
parser = Parser
  parseObject
  parseVecLit
  parseAttDecl
  parseConstDecl
  parseVarDecl
  parseDecl
  parseDeclStat
  parseDeclStats
  parseBlockBody

parseProgram :: ReadP [ConstDecl]
parseProgram = skipFilling *> many (parseConstDecl parser)

parseString :: String -> Either ParseError [ConstDecl]
parseString = parseString' parseProgram "String"

parseFile :: FilePath -> IO (Either ParseError [ConstDecl])
parseFile = parseFile' parseProgram
