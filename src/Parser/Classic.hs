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
  , parseProgram
  , parser
  ) where

import Ast (ConstDecl)
import Parser.Common (ParseErrorImpl, skipFilling, parseFile', parseString')
import Parser.Classic.Exprs ( parseExpr, parseExprZero )
import Parser.Classic.TypeObjects (parseOptImmTypeObject)
import Parser.Classic.Decls ( parseConstDecl )
import Parser.Classic.DeclStats ( parseDeclStats )
import Parser.Types (Parser(Parser))

import Control.Applicative ((*>))
import Text.ParserCombinators.ReadP (ReadP, many)

type ParseError = ParseErrorImpl [ConstDecl]

parser :: Parser
parser = Parser
  parseExpr
  parseExprZero
  parseOptImmTypeObject
  parseDeclStats

parseProgram :: ReadP [ConstDecl]
parseProgram = skipFilling *> many (parseConstDecl parser)

parseString :: String -> Either ParseError [ConstDecl]
parseString = parseString' parseProgram "String"

parseFile :: FilePath -> IO (Either ParseError [ConstDecl])
parseFile = parseFile' parseProgram
