module Parser.Classic where

import Ast (ConstDecl)
import Parser.Common (ParseErrorImpl, parseFile', parseString')
import Parser.ClassicConstDecls (parseConstDecl)

import Control.Applicative ((*>))
import Text.ParserCombinators.ReadP (ReadP, many, skipSpaces)

type ParseError = ParseErrorImpl [ConstDecl]

parseProgram :: ReadP [ConstDecl]
parseProgram = skipSpaces *> many parseConstDecl

parseString :: String -> Either ParseError [ConstDecl]
parseString = parseString' parseProgram "String"

parseFile :: FilePath -> IO (Either ParseError [ConstDecl])
parseFile = parseFile' parseProgram
