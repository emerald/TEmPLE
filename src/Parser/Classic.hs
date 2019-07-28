module Parser.Classic where

import Ast (ConstDecl)
import Parser.Common (ParseErrorImpl, skipFilling, parseFile', parseString')
import Parser.ClassicObjects (parseObject)
import Parser.ClassicDecls (parseConstDecl, parseVarDecl, parseDecl)
import Parser.Types (Parser(Parser))

import Control.Applicative ((*>))
import Text.ParserCombinators.ReadP (ReadP, many)

type ParseError = ParseErrorImpl [ConstDecl]

parser :: Parser
parser = Parser parseObject parseConstDecl parseVarDecl parseDecl

parseProgram :: ReadP [ConstDecl]
parseProgram = skipFilling *> many (parseConstDecl parser)

parseString :: String -> Either ParseError [ConstDecl]
parseString = parseString' parseProgram "String"

parseFile :: FilePath -> IO (Either ParseError [ConstDecl])
parseFile = parseFile' parseProgram
