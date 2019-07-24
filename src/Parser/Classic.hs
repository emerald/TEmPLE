module Parser.Classic where

import Ast (ConstDecl)
import Parser.Common (ParseErrorImpl, parseFile', parseString')
import Parser.ClassicObjects (parseObject)
import Parser.ClassicConstDecls (parseConstDecl)
import Parser.Types (Parser(Parser))

import Control.Applicative ((*>))
import Text.ParserCombinators.ReadP (ReadP, many, skipSpaces)

type ParseError = ParseErrorImpl [ConstDecl]

parser :: Parser
parser = Parser parseObject parseConstDecl

parseProgram :: ReadP [ConstDecl]
parseProgram = skipSpaces *> many (parseConstDecl parser)

parseString :: String -> Either ParseError [ConstDecl]
parseString = parseString' parseProgram "String"

parseFile :: FilePath -> IO (Either ParseError [ConstDecl])
parseFile = parseFile' parseProgram
