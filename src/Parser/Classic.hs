module Parser.Classic where

import Ast
import Parser.Common
import Parser.ClassicNames
import Parser.ClassicTypes

import Control.Applicative ((*>), liftA3)
import qualified Control.Applicative as App
import Text.ParserCombinators.ReadP

type ParseError = ParseErrorImpl [ConstDecl]

parseExpr :: ReadP Expr
parseExpr = token $ parseName >>= return . EVar

parseConstDecl :: ReadP ConstDecl
parseConstDecl = liftA3 Const
  (stoken1 "const" *> parseName)
  (App.optional parseType)
  (stoken "<-" *> parseExpr)

parseProgram :: ReadP [ConstDecl]
parseProgram = skipSpaces *> many parseConstDecl

parseString :: String -> Either ParseError [ConstDecl]
parseString = parseString' parseProgram "String"

parseFile :: FilePath -> IO (Either ParseError [ConstDecl])
parseFile = parseFile' parseProgram
