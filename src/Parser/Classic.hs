module Parser.Classic where

import Ast
import Parser.Common

import Control.Applicative ((*>), liftA2, liftA3)
import qualified Control.Applicative as App
import Control.Monad (mfilter)
import Text.ParserCombinators.ReadP

type ParseError = ParseErrorImpl [ConstDecl]

first :: Char -> Bool
first = flip elem ('_' : ['A'..'Z'] ++ ['a'..'z'])

rest :: Char -> Bool
rest c = first c || c `elem` (['0'..'9'])

keywords :: [String]
keywords =
  [ "const"
  , "integer"
  , "real"
  , "character"
  , "string"
  ]

parseName :: ReadP Name
parseName = token $ mfilter (not . (`elem` keywords)) $
  liftA2 (:) (satisfy first) (munch rest)

parseType :: ReadP Type
parseType = (*>) (stoken ":") $ token $ word $
  [ ("integer", TInt)
  , ("real", TDouble)
  , ("character", TChar)
  , ("string", TString)
  ]

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
