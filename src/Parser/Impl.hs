module Parser.Impl where

import Ast

import Data.Char (isSpace)
import Control.Applicative ((<*), (*>), liftA2, liftA3)
import qualified Control.Applicative as App
import Control.Monad (mfilter, void)
import Text.ParserCombinators.ReadP

data ParseErrorImpl a
  = NoParse FilePath
  | AmbiguousGrammar [a] FilePath
  | NotImplemented
  deriving (Eq, Show)

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

token :: ReadP a -> ReadP a
token = flip (<*) skipSpaces

stoken :: String -> ReadP ()
stoken = void . token . string

stoken1 :: String -> ReadP ()
stoken1 s = string s >> munch1 isSpace >> return ()

word :: [(String, a)] -> ReadP a
word = choice . map (\(w, a) -> string w *> return a)

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

parse :: ReadP a -> String -> [(a, String)]
parse = readP_to_S

fullParse :: ReadP a -> String -> [a]
fullParse p s = fmap fst $ parse (p <* eof) s

parseString' :: ReadP a -> FilePath -> String -> Either (ParseErrorImpl a) a
parseString' p path s =
  case fullParse p s of
    [] -> Left $ NoParse path
    [a] -> Right a
    as -> Left $ AmbiguousGrammar as path

parseString :: String -> Either ParseError [ConstDecl]
parseString = parseString' parseProgram "String"

parseFile' :: ReadP a -> FilePath -> IO (Either (ParseErrorImpl a) a)
parseFile' p path = fmap (parseString' p path) $ readFile path

parseFile :: FilePath -> IO (Either ParseError [ConstDecl])
parseFile = parseFile' parseProgram
