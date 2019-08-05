module Parser.Types
  ( Parser(Parser)
  , parseAttDecl
  , parseConstDecl
  , parseVarDecl
  , parseDecl
  , parseObject
  ) where

import Ast (Object, ConstDecl, VarDecl, Decl)

import Text.ParserCombinators.ReadP (ReadP)

data Parser
  = Parser
  { parseObject' :: Parser -> ReadP Object
  , parseAttDecl' :: Parser -> ReadP (Bool, Decl)
  , parseConstDecl' :: Parser -> ReadP ConstDecl
  , parseVarDecl' :: Parser -> ReadP VarDecl
  , parseDecl' :: Parser -> ReadP Decl
  }

parse :: (Parser -> Parser -> ReadP a) -> Parser -> ReadP a
parse f p = f p p

parseAttDecl :: Parser -> ReadP (Bool, Decl)
parseAttDecl = parse parseAttDecl'

parseConstDecl :: Parser -> ReadP ConstDecl
parseConstDecl = parse parseConstDecl'

parseVarDecl :: Parser -> ReadP VarDecl
parseVarDecl = parse parseVarDecl'

parseDecl :: Parser -> ReadP Decl
parseDecl = parse parseDecl'

parseObject :: Parser -> ReadP Object
parseObject = parse parseObject'
