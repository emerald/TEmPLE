module Parser.Types
  ( Parser(Parser)
  , parseConstDecl
  , parseObject
  ) where

import Ast (Object, ConstDecl)

import Text.ParserCombinators.ReadP (ReadP)

data Parser
  = Parser
  { parseObject' :: Parser -> ReadP Object
  , parseConstDecl' :: Parser -> ReadP ConstDecl
  }

parse :: (Parser -> Parser -> ReadP a) -> Parser -> ReadP a
parse f p = f p p

parseConstDecl :: Parser -> ReadP ConstDecl
parseConstDecl = parse parseConstDecl'

parseObject :: Parser -> ReadP Object
parseObject = parse parseObject'
