module Parser.Types
  ( Parser(Parser)
  , parseVecLit
  , parseAttDecl
  , parseConstDecl
  , parseVarDecl
  , parseDecl
  , parseObject
  , parseDeclStat
  , parseDeclStats
  , parseBlockBody
  ) where

import Ast (Object, ConstDecl, BlockBody, VarDecl, Decl, DeclStat, Lit)

import Text.ParserCombinators.ReadP (ReadP)

data Parser
  = Parser
  { parseObject' :: Parser -> ReadP Object
  , parseVecLit' :: Parser -> ReadP Lit
  , parseAttDecl' :: Parser -> ReadP (Bool, Decl)
  , parseConstDecl' :: Parser -> ReadP ConstDecl
  , parseVarDecl' :: Parser -> ReadP VarDecl
  , parseDecl' :: Parser -> ReadP Decl
  , parseDeclStat' :: Parser -> ReadP DeclStat
  , parseDeclStats' :: Parser -> ReadP [DeclStat]
  , parseBlockBody' :: Parser -> ReadP BlockBody
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

parseVecLit :: Parser -> ReadP Lit
parseVecLit = parse parseVecLit'

parseDeclStat :: Parser -> ReadP DeclStat
parseDeclStat = parse parseDeclStat'

parseDeclStats :: Parser -> ReadP [DeclStat]
parseDeclStats = parse parseDeclStats'

parseBlockBody :: Parser -> ReadP BlockBody
parseBlockBody = parse parseBlockBody'
