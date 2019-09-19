module Parser.Types
  ( Parser(Parser)
  , parseClass
  , parseVecLit
  , parseAttDecl
  , parseConstDecl
  , parseVarDecl
  , parseDecl
  , parseObject
  , parseObjectBody
  , parseTypeObject
  , parseDeclStat
  , parseDeclStats
  , parseBlockBody
  ) where

import Ast
  ( Class
  , Object
  , ObjectBody
  , TypeObject
  , ConstDecl
  , BlockBody
  , VarDecl
  , Decl
  , DeclStat
  , Lit
  , Operation
  )

import Text.ParserCombinators.ReadP (ReadP)

data Parser
  = Parser
  { parseClass' :: Parser -> ReadP Class
  , parseObject' :: Parser -> ReadP Object
  , parseObjectBody' :: Parser -> ReadP ObjectBody
  , parseTypeObject' :: Parser -> ReadP TypeObject
  , parseVecLit' :: Parser -> ReadP Lit
  , parseAttDecl' :: Parser -> ReadP ((Bool, Decl), [Operation])
  , parseConstDecl' :: Parser -> ReadP ConstDecl
  , parseVarDecl' :: Parser -> ReadP VarDecl
  , parseDecl' :: Parser -> ReadP Decl
  , parseDeclStat' :: Parser -> ReadP DeclStat
  , parseDeclStats' :: Parser -> ReadP [DeclStat]
  , parseBlockBody' :: Parser -> ReadP BlockBody
  }

parse :: (Parser -> Parser -> ReadP a) -> Parser -> ReadP a
parse f p = f p p

parseAttDecl :: Parser -> ReadP ((Bool, Decl), [Operation])
parseAttDecl = parse parseAttDecl'

parseClass :: Parser -> ReadP Class
parseClass = parse parseClass'

parseConstDecl :: Parser -> ReadP ConstDecl
parseConstDecl = parse parseConstDecl'

parseVarDecl :: Parser -> ReadP VarDecl
parseVarDecl = parse parseVarDecl'

parseDecl :: Parser -> ReadP Decl
parseDecl = parse parseDecl'

parseObject :: Parser -> ReadP Object
parseObject = parse parseObject'

parseObjectBody :: Parser -> ReadP ObjectBody
parseObjectBody = parse parseObjectBody'

parseTypeObject :: Parser -> ReadP TypeObject
parseTypeObject = parse parseTypeObject'

parseVecLit :: Parser -> ReadP Lit
parseVecLit = parse parseVecLit'

parseDeclStat :: Parser -> ReadP DeclStat
parseDeclStat = parse parseDeclStat'

parseDeclStats :: Parser -> ReadP [DeclStat]
parseDeclStats = parse parseDeclStats'

parseBlockBody :: Parser -> ReadP BlockBody
parseBlockBody = parse parseBlockBody'
