module Parser.Types
  ( Parser(Parser)
  , parseClass
  , parseVecLit
  , parseObjConstrDecl
  , parseConstDecl
  , parseVarDecl
  , parseDecl
  , parseObject
  , parseObjectBody
  , parseTypeObject
  , parseOptImmTypeObject
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
  { parseClass' :: Parser -> Bool -> Bool -> ReadP Class
  , parseObject' :: Parser -> Bool -> Bool -> ReadP Object
  , parseObjectBody' :: Parser -> ReadP ObjectBody
  , parseTypeObject' :: Parser -> Bool -> ReadP TypeObject
  , parseOptImmTypeObject' :: Parser -> ReadP TypeObject
  , parseVecLit' :: Parser -> ReadP Lit
  , parseObjConstrDecl' :: Parser -> ReadP ((Bool, Decl), [Operation])
  , parseConstDecl' :: Parser -> ReadP ConstDecl
  , parseVarDecl' :: Parser -> ReadP VarDecl
  , parseDecl' :: Parser -> ReadP Decl
  , parseDeclStat' :: Parser -> ReadP DeclStat
  , parseDeclStats' :: Parser -> ReadP [DeclStat]
  , parseBlockBody' :: Parser -> ReadP BlockBody
  }

parse :: (Parser -> Parser -> a) -> Parser -> a
parse f p = f p p

parseObjConstrDecl :: Parser -> ReadP ((Bool, Decl), [Operation])
parseObjConstrDecl = parse parseObjConstrDecl'

parseClass :: Parser -> Bool -> Bool -> ReadP Class
parseClass = parse parseClass'

parseConstDecl :: Parser -> ReadP ConstDecl
parseConstDecl = parse parseConstDecl'

parseVarDecl :: Parser -> ReadP VarDecl
parseVarDecl = parse parseVarDecl'

parseDecl :: Parser -> ReadP Decl
parseDecl = parse parseDecl'

parseObject :: Parser -> Bool -> Bool -> ReadP Object
parseObject = parse parseObject'

parseObjectBody :: Parser -> ReadP ObjectBody
parseObjectBody = parse parseObjectBody'

parseTypeObject :: Parser -> Bool -> ReadP TypeObject
parseTypeObject = parse parseTypeObject'

parseOptImmTypeObject :: Parser -> ReadP TypeObject
parseOptImmTypeObject = parse parseOptImmTypeObject'

parseVecLit :: Parser -> ReadP Lit
parseVecLit = parse parseVecLit'

parseDeclStat :: Parser -> ReadP DeclStat
parseDeclStat = parse parseDeclStat'

parseDeclStats :: Parser -> ReadP [DeclStat]
parseDeclStats = parse parseDeclStats'

parseBlockBody :: Parser -> ReadP BlockBody
parseBlockBody = parse parseBlockBody'
