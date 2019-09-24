module Parser.Types
  ( Parser(Parser)
  , parseExpr
  , parseObjConstrDecl
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
  ( Expr
  , Object
  , ObjectBody
  , TypeObject
  , BlockBody
  , Decl
  , DeclStat
  , Lit
  , Operation
  )

import Text.ParserCombinators.ReadP (ReadP)

data Parser
  = Parser
  { parseExpr' :: Parser -> ReadP Expr
  , parseObject' :: Parser -> Bool -> Bool -> ReadP Object
  , parseObjectBody' :: Parser -> ReadP ObjectBody
  , parseTypeObject' :: Parser -> Bool -> ReadP TypeObject
  , parseOptImmTypeObject' :: Parser -> ReadP TypeObject
  , parseObjConstrDecl' :: Parser -> ReadP ((Bool, Decl), [Operation])
  , parseDecl' :: Parser -> ReadP Decl
  , parseDeclStat' :: Parser -> ReadP DeclStat
  , parseDeclStats' :: Parser -> ReadP [DeclStat]
  , parseBlockBody' :: Parser -> ReadP BlockBody
  }

parse :: (Parser -> Parser -> a) -> Parser -> a
parse f p = f p p

parseExpr :: Parser -> ReadP Expr
parseExpr = parse parseExpr'

parseObjConstrDecl :: Parser -> ReadP ((Bool, Decl), [Operation])
parseObjConstrDecl = parse parseObjConstrDecl'

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

parseDeclStat :: Parser -> ReadP DeclStat
parseDeclStat = parse parseDeclStat'

parseDeclStats :: Parser -> ReadP [DeclStat]
parseDeclStats = parse parseDeclStats'

parseBlockBody :: Parser -> ReadP BlockBody
parseBlockBody = parse parseBlockBody'
