module Parser.Types
  ( Parser(Parser)
  , parseExpr
  , parseTypeObject
  , parseOptImmTypeObject
  , parseDeclStat
  , parseDeclStats
  ) where

import Ast
  ( Expr
  , TypeObject
  , DeclStat
  , Lit
  , Operation
  )

import Text.ParserCombinators.ReadP (ReadP)

data Parser
  = Parser
  { parseExpr' :: Parser -> ReadP Expr
  , parseTypeObject' :: Parser -> Bool -> ReadP TypeObject
  , parseOptImmTypeObject' :: Parser -> ReadP TypeObject
  , parseDeclStat' :: Parser -> ReadP DeclStat
  , parseDeclStats' :: Parser -> ReadP [DeclStat]
  }

parse :: (Parser -> Parser -> a) -> Parser -> a
parse f p = f p p

parseExpr :: Parser -> ReadP Expr
parseExpr = parse parseExpr'

parseTypeObject :: Parser -> Bool -> ReadP TypeObject
parseTypeObject = parse parseTypeObject'

parseOptImmTypeObject :: Parser -> ReadP TypeObject
parseOptImmTypeObject = parse parseOptImmTypeObject'

parseDeclStat :: Parser -> ReadP DeclStat
parseDeclStat = parse parseDeclStat'

parseDeclStats :: Parser -> ReadP [DeclStat]
parseDeclStats = parse parseDeclStats'
