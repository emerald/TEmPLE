module Parser.Types
  ( Parser(Parser)
  , parseExpr
  , parseExprZero
  , parseOptImmTypeObject
  , parseDeclStats
  ) where

import Ast ( Expr, DeclStat, TypeObject )

import Text.ParserCombinators.ReadP (ReadP)

data Parser
  = Parser
  { parseExpr' :: Parser -> ReadP Expr
  , parseExprZero' :: Parser -> ReadP Expr
  , parseOptImmTypeObject' :: Parser -> ReadP TypeObject
  , parseDeclStats' :: Parser -> ReadP [DeclStat]
  }

parse :: (Parser -> Parser -> a) -> Parser -> a
parse f p = f p p

parseExpr :: Parser -> ReadP Expr
parseExpr = parse parseExpr'

parseExprZero :: Parser -> ReadP Expr
parseExprZero = parse parseExprZero'

parseOptImmTypeObject :: Parser -> ReadP TypeObject
parseOptImmTypeObject = parse parseOptImmTypeObject'

parseDeclStats :: Parser -> ReadP [DeclStat]
parseDeclStats = parse parseDeclStats'
