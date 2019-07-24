module Parser.ClassicExprs
  ( parseExpr
  ) where

import Ast (Expr(..))
import Parser.Common (token, stoken)
import Parser.ClassicNames (parseName)
import Parser.ClassicLits (parseLit)
import Parser.Types (Parser, parseObject)

import Text.ParserCombinators.ReadP (ReadP, between, choice)

parseExpr :: Parser -> ReadP Expr
parseExpr p = token $ choice
  [ fmap ELit parseLit
  , fmap EVar parseName
  , fmap EObj (parseObject p)
  , between (stoken "(") (stoken ")") (parseExpr p)
  ]
