module Parser.ClassicExprs
  ( parseExpr
  ) where

import Ast (Expr(..))
import Parser.Common (token, stoken)
import Parser.ClassicNames (parseName)
import Parser.ClassicLits (parseLit)

import Text.ParserCombinators.ReadP (ReadP, between, choice)

parseExpr :: ReadP Expr
parseExpr = token $ choice
  [ fmap ELit parseLit
  , fmap EVar parseName
  , between (stoken "(") (stoken ")") parseExpr
  ]
