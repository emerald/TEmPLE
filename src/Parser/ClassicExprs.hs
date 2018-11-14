module Parser.ClassicExprs
  ( parseExpr
  ) where

import Ast (Expr(..))
import Parser.Common (token)
import Parser.ClassicNames (parseName)
import Parser.ClassicLits (parseLit)

import Text.ParserCombinators.ReadP (ReadP, choice)

parseExpr :: ReadP Expr
parseExpr = token $ choice
  [ fmap ELit parseLit
  , fmap EVar parseName
  ]
