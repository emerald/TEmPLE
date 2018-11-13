module Parser.ClassicExprs
  ( parseExpr
  ) where

import Ast (Expr(..))
import Parser.Common (token)
import Parser.ClassicNames (parseName)

import Text.ParserCombinators.ReadP (ReadP)

parseExpr :: ReadP Expr
parseExpr = token $ parseName >>= return . EVar
