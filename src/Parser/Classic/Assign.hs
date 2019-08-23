module Parser.Classic.Assign
  ( parseAssign
  ) where

import Ast (DeclStat(Assign))

import Parser.Classic.Exprs (parseExprList)
import Parser.Classic.Idents (parseIdentList)

import Parser.Common (stoken)
import Parser.Types (Parser)

import Text.ParserCombinators.ReadP (ReadP)

parseAssign :: Parser -> ReadP DeclStat
parseAssign p = do
  (i, is) <- parseIdentList
  stoken "<-"
  (e, es) <- parseExprList p
  return $ Assign ((i, is), (e, es))
