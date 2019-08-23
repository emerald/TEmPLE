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
  il <- parseIdentList
  stoken "<-"
  el <- parseExprList p
  return $ Assign (il, el)
