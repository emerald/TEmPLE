module Parser.Classic.Assign
  ( parseAssign
  ) where

import Ast (DeclStat(AssignExpr, AssignInvoke))

import Parser.Classic.Exprs (parseExprList)
import Parser.Classic.Idents (parseIdentList)
import Parser.Classic.ProcInvocs (parseProcInvoc)

import Parser.Common (stoken)
import Parser.Types (Parser)

import Text.ParserCombinators.ReadP (ReadP, choice)

parseAssign :: Parser -> ReadP DeclStat
parseAssign p = do
  il <- parseIdentList
  stoken "<-"
  choice
    [ parseExprList p >>= return . AssignExpr . ((,) il)
    , parseProcInvoc p >>= return . AssignInvoke . ((,) il)
    ]
