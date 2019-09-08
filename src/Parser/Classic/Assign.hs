module Parser.Classic.Assign
  ( parseAssign
  ) where

import Ast (DeclStat(AssignExpr, AssignInvoke))

import Parser.Classic.Exprs (parseExprList, parseExprZeroList)
import Parser.Classic.ProcInvocs (parseProcInvoc)

import Parser.Common (stoken)
import Parser.Types (Parser)

import Text.ParserCombinators.ReadP (ReadP, choice)

parseAssign :: Parser -> ReadP DeclStat
parseAssign p = do
  il <- parseExprZeroList p
  stoken "<-"
  choice
    [ parseExprList p >>= return . AssignExpr . ((,) il)
    , parseProcInvoc p >>= return . AssignInvoke . ((,) il)
    ]
