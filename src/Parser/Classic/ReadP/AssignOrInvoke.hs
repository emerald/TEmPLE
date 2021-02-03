module Parser.Classic.ReadP.AssignOrInvoke
  ( parseAssignOrInvoke
  ) where

import Ast ( AssignOrInvoke(..) )

import Parser.Classic.ReadP.ProcInvocs ( parseProcInvoc )

import Parser.Classic.ReadP.Exprs ( parseExprList, parseExprZeroList )

import Parser.Utils.ReadP (stoken)
import Parser.Types (Parser)

import Text.ParserCombinators.ReadP (ReadP, choice)

parseAssignOrInvoke :: Parser -> ReadP AssignOrInvoke
parseAssignOrInvoke p = choice
  [ parseAssign p
  , fmap Invoke $ parseProcInvoc p
  ]

parseAssign :: Parser -> ReadP AssignOrInvoke
parseAssign p = do
  il <- parseExprZeroList p
  stoken "<-"
  parseExprList p >>= return . AssignExpr . ((,) il)
