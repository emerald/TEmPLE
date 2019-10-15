module Parser.Classic.AssignOrInvoke
  ( parseAssignOrInvoke
  ) where

import Ast ( AssignOrInvoke(..) )

import Parser.Classic.ProcInvocs ( parseProcInvoc )

import Parser.Classic.Exprs ( parseExprList, parseExprZeroList )

import Parser.Common (stoken)
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
