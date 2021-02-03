module Parser.Classic.ReadP.ProcInvocs
  ( parseProcInvoc, parseProcInvoc'
  ) where

import Ast (ArgType(..), ProcInvoc(..), Expr)

import Parser.Types (parseExpr, parseExprZero)
import Parser.Classic.ReadP.Idents (parseIdent)
import Parser.Classic.ReadP.Operators (parseOperator)

import Parser.Common (optCommaList, stoken, word1)
import Parser.Types (Parser)

import Control.Applicative (liftA2)

import Text.ParserCombinators.ReadP (ReadP, between, choice)

parseProcInvoc :: Parser -> ReadP ProcInvoc
parseProcInvoc p = do
  e <- parseExprZero p
  stoken "."
  parseProcInvoc' p e

parseProcInvoc' :: Parser -> Expr -> ReadP ProcInvoc
parseProcInvoc' p e = do
  op <- choice [ parseIdent, parseOperator ]
  args <- parseArgs p
  return $ ProcInvoc (e, op, args)

parseArgs :: Parser -> ReadP [(ArgType, Expr)]
parseArgs p = optCommaList (parseArg p)
  (between (stoken "[") (stoken "]"))

parseArg :: Parser -> ReadP (ArgType, Expr)
parseArg p = liftA2 (,)
  (choice
    [ word1 [("move", ArgMove), ("visit", ArgVisit)]
    , return ArgSend
    ])
  (parseExpr p)
