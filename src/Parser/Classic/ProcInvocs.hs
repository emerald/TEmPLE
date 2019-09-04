module Parser.Classic.ProcInvocs
  ( parseProcInvoc
  ) where

import Ast (ArgType(..), ProcInvoc(..), Expr)

import Parser.Classic.Exprs (parseExpr)
import Parser.Classic.Idents (parseIdent)
import Parser.Classic.Operators (parseOperator)

import Parser.Common (commaList, stoken, word1)
import Parser.Types (Parser)

import Data.List.NonEmpty (toList)
import Control.Applicative (liftA2)

import Text.ParserCombinators.ReadP (ReadP, between, choice)

parseProcInvoc :: Parser -> ReadP ProcInvoc
parseProcInvoc p = do
  e <- parseExpr p
  stoken "."
  op <- choice [ parseIdent, parseOperator ]
  args <- parseArgs p
  return $ ProcInvoc (e, op, args)

parseArgs :: Parser -> ReadP [(ArgType, Expr)]
parseArgs p = choice
  [ between (stoken "[") (stoken "]")
    (fmap toList $ commaList $ parseArg p)
  , return []
  ]

parseArg :: Parser -> ReadP (ArgType, Expr)
parseArg p = liftA2 (,)
  (choice
    [ word1 [("move", ArgMove), ("visit", ArgVisit)]
    , return ArgSend
    ])
  (parseExpr p)
