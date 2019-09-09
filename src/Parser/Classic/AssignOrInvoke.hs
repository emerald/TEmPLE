module Parser.Classic.AssignOrInvoke
  ( parseAssignOrInvoke
  ) where

import Ast
  ( AssignOrInvoke(..)
  , ArgType(..), ProcInvoc(..), Expr
  )

import Parser.Classic.Idents (parseIdent)
import Parser.Classic.Operators (parseOperator)

import Parser.Classic.Exprs
  ( parseExpr, parseExprList
  , parseExprZero, parseExprZeroList
  )

import Parser.Common (commaList, stoken, word1)
import Parser.Types (Parser)

import Data.List.NonEmpty (toList)
import Control.Applicative (liftA2)
import Text.ParserCombinators.ReadP (ReadP, between, choice)

parseAssignOrInvoke :: Parser -> ReadP AssignOrInvoke
parseAssignOrInvoke p = choice
  [ parseAssign p
  , fmap Invoke $ parseProcInvoc p
  ]

parseAssign :: Parser -> ReadP AssignOrInvoke
parseAssign p = do
  il <- parseExprZeroList p
  stoken "<-"
  choice
    [ parseExprList p >>= return . AssignExpr . ((,) il)
    , parseProcInvoc p >>= return . AssignInvoke . ((,) il)
    ]

parseProcInvoc :: Parser -> ReadP ProcInvoc
parseProcInvoc p = do
  e <- parseExprZero p
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
