module Parser.ClassicExprs
  ( parseExpr
  , prec5, prec6, prec7, prec8
  ) where

import Ast (Expr(..))
import Parser.Common (stoken, word)
import Parser.ClassicIdents (parseIdent)
import Parser.ClassicLits (parseLit)
import Parser.ClassicWords (WKeywords(..))
import Parser.Types (Parser, parseObject)

import Text.ParserCombinators.ReadP (ReadP, between, chainl1, choice)

parseExpr9 :: Parser -> ReadP Expr
parseExpr9 p = choice
  [ fmap ELit parseLit
  , fmap EVar parseIdent
  , fmap EObj (parseObject p)
  , between (stoken "(") (stoken ")") (parseExpr p)
  ]

prec8 :: [(String, Expr -> Expr)]
prec8
  = [ ("-", ENegate)
    , ("~", ENegate)
    , (show WLocate,    ELocate)
    , (show WIsFixed,   EIsFixed)
    , (show WIsLocal,   EIsLocal)
    , (show WAwaiting,  EAwaiting)
    , (show WCodeOf,    ECodeOf)
    , (show WNameOf,    ENameOf)
    , (show WTypeOf,    ETypeOf)
    , (show WSynTypeOf, ESynTypeOf)
    ]

parseExpr8 :: Parser -> ReadP Expr
parseExpr8 p = choice
  [ word prec8 <*> (parseExpr8 p)
  , parseExpr9 p
  ]

prec7 :: [(String, Expr -> Expr -> Expr)]
prec7
  = [ ("*", ETimes)
    , ("/", EDiv)
    , ("#", EMod)
    ]

parseExpr7 :: Parser -> ReadP Expr
parseExpr7 p = chainl1 (parseExpr8 p) $ word prec7

prec6 :: [(String, Expr -> Expr -> Expr)]
prec6
  = [ ("+", EPlus)
    , ("-", EMinus)
    ]

parseExpr6 :: Parser -> ReadP Expr
parseExpr6 p = chainl1 (parseExpr7 p) $ word prec6

prec5 :: [(String, Expr -> Expr -> Expr)]
prec5
  = [ ("=",  EEq)
    , ("!=", ENeq)
    , ("<",  ELt)
    , ("<=", ELeq)
    , (">",  EGt)
    , (">=", EGeq)
    ]

parseExpr5 :: Parser -> ReadP Expr
parseExpr5 p = chainl1 (parseExpr6 p) $ word prec5

parseExpr :: Parser -> ReadP Expr
parseExpr = parseExpr5
