module Parser.Classic.Exprs
  ( parseExpr, parseExprList
  , parseExprZero, parseExprZeroList
  , prec2, prec3, prec4
  , prec5, prec6, prec7, prec8
  ) where

import Ast (Expr(..))
import Parser.Common (commaList, prefixInfix, stoken, word)
import Parser.Classic.Idents (parseIdent)
import Parser.Classic.Lits (parseLit)
import Parser.Classic.ProcInvocs (parseProcInvoc')
import Parser.Types (Parser)

import qualified Parser.Classic.Words as W
  ( Keywords(..) )

import Data.List.NonEmpty (NonEmpty)
import Text.ParserCombinators.ReadP
  ( ReadP, (<++)
  , between, choice
  )

parseExprZero :: Parser -> ReadP Expr
parseExprZero = parseExpr9

parseExprZeroList :: Parser -> ReadP (NonEmpty Expr)
parseExprZeroList = commaList . parseExpr

parseExpr10 :: Parser -> ReadP Expr
parseExpr10 p = choice
  [ fmap ELit $ parseLit p
  , fmap EVar parseIdent
  , between (stoken "(") (stoken ")") (parseExpr p)
  ]

parseExpr9 :: Parser -> ReadP Expr
parseExpr9 p = parseExpr10 p >>= \e -> choice
  [ stoken "$" *> fmap (ESelect e) parseIdent
  , stoken "." *> fmap EInvoke (parseProcInvoc' p e)
  , return e
  ]

prec8 :: [(String, Expr -> Expr)]
prec8
  = [ ("-", ENegate)
    , ("~", ENegate)
    , (show W.Locate,    ELocate)
    , (show W.IsFixed,   EIsFixed)
    , (show W.IsLocal,   EIsLocal)
    , (show W.Awaiting,  EAwaiting)
    , (show W.CodeOf,    ECodeOf)
    , (show W.NameOf,    ENameOf)
    , (show W.TypeOf,    ETypeOf)
    , (show W.SynTypeOf, ESynTypeOf)
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
parseExpr7 p = chainl1' p parseExpr8 $ word prec7

prec6 :: [(String, Expr -> Expr -> Expr)]
prec6
  = [ ("+", EPlus)
    , ("-", EMinus)
    ]

parseExpr6 :: Parser -> ReadP Expr
parseExpr6 p = chainl1' p parseExpr7 $ word prec6

prec5 :: [(String, Expr -> Expr -> Expr)]
prec5
  = [ ("=",  EEq)
    , ("!=", ENeq)
    , ("*>", EConfTo)
    , ("<",  ELt)
    , ("<=", ELeq)
    , (">",  EGt)
    , (">=", EGeq)
    ]

parseExpr5 :: Parser -> ReadP Expr
parseExpr5 p = chainl1' p parseExpr6 $ word prec5

chainl1' :: Parser -> (Parser -> ReadP Expr) -> ReadP (Expr -> Expr -> Expr) -> ReadP Expr
chainl1' p fp op = fp p >>= rest
  where
  rest x = choice
    [ return x
    , do  f <- op
          y <- parseUnaryExpr p fp
          rest (f x y)
    ]

parseUnaryExpr :: Parser -> (Parser -> ReadP Expr) -> ReadP Expr
parseUnaryExpr p f = choice
  [ word prec4 <*> (parseExpr4 p)
  , prefixInfix EViewAs     W.View      W.As (parseExpr1 p)
  , prefixInfix ERestrictTo W.Restrict  W.To (parseExpr1 p)
  ] <++ f p

prec4 :: [(String, Expr -> Expr)]
prec4
  = [ ("!",  ENeg)
    ]

parseExpr4 :: Parser -> ReadP Expr
parseExpr4 p = choice
  [ word prec4 <*> (parseExpr4 p)
  , parseExpr5 p
  ]

prec3 :: [(String, Expr -> Expr -> Expr)]
prec3
  = [ ("&", EAnd)
    , (show W.And, EShortAnd)
    ]

parseExpr3 :: Parser -> ReadP Expr
parseExpr3 p = chainl1' p parseExpr4 $ word prec3

prec2 :: [(String, Expr -> Expr -> Expr)]
prec2
  = [ ("|", EOr)
    , (show W.Or, EShortOr)
    ]

parseExpr2 :: Parser -> ReadP Expr
parseExpr2 p = chainl1' p parseExpr3 $ word prec2

parseExpr1 :: Parser -> ReadP Expr
parseExpr1 p = choice
  [ prefixInfix EViewAs     W.View      W.As (parseExpr1 p)
  , prefixInfix ERestrictTo W.Restrict  W.To (parseExpr1 p)
  , parseExpr2 p
  ]

parseExpr :: Parser -> ReadP Expr
parseExpr = parseExpr1

parseExprList :: Parser -> ReadP (NonEmpty Expr)
parseExprList p = commaList $ parseExpr p
