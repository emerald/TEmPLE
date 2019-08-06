module Parser.ClassicExprs
  ( parseExpr
  ) where

import Ast (Expr(..))
import Parser.Common (token, stoken, word)
import Parser.ClassicIdents (parseIdent)
import Parser.ClassicLits (parseLit)
import Parser.Types (Parser, parseObject)

import Text.ParserCombinators.ReadP (ReadP, between, chainl1, choice)

unzipWith :: (a -> b -> c) -> [(a, b)] -> [c]
unzipWith f pairs = map ( \ (a, b) -> f a b ) pairs

stokena :: String -> a -> ReadP a
stokena s a = stoken s *> return a

parseExpr9 :: Parser -> ReadP Expr
parseExpr9 p = choice
  [ fmap ELit parseLit
  , fmap EVar parseIdent
  , fmap EObj (parseObject p)
  , between (stoken "(") (stoken ")") (parseExpr p)
  ]

parseExpr7 :: Parser -> ReadP Expr
parseExpr7 p = chainl1 (parseExpr9 p) $ word
    [ ("*", ETimes)
    , ("/", EDiv)
    , ("#", EMod)
    ]

parseExpr6 :: Parser -> ReadP Expr
parseExpr6 p = chainl1 (parseExpr7 p) $ word
    [ ("+",  EPlus)
    , ("-", EMinus)
    ]

parseExpr5 :: Parser -> ReadP Expr
parseExpr5 p = chainl1 (parseExpr6 p) $ word
    [ ("=",  EEq)
    , ("!=", ENeq)
    , ("<",  ELt)
    , ("<=", ELeq)
    , (">",  EGt)
    , (">=", EGeq)
    ]

parseExpr :: Parser -> ReadP Expr
parseExpr = parseExpr5
