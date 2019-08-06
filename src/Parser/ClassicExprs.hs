module Parser.ClassicExprs
  ( parseExpr
  ) where

import Ast (Expr(..))
import Parser.Common (token, stoken)
import Parser.ClassicIdents (parseIdent)
import Parser.ClassicLits (parseLit)
import Parser.Types (Parser, parseObject)

import Text.ParserCombinators.ReadP (ReadP, between, chainl1, choice)

unzipWith :: (a -> b -> c) -> [(a, b)] -> [c]
unzipWith f pairs = map ( \ (a, b) -> f a b ) pairs

stokena :: String -> a -> ReadP a
stokena s a = stoken s *> return a

parseExpr0 :: Parser -> ReadP Expr
parseExpr0 p = token $ choice
  [ fmap ELit parseLit
  , fmap EVar parseIdent
  , fmap EObj (parseObject p)
  , between (stoken "(") (stoken ")") (parseExpr p)
  ]

parseExpr5 :: Parser -> ReadP Expr
parseExpr5 p = chainl1 (parseExpr0 p) $
  choice $ unzipWith stokena $
    [ ("=",  EEq)
    , ("!=", ENeq)
    , ("<",  ELt)
    , ("<=", ELeq)
    , (">",  EGt)
    , (">=", EGeq)
    ]

parseExpr :: Parser -> ReadP Expr
parseExpr = parseExpr5
