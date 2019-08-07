module Parser.Classic.Decls
  ( parseAttDecl
  , parseConstDecl
  , parseVarDecl
  , parseDecl
  ) where

import Ast (ConstDecl(..), Decl(..), Expr, Type, VarDecl(..))
import Parser.Common (stoken, stoken1, token)
import Parser.Classic.Attached (parseAttached)
import Parser.Classic.Idents (parseIdent)
import Parser.Classic.Types (parseType)
import Parser.Classic.Exprs (parseExpr)
import Parser.Classic.Words (WKeywords(WConst, WVar))
import Parser.Types (Parser)

import Control.Applicative ((*>))
import qualified Control.Applicative as App
import Text.ParserCombinators.ReadP (ReadP, choice, many)

parseDeclTypeExpr :: Parser -> ReadP (Maybe Type, Expr)
parseDeclTypeExpr p = do
  t <- (App.optional parseType)
  e <- (stoken "<-" *> (parseExpr p))
  return (t, e)

parseConstDecl :: Parser -> ReadP ConstDecl
parseConstDecl p = do
  stoken1 (show WConst)
  i <- parseIdent
  (t, e) <- parseDeclTypeExpr p
  return $ Const (i, t, e)

parseVarDecl :: Parser -> ReadP VarDecl
parseVarDecl p = do
  stoken1 (show WVar)
  i <- parseIdent
  is <- many (stoken "," *> parseIdent)
  (t, e) <- parseDeclTypeExpr p
  return $ Var (i, is, t, e)

parseDecl :: Parser -> ReadP Decl
parseDecl p = choice
  [ fmap DConst $ parseConstDecl p
  , fmap DVar $ parseVarDecl p
  ]

parseAttDecl :: Parser -> ReadP (Bool, Decl)
parseAttDecl p = token $ do
  a <- parseAttached
  d <- parseDecl p
  return (a, d)
