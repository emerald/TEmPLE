module Parser.ClassicDecls
  ( parseConstDecl
  , parseVarDecl
  , parseDecl
  ) where

import Ast (ConstDecl(..), Decl(..), Expr, Ident, Type, VarDecl(..))
import Parser.Common (stoken, stoken1, token)
import Parser.ClassicIdents (parseIdent)
import Parser.ClassicTypes (parseType)
import Parser.ClassicExprs (parseExpr)
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
  stoken1 "const"
  i <- parseIdent
  (t, e) <- parseDeclTypeExpr p
  return $ Const (i, t, e)

parseVarDecl :: Parser -> ReadP VarDecl
parseVarDecl p = do
  stoken1 "var"
  i <- parseIdent
  is <- many (stoken "," *> parseIdent)
  (t, e) <- parseDeclTypeExpr p
  return $ Var (i, is, t, e)

parseDecl :: Parser -> ReadP Decl
parseDecl p = token $ choice
  [ fmap DConst $ parseConstDecl p
  , fmap DVar $ parseVarDecl p
  ]
