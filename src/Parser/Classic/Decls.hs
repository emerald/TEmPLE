module Parser.Classic.Decls
  ( parseAttDecl
  , parseConstDecl
  , parseVarDecl
  , parseDecl
  ) where

import Ast ( ConstDecl(..), Decl(..), VarDecl(..), Operation )
import Parser.Common (stoken, stoken1, token)
import Parser.Classic.Attached (parseAttached)
import Parser.Classic.Idents (parseIdent, parseIdentList)
import Parser.Classic.Types (parseType)
import Parser.Classic.Transforms ( makeConstField, makeVarField )
import Parser.Classic.Exprs (parseExpr)
import Parser.Types (Parser)

import qualified Parser.Classic.Words as W
  ( Keywords(Const, Field, Var))

import Control.Applicative ((*>))
import Control.Applicative (optional)
import Text.ParserCombinators.ReadP (ReadP, choice)

parseConstDecl' :: Parser -> ReadP ConstDecl
parseConstDecl' p = do
  i <- parseIdent
  mt <- optional parseType
  e <- (stoken "<-" *> (parseExpr p))
  return $ Const (i, mt, e)

parseConstDecl :: Parser -> ReadP ConstDecl
parseConstDecl p = stoken1 (show W.Const) *> parseConstDecl' p

parseVarDecl' :: Parser -> ReadP VarDecl
parseVarDecl' p = do
  il <- parseIdentList
  t <- parseType
  me <- optional (stoken "<-" *> (parseExpr p))
  return $ Var (il, t, me)

parseVarDecl :: Parser -> ReadP VarDecl
parseVarDecl p = stoken1 (show W.Var) *> parseVarDecl' p

parseConstField :: Parser -> ReadP (Decl, [Operation])
parseConstField p = do
  i <- parseIdent
  t <- parseType
  e <- (stoken "<-" *> (parseExpr p))
  return $ makeConstField (i, t, e)

parseVarField :: Parser -> ReadP (Decl, [Operation])
parseVarField p = do
  i <- parseIdent
  t <- parseType
  me <- optional (stoken "<-" *> (parseExpr p))
  return $ makeVarField (i, t, me)

parseConst :: Parser -> ReadP (Decl, [Operation])
parseConst p = stoken1 (show W.Const) *> choice
  [ stoken1 (show W.Field) *> parseConstField p
  , fmap (flip (,) [] . DConst) $ parseConstDecl' p
  ]

parseVar :: Parser -> ReadP (Decl, [Operation])
parseVar p = choice
  [ stoken1 (show W.Field) *> parseVarField p
  , fmap (flip (,) [] . DVar) $ parseVarDecl p
  ]

parseDecl :: Parser -> ReadP Decl
parseDecl p = choice
  [ fmap DConst $ parseConstDecl p
  , fmap DVar $ parseVarDecl p
  ]

parseAttDecl :: Parser -> ReadP ((Bool, Decl), [Operation])
parseAttDecl p = token $ do
  a <- parseAttached
  (d, ops) <- choice [ parseConst p, parseVar p ]
  return ((a, d), ops)
