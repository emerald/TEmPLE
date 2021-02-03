module Parser.Classic.ReadP.Decls
  ( parseObjConstrDecl
  , parseConstDecl
  , parseVarDecl
  , parseDecl
  ) where

import Ast ( ConstDecl(..), Decl(..), VarDecl(..), Operation )
import Parser.Common (stoken, stoken1, token)
import Parser.Classic.ReadP.Attached (parseAttached)
import Parser.Classic.ReadP.Idents (parseIdent, parseIdentList)
import Parser.Classic.ReadP.Types (parseType)
import Parser.Classic.ReadP.Transforms ( makeConstField, makeVarField )
import Parser.Types (Parser, parseExpr)

import qualified Parser.Classic.Words as W
  ( Keywords(Const, Field, Var))

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

parseObjConstrDecl :: Parser -> ReadP ((Bool, Decl), [Operation])
-- ^ Parse a declaration inside an object constructor.
--
-- Here, fields are allowed, and the declarations may be attached to
-- the object instance.
parseObjConstrDecl p = token $ do
  a <- parseAttached
  (d, ops) <- choice [ parseConst p, parseVar p ]
  return ((a, d), ops)
