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
import Text.ParserCombinators.ReadP (ReadP, choice)

parseDecl' :: String
  -> ((Ident, Maybe Type, Expr) -> a)
  -> Parser -> ReadP a
parseDecl' k c p = do
  i <- (stoken1 k *> parseIdent)
  t <- (App.optional parseType)
  e <- (stoken "<-" *> (parseExpr p))
  return $ c (i, t, e)

parseConstDecl :: Parser -> ReadP ConstDecl
parseConstDecl = parseDecl' "const" Const

parseVarDecl :: Parser -> ReadP VarDecl
parseVarDecl = parseDecl' "var" Var

parseDecl :: Parser -> ReadP Decl
parseDecl p = token $ choice
  [ fmap DConst $ parseConstDecl p
  , fmap DVar $ parseVarDecl p
  ]
