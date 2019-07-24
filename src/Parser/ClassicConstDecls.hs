module Parser.ClassicConstDecls
  ( parseConstDecl
  ) where

import Ast (ConstDecl(..))
import Parser.Common (stoken, stoken1)
import Parser.ClassicIdents (parseIdent)
import Parser.ClassicTypes (parseType)
import Parser.ClassicExprs (parseExpr)
import Parser.Types (Parser)

import Control.Applicative ((*>), liftA3)
import qualified Control.Applicative as App
import Text.ParserCombinators.ReadP (ReadP)

parseConstDecl :: Parser -> ReadP ConstDecl
parseConstDecl p = liftA3 Const
  (stoken1 "const" *> parseIdent)
  (App.optional parseType)
  (stoken "<-" *> (parseExpr p))
