module Parser.ClassicConstDecls
  ( parseConstDecl
  ) where

import Ast (ConstDecl(..))
import Parser.Common (stoken, stoken1)
import Parser.ClassicNames (parseName)
import Parser.ClassicTypes (parseType)
import Parser.ClassicExprs (parseExpr)

import Control.Applicative ((*>), liftA3)
import qualified Control.Applicative as App
import Text.ParserCombinators.ReadP (ReadP)

parseConstDecl :: ReadP ConstDecl
parseConstDecl = liftA3 Const
  (stoken1 "const" *> parseName)
  (App.optional parseType)
  (stoken "<-" *> parseExpr)
