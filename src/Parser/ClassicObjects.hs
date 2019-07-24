module Parser.ClassicObjects
  ( parseObject
  ) where

import Ast (Object(..))
import Parser.Common (stoken, stoken1)
import Parser.ClassicIdents (parseIdent)
import Parser.Types (Parser, parseConstDecl)

import Control.Monad (void)
import Text.ParserCombinators.ReadP (ReadP, many)

parseObject :: Parser -> ReadP Object
parseObject p = do
  name <- (stoken1 "object" *> parseIdent)
  decls <- many (parseConstDecl p)
  void (stoken1 "end" >> stoken name)
  return $ Object name decls