module Parser.Classic.Objects
  ( parseObject
  ) where

import Ast (Object(..))
import Parser.Common (stoken, stoken1)
import Parser.Classic.Idents (parseIdent)
import Parser.Types (Parser, parseAttDecl)

import qualified Parser.Classic.Words as W
  ( Keywords(Object, End) )

import Control.Monad (void)
import Text.ParserCombinators.ReadP (ReadP, many)

parseObject :: Parser -> ReadP Object
parseObject p = do
  name <- (stoken1 (show W.Object) *> parseIdent)
  decls <- many (parseAttDecl p)
  void (stoken1 (show W.End) >> stoken name)
  return $ Object name decls
