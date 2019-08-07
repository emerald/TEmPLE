module Parser.Classic.Objects
  ( parseObject
  ) where

import Ast (Object(..))
import Parser.Common (stoken, stoken1)
import Parser.Classic.Idents (parseIdent)
import Parser.Classic.Words (WKeywords(WObject, WEnd))
import Parser.Types (Parser, parseAttDecl)

import Control.Monad (void)
import Text.ParserCombinators.ReadP (ReadP, many)

parseObject :: Parser -> ReadP Object
parseObject p = do
  name <- (stoken1 (show WObject) *> parseIdent)
  decls <- many (parseAttDecl p)
  void (stoken1 (show WEnd) >> stoken name)
  return $ Object name decls
