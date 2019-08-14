module Parser.Classic.Objects
  ( parseObject
  ) where

import Ast (Object(..), DeclStat(..))
import Parser.Common (stoken, stoken1)
import Parser.Classic.Idents (parseIdent)
import Parser.Types (Parser, parseAttDecl, parseDecl)

import qualified Parser.Classic.Words as W
  ( Keywords(Object, End, Initially) )

import Control.Applicative (optional)
import Control.Monad (void)
import Text.ParserCombinators.ReadP (ReadP, many)

parseObject :: Parser -> ReadP Object
parseObject p = do
  name <- (stoken1 (show W.Object) *> parseIdent)
  decls <- many (parseAttDecl p)
  initially <- parseInitially p
  void (stoken1 (show W.End) >> stoken name)
  return $ Object name decls initially

parseInitially :: Parser -> ReadP (Maybe [DeclStat])
parseInitially p = optional $
  stoken1 (show W.Initially) *>
  parseDeclStats p <*
  stoken1 (show W.End) <*
  stoken1 (show W.Initially)

parseDeclStats :: Parser -> ReadP [DeclStat]
parseDeclStats p = many $ fmap Decl $ parseDecl p
