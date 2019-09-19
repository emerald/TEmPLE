module Parser.Classic.Classes
  ( parseClass
  ) where

import Ast ( Class(Class) )

import Parser.Common ( stoken, stoken1 )
import Parser.Classic.Builtins ( parseBuiltin )
import Parser.Classic.Idents ( parseIdent, prefixedIdent )
import Parser.Classic.Operations ( parseOperation )
import Parser.Classic.Params ( parseOptParams )
import Parser.Types ( Parser, parseObjectBody )

import qualified Parser.Classic.Words as W
  ( Keywords( Class, End ) )

import Control.Applicative ( optional )
import Control.Monad ( void )
import Text.ParserCombinators.ReadP ( ReadP, between, many )

parseClass :: Parser -> Bool -> Bool -> ReadP Class
parseClass p imm mon = do
  name <- prefixedIdent W.Class
  baseclass <- optional $ between (stoken "(") (stoken ")") parseIdent
  params <- parseOptParams
  builtin <- optional $ parseBuiltin
  classOps <- many $ (stoken1 (show W.Class) *> parseOperation p)
  body <- parseObjectBody p
  void (stoken1 (show W.End) >> stoken1 name)
  return $ Class ((imm, mon), builtin, name, baseclass, params, classOps, body)
