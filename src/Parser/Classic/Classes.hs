module Parser.Classic.Classes
  ( parseClass
  ) where

import Ast ( Class(Class) )

import Parser.Common ( stoken, stoken1, stoken1Bool )
import Parser.Classic.Idents ( parseIdent )
import Parser.Classic.Operations ( parseOperation )
import Parser.Classic.Params ( parseOptParams )
import Parser.Types ( Parser, parseObjectBody )

import qualified Parser.Classic.Words as W
  ( Keywords( Class, End, Immutable, Monitor ) )

import Control.Applicative ( optional )
import Control.Monad ( void )
import Text.ParserCombinators.ReadP ( ReadP, between, many )

parseClass :: Parser -> ReadP Class
parseClass p = do
  immutable <- stoken1Bool (show W.Immutable)
  monitor <- stoken1Bool (show W.Monitor)
  name <- (stoken1 (show W.Class) *> parseIdent)
  baseclass <- optional $ between (stoken "(") (stoken ")") parseIdent
  params <- parseOptParams
  classOps <- many $ (stoken1 (show W.Class) *> parseOperation p)
  body <- parseObjectBody p
  void (stoken1 (show W.End) >> stoken1 name)
  return $ Class (immutable, monitor, name, baseclass, params, classOps, body)
