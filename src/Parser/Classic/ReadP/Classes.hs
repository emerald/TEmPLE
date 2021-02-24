-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.Classes
  ( parseClass
  ) where

import Ast ( Class(Class) )

import Parser.Utils.ReadP ( stoken, stoken1 )
import Parser.Classic.ReadP.Builtins ( parseBuiltin )
import Parser.Classic.ReadP.Common ( end )
import Parser.Classic.ReadP.Idents ( parseIdent, prefixedIdent )
import Parser.Classic.ReadP.Objects ( parseObjectBody )
import Parser.Classic.ReadP.Operations ( parseOperation )
import Parser.Classic.ReadP.Params ( parseOptParams )
import Parser.Types ( Parser )

import qualified Parser.Classic.Words as W
  ( Keywords( Class ) )

import Control.Applicative ( optional )
import Text.ParserCombinators.ReadP ( ReadP, between, many )

parseClass :: Parser -> Bool -> Bool -> ReadP Class
parseClass p imm mon = do
  name <- prefixedIdent W.Class
  baseclass <- optional $ between (stoken "(") (stoken ")") parseIdent
  params <- parseOptParams
  builtin <- optional $ parseBuiltin
  classOps <- many $ (stoken1 (show W.Class) *> parseOperation p)
  body <- parseObjectBody p
  end name
  return $ Class ((imm, mon), builtin, name, baseclass, params, classOps, body)
