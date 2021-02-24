-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.Records
  ( parseRecord
  ) where

import Ast
  ( Class(Class), Ident
  , ObjectBody(ObjectBody)
  , Param(Param), Type(..)
  )

import Parser.Classic.ReadP.Common ( end )
import Parser.Classic.ReadP.Idents ( parseIdent, prefixedIdent )
import Parser.Classic.ReadP.Types ( parseType )
import Parser.Classic.ReadP.Transforms ( makeVarField )

import qualified Parser.Classic.Words as W
  ( Keywords( Attached, Var, Record ) )

import Parser.Utils.ReadP ( stoken1, stoken1Bool )

import Text.ParserCombinators.ReadP ( ReadP, optional, many1 )

parseRecord :: Bool -> ReadP Class
parseRecord imm = do
  name <- prefixedIdent W.Record
  rFields <- many1 parseRecordField
  end name
  let params = map
        (\(_, i, t) -> Param (False, Just i, t)) rFields
  let (atts, fields) = unzip $ map
        (\(att, i, t) -> (att, (i, t, Nothing))) rFields
  let (decls, ops') = unzip $ map makeVarField fields
  let vars = zip atts decls
  let ops = concat ops'
  return $ Class
    ( (imm, False)
    , Nothing
    , name
    , Nothing -- base class
    , params
    , []
    , ObjectBody
      ( vars
      , ops
      , Nothing
      , Nothing
      , Nothing
      )
    )

parseRecordField :: ReadP (Bool, Ident, Type)
parseRecordField = do
  att <- stoken1Bool (show W.Attached)
  optional $ stoken1 (show W.Var)
  i <- parseIdent
  t <- parseType
  return (att, i, t)
