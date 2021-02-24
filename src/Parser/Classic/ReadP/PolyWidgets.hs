-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.PolyWidgets
  ( parsePolyWidget
  ) where

import Ast ( PolyWidget(..) )

import qualified Parser.Classic.Words as W
  ( Keywords(ForAll, SuchThat, Where) )

import Parser.Classic.ReadP.Idents ( parseIdent )
import Parser.Utils.ReadP ( prefix, stoken )
import Parser.Types ( Parser, parseOptImmTypeObject, parseExpr )

import Text.ParserCombinators.ReadP ( ReadP, choice )

parsePolyWidget :: Parser -> ReadP PolyWidget
parsePolyWidget p = choice
  [ parseForAll
  , parseSuchThat p
  , parseWhere p
  ]

parseForAll :: ReadP PolyWidget
parseForAll = prefix ForAll (W.ForAll) parseIdent

parseWhere :: Parser -> ReadP PolyWidget
parseWhere p = prefix Where (W.Where) $ do
  ident <- parseIdent
  stoken "<-"
  expr <- parseExpr p
  return (ident, expr)

parseSuchThat :: Parser -> ReadP PolyWidget
parseSuchThat p = prefix SuchThat (W.SuchThat) $ do
  ident <- parseIdent
  stoken "*>"
  to <- parseOptImmTypeObject p
  return (ident, to)
