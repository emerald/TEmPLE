module Parser.Classic.PolyWidgets
  ( parsePolyWidget
  ) where

import Ast ( PolyWidget(..) )

import qualified Parser.Classic.Words as W
  ( Keywords(ForAll, Where) )

import Parser.Classic.Exprs ( parseExpr )
import Parser.Classic.Idents ( parseIdent )
import Parser.Common ( prefix, stoken )
import Parser.Types ( Parser )

import Text.ParserCombinators.ReadP ( ReadP, choice )

parsePolyWidget :: Parser -> ReadP PolyWidget
parsePolyWidget p = choice
  [ parseForAll
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