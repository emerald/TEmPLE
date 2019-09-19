module Parser.Classic.Enums
  ( parseEnum
  ) where

import Ast ( Enumeration(Enum) )

import Parser.Classic.Common ( end )
import Parser.Classic.Idents ( parseIdent, prefixedIdent )

import qualified Parser.Classic.Words as W
  ( Keywords( Enumeration ) )

import Parser.Common ( commaList )

import Text.ParserCombinators.ReadP ( ReadP )

parseEnum :: ReadP Enumeration
parseEnum = do
  name <- prefixedIdent W.Enumeration
  idents <- commaList parseIdent
  end name
  return $ Enum (name, idents)
