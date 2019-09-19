module Parser.Classic.Enums
  ( parseEnum
  ) where

import Ast ( Enumeration(Enum) )

import Parser.Classic.Idents ( parseIdent, prefixedIdent )

import qualified Parser.Classic.Words as W
  ( Keywords( Enumeration, End ) )

import Parser.Common ( commaList, stoken1 )

import Control.Monad ( void )
import Text.ParserCombinators.ReadP ( ReadP )

parseEnum :: ReadP Enumeration
parseEnum = do
  name <- prefixedIdent W.Enumeration
  idents <- commaList parseIdent
  void (stoken1 (show W.End) >> stoken1 name)
  return $ Enum (name, idents)
