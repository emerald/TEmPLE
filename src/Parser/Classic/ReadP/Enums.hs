-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.Enums
  ( parseEnum
  ) where

import Ast ( Enumeration(Enum) )

import Parser.Classic.ReadP.Common ( end )
import Parser.Classic.ReadP.Idents ( parseIdent, prefixedIdent )

import qualified Parser.Classic.Words as W
  ( Keywords( Enumeration ) )

import Parser.Utils.ReadP ( commaList )

import Text.ParserCombinators.ReadP ( ReadP )

parseEnum :: ReadP Enumeration
parseEnum = do
  name <- prefixedIdent W.Enumeration
  idents <- commaList parseIdent
  end name
  return $ Enum (name, idents)
