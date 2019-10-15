{-# LANGUAGE DeriveGeneric #-}

module Parser.Classic.Enums
  ( EnumError(..)
  , parseEnum
  ) where

import Ast ( Enumeration(Enum) )

import Parser.ParserM ( ParserM, emap )
import Parser.Classic.Common ( end )
import Parser.Classic.Idents ( IdentError, parseIdent, prefixedIdent )

import qualified Parser.Classic.Words as W
  ( Keywords( Enumeration ) )

import Parser.Common ( BasicError, commaList )

import Text.PrettyPrint.GenericPretty ( Generic, Out )

data EnumError
  = Start IdentError
  | Item IdentError
  | End BasicError
  deriving (Eq, Generic, Ord, Show)

instance Out EnumError

parseEnum :: ParserM EnumError Enumeration
parseEnum = do
  name <- emap Start $ prefixedIdent W.Enumeration
  idents <- emap Item $ commaList parseIdent
  emap End $ end name
  return $ Enum (name, idents)
