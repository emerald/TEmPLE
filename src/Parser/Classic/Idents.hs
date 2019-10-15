{-# LANGUAGE DeriveGeneric #-}

module Parser.Classic.Idents
  ( IdentError(..)
  , firstChars
  , restChars
  , reserved
  , parseIdent
  , parseIdentList
  , prefixedIdent
  ) where

import Ast (Ident)
import Parser.ParserM
import Parser.Common (BasicError, commaList, token, stoken1)
import Parser.Classic.Words (reserved)

import Control.Applicative (liftA2)
import Data.List.NonEmpty (NonEmpty)
import Text.ParserCombinators.ReadP (munch, satisfy)

import Text.PrettyPrint.GenericPretty (Generic, Out)

firstChars :: [Char]
firstChars = ('_' : ['A'..'Z'] ++ ['a'..'z'])

restChars :: [Char]
restChars = firstChars ++ ['0'..'9']

first :: Char -> Bool
first = flip elem firstChars

rest :: Char -> Bool
rest = flip elem restChars

data IdentError
  = InvalidFirstIdentChar
  | InvalidRestIdentChar
  | ReservedIdent String
  | InvalidIdentPrefix BasicError
  deriving (Eq, Generic, Ord, Show)

instance Out IdentError

parseIdent :: ParserM IdentError Ident
parseIdent = token $
    liftA2 (:) parseFirst parseRest >>= parseNonReserved
  where
    parseFirst :: ParserM IdentError Char
    parseFirst = liftRP (satisfy first) InvalidFirstIdentChar

    parseRest :: ParserM IdentError [Char]
    parseRest = liftRP (munch rest) InvalidRestIdentChar

    parseNonReserved :: String -> ParserM IdentError Ident
    parseNonReserved s = liftBool
     (not . (`elem` reserved))
     (ReservedIdent s)
     s

parseIdentList :: ParserM IdentError (NonEmpty Ident)
parseIdentList = commaList parseIdent

prefixedIdent :: Show a => a -> ParserM IdentError Ident
prefixedIdent s = emap InvalidIdentPrefix (stoken1 (show s)) *> parseIdent
