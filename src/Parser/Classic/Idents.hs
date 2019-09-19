module Parser.Classic.Idents
  ( firstChars
  , restChars
  , reserved
  , parseIdent
  , parseIdentList
  , prefixedIdent
  ) where

import Ast (Ident)
import Parser.Common (commaList, token, stoken1)
import Parser.Classic.Words (reserved)

import Control.Applicative (liftA2)
import Control.Monad (mfilter)
import Data.List.NonEmpty (NonEmpty)
import Text.ParserCombinators.ReadP (ReadP, munch, satisfy)

firstChars :: [Char]
firstChars = ('_' : ['A'..'Z'] ++ ['a'..'z'])

restChars :: [Char]
restChars = firstChars ++ ['0'..'9']

first :: Char -> Bool
first = flip elem firstChars

rest :: Char -> Bool
rest = flip elem restChars

parseIdent :: ReadP Ident
parseIdent = token $ mfilter (not . (`elem` reserved)) $
  liftA2 (:) (satisfy first) (munch rest)

parseIdentList :: ReadP (NonEmpty Ident)
parseIdentList = commaList parseIdent

prefixedIdent :: Show a => a -> ReadP Ident
prefixedIdent s = stoken1 (show s) *> parseIdent
