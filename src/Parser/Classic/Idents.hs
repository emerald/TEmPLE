module Parser.Classic.Idents
  ( firstChars
  , restChars
  , reserved
  , parseIdent
  , parseIdentList
  ) where

import Ast (Ident)
import Parser.Common (token, stoken)
import Parser.Classic.Words (reserved)

import Control.Applicative (liftA2)
import Control.Monad (mfilter)
import Text.ParserCombinators.ReadP (ReadP, many, munch, satisfy)

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

parseIdentList :: ReadP (Ident, [Ident])
parseIdentList =
  liftA2 (,) parseIdent (many (stoken "," *> parseIdent))
