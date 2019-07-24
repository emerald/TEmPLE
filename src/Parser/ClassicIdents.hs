module Parser.ClassicIdents
  ( firstChars
  , restChars
  , keywords
  , parseIdent
  ) where

import Ast (Ident)
import Parser.Common (token)

import Control.Applicative (liftA2)
import Control.Monad (mfilter)
import Text.ParserCombinators.ReadP (ReadP, munch, satisfy)

firstChars :: [Char]
firstChars = ('_' : ['A'..'Z'] ++ ['a'..'z'])

restChars :: [Char]
restChars = firstChars ++ ['0'..'9']

first :: Char -> Bool
first = flip elem firstChars

rest :: Char -> Bool
rest = flip elem restChars

literals :: [String]
literals =
  [ "nil"
  , "true"
  , "false"
  ]

keywords :: [String]
keywords =
  [ "const"
  , "var"
  ]

reserved :: [String]
reserved = literals ++ keywords

parseIdent :: ReadP Ident
parseIdent = token $ mfilter (not . (`elem` reserved)) $
  liftA2 (:) (satisfy first) (munch rest)
