module Parser.ClassicNames
  ( firstChars
  , restChars
  , keywords
  , parseName
  ) where

import Ast (Name)
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

keywords :: [String]
keywords =
  [ "const"
  , "integer"
  , "real"
  , "character"
  , "string"
  , "nil"
  , "true"
  , "false"
  ]

parseName :: ReadP Name
parseName = token $ mfilter (not . (`elem` keywords)) $
  liftA2 (:) (satisfy first) (munch rest)
