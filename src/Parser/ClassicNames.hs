module Parser.ClassicNames where

import Ast
import Parser.Common

import Control.Applicative (liftA2)
import Control.Monad (mfilter)
import Text.ParserCombinators.ReadP
  ( ReadP
  , munch
  , satisfy
  )

first :: Char -> Bool
first = flip elem ('_' : ['A'..'Z'] ++ ['a'..'z'])

rest :: Char -> Bool
rest c = first c || c `elem` (['0'..'9'])

keywords :: [String]
keywords =
  [ "const"
  , "integer"
  , "real"
  , "character"
  , "string"
  ]

parseName :: ReadP Name
parseName = token $ mfilter (not . (`elem` keywords)) $
  liftA2 (:) (satisfy first) (munch rest)
