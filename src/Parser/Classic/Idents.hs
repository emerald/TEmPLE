-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.Idents
  ( firstChars
  , restChars
  , isFirstChar
  , isRestChar
  ) where

firstChars :: [Char]
firstChars = ('_' : ['A'..'Z'] ++ ['a'..'z'])

restChars :: [Char]
restChars = firstChars ++ ['0'..'9']

isFirstChar :: Char -> Bool
isFirstChar = flip elem firstChars

isRestChar :: Char -> Bool
isRestChar = flip elem restChars
