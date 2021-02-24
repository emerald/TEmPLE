-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.Operators
  ( operatorChars
  , parseOperator
  , reservedOperators
  ) where

import Ast (Ident)

import Parser.Utils.ReadP (token)

import Text.ParserCombinators.ReadP (ReadP, munch1)

operatorChars :: [Char]
operatorChars
  = [ '!', '#', '&', '*'
    , '+', '-', '/', '<'
    , '=', '>', '?', '@'
    , '^', '|', '~'
    ]

reservedOperators :: [String]
reservedOperators
  = [ "*>", "==", "!=="
    , "<-", "->"
    ]

parseOperator :: ReadP Ident
parseOperator = token $ munch1 (`elem` operatorChars)
