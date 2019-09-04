module Parser.Classic.Operators
  ( operatorChars
  , parseOperator
  , reservedOperators
  ) where

import Ast (Ident)

import Parser.Common (token)

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
