module Parser.Classic.DeclStats
  ( parseDeclStat
  ) where

import Ast (DeclStat(..))

import Parser.Types (Parser, parseDecl)

import Text.ParserCombinators.ReadP (ReadP, choice)

parseDeclStat :: Parser -> ReadP DeclStat
parseDeclStat p = choice
  [ fmap Decl $ parseDecl p
  ]
