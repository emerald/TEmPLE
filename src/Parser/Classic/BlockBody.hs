module Parser.Classic.BlockBody
  ( parseBlockBody
  ) where

import Ast (BlockBody(..))

import Parser.Types (Parser, parseDeclStat)

import Text.ParserCombinators.ReadP (ReadP, many)

parseBlockBody :: Parser -> ReadP BlockBody
parseBlockBody p = do
  ds <- many $ parseDeclStat p
  let uh = Nothing
  let fh = Nothing
  return $ BlockBody (ds, uh, fh)
