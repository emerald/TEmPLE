module Parser.ClassicTypes where

import Ast
import Parser.Common

import Control.Applicative ((*>))
import Text.ParserCombinators.ReadP (ReadP)

parseType :: ReadP Type
parseType = (*>) (stoken ":") $ token $ word $
  [ ("integer", TInt)
  , ("real", TDouble)
  , ("character", TChar)
  , ("string", TString)
  ]
