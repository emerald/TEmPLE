module Parser.ClassicTypes
  ( parseType
  ) where

import Ast (Type(..))
import Parser.Common (stoken, token, word)

import Control.Applicative ((*>))
import Text.ParserCombinators.ReadP (ReadP)

parseType :: ReadP Type
parseType = (*>) (stoken ":") $ token $ word $
  [ ("integer", TInt)
  , ("real", TDouble)
  , ("character", TChar)
  , ("string", TString)
  ]
