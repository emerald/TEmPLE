module Parser.Classic.Types
  ( types
  , parseType
  , parseRawType
  ) where

import Ast (Type(..))
import Parser.Common (stoken, token, word)

import Control.Applicative ((*>))
import Text.ParserCombinators.ReadP (ReadP)

types :: [(String, Type)]
types =
  [ ("integer",   TInt    )
  , ("real",      TDouble )
  , ("character", TChar   )
  , ("string",    TString )
  ]

parseType :: ReadP Type
parseType = (*>) (stoken ":") $ parseRawType

parseRawType :: ReadP Type
parseRawType = token $ word $ types
