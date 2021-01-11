module Parser.Classic.Types
  ( types
  , parseType
  , parseRawType
  ) where

import Ast (Type(..))
import Parser.Common (stoken, token)
import Parser.Classic.Idents ( parseIdent )

import Data.List ( find )
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
parseRawType = token $ do
  ident <- parseIdent
  return $ case (find ((== ident) . fst) types) of
    Just (_, t) -> t
    Nothing -> TIdent ident
