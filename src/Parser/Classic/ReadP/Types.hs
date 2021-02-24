-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.Types
  ( types
  , parseType
  , parseRawType
  ) where

import Ast (Type(..))
import Parser.Utils.ReadP (stoken, token)
import Parser.Classic.ReadP.Idents ( parseIdent )

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
