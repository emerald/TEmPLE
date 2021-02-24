-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.TypeObjects
  ( parseTypeObject
  , parseOptImmTypeObject
  ) where

import Ast ( TypeObject(..) )
import Parser.Utils.ReadP ( stoken1Bool )
import Parser.Classic.ReadP.Builtins ( parseBuiltin )
import Parser.Classic.ReadP.Common ( end )
import Parser.Classic.ReadP.Idents ( prefixedIdent )
import Parser.Classic.ReadP.OpSigs ( parseOpSig )
import Parser.Types ( Parser )

import qualified Parser.Classic.Words as W
  ( Keywords( TypeObject, Immutable ) )

import Control.Applicative ( optional )
import Text.ParserCombinators.ReadP ( ReadP, many )

parseTypeObject :: Parser -> Bool -> ReadP TypeObject
parseTypeObject p imm = do
  name <- prefixedIdent W.TypeObject
  builtin <- optional $ parseBuiltin
  opsigs <- many $ parseOpSig p
  end name
  return $ TypeObject
    ( imm
    , builtin
    , name
    , opsigs
    )

parseOptImmTypeObject :: Parser -> ReadP TypeObject
parseOptImmTypeObject p = do
  imm <- stoken1Bool (show W.Immutable)
  parseTypeObject p imm
