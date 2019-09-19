module Parser.Classic.TypeObjects
  ( parseTypeObject
  , parseOptImmTypeObject
  ) where

import Ast ( TypeObject(..) )
import Parser.Common ( stoken1Bool )
import Parser.Classic.Builtins ( parseBuiltin )
import Parser.Classic.Common ( end )
import Parser.Classic.Idents ( prefixedIdent )
import Parser.Classic.OpSigs ( parseOpSig )
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
