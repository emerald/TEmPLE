module Parser.Classic.TypeObjects
  ( parseTypeObject
  , parseOptImmTypeObject
  ) where

import Ast ( TypeObject(..) )
import Parser.Common ( stoken1, stoken1Bool )
import Parser.Classic.Builtins ( parseBuiltin )
import Parser.Classic.Idents ( parseIdent )
import Parser.Classic.OpSigs ( parseOpSig )
import Parser.Types ( Parser )

import qualified Parser.Classic.Words as W
  ( Keywords( TypeObject, End, Immutable ) )

import Control.Applicative ( optional )
import Control.Monad ( void )
import Text.ParserCombinators.ReadP ( ReadP, many )

parseTypeObject :: Parser -> Bool -> ReadP TypeObject
parseTypeObject p imm = do
  name <- (stoken1 (show W.TypeObject) *> parseIdent)
  builtin <- optional $ parseBuiltin
  opsigs <- many $ parseOpSig p
  void (stoken1 (show W.End) >> stoken1 name)
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
