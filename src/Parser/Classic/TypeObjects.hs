module Parser.Classic.TypeObjects
  ( parseTypeObject
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

parseTypeObject :: Parser -> ReadP TypeObject
parseTypeObject p = do
  immutable <- stoken1Bool (show W.Immutable)
  name <- (stoken1 (show W.TypeObject) *> parseIdent)
  builtin <- optional $ parseBuiltin
  opsigs <- many $ parseOpSig p
  void (stoken1 (show W.End) >> stoken1 name)
  return $ TypeObject
    ( immutable
    , builtin
    , name
    , opsigs
    )
