module Parser.Classic.Operations
  ( parseOperation
  ) where

import Ast (Operation(..), OpSig(..), OpKind(..), Param(..))

import qualified Parser.Classic.Words as W
  ( Keywords(Attached, End, Export, Function, Op, Operation) )

import Parser.Classic.Idents ( parseIdent )
import Parser.Classic.Types ( parseRawType )
import Parser.Common
  ( optCommaList
  , stoken, stoken1, stoken1Bool
  , word1
  )
import Parser.Types ( Parser, parseBlockBody )

import Control.Applicative ( (<*), (*>), optional )
import Control.Monad ( void )
import Text.ParserCombinators.ReadP ( ReadP, between )

parseOperation :: Parser -> ReadP Operation
parseOperation p = do
  export <- stoken1Bool (show W.Export)
  opsig <- parseOpSig p
  body <- parseBlockBody p
  let OpSig (_, name, _, _) = opsig
  void (stoken1 (show W.End) >> stoken1 name)
  return $ Operation (export, opsig, body)

parseOpSig :: Parser -> ReadP OpSig
parseOpSig p = do
  opkind <- parseOpKind
  name <- parseIdent
  let inBrackets = between (stoken "[") (stoken "]")
  params <- optCommaList parseParam inBrackets
  results <- optCommaList parseParam
    (\p' -> stoken "->" *> inBrackets p')
  return $ OpSig (opkind, name, params, results)

parseParam :: ReadP Param
parseParam = do
  attached <- stoken1Bool (show W.Attached)
  ident <- optional $ parseIdent <* (stoken ":")
  ty <- parseRawType
  return $ Param (attached, ident, ty)

parseOpKind :: ReadP OpKind
parseOpKind = word1
  [ (show W.Operation, Op)
  , (show W.Op, Op)
  , (show W.Function, Fun)
  ]
