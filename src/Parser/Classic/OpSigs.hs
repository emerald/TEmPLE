module Parser.Classic.OpSigs
  ( parseOpSig
  ) where

import Ast (Ident, OpSig(..), OpKind(..), Param(..))

import qualified Parser.Classic.Words as W
  ( Keywords(Attached, Function, Op, Operation) )

import Parser.Classic.Idents ( parseIdent )
import Parser.Classic.Operators ( parseOperator, reservedOperators )
import Parser.Classic.Types ( parseRawType )
import Parser.Classic.PolyWidgets ( parsePolyWidget )
import Parser.Common
  ( optCommaList
  , stoken, stoken1Bool
  , word1
  )
import Parser.Types ( Parser )

import Control.Applicative ( (<*), (*>), optional )
import Control.Monad ( mfilter )
import Text.ParserCombinators.ReadP ( ReadP, between, choice, many )

parseOpSig :: Parser -> ReadP OpSig
parseOpSig p = do
  opkind <- parseOpKind
  name <- parseDefOpName
  let inBrackets = between (stoken "[") (stoken "]")
  params <- optCommaList parseParam inBrackets
  results <- optCommaList parseParam
    (\p' -> stoken "->" *> inBrackets p')
  widgets <- many $ parsePolyWidget p
  return $ OpSig (opkind, name, params, results, widgets)

parseDefOpName :: ReadP Ident
parseDefOpName = choice
  [ parseIdent
  , mfilter (not . (`elem` reservedOperators)) parseOperator
  ]

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
