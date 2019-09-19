module Parser.Classic.OpSigs
  ( parseOpSig
  ) where

import Ast ( Ident, OpSig(..), OpKind(..) )

import qualified Parser.Classic.Words as W
  ( Keywords( Function, Op, Operation ) )

import Parser.Classic.Idents ( parseIdent )
import Parser.Classic.Operators ( parseOperator, reservedOperators )
import Parser.Classic.Params ( parseParam, parseOptParams )
import Parser.Classic.PolyWidgets ( parsePolyWidget )
import Parser.Common
  ( inBrackets
  , optCommaList
  , stoken
  , word1
  )
import Parser.Types ( Parser )

import Control.Applicative ( (*>) )
import Control.Monad ( mfilter )
import Text.ParserCombinators.ReadP ( ReadP, choice, many )

parseOpSig :: Parser -> ReadP OpSig
parseOpSig p = do
  opkind <- parseOpKind
  name <- parseDefOpName
  params <- parseOptParams
  results <- optCommaList parseParam
    (\p' -> stoken "->" *> inBrackets p')
  widgets <- many $ parsePolyWidget p
  return $ OpSig (opkind, name, params, results, widgets)

parseDefOpName :: ReadP Ident
parseDefOpName = choice
  [ parseIdent
  , mfilter (not . (`elem` reservedOperators)) parseOperator
  ]

parseOpKind :: ReadP OpKind
parseOpKind = word1
  [ (show W.Operation, Op)
  , (show W.Op, Op)
  , (show W.Function, Fun)
  ]
