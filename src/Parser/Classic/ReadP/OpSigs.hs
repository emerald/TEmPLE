-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.OpSigs
  ( parseOpSig
  ) where

import Ast ( Ident, OpSig(..), OpKind(..) )

import qualified Parser.Classic.Words as W
  ( Keywords( Function, Op, Operation ) )

import Parser.Classic.ReadP.Idents ( parseIdent )
import Parser.Classic.ReadP.Operators ( parseOperator, reservedOperators )
import Parser.Classic.ReadP.Params ( parseParam, parseOptParams )
import Parser.Classic.ReadP.PolyWidgets ( parsePolyWidget )
import Parser.Utils.ReadP
  ( inBrackets
  , optCommaList
  , stoken
  , word1
  )
import Parser.Types ( Parser )

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
