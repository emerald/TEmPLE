module Parser.Classic.Params
  ( parseParam
  , parseOptParams
  ) where

import Ast ( Param(..) )

import qualified Parser.Classic.Words as W
  ( Keywords( Attached ) )

import Parser.Classic.Idents ( parseIdent )
import Parser.Classic.Types ( parseRawType )
import Parser.Common
  ( inBrackets, optCommaList
  , stoken, stoken1Bool
  )

import Control.Applicative ( (<*), optional )
import Text.ParserCombinators.ReadP ( ReadP )

parseParam :: ReadP Param
parseParam = do
  attached <- stoken1Bool (show W.Attached)
  ident <- optional $ parseIdent <* (stoken ":")
  ty <- parseRawType
  return $ Param (attached, ident, ty)

parseOptParams :: ReadP [Param]
parseOptParams = optCommaList parseParam inBrackets
