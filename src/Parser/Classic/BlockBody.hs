module Parser.Classic.BlockBody
  ( parseBlockBody
  ) where

import Ast (Ident, BlockBody(..), DeclStat)

import Parser.Classic.Idents (parseIdent)
import Parser.Common (stoken, stoken1)
import Parser.Types (Parser, parseDeclStat)

import qualified Parser.Classic.Words as W
  ( Keywords(Failure, End, Unavailable) )

import Control.Applicative (optional)
import Text.ParserCombinators.ReadP (ReadP, between, many)

parseBlockBody :: Parser -> ReadP BlockBody
parseBlockBody p = do
  ds <- many $ parseDeclStat p
  uh <- optional $ parseUnavailable p
  fh <- optional $ parseFailure p
  return $ BlockBody (ds, uh, fh)

parseUnavailable :: Parser -> ReadP (Maybe Ident, [DeclStat])
parseUnavailable p = do
  stoken1 $ show W.Unavailable
  mid <- optional $ between
          (stoken "[")
          (stoken "]")
          parseIdent
  ds <- many $ parseDeclStat p
  stoken1 $ show W.End
  stoken1 $ show W.Unavailable
  return (mid, ds)

parseFailure :: Parser -> ReadP [DeclStat]
parseFailure p = between
  (stoken1 $ show W.Failure)
  (stoken1 (show W.End) *> stoken1 (show W.Failure))
  (many $ parseDeclStat p)
