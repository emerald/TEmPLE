module Parser.Classic.ReadP.IfThenElse
  ( parseIfThenElse
  ) where

import Ast (DeclStat(IfThenElse))

import Parser.Classic.ReadP.Common ( endShow )
import Parser.Classic.ReadP.Exprs (parseExpr)
import Parser.Types (parseDeclStats)

import qualified Parser.Classic.Words as W
  ( Keywords(Else, ElseIf, If, Then) )

import Parser.Utils.ReadP (stoken1)
import Parser.Types (Parser)

import Control.Applicative (optional)
import Text.ParserCombinators.ReadP (ReadP, many)

parseIfThenElse :: Parser -> ReadP DeclStat
parseIfThenElse p = do
  stoken1 (show W.If)
  e <- parseExpr p
  stoken1 (show W.Then)
  ds <- parseDeclStats p
  elseifs <- many $ do
    stoken1 (show W.ElseIf)
    e' <- parseExpr p
    stoken1 (show W.Then)
    ds' <- parseDeclStats p
    return (e', ds')
  else_branch <- optional $
    (stoken1 $ show W.Else) *> parseDeclStats p
  endShow W.If
  return $ IfThenElse ((e, ds), elseifs, else_branch)
