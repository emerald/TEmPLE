module Parser.Classic.Loops
  ( parseExit
  , parseLoop
  , parseFor
  ) where

import Ast (DeclStat(Exit, For1, For2, Loop))

import Parser.Classic.Common ( endShow )
import Parser.Classic.Idents (parseIdent)
import Parser.Classic.Types (parseType)
import Parser.Classic.AssignOrInvoke (parseAssignOrInvoke)
import Parser.Classic.Exprs (parseExpr)
import Parser.Types (parseDeclStats)

import qualified Parser.Classic.Words as W
  ( Keywords(By, Exit, For, Loop, When, While) )

import Parser.Common (stoken, stoken1)
import Parser.Types (Parser)

import Control.Applicative ( optional )
import Text.ParserCombinators.ReadP (ReadP, choice)

parseLoop :: Parser -> ReadP DeclStat
parseLoop p = do
  stoken1 (show W.Loop)
  ds <- parseDeclStats p
  endShow W.Loop
  return $ Loop ds

parseExit :: Parser -> ReadP DeclStat
parseExit p = do
  stoken1 (show W.Exit)
  fmap Exit $ optional $
    stoken1 (show W.When) *> parseExpr p

parseFor :: Parser -> ReadP DeclStat
parseFor p = choice
  [ parseFor1 p
  , parseFor2 p
  ] <* endShow W.For

parseFor1 :: Parser -> ReadP DeclStat
parseFor1 p = do
  stoken (show W.For)
  stoken "("
  initial <- parseAssignOrInvoke p
  stoken ":"
  cond <- parseExpr p
  stoken ":"
  stop <- parseAssignOrInvoke p
  stoken ")"
  ds <- parseDeclStats p
  return $ For1 initial cond stop ds

parseFor2 :: Parser -> ReadP DeclStat
parseFor2 p = do
  stoken1 (show W.For)
  i <- parseIdent
  t <- parseType
  e <- (stoken "<-" *> (parseExpr p))
  stoken1 (show W.While)
  cond <- parseExpr p
  stoken1 (show W.By)
  stop <- parseAssignOrInvoke p
  ds <- parseDeclStats p
  return $ For2 (i, t, e) cond stop ds
