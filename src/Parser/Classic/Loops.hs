module Parser.Classic.Loops
  ( parseLoop
  , parseExit
  ) where

import Ast (DeclStat(Loop, Exit))

import Parser.Classic.Exprs (parseExpr)
import Parser.Types (parseDeclStats)

import qualified Parser.Classic.Words as W
  ( Keywords(Loop, End, Exit, When) )

import Parser.Common (stoken1)
import Parser.Types (Parser)

import Control.Applicative ( optional )
import Text.ParserCombinators.ReadP (ReadP)

parseLoop :: Parser -> ReadP DeclStat
parseLoop p = do
  stoken1 (show W.Loop)
  ds <- parseDeclStats p
  stoken1 (show W.End)
  stoken1 (show W.Loop)
  return $ Loop ds

parseExit :: Parser -> ReadP DeclStat
parseExit p = do
  stoken1 (show W.Exit)
  fmap Exit $ optional $
    stoken1 (show W.When) *> parseExpr p
