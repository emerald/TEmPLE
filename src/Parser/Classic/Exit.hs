module Parser.Classic.Exit
  ( parseExit
  ) where

import Ast ( DeclStat(Exit) )

import Parser.Classic.Exprs ( parseExpr )

import qualified Parser.Classic.Words as W
  ( Keywords(Exit, When) )

import Parser.Common ( stoken1 )
import Parser.Types ( Parser )

import Control.Applicative ( (*>), optional )
import Text.ParserCombinators.ReadP ( ReadP )

parseExit :: Parser -> ReadP DeclStat
parseExit p = do
  stoken1 (show W.Exit)
  fmap Exit $ optional $
    stoken1 (show W.When) *> parseExpr p
