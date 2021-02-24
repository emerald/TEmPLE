-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.DeclStats
  ( parseDeclStat
  , parseDeclStats
  ) where

import Ast (DeclStat(..), Expr)

import Parser.Utils.ReadP (prefix, prefixInfix, stoken1, word1)
import Parser.Classic.ReadP.Decls ( parseDecl )
import Parser.Classic.ReadP.Exprs (parseExpr)
import Parser.Classic.ReadP.AssignOrInvoke (parseAssignOrInvoke)
import Parser.Classic.ReadP.BlockBody (parseBlockBody)
import Parser.Classic.ReadP.Loops (parseExit, parseFor, parseLoop)
import Parser.Classic.ReadP.IfThenElse (parseIfThenElse)

import qualified Parser.Classic.Words as W
  ( Keywords
    ( Assert
    , At
    , Begin
    , Checkpoint
    , End
    , Fix
    , Move
    , To
    , Refix
    , Return
    , ReturnAndFail
    , Signal
    , Unfix
    , Wait
    )
  )

import Parser.Types (Parser)

import Text.ParserCombinators.ReadP (ReadP, between, choice, many)

parseDeclStat :: Parser -> ReadP DeclStat
parseDeclStat p = choice
  [ fmap Decl $ parseDecl p
  , fmap AssignOrInvoke $ parseAssignOrInvoke p
  , parseIfThenElse p
  , parseLoop p
  , parseExit p
  , parseFor p
  , parseCompound p
  , prefix      Assert  W.Assert      pe

  -- Location-Related Statements
  , prefixInfix FixAt   W.Fix   W.At  pe
  , prefixInfix MoveTo  W.Move  W.To  pe
  , prefixInfix RefixAt W.Refix W.At  pe
  , prefix      Unfix   W.Unfix       pe

  -- Condition-Variable-Related Statements
  , prefix      Signal  W.Signal      pe
  , prefix      Wait    W.Wait        pe
  , word1
    [ (show W.Checkpoint,    Checkpoint)
    , (show W.Return,        Return)
    , (show W.ReturnAndFail, ReturnAndFail)
    ]
  ]
  where
    pe :: ReadP Expr
    pe = parseExpr p

parseDeclStats :: Parser -> ReadP [DeclStat]
parseDeclStats = many . parseDeclStat

parseCompound :: Parser -> ReadP DeclStat
parseCompound p = fmap Compound $
  between
    (stoken1 $ show W.Begin)
    (stoken1 $ show W.End)
    (parseBlockBody p)
