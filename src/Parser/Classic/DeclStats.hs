module Parser.Classic.DeclStats
  ( parseDeclStat
  ) where

import Ast (DeclStat(..), Expr)

import Parser.Common (prefix, prefixInfix, stoken1, word1)
import Parser.Classic.Exprs (parseExpr)
import Parser.Classic.Assign (parseAssign)
import Parser.Classic.BlockBody (parseBlockBody)
import Parser.Classic.Exit (parseExit)
import Parser.Classic.IfThenElse (parseIfThenElse)
import Parser.Classic.ProcInvocs (parseProcInvoc)

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

import Parser.Types (Parser, parseDecl)

import Text.ParserCombinators.ReadP (ReadP, between, choice)

parseDeclStat :: Parser -> ReadP DeclStat
parseDeclStat p = choice
  [ fmap Decl $ parseDecl p
  , parseAssign p
  , fmap Invoke $ parseProcInvoc p
  , parseIfThenElse p
  , parseExit p
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

parseCompound :: Parser -> ReadP DeclStat
parseCompound p = fmap Compound $
  between
    (stoken1 $ show W.Begin)
    (stoken1 $ show W.End)
    (parseBlockBody p)
