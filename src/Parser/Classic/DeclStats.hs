module Parser.Classic.DeclStats
  ( parseDeclStat
  ) where

import Ast (DeclStat(..))

import Parser.Common (prefix, prefixInfix)
import Parser.Classic.Exprs (parseExpr)

import qualified Parser.Classic.Words as W
  ( Keywords
    ( Assert
    , Fix
    , Move
    , To
    , Refix
    , Signal
    , Unfix
    , Wait
    )
  )

import Parser.Types (Parser, parseDecl)

import Text.ParserCombinators.ReadP (ReadP, choice)

parseDeclStat :: Parser -> ReadP DeclStat
parseDeclStat p = choice
  [ fmap Decl $ parseDecl p
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
    [ (W.Checkpoint,    Checkpoint)
    , (W.Return,        Return)
    , (W.ReturnAndFail, ReturnAndFail)
    ]
  ]
  where pe = parseExpr p
