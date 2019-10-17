{-|
Module      : CodeGen.Erlang.PrettyAbstractFormat
Description : A pretty-printer for the Erlang Abstract Format
Copyright   : (c) Oleks Shturmov, 2019
License     : BSD 3-Clause (see the file LICENSE)

Maintainer  : oleks@oleks.info

A pretty printer for Erlang abstract syntax tree (see also
"CodeGen.Erlang.Ast"), printing valid Erlang code in Erlang Abstract
Format, as used by the Erlang compiler.

A description of the Erlang Abstract Foramt is available at (1):
<http://erlang.org/doc/apps/erts/absform.html>. This module closely
follows this description.
-}
module CodeGen.Erlang.PrettyAbstractFormat where

import CodeGen.Erlang.AbstractFormat

import Data.Char ( ord )
import Data.Text.Prettyprint.Doc
  ( Doc
  , Pretty ( pretty )
  , braces, brackets
  , comma
  , dquotes, squotes
  , punctuate
  , cat, sep
  )

commaCat :: [Doc ann] -> Doc ann
commaCat = cat . (punctuate comma)

commaSep :: [Doc ann] -> Doc ann
commaSep = sep . (punctuate comma)

makeAtom :: String -> [Doc ann] -> Doc ann
makeAtom kind rest = braces $ commaCat $
  [pretty kind, pretty "0"] ++ rest

prettyList :: Pretty b => (a -> b) -> [a] -> Doc ann
prettyList f = brackets . commaSep . (map (pretty . f))

newtype PrettyForm
  = PrettyForm Form
  deriving (Eq, Ord, Show)

prettyForm :: Form -> Doc ann
prettyForm = pretty . PrettyForm

instance Pretty PrettyForm where
  pretty (PrettyForm form)
    = case form of
        Function name arity cls -> makeAtom "function" $
          [pretty name, pretty arity, prettyClauseList cls]

newtype PrettyAtomicLit
  = PrettyAtomicLit AtomicLit
  deriving (Eq, Ord, Show)

prettyAtomicLit :: AtomicLit -> Doc ann
prettyAtomicLit = pretty . PrettyAtomicLit

instance Pretty PrettyAtomicLit where
  pretty (PrettyAtomicLit lit)
    = case lit of
        Atom    x -> makeAtom "atom"    [(pretty x)]
        Char    x -> makeAtom "char"    [(pretty $ ord x)]
        Float   x -> makeAtom "float"   [(pretty x)]
        Integer x -> makeAtom "integer" [(pretty x)]
        String  x -> makeAtom "string"  [(dquotes $ pretty x)]

newtype PrettyPattern
  = PrettyPattern Pattern
  deriving (Eq, Ord, Show)

prettyPattern :: Pattern -> Doc ann
prettyPattern = pretty . PrettyPattern

prettyPatternList :: [Pattern] -> Doc ann
prettyPatternList = prettyList PrettyPattern

instance Pretty PrettyPattern where
  pretty (PrettyPattern pat)
    = case pat of
        PatternLit l -> prettyAtomicLit l

prettyName :: PrintName -> Doc ann
prettyName n = squotes $ pretty n

newtype PrettyExpr
  = PrettyExpr Expr
  deriving (Eq, Ord, Show)

prettyExpr :: Expr -> Doc ann
prettyExpr = pretty . PrettyExpr

prettyExprList :: [Expr] -> Doc ann
prettyExprList = prettyList PrettyExpr

instance Pretty PrettyExpr where
  pretty (PrettyExpr expr)
    = case expr of
        Lit l -> prettyAtomicLit l
        Var n -> makeAtom "var" [prettyName n]
        Rem e1 e2 -> makeAtom "op"
            [ prettyName "rem", prettyExpr e1, prettyExpr e2 ]
        Div e1 e2 -> makeAtom "op"
            [ prettyName "div", prettyExpr e1, prettyExpr e2 ]
        Times e1 e2 -> makeAtom "op"
          [ prettyName "*", prettyExpr e1, prettyExpr e2 ]
        Plus e1 e2 -> makeAtom "op"
          [ prettyName "+", prettyExpr e1, prettyExpr e2 ]
        Minus e1 e2 -> makeAtom "op"
          [ prettyName "-", prettyExpr e1, prettyExpr e2 ]

newtype PrettyClause
  = PrettyClause Clause
  deriving (Eq, Ord, Show)

prettyClause :: Clause -> Doc ann
prettyClause = pretty . PrettyClause

prettyClauseList :: [Clause] -> Doc ann
prettyClauseList = prettyList PrettyClause

instance Pretty PrettyClause where
  pretty (PrettyClause (Clause (ps, gs, es)))
    = makeAtom "clause" $
        [prettyPatternList ps, prettyGuardList gs, prettyExprList es]

newtype PrettyGuard
  = PrettyGuard Guard
  deriving (Eq, Ord, Show)

prettyGuard :: Guard -> Doc ann
prettyGuard = pretty . PrettyGuard

prettyGuardList :: [Guard] -> Doc ann
prettyGuardList = prettyList PrettyGuard

instance Pretty PrettyGuard where
  pretty (PrettyGuard guard)
    = case guard of
        GuardLit l -> prettyAtomicLit l
