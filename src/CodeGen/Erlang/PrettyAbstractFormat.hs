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
  , braces
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

prettyName :: PrintName -> Doc ann
prettyName n = squotes $ pretty n

newtype PrettyExpr
  = PrettyExpr Expr
  deriving (Eq, Ord, Show)

prettyExpr :: Expr -> Doc ann
prettyExpr = pretty . PrettyExpr

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
