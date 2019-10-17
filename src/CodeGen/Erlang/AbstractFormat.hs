{-|
Module      : CodeGen.Erlang.Ast
Description : The Erlang Abstract Format in terms of Haskell types
Copyright   : (c) Oleks Shturmov, 2019
License     : BSD 3-Clause (see the file LICENSE)

Maintainer  : oleks@oleks.info

To enable the generation of Erlang source code, this modiles defines
the [Erlang Abstract
Format](http://erlang.org/doc/apps/erts/absform.html) in terms of
Haskell types.

This module is accompanied by a pretty-printer:
"CodeGen.Erlang.PrettyAbstractFormat". Another pretty-printer is
concievable, pretty-printing abstractly formatted code as actual
pretty Erlang source code. However, such a tool may already exist
elsewhere..

A description of the Erlang Abstract Foramt is available at (1):
<http://erlang.org/doc/apps/erts/absform.html>. This module closely
follows this description, with one notable exception: line-numbers are
omitted. These are to be induced by actual pretty-printers, if need
be.
-}
module CodeGen.Erlang.AbstractFormat where

type Name = String
type Arity = Int

newtype ModuleDecl
  = ModuleDecl [Form]
  deriving (Eq, Ord, Show)

data Attr
  = File String Int
  | Module String
  deriving (Eq, Ord, Show)

-- | Forms as defined in [(1), Section 8.1](http://erlang.org/doc/apps/erts/absform.html#module-declarations-and-forms).
data Form
  = Attribute Attr
  | Function Name Arity [Clause]
  | Eof
  deriving (Eq, Ord, Show)

-- | Atomic literals as defined in [(1), Section 8.2](http://erlang.org/doc/apps/erts/absform.html#atomic-literals).
data AtomicLit
  = Atom    String
  | Char    Char
  | Float   Double
  | Integer Int
  | String  String
  deriving (Eq, Ord, Show)

-- | Patterns as defined in [(1), Section 8.3](http://erlang.org/doc/apps/erts/absform.html#patterns).
data Pattern
  = PatternLit AtomicLit
  deriving (Eq, Ord, Show)

type PrintName = String

-- | Expressions as defined in [(1), Section 8.4](http://erlang.org/doc/apps/erts/absform.html#expressions).
data Expr
  = Lit AtomicLit
  | Var PrintName
--
  | Rem Expr Expr
  | Div Expr Expr
  | Times Expr Expr
--
  | Plus Expr Expr
  | Minus Expr Expr
--
  deriving (Eq, Ord, Show)

-- | Clauses as defined in [(1), Section 8.5](http://erlang.org/doc/apps/erts/absform.html#clauses).
newtype Clause
  = Clause ([Pattern], [Guard], [Expr])
  deriving (Eq, Ord, Show)

-- | Guards as defined in [(1), Section 8.6](http://erlang.org/doc/apps/erts/absform.html#guards).
data Guard
  = GuardLit AtomicLit
  deriving (Eq, Ord, Show)
