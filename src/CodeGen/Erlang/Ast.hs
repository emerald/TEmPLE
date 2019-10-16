{-|
Module      : CodeGen.Erlang.Ast
Description : An abstract syntax tree for Erlang code
Copyright   : (c) Oleks Shturmov, 2019
License     : BSD 3-Clause (see the file LICENSE)

Maintainer  : oleks@oleks.info

We define an abstract syntax tree, to enable the generation of both
Erlang source code, and Erlang code in the Erlang Abstract Format, as
used by the Erlang compiler. This module will be accompanied by two
adjacent pretty-printers.

A description of the Erlang Abstract Foramt is available at (1):
<http://erlang.org/doc/apps/erts/absform.html>. This module closely
follows this descrition.
-}
module CodeGen.Erlang.Ast where

-- | Atomic literals as defined in [(1), Section 8.2](http://erlang.org/doc/apps/erts/absform.html#atomic-literals).
-- The AST here lacks line numbers, these should be induced by latter
-- pretty printers, if needed.
data AtomicLit
  = Atom    String
  | Char    Char
  | Float   Double
  | Integer Int
  | String  String
  deriving (Eq, Ord, Show)
