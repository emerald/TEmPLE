{-
Module      : CodeGen.Erlang.Ast
Description : An abstract syntax tree for Erlang code
Copyright   : (c) Oleks Shturmov, 2019
License     : BSD 3-Clause (see the file LICENSE)

Maintainer  : oleks@oleks.info

We define an abstract syntax tree, to enable the generation of both
Erlang source code, and Erlang code in the Erlang Abstract Format, as
used by the Erlang compiler. This module will be accompanied by two
adjacent pretty-printers.

A description of the Erlang Abstract Foramt is available at:

    http://erlang.org/doc/apps/erts/absform.html

This module closely follows this descrition.
-}
module CodeGen.Erlang.Ast where

type Line = Integer

data AtomicLit
  = Atom    Line String
  | Char    Line Char
  | Float   Line Double
  | Integer Line Integer
  | String  Line String
  deriving (Eq, Ord, Show)
