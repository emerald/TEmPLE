{-|
Module      : CodeGen.Erlang.PrettyAbstractFormat
Description : An abstract syntax tree for Erlang code
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

import CodeGen.Erlang.Ast

import Data.Char ( ord )
import Data.Text.Prettyprint.Doc
  ( Doc
  , Pretty ( pretty )
  , braces
  , comma
  , dquotes
  , punctuate
  , cat, sep
  )

newtype PrettyAtomicLit
  = PrettyAtomicLit AtomicLit
  deriving (Eq, Ord, Show)

commaCat :: [Doc ann] -> Doc ann
commaCat = cat . (punctuate comma)

commaSep :: [Doc ann] -> Doc ann
commaSep = sep . (punctuate comma)

instance Pretty PrettyAtomicLit where
  pretty (PrettyAtomicLit lit)
    = case lit of
        Atom    x -> makeAtom "atom"    0 (pretty x)
        Char    x -> makeAtom "char"    0 (pretty $ ord x)
        Float   x -> makeAtom "float"   0 (pretty x)
        Integer x -> makeAtom "integer" 0 (pretty x)
        String  x -> makeAtom "string"  0 (dquotes $ pretty x)
    where
      makeAtom :: String -> Integer -> Doc ann -> Doc ann
      makeAtom kind line atom = braces $
        commaCat [pretty kind, pretty line, atom]

