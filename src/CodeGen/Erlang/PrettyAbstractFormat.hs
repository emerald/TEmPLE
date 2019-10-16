{-
Module      : CodeGen.Erlang.PrettyAbstractFormat
Description : An abstract syntax tree for Erlang code
Copyright   : (c) Oleks Shturmov, 2019
License     : BSD 3-Clause (see the file LICENSE)

Maintainer  : oleks@oleks.info

A pretty printer for Erlang abstract syntax tree (see also
"CodeGen.Erlang.Ast"), printing valid Erlang code in Erlang Abstract
Format, as used by the Erlang compiler.

A description of the Erlang Abstract Foramt is available at:

    http://erlang.org/doc/apps/erts/absform.html

This module closely follows this descrition.
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
        Atom    l x -> makeAtom "atom"    l (pretty x)
        Char    l x -> makeAtom "char"    l (pretty $ ord x)
        Float   l x -> makeAtom "float"   l (pretty x)
        Integer l x -> makeAtom "integer" l (pretty x)
        String  l x -> makeAtom "string"  l (dquotes $ pretty x)
    where
      makeAtom :: String -> Integer -> Doc ann -> Doc ann
      makeAtom kind line atom = braces $
        commaCat [pretty kind, pretty line, atom]

