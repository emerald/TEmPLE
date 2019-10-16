module CodeGen.Erlang.Lits where

import Ast
import qualified CodeGen.Erlang.Ast as E

genLit :: Lit -> E.AtomicLit
genLit lit
  = case lit of

      LInt    i -> E.Integer i
      LChar   c -> E.Char c
      LDouble d -> E.Float d
      LString s -> E.String s

      LBool True -> E.Atom "true"
      LBool False -> E.Atom "false"

      _ -> undefined
