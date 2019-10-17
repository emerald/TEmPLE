module CodeGen.Erlang.Decls where

import Ast ( ConstDecl ( Const ) )

import CodeGen.Erlang.AbstractFormat
  ( Clause ( Clause )
  , Form ( Function )
  )
import CodeGen.Erlang.Exprs ( genExpr )

genConstDecl :: ConstDecl -> Form
genConstDecl (Const (ident, _, e)) =
  Function ident 0 [Clause([], [], [genExpr e])]
