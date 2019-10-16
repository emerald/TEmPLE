module CodeGen.Erlang.Exprs where

import Ast
import qualified CodeGen.Erlang.AbstractFormat as E
import CodeGen.Erlang.Lits ( genLit )

genExpr :: Expr -> E.Expr
genExpr expr
  = case expr of
      ELit l -> E.Lit $ genLit l
      EVar v -> E.Var v
--
      ETimes e1 e2 -> E.Times (genExpr e1) (genExpr e2)
      EDiv e1 e2 -> E.Div (genExpr e1) (genExpr e2)
      EMod e1 e2 -> E.Rem (genExpr e1) (genExpr e2)
--
      EPlus e1 e2 -> E.Plus (genExpr e1) (genExpr e2)
      EMinus e1 e2 -> E.Minus (genExpr e1) (genExpr e2)

      _ -> undefined
