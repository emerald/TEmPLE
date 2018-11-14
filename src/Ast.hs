module Ast where

type Name
  = String

data Type
  = TInt
  | TDouble
  | TChar
  | TString
  deriving (Eq, Show)

data Lit
  = LInt Int
  | LDouble Double
  | LChar Char
  | LString String
  | LBool Bool
  | LNil
  deriving (Eq, Show)

data Expr
  = ELit Lit
  | EVar Name
  deriving (Eq, Show)

data ConstDecl
  = Const Name (Maybe Type) Expr
  deriving (Eq, Show)

type Compilation = [ConstDecl]
