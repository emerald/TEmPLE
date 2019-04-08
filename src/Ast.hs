module Ast where

type Name
  = String

data Type
  = TInt
  | TDouble
  | TChar
  | TString
  deriving (Eq, Ord, Show)

data Lit
  = LInt Int
  | LDouble Double
  | LChar Char
  | LString String
  | LBool Bool
  | LNil
  deriving (Eq, Ord, Show)

data Expr
  = ELit Lit
  | EVar Name
  deriving (Eq, Ord, Show)

data ConstDecl
  = Const Name (Maybe Type) Expr
  deriving (Eq, Ord, Show)

type Compilation = [ConstDecl]
