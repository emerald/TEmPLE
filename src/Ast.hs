module Ast where

type Ident
  = String

data Type
  = TInt
  | TChar
  | TDouble
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

data Object
  = Object Ident [ConstDecl]
  deriving (Eq, Ord, Show)

data Expr
  = ELit Lit
  | EVar Ident
  | EObj Object
  deriving (Eq, Ord, Show)

data ConstDecl
  = Const Ident (Maybe Type) Expr
  deriving (Eq, Ord, Show)

type Compilation = [ConstDecl]
