module Ast where

import Data.List.NonEmpty (NonEmpty)

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
  | LSelf
  | LObj Object
  deriving (Eq, Ord, Show)

data Object
  = Object
  { objectName :: Ident
  , objectDecls :: [(Bool, Decl)]
  , objectInitially :: Maybe BlockBody
  , objectProcess   :: Maybe BlockBody
  , objectRecovery  :: Maybe BlockBody
  }
  deriving (Eq, Ord, Show)

data DeclStat
  = Decl Decl
  | Assert Expr
  | AssignExpr ((NonEmpty Ident), (NonEmpty Expr))
  | AssignInvoke ((NonEmpty Ident), ProcInvoc)
  | Invoke ProcInvoc
  | Checkpoint
  | Compound BlockBody
  | FixAt Expr Expr
  | MoveTo Expr Expr
  | RefixAt Expr Expr
  | Return
  | ReturnAndFail
  | Signal Expr
  | Unfix Expr
  | Wait Expr
  deriving (Eq, Ord, Show)

newtype ProcInvoc = ProcInvoc
  ( Expr
  , Ident
  , [(ArgType, Expr)]
  )
  deriving (Eq, Ord, Show)

data ArgType
  = ArgMove
  | ArgVisit
  | ArgSend
  deriving (Eq, Ord, Show)

newtype BlockBody = BlockBody
  ( [DeclStat]
  , Maybe (Maybe Ident, [DeclStat])
  , Maybe [DeclStat]
  )
  deriving (Eq, Ord, Show)

data Expr
  = ELit Lit
  | EVar Ident
  -- Precedence 8:
  | ENegate Expr
  | ELocate Expr
  | EIsFixed Expr
  | EIsLocal Expr
  | EAwaiting Expr
  | ECodeOf Expr
  | ENameOf Expr
  | ETypeOf Expr
  | ESynTypeOf Expr
  -- Precedence 7:
  | ETimes Expr Expr
  | EDiv Expr Expr
  | EMod Expr Expr
  -- Precedence 6:
  | EPlus Expr Expr
  | EMinus Expr Expr
  -- Precedence 5:
  | EEq Expr Expr
  | ENeq Expr Expr
  | EConfTo Expr Expr
  | ELt Expr Expr
  | ELeq Expr Expr
  | EGt Expr Expr
  | EGeq Expr Expr
  -- Precedence 4:
  | ENeg Expr
  -- Precedence 3:
  | EAnd Expr Expr
  | EShortAnd Expr Expr
  -- Precedence 2:
  | EOr Expr Expr
  | EShortOr Expr Expr
  -- Precedence 1:
  | EViewAs Expr Expr
  | ERestrictTo Expr Expr
  deriving (Eq, Ord, Show)

newtype ConstDecl
  = Const (Ident, (Maybe Type), Expr)
  deriving (Eq, Ord, Show)

newtype VarDecl
  = Var (NonEmpty Ident, (Maybe Type), Expr)
  deriving (Eq, Ord, Show)

data Decl
  = DConst ConstDecl
  | DVar VarDecl
  deriving (Eq, Ord, Show)

type Compilation = [ConstDecl]
