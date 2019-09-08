{-# LANGUAGE DeriveGeneric #-}

module Ast where

import Data.List.NonEmpty (NonEmpty, toList)
import Text.PrettyPrint.GenericPretty

type Ident
  = String

data Type
  = TInt
  | TChar
  | TDouble
  | TString
  deriving (Eq, Generic, Ord, Show)

instance Out Type

data Lit
  = LInt Int
  | LDouble Double
  | LChar Char
  | LString String
  | LBool Bool
  | LNil
  | LSelf
  | LObj Object
  deriving (Eq, Generic, Ord, Show)

instance Out Lit

data Object
  = Object
  { objectName :: Ident
  , objectDecls :: [(Bool, Decl)]
  , objectInitially :: Maybe BlockBody
  , objectProcess   :: Maybe BlockBody
  , objectRecovery  :: Maybe BlockBody
  }
  deriving (Eq, Generic, Ord, Show)

instance Out Object

data AssignOrInvoke
  = AssignExpr ((NonEmpty Expr), (NonEmpty Expr))
  | AssignInvoke ((NonEmpty Expr), ProcInvoc)
  | Invoke ProcInvoc
  deriving (Eq, Generic, Ord, Show)

instance Out AssignOrInvoke

data DeclStat
  = Decl Decl
  | Assert Expr
  | AssignOrInvoke AssignOrInvoke
  | Exit (Maybe Expr)
  | IfThenElse ((Expr, [DeclStat]), [(Expr, [DeclStat])], (Maybe [DeclStat]))
  | Loop [DeclStat]
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
  deriving (Eq, Generic, Ord, Show)

instance (Out a) => Out (NonEmpty a) where
  docPrec d l = docPrec d (toList l)

instance Out DeclStat

newtype ProcInvoc = ProcInvoc
  ( Expr
  , Ident
  , [(ArgType, Expr)]
  )
  deriving (Eq, Generic, Ord, Show)

instance Out ProcInvoc

data ArgType
  = ArgMove
  | ArgVisit
  | ArgSend
  deriving (Eq, Generic, Ord, Show)

instance Out ArgType

newtype BlockBody = BlockBody
  ( [DeclStat]
  , Maybe (Maybe Ident, [DeclStat])
  , Maybe [DeclStat]
  )
  deriving (Eq, Generic, Ord, Show)

instance Out BlockBody

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
  deriving (Eq, Generic, Ord, Show)

instance Out Expr

newtype ConstDecl
  = Const (Ident, (Maybe Type), Expr)
  deriving (Eq, Generic, Ord, Show)

instance Out ConstDecl

newtype VarDecl
  = Var (NonEmpty Ident, (Maybe Type), Expr)
  deriving (Eq, Generic, Ord, Show)

instance Out VarDecl

data Decl
  = DConst ConstDecl
  | DVar VarDecl
  deriving (Eq, Generic, Ord, Show)

instance Out Decl

type Compilation = [ConstDecl]
