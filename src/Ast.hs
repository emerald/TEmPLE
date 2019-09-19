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
  | TIdent Ident
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
  | LTypeObj TypeObject
  | LClass Class
  | LVec (NonEmpty Expr) (Maybe Type)
  deriving (Eq, Generic, Ord, Show)

instance Out Lit

data OpKind
  = Op
  | Fun
  deriving (Eq, Generic, Ord, Show)

instance Out OpKind

newtype Param
  = Param (Bool, Maybe Ident, Type)
  deriving (Eq, Generic, Ord, Show)

instance Out Param

data PolyWidget
  = Where (Ident, Expr)
  | SuchThat (Ident, TypeObject)
  | ForAll Ident
  deriving (Eq, Generic, Ord, Show)

instance Out PolyWidget

newtype OpSig
  = OpSig (OpKind, Ident, [Param], [Param], [PolyWidget])
  deriving (Eq, Generic, Ord, Show)

instance Out OpSig

newtype Operation
  = Operation (Bool, OpSig, BlockBody)
  deriving (Eq, Generic, Ord, Show)

instance Out Operation

newtype TypeObject
  = TypeObject
  ( Bool      -- immutable
  , Maybe Int -- builtin
  , Ident     -- name
  , [OpSig]   -- ops
  )
  deriving (Eq, Generic, Ord, Show)

instance Out TypeObject

newtype Class
  = Class
  ( Bool          -- immutable
  , Bool          -- monitor
  , Ident         -- name
  , Maybe Ident   -- base class
  , [Param]       -- parameters
  , [Operation]   -- class operations
  , ObjectBody    -- remaining elements
  )
  deriving (Eq, Generic, Ord, Show)

instance Out Class

newtype ObjectBody = ObjectBody
  ( [(Bool, Decl)]
  , [Operation]
  , Maybe BlockBody
  , Maybe BlockBody
  , Maybe BlockBody
  )
  deriving (Eq, Generic, Ord, Show)

instance Out ObjectBody

data Object
  = Object
  { objectImmutable :: Bool
  , objectMonitor :: Bool
  , objectName :: Ident
  , objectBody :: ObjectBody
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
  | For1 AssignOrInvoke Expr AssignOrInvoke [DeclStat]
  | For2 (Ident, Type, Expr) Expr AssignOrInvoke [DeclStat]
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
  = Const (Ident, Maybe Type, Expr)
  deriving (Eq, Generic, Ord, Show)

instance Out ConstDecl

newtype VarDecl
  = Var (NonEmpty Ident, Type, Maybe Expr)
  deriving (Eq, Generic, Ord, Show)

instance Out VarDecl

data Decl
  = DConst ConstDecl
  | DVar VarDecl
  deriving (Eq, Generic, Ord, Show)

instance Out Decl

type Compilation = [ConstDecl]
