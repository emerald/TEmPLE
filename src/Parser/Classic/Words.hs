-----------------------------------------------------------------------------
-- |
-- Module      :  TEmPLE.Parser.ClassicAttached
-- Copyright   :  (c) Oleks Shturmov
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
--
-- A module listing the reserved words in classical Emerald.
module Parser.Classic.Words
  ( Keywords(..)
  , Literals(..)
  , literals, keywords, reserved
  ) where

import Prelude hiding (True, False)

data Literals
  = Nil
  | True
  | False
  deriving (Bounded, Enum)

instance Show Literals where
  show Nil   = "nil"
  show True  = "true"
  show False = "false"

allLiterals :: [Literals]
allLiterals = [minBound..maxBound]

literals :: [String]
literals = map show allLiterals

data Keywords
  = Attached
  | Awaiting
  | CodeOf
  | Const
  | End
  | IsFixed
  | IsLocal
  | Locate
  | NameOf
  | Object
  | TypeOf
  | SynTypeOf
  | Var
  deriving (Bounded, Enum)

instance Show Keywords where
  show Attached  = "attached"
  show Awaiting  = "awaiting"
  show Const     = "const"
  show CodeOf    = "codeof"
  show End       = "end"
  show IsFixed   = "isfixed"
  show IsLocal   = "islocal"
  show Locate    = "locate"
  show NameOf    = "nameof"
  show Object    = "object"
  show TypeOf    = "typeof"
  show SynTypeOf = "syntactictypeof"
  show Var       = "var"

allKeywords :: [Keywords]
allKeywords = [minBound..maxBound]

keywords :: [String]
keywords = map show allKeywords

reserved :: [String]
reserved = literals ++ keywords
