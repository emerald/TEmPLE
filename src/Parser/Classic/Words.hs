-----------------------------------------------------------------------------
-- |
-- Module      :  TEmPLE.Parser.ClassicAttached
-- Copyright   :  (c) Oleks Shturmov
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
--
-- A module listing the reserved words in classical Emerald.
--
-- Classical Emerald makes a distinction between reserved literals
-- (e.g., `nil`) and keywords (e.g., `object`) — so do we.

module Parser.Classic.Words
  ( Keywords(..)
  , Literals(..)
  , literals, keywords, reserved
  ) where

import Prelude hiding (True, False)

data Literals
  = Nil
  | Self
  | True
  | False
  deriving (Bounded, Enum)

instance Show Literals where
  show Nil   = "nil"
  show Self  = "self"
  show True  = "true"
  show False = "false"

allLiterals :: [Literals]
allLiterals = [minBound..maxBound]

-- | The list of reserved literals.
literals :: [String]
literals = map show allLiterals

data Keywords
  = And
  | As
  | Assert
  | Attached
  | Awaiting
  | Begin
  | Checkpoint
  | CodeOf
  | Const
  | End
  | Fix
  | Initially
  | IsFixed
  | IsLocal
  | Locate
  | Move
  | NameOf
  | Object
  | Or
  | Process
  | Recovery
  | Refix
  | Restrict
  | Return
  | ReturnAndFail
  | Signal
  | SynTypeOf
  | To
  | TypeOf
  | Unfix
  | Var
  | View
  | Wait
  deriving (Bounded, Enum)

instance Show Keywords where
  show And       = "and"
  show As        = "as"
  show Assert    = "assert"
  show Attached  = "attached"
  show Awaiting  = "awaiting"
  show Begin     = "begin"
  show Checkpoint = "checkpoint"
  show Const     = "const"
  show CodeOf    = "codeof"
  show End       = "end"
  show Fix       = "fix"
  show Initially = "initially"
  show IsFixed   = "isfixed"
  show IsLocal   = "islocal"
  show Locate    = "locate"
  show Move      = "move"
  show NameOf    = "nameof"
  show Object    = "object"
  show Or        = "or"
  show Process   = "process"
  show Recovery  = "recovery"
  show Refix     = "refix"
  show Restrict  = "restrict"
  show Return    = "return"
  show ReturnAndFail = "returnandfail"
  show Signal    = "signal"
  show SynTypeOf = "syntactictypeof"
  show To        = "to"
  show TypeOf    = "typeof"
  show Unfix     = "unfix"
  show Var       = "var"
  show View      = "view"
  show Wait      = "wait"

allKeywords :: [Keywords]
allKeywords = [minBound..maxBound]

-- | The list of reserved keywords.
keywords :: [String]
keywords = map show allKeywords

-- | The complete list of reserved words.
reserved :: [String]
reserved = literals ++ keywords
