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
  ( WKeywords(..)
  , WLits(..)
  , literals, keywords, reserved
  ) where

data WLits
  = WNil
  | WTrue
  | WFalse
  deriving (Bounded, Enum)

instance Show WLits where
  show WNil   = "nil"
  show WTrue  = "true"
  show WFalse = "false"

allLits :: [WLits]
allLits = [minBound..maxBound]

literals :: [String]
literals = map show allLits

data WKeywords
  = WAttached
  | WAwaiting
  | WCodeOf
  | WConst
  | WEnd
  | WIsFixed
  | WIsLocal
  | WLocate
  | WNameOf
  | WObject
  | WTypeOf
  | WSynTypeOf
  | WVar
  deriving (Bounded, Enum)

instance Show WKeywords where
  show WAttached  = "attached"
  show WAwaiting  = "awaiting"
  show WConst     = "const"
  show WCodeOf    = "codeof"
  show WEnd       = "end"
  show WIsFixed   = "isfixed"
  show WIsLocal   = "islocal"
  show WLocate    = "locate"
  show WNameOf    = "nameof"
  show WObject    = "object"
  show WTypeOf    = "typeof"
  show WSynTypeOf = "syntactictypeof"
  show WVar       = "var"

allKeywords :: [WKeywords]
allKeywords = [minBound..maxBound]

keywords :: [String]
keywords = map show allKeywords

reserved :: [String]
reserved = literals ++ keywords
