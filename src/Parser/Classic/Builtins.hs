-----------------------------------------------------------------------------
-- |
-- Module      :  TEmPLE.Parser.Classic.Builtins
-- Copyright   :  (c) Oleks Shturmov
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
--
-- Introduced as there are a couple places where the `builtin`
-- keyword (optionally) occurs in classical Emerald.

module Parser.Classic.Builtins
  ( parseBuiltin
  ) where

import Parser.Common ( stoken1 )
import Parser.Classic.Words ( Keywords( Builtin ) )
import Parser.Classic.NumLits ( parseIntLit )

import Control.Applicative ( (*>) )
import Text.ParserCombinators.ReadP (ReadP)

parseBuiltin :: ReadP Int
parseBuiltin = stoken1 (show Builtin) *> parseIntLit
