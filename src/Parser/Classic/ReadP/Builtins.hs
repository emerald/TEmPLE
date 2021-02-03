-----------------------------------------------------------------------------
-- |
-- Module      :  TEmPLE.Parser.Classic.ReadP.Builtins
-- Copyright   :  (c) Oleks Shturmov
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
--
-- Introduced as there are a couple places where the `builtin`
-- keyword (optionally) occurs in classical Emerald.

module Parser.Classic.ReadP.Builtins
  ( parseBuiltin
  ) where

import Parser.Utils.ReadP ( stoken1 )
import Parser.Classic.Words ( Keywords( Builtin ) )
import Parser.Classic.ReadP.NumLits ( parseIntLit )

import Text.ParserCombinators.ReadP (ReadP)

parseBuiltin :: ReadP Int
parseBuiltin = stoken1 (show Builtin) *> parseIntLit
