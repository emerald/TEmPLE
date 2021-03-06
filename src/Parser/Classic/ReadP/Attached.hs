-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
--
-- Introduced as there are a couple places where the `attached`
-- keyword (optionally) occurs in classical Emerald.
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.Attached
  ( parseAttached
  ) where

import Parser.Utils.ReadP (stoken1Bool)
import Parser.Classic.Words (Keywords(Attached))

import Text.ParserCombinators.ReadP (ReadP)

parseAttached :: ReadP Bool
parseAttached = stoken1Bool (show Attached)
