-----------------------------------------------------------------------------
-- |
-- Module      :  TEmPLE.Parser.Classic.Attached
-- Copyright   :  (c) Oleks Shturmov
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
--
-- Introduced as there are a couple places where the `attached`
-- keyword (optionally) occurs in classical Emerald.

module Parser.Classic.Attached
  ( parseAttached
  ) where

import Parser.Common (stoken1Bool)
import Parser.Classic.Words (Keywords(Attached))

import Control.Applicative ((*>))
import Text.ParserCombinators.ReadP (ReadP)

parseAttached :: ReadP Bool
parseAttached = stoken1Bool (show Attached)
