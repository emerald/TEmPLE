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

import Parser.Common (stoken1)
import Parser.Classic.Words (WKeywords(WAttached))

import Control.Applicative ((*>))
import Text.ParserCombinators.ReadP (ReadP, choice)

parseAttached :: ReadP Bool
parseAttached = choice
  [ stoken1 (show WAttached) *> return True
  , return False
  ]
