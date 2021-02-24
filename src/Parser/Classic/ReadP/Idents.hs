-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.Idents
  ( reserved
  , parseIdent
  , parseIdentList
  , prefixedIdent
  ) where

import Ast (Ident)
import Parser.Utils.ReadP (commaList, token, stoken1)
import Parser.Classic.Words (reserved)

import Control.Applicative (liftA2)
import Control.Monad (mfilter)
import Data.List.NonEmpty (NonEmpty)
import Text.ParserCombinators.ReadP (ReadP, munch, satisfy)

import Parser.Classic.Idents ( isFirstChar, isRestChar )

parseIdent :: ReadP Ident
parseIdent = token $ mfilter (not . (`elem` reserved)) $
  liftA2 (:) (satisfy isFirstChar) (munch isRestChar)

parseIdentList :: ReadP (NonEmpty Ident)
parseIdentList = commaList parseIdent

prefixedIdent :: Show a => a -> ReadP Ident
prefixedIdent s = stoken1 (show s) *> parseIdent
