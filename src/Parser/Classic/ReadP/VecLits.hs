-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.VecLits
  ( parseVecLit
  ) where

import Ast (Lit(LVec))

import Parser.Types ( Parser, parseExpr )
import Parser.Classic.ReadP.Types ( parseType )
import Parser.Utils.ReadP ( commaList, stoken )

import Control.Applicative ( liftA2, optional )
import Text.ParserCombinators.ReadP ( ReadP, between )

parseVecLit :: Parser -> ReadP Lit
parseVecLit p = between (stoken "{") (stoken "}") $
  liftA2 LVec (commaList (parseExpr p)) (optional parseType)
