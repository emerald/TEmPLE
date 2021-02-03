module Parser.Classic.ReadP.VecLits
  ( parseVecLit
  ) where

import Ast (Lit(LVec))

import Parser.Types ( Parser, parseExpr )
import Parser.Classic.ReadP.Types ( parseType )
import Parser.Common ( commaList, stoken )

import Control.Applicative ( liftA2, optional )
import Text.ParserCombinators.ReadP ( ReadP, between )

parseVecLit :: Parser -> ReadP Lit
parseVecLit p = between (stoken "{") (stoken "}") $
  liftA2 LVec (commaList (parseExpr p)) (optional parseType)
