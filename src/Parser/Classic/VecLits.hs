module Parser.Classic.VecLits
  ( parseVecLit
  ) where

import Ast (Lit(LVec))

import Parser.Types ( Parser )
import Parser.Classic.Exprs ( parseExpr )
import Parser.Classic.Types ( parseType )
import Parser.Common ( commaList, stoken )

import Control.Applicative ( liftA2, optional )
import Text.ParserCombinators.ReadP ( ReadP, between, many )

parseVecLit :: Parser -> ReadP Lit
parseVecLit p = between (stoken "{") (stoken "}") $
  liftA2 LVec (commaList (parseExpr p)) (optional parseType)
