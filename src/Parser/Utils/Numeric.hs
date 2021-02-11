module Parser.Utils.Numeric
  ( integralToInt
  ) where

import Numeric ( readDec )

integralToInt :: String -> Int
integralToInt = fst . head . readDec
