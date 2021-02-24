-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.Megaparsec.Types
  ( Parser,
    ParseError
  ) where

import Text.Megaparsec ( Parsec, ParseError )
import Data.Void ( Void )

type Parser = Parsec Void String
