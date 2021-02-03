module Parser.Classic.Megaparsec.Types
  ( Parser,
    ParseError
  ) where

import Text.Megaparsec ( Parsec, ParseError )
import Data.Void ( Void )

type Parser = Parsec Void String
