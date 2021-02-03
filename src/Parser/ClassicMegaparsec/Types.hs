module Parser.ClassicMegaparsec.Types
  ( Parser,
    ParseError
  ) where

import Text.Megaparsec ( Parsec, ParseError )
import Data.Void ( Void )
import Data.Text ( Text )

type Parser = Parsec Void String
