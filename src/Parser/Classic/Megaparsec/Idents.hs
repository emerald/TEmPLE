module Parser.Classic.Megaparsec.Idents
  ( parseIdent
  , parseIdentList
  , prefixedIdent
  ) where

import Ast (Ident)
import Control.Applicative ( liftA2 )
import Control.Monad ( mfilter )
import Data.List.NonEmpty ( NonEmpty )
import Parser.Classic.Idents ( isFirstChar, isRestChar )
import Parser.Classic.Megaparsec.Base ( commaList, token, stoken1 )
import Parser.Classic.Megaparsec.Types ( Parser )
import Parser.Classic.Words ( reserved )
import Text.Megaparsec ( many, satisfy )

parseIdent :: Parser Ident
parseIdent = token $ mfilter (not . (`elem` reserved)) $
  liftA2 (:) (satisfy isFirstChar) (many $ satisfy isRestChar)

parseIdentList :: Parser (NonEmpty Ident)
parseIdentList = commaList parseIdent

prefixedIdent :: Show a => a -> Parser Ident
prefixedIdent s = stoken1 (show s) *> parseIdent
