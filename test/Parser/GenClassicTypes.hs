module Parser.GenClassicTypes
  ( ValidType(..)
  , InvalidType(..)
  ) where

import Ast (Type(..))

import Parser.ClassicTypes (types)

import Parser.GenCommon (token)

import Control.Applicative (liftA2)

import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary, elements, suchThat
  )

typeClause :: String -> Gen String
typeClause s = liftA2 (++) (token ":") (token s)

newtype ValidType = ValidType { validType :: (String, Type) }
  deriving (Eq, Show)

instance Arbitrary ValidType where
  arbitrary = do
    (s, t) <- elements types
    text <- typeClause s
    return $ ValidType (text, t)

newtype InvalidType = InvalidType { invalidType :: String }
  deriving (Eq, Show)

notAType :: String -> Bool
notAType = not . (`elem` (map fst types))

instance Arbitrary InvalidType where
  arbitrary = fmap InvalidType $
    suchThat arbitrary notAType >>= typeClause
