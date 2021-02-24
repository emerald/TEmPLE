-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.Gen.Types
  ( ValidType(..)
  , InvalidType(..)
  ) where

import Ast (Type(..))

import Parser.Classic.ReadP.Types (types)

import Parser.Classic.Gen.Idents ( invalidIdentString )

import Parser.GenCommon (token)

import Control.Applicative (liftA2)

import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary, elements
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

instance Arbitrary InvalidType where
  arbitrary = fmap InvalidType $ invalidIdentString
