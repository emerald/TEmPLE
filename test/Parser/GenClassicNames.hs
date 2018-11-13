module Parser.GenClassicNames
  ( ValidName(..)
  , InvalidName(..)
  ) where

import Ast (Name)

import Parser.ClassicNames (firstChars, restChars, keywords)

import Parser.GenCommon (spaces)

import Control.Applicative (liftA2)

import Test.Tasty.QuickCheck
  ( Arbitrary
  , arbitrary, elements, listOf, suchThat
  )

data ValidName = ValidName String Name
  deriving (Eq, Show)

instance Arbitrary ValidName where
  arbitrary = do
    name <- flip suchThat (not . (`elem` keywords)) $
      liftA2 (:) (elements firstChars) (listOf (elements restChars))
    text <- fmap (name ++) spaces
    return $ ValidName text name

data InvalidName = InvalidName String
  deriving (Eq, Show)

instance Arbitrary InvalidName where
  arbitrary = fmap InvalidName $
    liftA2 (:)
      (suchThat arbitrary (not . (`elem` firstChars)))
      (listOf arbitrary)
