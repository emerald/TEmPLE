module Parser.GenClassicNames
  ( ValidName(..)
  , InvalidName(..)
  ) where

import Ast (Name)

import Parser.ClassicNames (firstChars, restChars, keywords)

import Parser.GenCommon (token)

import Control.Applicative (liftA2)

import Test.Tasty.QuickCheck
  ( Arbitrary
  , arbitrary, elements, listOf, suchThat
  )

newtype ValidName = ValidName { validName :: (String, Name) }
  deriving (Eq, Show)

instance Arbitrary ValidName where
  arbitrary = do
    name <- flip suchThat (not . (`elem` keywords)) $
      liftA2 (:) (elements firstChars) (listOf (elements restChars))
    text <- token name
    return $ ValidName (text, name)

newtype InvalidName = InvalidName { invalidName :: String }
  deriving (Eq, Show)

instance Arbitrary InvalidName where
  arbitrary = fmap InvalidName $
    liftA2 (:)
      (suchThat arbitrary (not . (`elem` firstChars)))
      (listOf arbitrary)
