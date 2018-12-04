module Parser.GenClassicNames
  ( ValidName(..)
  , InvalidName(..)
  ) where

import Ast (Name)

import Parser.ClassicNames (firstChars, restChars, keywords)

import Parser.GenCommon (token)

import Data.Char (isSpace)
import Control.Applicative (liftA2)

import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary, choose, elements, listOf, shuffle, sized, suchThat
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

validInvalidChars :: [Char] -> (Char -> Bool) -> (Gen Char, Gen Char)
validInvalidChars cs except =
  ( elements cs
  , suchThat arbitrary $
      \c -> not $ (c `elem` cs) || except c
  )

validInvalidFirst :: (Gen Char, Gen Char)
validInvalidFirst = validInvalidChars firstChars $ const False

validInvalidRest :: (Gen Char, Gen Char)
validInvalidRest = validInvalidChars restChars isSpace

validInvalid :: Int -> [(Gen Char, Gen Char)]
validInvalid 0 = []
validInvalid n =
  (validInvalidFirst : (replicate (n-1) validInvalidRest))

instance Arbitrary InvalidName where
  arbitrary = fmap InvalidName $ sized $ \ n -> do
    case n of
      0 -> return ""
      _ -> do
        n_invalid <- choose (1, n)
        let n_valid = n - n_invalid
        fs <- shuffle $
          replicate n_valid fst ++ replicate n_invalid snd
        let gens = fs <*> validInvalid n
        sequence gens
