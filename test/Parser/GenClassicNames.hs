module Parser.GenClassicNames
  ( ValidName(..)
  , InvalidName(..)
  , validNameString
  ) where

import Ast (Name)

import Parser.ClassicNames (firstChars, restChars, keywords)

import Parser.GenCommon (spaces)

import Data.Char (isSpace)
import Data.List (subsequences)
import Control.Applicative (liftA2)

import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary, shrink
  , choose, elements, listOf
  , shuffle, sized, suchThat
  )

newtype ValidName
  = ValidName {
    validName :: (String, Name, [ValidName])
  }

instance Show ValidName where
  show (ValidName (s, n, _)) = s ++ " ~/~> " ++ show n

instance Eq ValidName where
  (ValidName (s1, l1, _)) == (ValidName (s2, l2, _))
    = (s1, l1) == (s2, l2)

validNameString :: ValidName -> String
validNameString (ValidName (s, _, _)) = s

shrinkName :: String -> [ValidName]
shrinkName name =
  let (first:rest) = name
  in  if length rest > 0
      then let smallerRest = take (length rest -1) rest
           in  map (vanillaName first) (reverse $ subsequences smallerRest)
      else []
    where
      vanillaName :: Char -> String -> ValidName
      vanillaName first rest =
        let name = first:rest in ValidName (name, name, [])

instance Arbitrary ValidName where
  arbitrary = do
    name <- flip suchThat (not . (`elem` keywords)) $
      liftA2 (:) (elements firstChars) (listOf (elements restChars))
    tail <- spaces
    let text = name ++ tail
    let shrunkenText =  if length tail > 0
                        then [ValidName (name, name, [])]
                        else []
    let shrunkenName = shrinkName name
    return $ ValidName (text, name, shrunkenText ++ shrunkenName)
    where
      vanillaName :: Char -> String -> ValidName
      vanillaName first rest =
        let name = first:rest in ValidName (name, name, [])

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
