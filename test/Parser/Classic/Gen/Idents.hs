-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.Gen.Idents
  ( InvalidIdent(..), ValidIdent(..)
  , invalidIdentString, validIdentString
  ) where

import Ast (Ident)

import Parser.Classic.Idents ( firstChars, restChars )
import Parser.Classic.Words ( reserved )

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

newtype ValidIdent
  = ValidIdent {
    validIdent :: (String, Ident, [ValidIdent])
  }

instance Show ValidIdent where
  show (ValidIdent (s, n, _)) = s ++ " ~/~> " ++ show n

instance Eq ValidIdent where
  (ValidIdent (s1, l1, _)) == (ValidIdent (s2, l2, _))
    = (s1, l1) == (s2, l2)

validIdentString :: Gen String
validIdentString = fmap (\(ValidIdent (s, _, _)) -> s) arbitrary

shrinkIdent :: String -> [ValidIdent]
shrinkIdent name =
  let (first:rest) = name
  in  if length rest > 0
      then let smallerRest = take (length rest -1) rest
           in  map (vanillaIdent first) (reverse $ subsequences smallerRest)
      else []
    where
      vanillaIdent :: Char -> String -> ValidIdent
      vanillaIdent first rest =
        let name' = first:rest in ValidIdent (name', name', [])

instance Arbitrary ValidIdent where
  arbitrary = do
    name <- flip suchThat (not . (`elem` reserved)) $
      liftA2 (:) (elements firstChars) (listOf (elements restChars))
    whitespace <- spaces
    let text = name ++ whitespace
    let shrunkenText =  if length whitespace > 0
                        then [ValidIdent (name, name, [])]
                        else []
    let shrunkenIdent = shrinkIdent name
    return $ ValidIdent (text, name, shrunkenText ++ shrunkenIdent)

  shrink (ValidIdent (_, _, is)) = is

newtype InvalidIdent = InvalidIdent { invalidIdent :: String }
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

instance Arbitrary InvalidIdent where
  arbitrary = fmap InvalidIdent $ sized $ \ n -> do
    case n of
      0 -> return ""
      _ -> do
        n_invalid <- choose (1, n)
        let n_valid = n - n_invalid
        fs <- shuffle $
          replicate n_valid fst ++ replicate n_invalid snd
        let gens = fs <*> validInvalid n
        sequence gens

invalidIdentString :: Gen String
invalidIdentString = fmap (\(InvalidIdent s) -> s) arbitrary
