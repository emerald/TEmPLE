module Parser.GenClassicIdentLists
  ( invalidIdentListString, validIdentListString
  , InvalidIdentList(..), ValidIdentList(..)
  ) where

import Ast ( Ident )

import Parser.GenCommon ( validOp )
import Parser.GenClassicDecls
  ( ValidDecl(..)
  , validDeclString, invalidDeclString
  )
import Parser.GenClassicIdents
  ( ValidIdent(..), InvalidIdent(..)
  , validIdentString
  )

import Control.Monad ( liftM2 )
import Data.List ( intercalate )

import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary, frequency, listOf, shrink
  )

newtype ValidIdentList
  = ValidIdentList
  { validIdentList :: (String, [Ident], [ValidIdentList]) }

instance Show ValidIdentList where
  show (ValidIdentList (sis, is, _)) = sis ++ " ~/~> " ++ show is

instance Eq ValidIdentList where
  (ValidIdentList (sis1, is1, _)) == (ValidIdentList (sis2, is2, _))
    = (sis1, is1) == (sis2, is2)

keyword :: String
keyword = ","

genCommaIdent :: Gen (String, Ident)
genCommaIdent = do
  (sn, n, _) <- fmap validIdent arbitrary
  comma <- validOp keyword
  return (comma ++ sn, n)

instance Arbitrary ValidIdentList where
  arbitrary = do
    ts <- listOf genCommaIdent
    return $ build (map (build []) (shrink ts)) ts
    where
      build :: [ValidIdentList] -> [(String, Ident)] -> ValidIdentList
      build vs ts =
        let (ss, is) = unzip ts
        in ValidIdentList (concat ss, is, vs)

  shrink (ValidIdentList (_, _, is)) = is

validIdentListString :: Gen String
validIdentListString = fmap (\(ValidIdentList (s, _, _)) -> s) arbitrary

newtype InvalidIdentList
  = InvalidIdentList { invalidIdentList :: String }
  deriving (Eq, Show)

instance Arbitrary InvalidIdentList where
  arbitrary = return $ InvalidIdentList "%"

invalidIdentListString :: Gen String
invalidIdentListString = fmap (\(InvalidIdentList s) -> s) arbitrary
