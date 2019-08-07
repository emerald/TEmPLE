module Parser.GenClassicIdentLists
  ( invalidIdentListString, validIdentListString
  , InvalidIdentList(..), ValidIdentList(..)
  ) where

import Ast ( Ident )

import Parser.GenCommon ( genValidInvalid, invalidOp, validOp )
import Parser.GenClassicIdents
  ( ValidIdent(..)
  , validIdentString, invalidIdentString
  )

import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary, listOf, shrink, sized
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

validCommaIdent :: Gen (String, Ident)
validCommaIdent = do
  (sn, n, _) <- fmap validIdent arbitrary
  comma <- validOp keyword
  return (comma ++ sn, n)

instance Arbitrary ValidIdentList where
  arbitrary = do
    ts <- listOf validCommaIdent
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

invalidCommaIdent :: Gen String
invalidCommaIdent = genValidInvalid $
  [ (validIdentString, invalidIdentString)
  , (validOp keyword, invalidOp keyword)
  ]

instance Arbitrary InvalidIdentList where
  arbitrary = fmap InvalidIdentList $ sized $ \n ->
    genValidInvalid $
      replicate n (fmap fst validCommaIdent, invalidCommaIdent)

invalidIdentListString :: Gen String
invalidIdentListString = fmap (\(InvalidIdentList s) -> s) arbitrary
