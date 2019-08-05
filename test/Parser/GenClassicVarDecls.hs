module Parser.GenClassicVarDecls
  ( ValidVarDecl(..)
  , InvalidVarDecl(..)
  ) where

import Ast ( VarDecl(..) )

import Parser.GenCommon ( genValidInvalid, invalidOp1, validOp1 )
import Parser.GenClassicDecls
  ( ValidDecl(..)
  , validDeclString, invalidDeclString
  )
import Parser.GenClassicIdents
  ( ValidIdent(..), InvalidIdent(..)
  , validIdentString, invalidIdentString
  )
import Parser.GenClassicIdentLists
  ( genInvalidIdentListString, genValidIdentListString
  , InvalidIdentList(..), ValidIdentList(..)
  )

import Control.Monad ( liftM2 )
import Data.List ( intercalate )

import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary, frequency, listOf
  )

newtype ValidVarDecl
  = ValidVarDecl { validVarDecl :: (String, VarDecl) }
  deriving (Eq, Show)

keyword :: String
keyword = "var"

validKeyword :: Gen String
validKeyword = validOp1 keyword

invalidKeyword :: Gen String
invalidKeyword = invalidOp1 keyword

instance Arbitrary ValidVarDecl where
  arbitrary = fmap ValidVarDecl $ do
    sk      <- validKeyword
    (sn, n, _) <- fmap validIdent arbitrary
    (sns, ns, _) <- fmap validIdentList arbitrary
    (ValidDecl (ste, (t, e))) <- arbitrary
    return (sk ++ sn ++ sns ++ ste, Var (n, ns, t, e))
    where
      genIdent = do
        (sn, n, _) <- fmap validIdent arbitrary
        return (sn, n)

newtype InvalidVarDecl
  = InvalidVarDecl { invalidVarDecl :: String }
  deriving (Eq, Show)

validInvalid =
  [ (validKeyword     , invalidKeyword)
  , (validIdentString , invalidIdentString)
  , (validDeclString  , invalidDeclString)
  ]

instance Arbitrary InvalidVarDecl where
  arbitrary = fmap InvalidVarDecl $
    genValidInvalid validInvalid
