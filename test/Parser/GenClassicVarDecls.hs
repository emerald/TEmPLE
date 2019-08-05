module Parser.GenClassicVarDecls
  ( ValidVarDecl(..)
  , InvalidVarDecl(..)
  ) where

import Ast ( VarDecl(..) )

import Parser.GenCommon ( invalidOp1, validOp1 )
import Parser.GenClassicDecls
  ( ValidDecl(..)
  , validDeclString, invalidDeclString
  )

import Control.Monad ( liftM2 )

import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary, frequency
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
    (ValidDecl (sd, (n, t, e))) <- arbitrary
    return (sk ++ sd, Var n t e)

newtype InvalidVarDecl
  = InvalidVarDecl { invalidVarDecl :: String }
  deriving (Eq, Show)

instance Arbitrary InvalidVarDecl where
  arbitrary = fmap InvalidVarDecl $
    frequency [ (20, cat invalidKeyword validDeclString)
              , (80, cat validKeyword invalidDeclString)
              ]
    where cat = liftM2 (++)
