module Parser.GenClassicConstDecls
  ( ValidConstDecl(..)
  , InvalidConstDecl(..)
  ) where

import Ast ( ConstDecl(..) )

import Parser.GenCommon ( invalidOp1, validOp1 )
import Parser.GenClassicDecls
  ( InvalidDecl(..), ValidDecl(..)
  , invalidDeclString, validDeclString
  )

import Control.Monad ( liftM2 )

import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary, frequency
  )

newtype ValidConstDecl
  = ValidConstDecl { validConstDecl :: (String, ConstDecl) }
  deriving (Eq, Show)

constOp :: String
constOp = "const"

validConst :: Gen String
validConst = validOp1 constOp

invalidConst :: Gen String
invalidConst = invalidOp1 constOp

instance Arbitrary ValidConstDecl where
  arbitrary = fmap ValidConstDecl $ do
    sc      <- validConst
    (ValidDecl (sd, (n, t, e))) <- arbitrary
    return (sc ++ sd, Const n t e)

newtype InvalidConstDecl
  = InvalidConstDecl { invalidConstDecl :: String }
  deriving (Eq, Show)

instance Arbitrary InvalidConstDecl where
  arbitrary = fmap InvalidConstDecl $
    frequency [ (20, cat invalidConst validDeclString)
              , (80, cat validConst invalidDeclString)
              ]
    where cat = liftM2 (++)
