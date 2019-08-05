module Parser.GenClassicConstDecls
  ( ValidConstDecl(..)
  , InvalidConstDecl(..)
  ) where

import Ast ( ConstDecl(..) )

import Parser.GenCommon ( genValidInvalid, invalidOp1, validOp1 )
import Parser.GenClassicDecls
  ( InvalidDecl(..), ValidDecl(..)
  , invalidDeclString, validDeclString
  )
import Parser.GenClassicIdents
  ( ValidIdent(..), InvalidIdent(..)
  , invalidIdentString, validIdentString
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
    (sn, n, _) <- fmap validIdent arbitrary
    (ValidDecl (ste, (t, e))) <- arbitrary
    return (sc ++ sn ++ ste, Const (n, t, e))

newtype InvalidConstDecl
  = InvalidConstDecl { invalidConstDecl :: String }
  deriving (Eq, Show)

validInvalid :: [(Gen String, Gen String)]
validInvalid =
  [ (validConst           , invalidConst)
  , (validIdentString     , invalidIdentString)
  , (validDeclString      , invalidDeclString)
  ]

instance Arbitrary InvalidConstDecl where
  arbitrary = fmap InvalidConstDecl $
    genValidInvalid validInvalid
