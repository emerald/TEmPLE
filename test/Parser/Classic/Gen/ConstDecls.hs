-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.Gen.ConstDecls
  ( ValidConstDecl(..)
  , InvalidConstDecl(..)
  ) where

import Ast ( ConstDecl(..) )

import Parser.GenCommon ( genValidInvalid, invalidOp1, validOp1 )
import Parser.Classic.Gen.Decls
  ( ValidDecl(..)
  , invalidDeclString, validDeclString
  )
import Parser.Classic.Gen.Idents
  ( ValidIdent(..)
  , invalidIdentString, validIdentString
  )

import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary
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
