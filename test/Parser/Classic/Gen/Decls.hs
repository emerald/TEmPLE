-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.Gen.Decls
  ( InvalidDecl(..), ValidDecl(..)
  , invalidDeclString, validDeclString
  ) where

import Ast ( Expr, Type )

import Parser.GenCommon ( genValidInvalid, invalidOp, validOp )
import Parser.Classic.Gen.Types ( ValidType(..), InvalidType(..) )
import Parser.Classic.Gen.Exprs
  ( ValidExpr(..), InvalidExpr(..)
  , validExprString
  )

import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary, oneof
  )

newtype ValidDecl
  = ValidDecl
  { validDecl :: (String, (Maybe Type, Expr)) }
  deriving (Eq, Show)

assOp :: String
assOp = "<-"

validAss :: Gen String
validAss = validOp assOp

invalidAss :: Gen String
invalidAss = invalidOp assOp

validMaybeType :: Gen (String, Maybe Type)
validMaybeType = oneof
  [ return ("", Nothing)
  , do
    (st, t) <- fmap validType arbitrary
    return (st, Just t)
  ]

instance Arbitrary ValidDecl where
  arbitrary = fmap ValidDecl $ do
    (st, t) <- validMaybeType
    sass    <- validAss
    (se, e, _) <- fmap validExpr arbitrary
    return (st ++ sass ++ se, (t, e))

validDeclString :: Gen String
validDeclString = fmap (fst . validDecl) arbitrary

newtype InvalidDecl
  = InvalidDecl { invalidDecl :: String }
  deriving (Eq, Show)

--  [ (fmap validIdentString arbitrary  , fmap invalidIdent arbitrary)

validInvalid :: [(Gen String, Gen String)]
validInvalid =
  [ (fmap fst validMaybeType          , fmap invalidType arbitrary)
  , (validAss                         , invalidAss)
  , (fmap validExprString arbitrary   , fmap invalidExpr arbitrary)
  ]

instance Arbitrary InvalidDecl where
  arbitrary = fmap InvalidDecl $
    genValidInvalid validInvalid

invalidDeclString :: Gen String
invalidDeclString = fmap invalidDecl arbitrary
