module Parser.GenClassicDecls
  ( InvalidDecl(..), ValidDecl(..)
  , invalidDeclString, validDeclString
  ) where

import Ast ( Expr, Ident, Type )

import Parser.GenCommon ( invalidOp, validOp )
import Parser.GenClassicIdents
  ( ValidIdent(..), InvalidIdent(..)
  , validIdentString
  )
import Parser.GenClassicTypes ( ValidType(..), InvalidType(..) )
import Parser.GenClassicExprs
  ( ValidExpr(..), InvalidExpr(..)
  , validExprString
  )

import Control.Applicative ( (<*>) )
import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary, choose, oneof, shuffle, suchThat
  )

newtype ValidDecl
  = ValidDecl
  { validDecl :: (String, (Ident, Maybe Type, Expr)) }
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
    (sn, n, _) <- fmap validIdent arbitrary
    (st, t) <- validMaybeType
    sass    <- validAss
    (se, e, _) <- fmap validExpr arbitrary
    return (sn ++ st ++ sass ++ se, (n, t, e))

validDeclString :: Gen String
validDeclString = fmap (fst . validDecl) arbitrary

newtype InvalidDecl
  = InvalidDecl { invalidDecl :: String }
  deriving (Eq, Show)

validInvalid :: [(Gen String, Gen String)]
validInvalid =
  [ (fmap validIdentString arbitrary  , fmap invalidIdent arbitrary)
  , (fmap fst validMaybeType          , fmap invalidType arbitrary)
  , (validAss                         , invalidAss)
  , (fmap validExprString arbitrary   , fmap invalidExpr arbitrary)
  ]

instance Arbitrary InvalidDecl where
  arbitrary = fmap InvalidDecl $ do
    let n = length validInvalid
    n_fst <- choose (1, n)
    let n_snd = n - n_fst
    fs <- shuffle $ replicate n_fst fst ++ replicate n_snd snd
    let gens = fs <*> validInvalid
    fmap concat $ sequence gens

invalidDeclString :: Gen String
invalidDeclString = fmap invalidDecl arbitrary
