module Parser.GenClassicConstDecls
  ( ValidConstDecl(..)
  , InvalidConstDecl(..)
  ) where

import Ast (ConstDecl(..), Type)

import Parser.GenCommon
  ( invalidOp, invalidOp1
  , token, token1
  , validOp, validOp1
  )
import Parser.GenClassicIdents
  ( ValidIdent(..), InvalidIdent(..)
  , validIdentString
  )
import Parser.GenClassicTypes (ValidType(..), InvalidType(..))
import Parser.GenClassicExprs
  ( ValidExpr(..), InvalidExpr(..)
  , validExprString
  )

import Control.Applicative ((<*>))
import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary, choose, oneof, shuffle, suchThat
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

instance Arbitrary ValidConstDecl where
  arbitrary = fmap ValidConstDecl $ do
    sc      <- validConst
    (sn, n, _) <- fmap validIdent arbitrary
    (st, t) <- validMaybeType
    sass    <- validAss
    (se, e, _) <- fmap validExpr arbitrary
    return (sc ++ sn ++ st ++ sass ++ se, Const n t e)

newtype InvalidConstDecl
  = InvalidConstDecl { invalidConstDecl :: String }
  deriving (Eq, Show)

validInvalid :: [(Gen String, Gen String)]
validInvalid =
  [ (validConst                     , invalidConst)
  , (fmap validIdentString arbitrary , fmap invalidIdent arbitrary)
  , (fmap fst validMaybeType        , fmap invalidType arbitrary)
  , (validAss                       , invalidAss)
  , (fmap validExprString arbitrary , fmap invalidExpr arbitrary)
  ]

instance Arbitrary InvalidConstDecl where
  arbitrary = fmap InvalidConstDecl $ do
    let n = length validInvalid
    n_fst <- choose (1, n)
    let n_snd = n - n_fst
    fs <- shuffle $ replicate n_fst fst ++ replicate n_snd snd
    let gens = fs <*> validInvalid
    fmap concat $ sequence gens
