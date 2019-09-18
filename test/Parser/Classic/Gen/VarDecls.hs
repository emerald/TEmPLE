module Parser.Classic.Gen.VarDecls
  ( ValidVarDecl(..)
  , InvalidVarDecl(..)
  ) where

import Ast ( VarDecl(..), Expr )

import Parser.GenCommon ( genValidInvalid, invalidOp, invalidOp1, validOp, validOp1 )
import Parser.Classic.Gen.Exprs ( ValidExpr(..), InvalidExpr(..), validExprString )
import Parser.Classic.Gen.Idents
  ( ValidIdent(..)
  , validIdentString, invalidIdentString
  )
import Parser.Classic.Gen.IdentLists
  ( invalidIdentListString, validIdentListString
  , ValidIdentList(..)
  )
import Parser.Classic.Gen.Types ( ValidType(..), InvalidType(..) )

import Data.List.NonEmpty (NonEmpty((:|)))

import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary, oneof
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

assOp :: String
assOp = "<-"

validAss :: Gen String
validAss = validOp assOp

invalidAss :: Gen String
invalidAss = invalidOp assOp

validMaybeInit :: Gen (String, Maybe Expr)
validMaybeInit = oneof
  [ return ("", Nothing)
  , do
    sass <- validAss
    (se, e, _) <- fmap validExpr arbitrary
    return (sass ++ se, Just e)
  ]

invalidMaybeInit :: Gen String
invalidMaybeInit = genValidInvalid $
  [ (validAss, invalidAss)
  , (fmap fst validMaybeInit, fmap invalidExpr arbitrary)
  ]

instance Arbitrary ValidVarDecl where
  arbitrary = fmap ValidVarDecl $ do
    sk      <- validKeyword
    (sn, n, _) <- fmap validIdent arbitrary
    (sns, ns, _) <- fmap validIdentList arbitrary
    (st, t) <- fmap validType arbitrary
    (si, ei) <- validMaybeInit
    let s = sk ++ sn ++ sns ++ st ++ si
    return (s, Var (n :| ns, t, ei))

newtype InvalidVarDecl
  = InvalidVarDecl { invalidVarDecl :: String }
  deriving (Eq, Show)

validInvalid :: [(Gen String, Gen String)]
validInvalid =
  [ (validKeyword         , invalidKeyword)
  , (validIdentString     , invalidIdentString)
  , (validIdentListString , invalidIdentListString)
  , (fmap (fst . validType) arbitrary, fmap invalidType arbitrary)
  , (fmap fst validMaybeInit, invalidMaybeInit)
  ]

instance Arbitrary InvalidVarDecl where
  arbitrary = fmap InvalidVarDecl $
    genValidInvalid validInvalid
