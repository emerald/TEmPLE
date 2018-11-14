module Parser.GenClassicExprs
  ( ValidExpr(..)
  , InvalidExpr(..)
  ) where

import Ast (Expr(..))

import Parser.GenCommon (token)
import Parser.GenClassicNames (ValidName(..), InvalidName(..))

import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary, oneof, resize, sized
  )

newtype ValidExpr = ValidExpr { validExpr :: (String, Expr) }
  deriving (Eq, Show)

genNameExpr :: Gen ValidExpr
genNameExpr = do
  (s, n) <- fmap validName arbitrary
  return $ ValidExpr (s, EVar n)

genParensExpr :: Gen ValidExpr
genParensExpr = do
  so <- token "("
  (se, e) <- fmap validExpr arbitrary
  sc <- token ")"
  return $ ValidExpr (so ++ se ++ sc, e)

genExpr :: Int -> Gen ValidExpr
genExpr 0 = oneof
  [ genNameExpr
  ]
genExpr n = resize (n `div` 2) $ oneof
  [ genParensExpr
  ]

instance Arbitrary ValidExpr where
  arbitrary = sized genExpr

newtype InvalidExpr = InvalidExpr { invalidExpr :: String }
  deriving (Eq, Show)

instance Arbitrary InvalidExpr where
  arbitrary = arbitrary >>= \ (InvalidName s) ->
    return $ InvalidExpr s
