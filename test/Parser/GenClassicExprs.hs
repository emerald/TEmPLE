module Parser.GenClassicExprs
  ( ValidExpr(..)
  , InvalidExpr(..)
  ) where

import Ast (Expr(..))

import Parser.GenClassicNames (ValidName(..), InvalidName(..))

import Test.Tasty.QuickCheck
  ( Arbitrary
  , arbitrary
  )

newtype ValidExpr = ValidExpr { validExpr :: (String, Expr) }
  deriving (Eq, Show)

instance Arbitrary ValidExpr where
  arbitrary = arbitrary >>= \ (ValidName (s, n)) ->
    return $ ValidExpr (s, (EVar n))

newtype InvalidExpr = InvalidExpr { invalidExpr :: String }
  deriving (Eq, Show)

instance Arbitrary InvalidExpr where
  arbitrary = arbitrary >>= \ (InvalidName s) ->
    return $ InvalidExpr s
