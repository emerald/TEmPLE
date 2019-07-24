module Parser.GenClassicExprs
  ( ValidExpr(..)
  , InvalidExpr(..)
  , validExprString
  ) where

import Ast (Expr(..))
import Parser.Common (fullParse)
import Parser.ClassicLits (parseLit)
import Parser.ClassicIdents (parseIdent)

import Parser.GenCommon (token)
import Parser.GenClassicLits (ValidLit(..), InvalidLit(..))
import Parser.GenClassicIdents (ValidIdent(..), InvalidIdent(..))

import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary, shrink
  , oneof, resize, sized, suchThat
  )

newtype ValidExpr
  = ValidExpr {
    validExpr :: (String, Expr, [ValidExpr])
  }

instance Show ValidExpr where
  show (ValidExpr (s, e, _)) = s ++ " ~/~> " ++ show e

instance Eq ValidExpr where
  (ValidExpr (s1, l1, _)) == (ValidExpr (s2, l2, _))
    = (s1, l1) == (s2, l2)

validExprString :: ValidExpr -> String
validExprString (ValidExpr (s, _, _)) = s

validLitToExpr :: ValidLit -> ValidExpr
validLitToExpr (ValidLit (s, l, shrunkenLits))
  = ValidExpr (s, ELit l, map validLitToExpr shrunkenLits)

genLitExpr :: Gen ValidExpr
genLitExpr = fmap validLitToExpr arbitrary

validIdentToExpr :: ValidIdent -> ValidExpr
validIdentToExpr (ValidIdent (s, n, shrunkenIdents))
  = ValidExpr (s, EVar n, map validIdentToExpr shrunkenIdents)

genIdentExpr :: Gen ValidExpr
genIdentExpr = fmap validIdentToExpr arbitrary

genParensExpr :: Gen ValidExpr
genParensExpr = do
  so <- token "("
  (se, e, es) <- fmap validExpr arbitrary
  sc <- token ")"
  return $ ValidExpr (so ++ se ++ sc, e, es)

genExpr :: Int -> Gen ValidExpr
genExpr 0 = oneof
  [ genIdentExpr
  , genLitExpr
  ]
genExpr n = resize (n `div` 2) $ oneof
  [ genParensExpr
  ]

instance Arbitrary ValidExpr where
  arbitrary = sized genExpr

  shrink (ValidExpr (_, _, es)) = es

newtype InvalidExpr = InvalidExpr { invalidExpr :: String }
  deriving (Eq, Show)

instance Arbitrary InvalidExpr where
  arbitrary = fmap InvalidExpr $ do
    suchThat (fmap invalidIdent arbitrary) $
      \ s -> length (fullParse parseLit s) == 0
