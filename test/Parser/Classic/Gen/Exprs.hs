-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.Gen.Exprs
  ( ValidExpr(..)
  , InvalidExpr(..)
  , validExprString
  ) where

import Ast (Expr(..))
import Parser.Utils.ReadP (fullParse)
import Parser.Classic.ReadP (parser)
import Parser.Classic.ReadP.Lits (parseLit)
import Parser.Classic.ReadP.Exprs (prec8, prec7, prec6, prec5)

import Parser.GenCommon (token, token1)
import Parser.Classic.Gen.Lits (ValidLit(..))
import Parser.Classic.Gen.Idents (ValidIdent(..), InvalidIdent(..))

import Test.Tasty.QuickCheck
  ( Arbitrary, Gen
  , arbitrary, shrink
  , elements, oneof, resize, sized, suchThat
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

genExpr8 :: Gen ValidExpr
genExpr8 = do
  (o, c) <- elements prec8
  so <- token1 o
  (se, e', es) <- fmap validExpr $ genExpr' genExpr8
  let e = c e'
  return $ ValidExpr (so ++ se, e, es)

genExpr7 :: Gen ValidExpr
genExpr7 = genBinExpr genExpr7 genExpr8 prec7

genExpr6 :: Gen ValidExpr
genExpr6 = genBinExpr genExpr6 genExpr7 prec6

genExpr5 :: Gen ValidExpr
genExpr5 = genBinExpr genExpr5 genExpr6 prec5

genBinExpr :: Gen ValidExpr -> Gen ValidExpr ->
  [(String, Expr -> Expr -> Expr)] -> Gen ValidExpr
genBinExpr left right prec = do
  (se1, e1, es1) <- fmap validExpr $ genExpr' left
  (o, c) <- elements prec
  so <- token1 o
  (se2, e2, es2) <- fmap validExpr $ genExpr' right
  let e = c e1 e2
  return $ ValidExpr (se1 ++ so ++ se2, e, es1 ++ es2)

genParensExpr :: Gen ValidExpr
genParensExpr = do
  so <- token "("
  (se, e, es) <- fmap validExpr arbitrary
  sc <- token ")"
  return $ ValidExpr (so ++ se ++ sc, e, es)

genExpr' :: Gen ValidExpr -> Gen ValidExpr
genExpr' g = sized $ \n ->
  case n of
    0 -> oneof [genIdentExpr, genLitExpr]
    _ -> resize (n `div` 2) $ oneof [g, genParensExpr]

instance Arbitrary ValidExpr where
  arbitrary = oneof $
    [ genExpr' genExpr8
    , genExpr' genExpr7
    , genExpr' genExpr6
    , genExpr' genExpr5
    ]

  shrink (ValidExpr (_, _, es)) = es

newtype InvalidExpr = InvalidExpr { invalidExpr :: String }
  deriving (Eq, Show)

instance Arbitrary InvalidExpr where
  arbitrary = fmap InvalidExpr $ do
    suchThat (fmap invalidIdent arbitrary) $
      \ s -> length (fullParse (parseLit parser) s) == 0
