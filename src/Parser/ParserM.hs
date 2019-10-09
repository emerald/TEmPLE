module Parser.ParserM
  ( ParserM
  , emap
  , liftBool, liftRP, liftRP'
  , parse
  , pfail
  ) where

import Text.ParserCombinators.ReadP ( ReadP, (<++), readP_to_S )
import qualified Text.ParserCombinators.ReadP as R ( pfail )

import Control.Applicative ( Alternative ( empty, (<|>) ) )
import Control.Monad ( MonadPlus ( mzero, mplus ) )

newtype ParserM e a = ParserM
  { runParserM :: ReadP (Either e a) }

instance Functor (ParserM e) where
  fmap f m = m >>= \a -> return $ f a

instance Applicative (ParserM e) where
  pure = return
  df <*> dx = df >>= \f -> dx >>= return . f

instance Alternative (ParserM e) where
  empty = ParserM R.pfail
  (ParserM a) <|> (ParserM b) = ParserM (a <|> b)

instance Monad (ParserM e) where
  return = ParserM . return . Right

  x >>= f = ParserM $ do
    mx <- runParserM x
    case mx of
      Left e -> return $ Left e
      Right v -> runParserM $ f v

instance MonadPlus (ParserM e) where
  mzero = empty
  mplus = (<|>)

pfail :: e -> ParserM e a
pfail = ParserM . return . Left

liftRP' :: ReadP a -> ParserM e a
liftRP' p = ParserM $ (p >>= return . Right)

liftRP :: ReadP a -> e -> ParserM e a
liftRP p e = ParserM $ (p >>= return . Right) <++ (return $ Left e)

liftBool :: (a -> Bool) -> e -> a -> ParserM e a
liftBool f e a =
  case f a of
    True  -> return a
    False -> pfail e

emap :: (e1 -> e2) -> ParserM e1 a -> ParserM e2 a
emap f (ParserM r) = ParserM $ do
  v <- r
  case v of
    Left e -> (return . Left . f) e
    Right a -> (return . Right) a

parse :: ParserM e a -> String -> [(Either e a, String)]
parse p = readP_to_S (runParserM p)
