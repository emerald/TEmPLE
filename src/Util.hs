module Util ( liftMaybe ) where

import Control.Monad ( MonadPlus, MonadPlus ( mzero ) )

-- Source: https://hackage.haskell.org/package/monad-extras-0.6.0/docs/src/Control-Monad-Extra.html#liftMaybe
liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe = maybe mzero return
