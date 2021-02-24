-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Util ( liftMaybe ) where

import Control.Monad ( MonadPlus, MonadPlus ( mzero ) )

-- Source: https://hackage.haskell.org/package/monad-extras-0.6.0/docs/src/Control-Monad-Extra.html#liftMaybe
liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe = maybe mzero return
