{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans
  (
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Crypto.Random

instance MonadRandom m => MonadRandom (ReaderT r m) where
  getRandomBytes = lift . getRandomBytes

instance MonadRandom m => MonadRandom (ExceptT e m) where
  getRandomBytes = lift . getRandomBytes
