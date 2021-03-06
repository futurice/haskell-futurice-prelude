{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.Clock (
    MonadClock (..),
    clocked,
    TimeSpec (..),
    timeSpecToSecondsD,
    ) where

import Control.DeepSeq     (NFData, force)
import Control.Monad.Trans
import System.Clock
       (Clock (Monotonic), TimeSpec (..), diffTimeSpec, getTime, toNanoSecs)

#ifndef __GHCJS__
import Servant.Server      (Handler)
#endif

-- | Class of monads which carry the notion of the current time.
class Monad m => MonadClock m where
    monotonicClock :: m TimeSpec

-- | Base instance for IO.
instance MonadClock IO where
    monotonicClock = getTime Monotonic

-- | Generic, overlapping instance.
instance {-# OVERLAPPING #-}
    ( MonadClock m
    , MonadTrans t
    , Monad (t m)
    ) => MonadClock (t m)
  where
    monotonicClock = lift monotonicClock

#ifndef __GHCJS__
instance MonadClock Handler where
    monotonicClock = liftIO monotonicClock
#endif

clocked :: (MonadClock m, NFData a) => m a -> m (TimeSpec, a)
clocked action = do
    start <- monotonicClock
    x <- action
    x' <- return $! force x
    end <- monotonicClock
    return (diffTimeSpec end start, x')

-- | Convert 'TimeSpec' to seconds represented by 'Double'
timeSpecToSecondsD :: TimeSpec -> Double
timeSpecToSecondsD = (* 1e-9) . fromInteger . toNanoSecs
