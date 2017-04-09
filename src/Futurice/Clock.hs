{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.Clock (
    MonadClock (..),
    clocked,
    TimeSpec (..),
    timeSpecToSecondsD,
    ) where

import Control.Monad.Trans
import Servant.Server (Handler)
import System.Clock
       (Clock (Monotonic), TimeSpec (..), diffTimeSpec, getTime, toNanoSecs)

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

instance MonadClock Handler where
    monotonicClock = liftIO monotonicClock

clocked :: MonadClock m => m a -> m (TimeSpec, a)
clocked action = do
    start <- monotonicClock
    x <- action
    end <- monotonicClock
    return (diffTimeSpec end start, x)

-- | Convert 'TimeSpec' to seconds represented by 'Double'
timeSpecToSecondsD :: TimeSpec -> Double
timeSpecToSecondsD = (* 1e-9) . fromInteger . toNanoSecs
