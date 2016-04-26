-- | Utilities for working in REPL.
module Futurice.REPL where

import Futurice.Prelude
import Prelude          ()

import Data.Time (diffUTCTime, getCurrentTime)

-- | Time the execution of IO action.
timeIO :: NFData a => IO a -> IO a
timeIO a = do
    start <- getCurrentTime
    x <- a >>= (evaluate $!!)
    end <- getCurrentTime
    print (diffUTCTime end start)
    return x

-- | Time the calculation of pure value.
time :: NFData a => a -> IO a
time a = do
    start <- getCurrentTime
    x <- evaluate $!! a
    end <- getCurrentTime
    print (diffUTCTime end start)
    return x

