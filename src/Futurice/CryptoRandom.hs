{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Copyright : (c) 2016-2017 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module Futurice.CryptoRandom (
    -- * Generation
    MonadCRandom(..),
    MonadCRandomR(..),
    CRandom (..),
    CRandomR (..),
    -- * Evaluation
    CryptoGen,
    mkCryptoGen,
    evalCRandTThrow,
    evalCRandTThrow',
    runCRandTThrow',
    -- ** error
    CryptoGenError,
    ContainsCryptoGenError,
    -- * Transformer
    CRandT,
    evalCRandT,
    runCRandT,
    ) where

import Control.Monad.CryptoRandom
       (CRandT, CRandom (..), CRandomR (..), ContainsGenError, GenError,
       MonadCRandom (..), MonadCRandomR (..), evalCRandT, newGenIO, runCRandT)
import Crypto.Random.DRBG         (HashDRBG)
import Futurice.Prelude
import Prelude ()

type CryptoGen = HashDRBG
type CryptoGenError = GenError
type ContainsCryptoGenError = ContainsGenError

-- | Make 'CryptoGen'.
--
-- We have choosen the 'CryptoGen', this helps when generator is
-- created and used right away: no ambigious type if we used
-- 'newGenIO' directly.
mkCryptoGen :: MonadIO m => m CryptoGen
mkCryptoGen = liftIO newGenIO

-- | Helper around 'evalCRandT'.
--
-- > evalCRandTThrow m g = evalCRandT m g >>= either pure throwM
evalCRandTThrow
    :: MonadThrow m
     => CRandT g GenError m a -> g -> m a
evalCRandTThrow m g = evalCRandT m g >>= either throwM pure

-- | Flipped 'evalCRandTThrow'.
evalCRandTThrow'
    :: MonadThrow m
    => g -> CRandT g GenError m a -> m a
evalCRandTThrow' = flip evalCRandTThrow
{-# INLINE evalCRandTThrow' #-}

-- | Flipper helper around 'runCRandT'.
runCRandTThrow'
    :: MonadThrow m
     => g -> CRandT g GenError m a -> m (a, g)
runCRandTThrow' g m = runCRandT m g >>= either throwM pure
