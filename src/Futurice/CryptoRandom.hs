{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Copyright : (c) 2016 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module Futurice.CryptoRandom (
    -- * Generation
    MonadCRandom(..),
    MonadCRandom',
    CRandom (..),
    -- * Evaluation
    CryptoGenError,
    CryptoGen,
    mkCryptoGen,
    evalCRandTThrow,
    evalCRandTThrow',
    -- * Transformer
    CRandT,
    evalCRandT,
    runCRandT,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Monad.CryptoRandom
       (CRandT, CRandom (..), GenError, MonadCRandom (..), evalCRandT,
       newGenIO, runCRandT)
import Crypto.Random.DRBG         (HashDRBG)

type CryptoGen = HashDRBG
type CryptoGenError = GenError
type MonadCRandom' m = MonadCRandom CryptoGenError m

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
