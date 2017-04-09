{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Copyright : (c) 2016-2017 Futurice Oy
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
    -- * PoolCRandT
    PoolCRandT (..),
    runPoolCRandT,
    PoolCRandTAbortException,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.MVar.Lifted (MVar, modifyMVar)
import Control.Monad.CryptoRandom
       (CRandT, CRandom (..), ContainsGenError, CryptoRandomGen, GenError,
       MonadCRandom (..), evalCRandT, newGenIO, runCRandT)
import Crypto.Random.DRBG             (HashDRBG)
import Data.Pool                      (Pool, withResource)

#ifdef MIN_VERSION_monad_http
import Control.Monad.Http.Class (MonadHttp (..))
#endif

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

-------------------------------------------------------------------------------
-- Random Pool
-------------------------------------------------------------------------------

newtype PoolCRandT g e m a = PoolCRandT
    { runPoolCRandT' :: ReaderT (Pool (MVar g)) m a
    }
  deriving
    ( Functor, Applicative, Monad
#ifdef MIN_VERSION_monad_http
    , MonadHttp
#endif
    , MonadThrow
    , MonadBase b
    , MonadIO
    )

data PoolCRandTAbortException = PoolCRandTAbortException
  deriving (Show, Typeable)

instance Exception PoolCRandTAbortException

-- | With this instance, you cannot catch anything.
instance MonadThrow m => MonadError e (PoolCRandT g e m) where
    throwError _ = throwM PoolCRandTAbortException
    catchError m _ = m

runPoolCRandT
    :: (MonadBase IO m, MonadThrow m)
    => Pool (MVar g) -> PoolCRandT g GenError m a -> m a
runPoolCRandT mvar action = runReaderT (runPoolCRandT' action) mvar

embedCRandT
    :: (MonadBaseControl IO m, CryptoRandomGen g, ContainsGenError e, MonadThrow m)
    => CRandT g e m a -> PoolCRandT g e m a
embedCRandT action = PoolCRandT $ ReaderT $ \pool -> withResource pool $ \mvar ->
    modifyMVar mvar $ \g -> do
        x <- runCRandT action g
        case x of
            -- should we re-init?
            Left _e       -> throwM PoolCRandTAbortException
            Right (a, g') -> pure (g', a)

instance (MonadBaseControl IO m, CryptoRandomGen g, ContainsGenError e, MonadThrow m)
    => MonadCRandom e (PoolCRandT g e m)
  where
    getCRandom                 = embedCRandT getCRandom
    getBytes n                 = embedCRandT (getBytes n)
    getBytesWithEntropy n entr = embedCRandT (getBytesWithEntropy n entr)
    doReseed seed              = embedCRandT (doReseed seed)

instance MonadTrans (PoolCRandT g e) where
    lift = PoolCRandT . lift

instance MonadBaseControl b m => MonadBaseControl b (PoolCRandT g e m) where
    type StM (PoolCRandT g e m) a = ComposeSt (PoolCRandT g e) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

instance MonadTransControl (PoolCRandT g e) where
    type StT (PoolCRandT g e) a = StT (ReaderT (Pool (MVar g))) a
    liftWith = defaultLiftWith PoolCRandT runPoolCRandT'
    restoreT = defaultRestoreT PoolCRandT

instance MonadReader r m => MonadReader r (PoolCRandT g e m) where
    ask = lift ask
    local f (PoolCRandT (ReaderT m)) = PoolCRandT (ReaderT (local f . m))
