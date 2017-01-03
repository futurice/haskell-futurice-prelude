{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.Stricter where

import Prelude
import Futurice.Prelude          hiding (Binary (..))
import Control.Monad.State.Class (MonadState (..))

import qualified Control.Monad.Fail as Fail

-------------------------------------------------------------------------------
-- On top of identity
-------------------------------------------------------------------------------

type Stricter s = StricterT s Identity

-- | Construct a state monad computation from a function.
-- (The inverse of 'runStricter'.)
rws :: (Monad m)
    => (s -> (a, s))  -- ^pure state transformer
    -> StricterT s m a   -- ^equivalent state-passing computation
rws f = StricterT (return . f)
{-# INLINE rws #-}

-- | Unwrap a state monad computation as a function.
-- (The inverse of 'state'.)
runStricter
    :: Stricter s a     -- ^ state-passing computation to execute
    -> s           -- ^ initial state
    -> (a, s)      -- ^ return value and final state
runStricter m = runIdentity . runStricterT m
{-# INLINE runStricter #-}

-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
--
-- * @'evalStricter' m s = 'fst' ('runStricter' m s)@
evalStricter
    :: Stricter s a  -- ^state-passing computation to execute
    -> s      -- ^initial value
    -> a      -- ^return value of the state computation
evalStricter m s = fst (runStricter m s)
{-# INLINE evalStricter #-}

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
--
-- * @'execStricter' m s = 'snd' ('runStricter' m s)@
execStricter
    :: Stricter s a  -- ^state-passing computation to execute
    -> s          -- ^initial value
    -> s          -- ^final state
execStricter m s = snd (runStricter m s)
{-# INLINE execStricter #-}

-- | Map both the return value and final state of a computation using
-- the given function.
--
-- * @'runStricter' ('mapStricter' f m) = f . 'runStricter' m@
mapStricter :: ((a, s) -> (b, s)) -> Stricter s a -> Stricter s b
mapStricter f = mapStricterT (Identity . f . runIdentity)
{-# INLINE mapStricter #-}

-- | @'withStricter' f m@ executes action @m@ on a state modified by
-- applying @f@.
--
-- * @'withStricter' f m = 'modify' f >> m@
withStricter :: (s -> s) -> Stricter s a -> Stricter s a
withStricter = withStricterT
{-# INLINE withStricter #-}

-------------------------------------------------------------------------------
-- Transformer
-------------------------------------------------------------------------------

-- | a Reader-Writer-Stricter transformer with single @s@.
newtype StricterT s m a = StricterT { runStricterT :: s -> m (a, s) }

-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
--
-- * @'evalStricterT' m s = 'liftM' 'fst' ('runStricterT' m s)@
evalStricterT :: (Monad m) => StricterT s m a -> s -> m a
evalStricterT m s = do
    (a, _) <- runStricterT m s
    return a
{-# INLINE evalStricterT #-}

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
--
-- * @'execStricterT' m s = 'liftM' 'snd' ('runStricterT' m s)@
execStricterT :: (Monad m) => StricterT s m a -> s -> m s
execStricterT m s = do
    (_, s') <- runStricterT m s
    return s'
{-# INLINE execStricterT #-}

-- | Map both the return value and final state of a computation using
-- the given function.
--
-- * @'runStricterT' ('mapStricterT' f m) = f . 'runStricterT' m@
mapStricterT :: (m (a, s) -> n (b, s)) -> StricterT s m a -> StricterT s n b
mapStricterT f m = StricterT $ f . runStricterT m
{-# INLINE mapStricterT #-}

-- | @'withStricterT' f m@ executes action @m@ on a state modified by
-- applying @f@.
--
-- * @'withStricterT' f m = 'modify' f >> m@
withStricterT :: (s -> s) -> StricterT s m a -> StricterT s m a
withStricterT f m = StricterT $ runStricterT m . f
{-# INLINE withStricterT #-}

instance (Functor m) => Functor (StricterT s m) where
    fmap f m = StricterT $ \ s ->
        fmap (\ (a, s') -> (f a, s')) $ runStricterT m s
    {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (StricterT s m) where
    pure a = StricterT $ \ s -> return (a, s)
    {-# INLINE pure #-}
    StricterT mf <*> StricterT mx = StricterT $ \ s -> do
        (f, s') <- mf s
        (x, s'') <- mx s'
        return (f x, s'')
    {-# INLINE (<*>) #-}

instance (Functor m, MonadPlus m) => Alternative (StricterT s m) where
    empty = StricterT $ \ _ -> mzero
    {-# INLINE empty #-}
    StricterT m <|> StricterT n = StricterT $ \ s -> m s `mplus` n s
    {-# INLINE (<|>) #-}

instance (Monad m) => Monad (StricterT s m) where
#if !(MIN_VERSION_base(4,8,0))
    return a = StricterT $ \ s -> return (a, s)
    {-# INLINE return #-}
#endif
    m >>= k  = StricterT $ \ s -> do
        (a, s') <- runStricterT m s
        runStricterT (k a) s'
    {-# INLINE (>>=) #-}
    fail str = StricterT $ \ _ -> fail str
    {-# INLINE fail #-}

instance (Fail.MonadFail m) => Fail.MonadFail (StricterT s m) where
    fail str = StricterT $ \ _ -> Fail.fail str
    {-# INLINE fail #-}

instance (MonadPlus m) => MonadPlus (StricterT s m) where
    mzero       = StricterT $ \ _ -> mzero
    {-# INLINE mzero #-}
    StricterT m `mplus` StricterT n = StricterT $ \ s -> m s `mplus` n s
    {-# INLINE mplus #-}

instance (MonadFix m) => MonadFix (StricterT s m) where
    mfix f = StricterT $ \ s -> mfix $ \ ~(a, _) -> runStricterT (f a) s
    {-# INLINE mfix #-}

instance MonadTrans (StricterT s) where
    lift m = StricterT $ \ s -> do
        a <- m
        return (a, s)
    {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (StricterT s m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance Monad m => MonadState s (StricterT s m) where
    get = state $ \ s -> (s, s)
    {-# INLINE get #-}
    put s = state $ \ _ -> ((), s)
    {-# INLINE put #-}
    state f = StricterT (return . f)
    {-# INLINE state #-}

instance Monad m => MonadReader s (StricterT s m) where
    ask = get
    reader f = state $ \s -> (f s, s)
    local f m = StricterT $ \s -> do
        (a, _) <- runStricterT m (f s)
        return (a, s)

instance (Monad m, Monoid s) => MonadWriter s (StricterT s m) where
    tell w = StricterT $ \w' ->
        let wt = w `mappend` w'
        in wt `seq` return ((), wt)

    listen m = StricterT $ \w' -> do
        (a, w) <- runStricterT m mempty
        let wt = w' `mappend` w in return ((a, w), wt)

    pass m = StricterT $ \w' -> do
        ((a, f), w) <- runStricterT m mempty
        let wt = f (w' `mappend` w) in return (a, wt)

-------------------------------------------------------------------------------
-- MTL classes
-------------------------------------------------------------------------------

instance MonadError e m => MonadError e (StricterT s m) where
    throwError = lift . throwError
    catchError m h = StricterT $ \s ->
        runStricterT m s `catchError` \e -> runStricterT (h e) s
