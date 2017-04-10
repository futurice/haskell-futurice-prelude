{-# LANGUAGE RankNTypes #-}
module Futurice.Control where

import Prelude ()
import Futurice.Prelude.Internal

-------------------------------------------------------------------------------
-- monad-control https://github.com/basvandijk/monad-control/pull/36
-------------------------------------------------------------------------------

-- | A function like 'Run' that runs a monad transformer @t@ which wraps the
-- monad transformers @n@ and @n'@. This is used in 'defaultLiftWith2'.
type RunDefault2 t n n' = forall m b. (Monad m, Monad (n' m)) => t m b -> m (StT n' (StT n b))

-- | Default definition for the 'liftWith' method.
defaultLiftWith2
    :: (Monad m, Monad (n' m), MonadTransControl n, MonadTransControl n')
    => (forall b.   n (n' m) b -> t m b)     -- ^ Monad constructor
    -> (forall o b. t o b -> n (n' o) b)     -- ^ Monad deconstructor
    -> (RunDefault2 t n n' -> m a)
    -> t m a
defaultLiftWith2 t unT = \f -> t $ liftWith $ \run -> liftWith $ \run' -> f $ run' . run . unT
{-# INLINABLE defaultLiftWith2 #-}

-- | Default definition for the 'restoreT' method for double 'MonadTransControl'.
defaultRestoreT2
    :: (Monad m, Monad (n' m), MonadTransControl n, MonadTransControl n')
    => (n (n' m) a -> t m a)     -- ^ Monad constructor
    -> m (StT n' (StT n a))
    -> t m a
defaultRestoreT2 t = t . restoreT . restoreT
{-# INLINABLE defaultRestoreT2 #-}


