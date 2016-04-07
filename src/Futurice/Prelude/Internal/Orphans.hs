{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module defines some orphan instances for types and classes from
-- packages "Futurice.Prelude" depends upon.
module Futurice.Prelude.Internal.Orphans () where

import Prelude        ()
import Prelude.Compat

import Control.Monad.Catch          (MonadCatch (..), MonadThrow (..))
import Control.Monad.CryptoRandom   (CRandT (..))
import Control.Monad.Logger         (MonadLogger (..))
import Control.Monad.Trans.Class    (lift)
import Data.Foldable                (toList)
import Data.Hashable                (Hashable (..))
import Data.Semigroup               (Semigroup (..))
import Data.Vector                  (Vector)
import Generics.SOP                 (I (..))
import Text.PrettyPrint.ANSI.Leijen (Doc)

-- | Defined in 'Futurice.Prelude'.
instance Semigroup Doc where
    (<>) = mappend

-- | Defined in 'Futurice.Prelude'.
--
-- <https://github.com/tibbe/hashable/issues/108>
-- <https://github.com/ekmett/vector-instances/pull/4>
instance Hashable a => Hashable (Vector a) where
    hashWithSalt salt = hashWithSalt salt . toList

-- | Defined in 'Futurice.Prelude'.
instance Eq a => Eq (I a) where
    I a == I b = a == b

-- | Defined in 'Futurice.Prelude'.
--
-- <https://github.com/TomMD/monadcryptorandom/pull/10>
instance MonadThrow m => MonadThrow (CRandT g e m) where
    throwM = lift . throwM

-- | Defined in 'Futurice.Prelude'.
instance MonadCatch m => MonadCatch (CRandT g e m) where
    catch m h = CRandT $ catch (unCRandT m) (unCRandT . h)

-- | Defined in 'Futurice.Prelude'.
instance MonadLogger m => MonadLogger (CRandT g e m) where
    monadLoggerLog a b c d = lift $ monadLoggerLog a b c d
