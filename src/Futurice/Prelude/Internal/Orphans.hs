{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module defines some orphan instances for types and classes from
-- packages "Futurice.Prelude" depends upon.
module Futurice.Prelude.Internal.Orphans () where

import Prelude        ()
import Prelude.Compat

import Data.Foldable                (toList)
import Data.Hashable                (Hashable (..))
import Data.Semigroup               (Semigroup (..))
import Data.Vector                  (Vector)
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
