{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- | See <https://github.com/ghc-proposals/ghc-proposals/pull/16>
module Futurice.Reflection (
    TypeRep,
    typeRep,
    eqTypeRep,
    -- * Internal
    trivialProof,
    ) where

import Prelude ()
import Futurice.Prelude

import Data.GADT.Compare  (GEq (..))
import Data.Type.Equality
import Unsafe.Coerce      (unsafeCoerce)

import qualified Data.Typeable as GHC

-- | Tagged type representation.
newtype TypeRep a = TypeRep GHC.TypeRep

instance GEq TypeRep where geq = eqTypeRep

-- | Returns a concrete representation of the type.
typeRep :: forall a. Typeable a => TypeRep a
typeRep = TypeRep (GHC.typeRep (Proxy :: Proxy a))

-- | Equality check for type representation.
eqTypeRep :: TypeRep a -> TypeRep b -> Maybe (a :~: b)
eqTypeRep (TypeRep a) (TypeRep b)
    | a == b    = Just (unsafeCoerce trivialProof)
    | otherwise = Nothing

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

trivialProof :: () :~: ()
trivialProof = Refl
