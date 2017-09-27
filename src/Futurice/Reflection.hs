{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- | See <https://github.com/ghc-proposals/ghc-proposals/pull/16>
module Futurice.Reflection (
    TypeRep,
    typeRep,
    eqTypeRep,
    cast,
    Typeable,
    -- * Internal
    trivialProof,
    ) where

import Futurice.Prelude
import Prelude ()

import Data.GADT.Compare  (GCompare (..), GEq (..), GOrdering (..))
import Data.Type.Equality
import Unsafe.Coerce      (unsafeCoerce)

import qualified Data.Typeable as GHC

-- | Tagged type representation.
newtype TypeRep (a :: ki) = TypeRep GHC.TypeRep
  deriving (Show)

instance GEq TypeRep where geq = eqTypeRep
instance GCompare TypeRep where gcompare = cmpTypeRep

-- | Returns a concrete representation of the type.
--
-- >>> typeRep :: TypeRep [Int]
-- TypeRep [Int]
--
typeRep :: forall a. Typeable a => TypeRep a
typeRep = TypeRep (GHC.typeRep (Proxy :: Proxy a))

-- | Equality check for type representation.
--
-- >>> eqTypeRep (typeRep :: TypeRep Int) (typeRep :: TypeRep Int)
-- Just Refl
--
-- >>> eqTypeRep (typeRep :: TypeRep Int) (typeRep :: TypeRep Char)
-- Nothing
--
eqTypeRep :: TypeRep a -> TypeRep b -> Maybe (a :~: b)
eqTypeRep (TypeRep a) (TypeRep b)
    | a == b    = Just (unsafeCoerce trivialProof)
    | otherwise = Nothing

cmpTypeRep :: TypeRep a -> TypeRep b -> GOrdering a b
cmpTypeRep (TypeRep a) (TypeRep b) = case compare a b of
    EQ -> unsafeCoerce (GEQ :: GOrdering () ())
    LT -> GLT
    GT -> GGT

-- | The type-safe cast operation
--
-- >>> cast (1 :: Int) :: Maybe Int
-- Just 1
--
-- >>> cast (1 :: Int) :: Maybe Double
-- Nothing
--
cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b
cast x = flip castWith x <$> eqTypeRep typeRep typeRep

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

-- | The trivial proof of @() ~ ()@ type equality, which we 'unsafeCoerce',
-- when we know what we are doing.
--
trivialProof :: () :~: ()
trivialProof = Refl
