{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.IdMap (
    -- * Constructing
    IdMap,
    HasKey (..),
    fromFoldable,
    idMapOf,
    unsafeFromMap,
    -- * Keys
    keysSet,
    -- * Operations
    Futurice.IdMap.filter,
    -- * Lens
    toIdMapOf,
    unsafeTraversal,
    Futurice.IdMap.ifolded,
    -- * Conversions
    toMap,
    -- * Debug
    valid,
    ) where

import Prelude ()
import Futurice.Prelude

import Data.Kind (Type)
import Control.Lens
       (At (..), Getting, Index, IndexedFold, IxValue, Ixed (..), iall,
       ifolded, set, to, toListOf, views)
import Test.QuickCheck (Arbitrary (..), listOf1)

import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- HasKey
-------------------------------------------------------------------------------

class Ord (Key a) => HasKey a where
    type Key a :: Type
    key :: Lens' a (Key a)

instance (HasKey a, HasKey b, Key a ~ Key b) => HasKey (Either a b) where
    type Key (Either a b) = Key a
    key = lens g s
      where
        g (Right x) = view key x
        g (Left x)  = view key x

        s (Right x) k = Right $ set key k x
        s (Left x)  k = Left  $ set key k x

-------------------------------------------------------------------------------
-- IdMap
-------------------------------------------------------------------------------

newtype IdMap a = IdMap (Map (Key a) a)
  deriving (Typeable, Generic)

deriving instance (Eq   (Key a), Eq a)   => Eq   (IdMap a)
deriving instance (Ord  (Key a), Ord a)  => Ord  (IdMap a)
deriving instance (Show (Key a), Show a) => Show (IdMap a)

toMap :: IdMap a -> Map (Key a) a
toMap (IdMap m) = m

unsafeFromMap :: Map (Key a) a -> IdMap a
unsafeFromMap = IdMap

fromFoldable :: (HasKey a, Foldable f) => f a -> IdMap a
fromFoldable = idMapOf folded

idMapOf :: HasKey a => Getting (IdMap a) s a -> s -> IdMap a
idMapOf l = views l (\x -> IdMap $ Map.singleton (x ^. key) x)

keysSet :: IdMap a -> Set (Key a)
keysSet = Map.keysSet . toMap

-- | Check the internal invariants
valid :: HasKey a => IdMap a -> Bool
valid (IdMap m) =
    iall (\k v -> k == v ^. key) m &&
    Map.valid m

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

filter :: (a -> Bool) -> IdMap a -> IdMap a
filter p (IdMap m) = IdMap (Map.filter p m)

-------------------------------------------------------------------------------
-- Lens
-------------------------------------------------------------------------------

toIdMapOf :: HasKey a => Getting (Endo [a]) s a -> s -> IdMap a
toIdMapOf l s = fromFoldable (toListOf l s)

-- | You must preserve the identifier of elements to this to be valid 'Traversal'
unsafeTraversal :: Traversal' (IdMap a) a
unsafeTraversal f (IdMap m) = IdMap <$> traverse f m

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance HasKey a => Semigroup (IdMap a) where
    IdMap a <> IdMap b = IdMap (a <> b)

instance HasKey a => Monoid (IdMap a) where
    mempty  = IdMap mempty
    mappend = (<>)

instance (NFData (Key a), NFData a) => NFData (IdMap a) where
    rnf (IdMap m) = rnf m

instance (HasKey a, Arbitrary a) => Arbitrary (IdMap a) where
    arbitrary = fromFoldable <$> listOf1 arbitrary
    shrink = map fromFoldable . shrink . toList

type instance Index (IdMap a)   = Key a
type instance IxValue (IdMap a) = a

instance Ord (Key a) => Ixed (IdMap a) where
    ix i f (IdMap m) = IdMap <$> ix i f m

instance Ord (Key a) => At (IdMap a) where
    at i f (IdMap m) = IdMap <$> at i f m

instance Foldable IdMap where
    foldMap f (IdMap m) = foldMap f m

ifolded :: IndexedFold (Key a) (IdMap a) a
ifolded = to toMap . Control.Lens.ifolded
