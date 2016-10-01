{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.IdMap (
    -- * Constructing
    IdMap,
    HasKey (..),
    fromFoldable,
    -- * Keys
    keysSet,
    -- * Lens
    toIdMapOf,
    unsafeTraversal,
    ) where

import Futurice.Prelude
import Prelude ()

import Control.Lens
       (At (..), Getting, Index, IxValue, Ixed (..), Traversal', toListOf)
import Data.Monoid     (Endo)
import Test.QuickCheck (Arbitrary (..), listOf1)

import qualified Data.Map as Map

class Ord (Key a) => HasKey a where
    type Key a :: *
    key :: Lens' a (Key a)

newtype IdMap a = IdMap (Map (Key a) a)
  deriving (Typeable, Generic)

deriving instance (Eq   (Key a), Eq a)   => Eq   (IdMap a)
deriving instance (Ord  (Key a), Ord a)  => Ord  (IdMap a)
deriving instance (Show (Key a), Show a) => Show (IdMap a)

instance Foldable IdMap where
    foldMap f (IdMap m) = foldMap f m

unIdMap :: IdMap a -> Map (Key a) a
unIdMap (IdMap m) = m

fromFoldable :: (HasKey a, Foldable f) => f a -> IdMap a
fromFoldable = IdMap . Map.fromList . map (\x -> (x ^. key, x)) . toList

keysSet :: IdMap a -> Set (Key a)
keysSet = Map.keysSet . unIdMap

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

instance (HasKey a, Arbitrary a) => Arbitrary (IdMap a) where
    arbitrary = fromFoldable <$> listOf1 arbitrary
    shrink = map fromFoldable . shrink . toList

type instance Index (IdMap a)   = Key a
type instance IxValue (IdMap a) = a

instance Ord (Key a) => Ixed (IdMap a) where
    ix i f (IdMap m) = IdMap <$> ix i f m

instance Ord (Key a) => At (IdMap a) where
    at i f (IdMap m) = IdMap <$> at i f m
