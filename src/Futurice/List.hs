{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.List (
    Append,
    append,
    TMap,
    npCompToTMap,
    tmapToNSComp,
    splitAppend,
    Head,
    UnSingleton,
    ) where

import Prelude ()
import Futurice.Prelude
import Generics.SOP     ((:.:) (..), SList (..), SListI (..))

-- $setup
-- >>> :set -XDataKinds

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

-- | '++' of type level lists.
--
-- >>> :kind! Append '[Bool, Int] '[String, Double]
-- Append '[Bool, Int] '[String, Double] :: [*]
-- = '[Bool, Int, String, Double]
--
type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append '[]       ys= ys
    Append (x ': xs) ys = x ': Append xs ys

append :: NP f xs -> NP f ys -> NP f (Append xs ys)
append Nil       ys = ys
append (x :* xs) ys = x :* append xs ys

-------------------------------------------------------------------------------
-- TMap
-------------------------------------------------------------------------------

-- | 'map' over type level lists.
--
-- >>> :kind! TMap Maybe '[Bool, Int]
-- TMap Maybe '[Bool, Int] :: [*]
-- = '[Maybe Bool, Maybe Int]
--
type family TMap f (xs :: [k]) :: [k] where
    TMap f '[] = '[]
    TMap f (x ': xs) = f x ': TMap f xs

-- | /TODO/ can we have implementation using unsafeCoerce?
npCompToTMap :: NP (f :.: g) xs -> NP f (TMap g xs)
npCompToTMap Nil = Nil
npCompToTMap (fg :* xs) = coerce fg :* npCompToTMap xs -- use coerce!

tmapToNSComp
    :: forall xs f g. SListI xs
    => NS f (TMap g xs) -> NS (f :.: g) xs
tmapToNSComp ns = case sList :: SList xs of
    SNil  -> case ns of {}
    SCons -> case ns of
        Z fg  -> Z (Comp fg) -- use coerce?
        S ns' -> S (tmapToNSComp ns')

splitAppend
    :: forall xs ys f. SListI xs
    => NS f (Append xs ys) -> Either (NS f xs) (NS f ys)
splitAppend ns = case sList :: SList xs of
    SNil  -> Right ns
    SCons -> case ns of
        Z f -> Left (Z f)
        S n -> first S (splitAppend n)

-------------------------------------------------------------------------------
-- Head
-------------------------------------------------------------------------------

-- | 'head' of type level lists. Partial type family.
--
-- >>> :kind! Head '[Int, Bool]
-- Head '[Int, Bool] :: *
-- = Int
--
-- >>> :kind! Head '[]
-- Head '[] :: k
-- = Head '[]
--
type family Head (xs :: [k]) where
    Head (x ': xs) = x
    -- TODO: add TypeError on GHC 8.0?

-------------------------------------------------------------------------------
-- UnSingleton
-------------------------------------------------------------------------------

-- | More partial function then 'Head'.
--
-- >>> :kind! UnSingleton '[Int]
-- UnSingleton '[Int] :: *
-- = Int
--
-- >>> :kind! UnSingleton '[]
-- UnSingleton '[] :: k
-- = UnSingleton '[]
--
-- >>> :kind! UnSingleton '[Int, Bool]
-- UnSingleton '[Int, Bool] :: *
-- = UnSingleton '[Int, Bool]
--
type family UnSingleton (xs :: [k]) where
    UnSingleton '[x] = x
    -- TODO: add TypeError on GHC 8.0?
