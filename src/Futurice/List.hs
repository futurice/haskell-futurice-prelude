{-# LANGUAGE DataKinds            #-}
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
    Head,
    UnSingleton,
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Coerce      (coerce)
import Generics.SOP     ((:.:) (..))

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
-- >>> λ *Futurice.List > :kind! UnSingleton '[Int]
-- UnSingleton '[Int] :: *
-- = Int
--
-- >>> λ *Futurice.List > :kind! UnSingleton '[]
-- UnSingleton '[] :: k
-- = UnSingleton '[]
--
-- >>> λ *Futurice.List > :kind! UnSingleton '[Int, Bool]
-- UnSingleton '[Int, Bool] :: *
-- = UnSingleton '[Int, Bool]
--
type family UnSingleton (xs :: [k]) where
    UnSingleton '[x] = x
    -- TODO: add TypeError on GHC 8.0?
