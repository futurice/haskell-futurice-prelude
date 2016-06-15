{-# LANGUAGE DataKinds, KindSignatures, GADTs, PolyKinds, TypeFamilies, TypeOperators #-}
-- | Peano numbers.
--
-- Adopted rom "Data.Vinyl.TypeLevel" from @vinyl@-package.
module Futurice.Peano (
    -- * Peano numbers
    Peano(..),
    SPeano(..),
    SPeanoI(..),
    -- * Functions
    Index,
    Image,
    PAdd,
    -- * Type aliases
    PZero, POne, PTwo, PThree, PFour, PFive,
    ) where

import Data.Proxy (Proxy (..))

-- | Peano natural numbers. Better then 'Nat' for type-level usage.
data Peano = PZ | PS Peano

-- | Singleton of 'Peano'.
data SPeano (p :: Peano) where
    SPZ :: SPeano 'PZ
    SPS :: SPeano p -> SPeano ('PS p)

-- | Convenience class to get 'SPeano'.
class SPeanoI p where
    singPeano :: Proxy p -> SPeano p
instance SPeanoI 'PZ where singPeano _ = SPZ
instance SPeanoI p => SPeanoI ('PS p) where
    singPeano _ = SPS $ singPeano Proxy 

-- | A partial relation that gives the index of a value in a list.
type family Index (x :: k) (xs :: [k]) :: Peano where
    Index x (x ': xs) = 'PZ
    Index x (y ': ys) = 'PS (Index x ys)

-- | A partial relation that gives the indices of a sublist in a larger list.
type family Image xs ys :: [Peano] where
    Image '[]       ys = '[]
    Image (x ': xs) ys = Index x ys ': Image xs ys

type family PAdd (n :: Peano) (m :: Peano) :: Peano where
    PAdd 'PZ     m = m
    PAdd ('PS n) m = 'PS (PAdd n m)

-------------------------------------------------------------------------------
-- aliases
-------------------------------------------------------------------------------

type PZero   = 'PZ
type POne    = 'PS PZero
type PTwo    = 'PS POne
type PThree  = 'PS PTwo
type PFour   = 'PS PThree
type PFive   = 'PS PFour
