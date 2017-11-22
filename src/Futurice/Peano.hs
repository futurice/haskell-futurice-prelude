{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- | Peano numbers.
--
-- Adopted rom "Data.Vinyl.TypeLevel" from @vinyl@-package.
module Futurice.Peano (
    module Data.Type.Nat,
    -- * Type-Functions
    Index,
    Image,
    ) where

import Data.Type.Nat

-- | A partial relation that gives the index of a value in a list.
type family Index (x :: k) (xs :: [k]) :: Nat where
    Index x (x ': xs) = 'Z
    Index x (y ': ys) = 'S (Index x ys)

-- | A partial relation that gives the indices of a sublist in a larger list.
type family Image xs ys :: [Nat] where
    Image '[]       ys = '[]
    Image (x ': xs) ys = Index x ys ': Image xs ys
