{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.List
    ( Append
    , append
    ) where

import Generics.SOP (NP (..))

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append '[]       ys= ys
    Append (x ': xs) ys = x ': Append xs ys

append :: NP f xs -> NP f ys -> NP f (Append xs ys)
append Nil       ys = ys
append (x :* xs) ys = x :* append xs ys
