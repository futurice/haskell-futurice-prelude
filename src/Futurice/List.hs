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
    -- appendSList,
    TMap,
    npCompToTMap,
    ) where

import Prelude ()
import Futurice.Prelude
import Generics.SOP     ((:.:) (..))

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append '[]       ys= ys
    Append (x ': xs) ys = x ': Append xs ys

append :: NP f xs -> NP f ys -> NP f (Append xs ys)
append Nil       ys = ys
append (x :* xs) ys = x :* append xs ys

-------------------------------------------------------------------------------
-- TMap
-------------------------------------------------------------------------------

type family TMap f (xs :: [k]) :: [k] where
    TMap f '[] = '[]
    TMap f (x ': xs) = f x ': TMap f xs

npCompToTMap :: NP (f :.: g) xs -> NP f (TMap g xs)
npCompToTMap Nil = Nil
npCompToTMap (Comp fg :* xs) = fg :* npCompToTMap xs
