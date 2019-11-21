{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Tools for ad-hoc reader monad environments.
--
-- A bit like in @vinyl@ - package, but reimplemented for @generics-sop@ data
-- structures.
--
-- TODO: move to own package once stabilised.
module Futurice.Has (
    Has(..),
    IsElem(..),
    IsElem',
    IsSubset(..),
    IsSubset',
    In, In', AllIn, AllIn',
    FlipIn, FlipIn',
--    type (∈),
--    type (⊆),
    ) where

import Control.Lens      (Lens', Prism', lens, set, view)
import Generics.SOP      (I (..), NP (..), NS (..))
import Generics.SOP.Lens (npHead, npTail, _I, _S, _Z)

import qualified Futurice.Peano as N

-- | Poor man 'Has' type-class. Useful with reader and state monads.
--
-- The inversed type parameterds are also handy for specifying reader environment:
--
-- @
-- foobarAction
--    :: (MonadIO m, MonadReader env m, All (Has env) '[ConfFoo, ConfBar])
--    => m ()
-- @
class Has r f where
    field :: Lens' r f

-- | n-ary products from "Generics.SOP" 'Has' all fields in it.
--
-- This is extremly handy for specifying ad-hoc environments.
instance In x xs => Has (NP I xs) x where
    field = proj . _I

-- | 'I' wrapped value 'Has' itself.
instance Has (I x) x where
    field = _I

-------------------------------------------------------------------------------
-- IsElem
-------------------------------------------------------------------------------

-- | The dictionary-less version of 'IsElem'.
class i ~ N.Index a xs => IsElem' a xs i
instance IsElem' x (x ': xs) 'N.Z
instance ('N.S i ~ N.Index x (y ': ys), IsElem' x ys i) => IsElem' x (y ': ys) ('N.S i)

-- | Class to look into products and sums thru lenses and prisms.
class IsElem' a xs i => IsElem a xs i where
    proj :: forall f. Lens'  (NP f xs) (f a)
    inj  :: forall f. Prism' (NS f xs) (f a)

instance IsElem x (x ': xs) 'N.Z where
    proj = npHead
    inj  = _Z

instance ('N.S i ~ N.Index x (y ': ys), IsElem x ys i) => IsElem x (y ': ys) ('N.S i) where
    proj = npTail . proj
    inj  = _S . inj

-------------------------------------------------------------------------------
-- IsSubset
-------------------------------------------------------------------------------

-- | The dictionary-less version of 'IsSubset'.
class (is ~ N.Image xs ys) => IsSubset' xs ys is
instance IsSubset' '[] ys '[]
instance (IsElem' x ys i, IsSubset' xs ys is) => IsSubset' (x ': xs) ys (i ': is)

-- | Class to look into subsets thru lenses and prisms
--
-- TODO: add  @injs@ - prism
class IsSubset' xs ys is => IsSubset xs ys is where
    projs :: forall f. Lens' (NP f ys) (NP f xs)

instance IsSubset '[] ys '[] where
    projs = lens (const Nil) const

instance (IsElem x ys i, IsSubset xs ys is) => IsSubset (x ': xs) ys (i ': is) where
    projs = lens v s
      where
        v ys = view proj ys :* view projs ys
        s :: NP f ys -> NP f (x ': xs) -> NP f ys
        s ys (x :* xs) = set proj x (set projs xs ys)

-------------------------------------------------------------------------------
-- Aliases
-------------------------------------------------------------------------------

type In     x  ys = IsElem    x  ys (N.Index x  ys)
type In'    x  ys = IsElem'   x  ys (N.Index x  ys)
type AllIn  xs ys = IsSubset  xs ys (N.Image xs ys)
type AllIn' xs ys = IsSubset' xs ys (N.Image xs ys)

-------------------------------------------------------------------------------
-- Flipped instances
-------------------------------------------------------------------------------

class In x ys => FlipIn ys x
instance In x ys => FlipIn ys x

class In' x ys => FlipIn' ys x
instance In' x ys => FlipIn' ys x

-- type x  ∈ ys = In' x ys
-- type xs ⊆ ys = AllIn' xs ys
