{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
    ) where

import Control.Lens      (Lens', Prism')
import Generics.SOP      (I (..), NP (..), NS (..))
import Generics.SOP.Lens (headLens, tailLens, uni, _S, _Z)

import Futurice.Peano

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
instance IsElem xs x (Index x xs) => Has (NP I xs) x where
    field = proj . uni

-- | Class to look into products and sums thru lenses and prisms.
--
-- See 'reifyDiagIsElem' and 'diagIsElemDict' for construction.
class i ~ Index a xs => IsElem xs a i where
    proj :: forall f. Lens'  (NP f xs) (f a)
    inj  :: forall f. Prism' (NS f xs) (f a)

instance IsElem (x ': xs) x 'PZ where
    proj = headLens
    inj  = _Z

instance ('PS i ~ Index x (y ': ys), IsElem ys x i) => IsElem (y ': ys) x ('PS i) where
    proj = tailLens . proj
    inj  = _S . inj
