{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances  #-}
#define OVERLAPPING_
#else
#define OVERLAPPING_ {-# OVERLAPPING #-}
#endif
-- |
--
-- *TODO* move to own package once stabilised.
module Futurice.Has (
    Has(..),
    IsElem(..),
    ) where

import Control.Lens      (Lens', Prism')
import Generics.SOP      (I (..), NP (..), NS (..))
import Generics.SOP.Lens (headLens, tailLens, uni, _S, _Z)

class Has f r where
    field :: Lens' r f

class IsElem xs a where
    proj :: Lens' (NP f xs) (f a)
    inj  :: Prism' (NS f xs) (f a)

instance OVERLAPPING_ IsElem (a ': xs) a where
    proj  = headLens
    inj   = _Z

instance IsElem xs a => IsElem (b ': xs) a where
    proj = tailLens . proj
    inj  = _S . inj

instance IsElem xs x => Has x (NP I xs) where
    field = proj . uni
