{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.IsMaybe where

import Futurice.Prelude
import Prelude ()

-- | Decision class for whether a type is 'Maybe'.
--
-- 'SIsMaybeI' provides an evidence. See 'SIsMaybe'.
--
class SIsMaybeI a (IsMaybeF a) => IsMaybe a
instance SIsMaybeI a (IsMaybeF a) => IsMaybe a

sIsMaybe :: IsMaybe a => Proxy a -> SIsMaybe a (IsMaybeF a)
sIsMaybe _ = sIsMaybe'

-------------------------------------------------------------------------------
-- Implementation details
-------------------------------------------------------------------------------

-- | Evidence of whether the type @a@ is @'Maybe' c@, for some @c@.
data SIsMaybe a (b :: Bool) where
    SIsMaybe    :: Proxy c -> SIsMaybe (Maybe c) 'True
    SIsNotMaybe :: IsMaybeF a ~ 'False => SIsMaybe a 'False

-- | To decide, this type family must be reduced.
type family IsMaybeF a :: Bool where
    IsMaybeF (Maybe b) = 'True
    IsMaybeF a         = 'False

class SIsMaybeI a b where
    sIsMaybe' :: SIsMaybe a b
instance SIsMaybeI (Maybe c) 'True where
    sIsMaybe' = SIsMaybe Proxy
instance IsMaybeF a ~ 'False => SIsMaybeI a 'False where
    sIsMaybe' = SIsNotMaybe
