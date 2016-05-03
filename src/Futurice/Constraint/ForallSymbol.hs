{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Futurice.Constraint.ForallSymbol (
    ForallFSymbol(..),
    Dict(..),
    ) where

import Data.Constraint (Constraint, Dict (..))
import Data.Proxy      (Proxy (..))
import GHC.TypeLits    (KnownSymbol, Symbol)

-- | 'ForallF' from "Data.Constraint.Forall", where f is @'Symbol' -> *@.
-- Provides @'KnownSymbol' s@ to work with.
class ForallFSymbol (p :: * -> Constraint) (f :: Symbol -> *) where
    instFSymbol :: forall s. KnownSymbol s => Proxy p -> Proxy f -> Proxy s -> Dict (p (f s))
