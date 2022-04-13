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
import GHC.TypeLits    (KnownSymbol, Symbol)
import Data.Kind (Type)

-- | 'ForallF' from "Data.Constraint.Forall", where f is @'Symbol' -> Type@.
-- Provides @'KnownSymbol' s@ to work with.
class ForallFSymbol (p :: Type -> Constraint) (f :: Symbol -> Type) where
    instFSymbol :: forall s. KnownSymbol s => Dict (p (f s))
