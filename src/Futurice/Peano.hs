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
    -- * Peano numbers
    Peano(..),
    SPeano(..),
    sPeanoToInteger,
    SPeanoI(..),
    -- * Type-Functions
    Index,
    Image,
    PAdd,
    -- * Type aliases
    PZero, POne, PTwo, PThree, PFour, PFive,
    -- * Proofs
    proofPAddZeroN,
    proofPAddNZero,
    -- ** Safe variants of unsafe proofs
    proofPAddNZero',
    ) where

import Data.Type.Equality
import Unsafe.Coerce      (unsafeCoerce)

-- $setup
-- >>> :set -XDataKinds

-- | Peano natural numbers. Better then 'Nat' for type-level usage.
data Peano = PZ | PS Peano

-- | Singleton of 'Peano'.
data SPeano (p :: Peano) where
    SPZ :: SPeano 'PZ
    SPS :: SPeano p -> SPeano ('PS p)

instance Show (SPeano p) where
    showsPrec _ SPZ     = showString "SPZ"
    showsPrec d (SPS n) = showParen (d > 10)
        $ showString "SPS "
        . showsPrec 11 n

-- TODO: peanoToNatural

-- | Convert 'SPeano' to 'Integer'
--
-- >>> sPeanoToInteger SPZ
-- 0
--
-- >>> sPeanoToInteger $ SPS $ SPS $ SPZ
-- 2
--
-- /TODO:/ Natural!
sPeanoToInteger :: SPeano p -> Integer
sPeanoToInteger SPZ     = 0
sPeanoToInteger (SPS n) = 1 + sPeanoToInteger n

-- | Convenience class to get 'SPeano'.
--
-- >>> singPeano :: SPeano PTwo
-- SPS (SPS SPZ)
class SPeanoI p where
    singPeano :: SPeano p
instance SPeanoI 'PZ where singPeano = SPZ
instance SPeanoI p => SPeanoI ('PS p) where
    singPeano = SPS singPeano

-- | A partial relation that gives the index of a value in a list.
type family Index (x :: k) (xs :: [k]) :: Peano where
    Index x (x ': xs) = 'PZ
    Index x (y ': ys) = 'PS (Index x ys)

-- | A partial relation that gives the indices of a sublist in a larger list.
type family Image xs ys :: [Peano] where
    Image '[]       ys = '[]
    Image (x ': xs) ys = Index x ys ': Image xs ys

-- | Plus
--
-- >>> singPeano :: SPeano (PAdd POne PTwo)
-- SPS (SPS (SPS SPZ))
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

-------------------------------------------------------------------------------
-- proofs
-------------------------------------------------------------------------------

-- | @0 + n = n@
proofPAddZeroN :: PAdd PZero n :~: n
proofPAddZeroN = Refl

-- | @n + 0 = n@
--
-- Uses 'unsafeCoerce', see 'proofPAddNZero'' for safe proof.
proofPAddNZero :: PAdd n PZero :~: n
proofPAddNZero = unsafeCoerce trivialProof

-- | @n + 0 = n@
proofPAddNZero' :: SPeanoI n => PAdd n PZero :~: n
proofPAddNZero' = proof singPeano
  where
    proof :: SPeano n -> PAdd n PZero :~: n
    proof SPZ     = Refl
    proof (SPS m) = case proof m of
        Refl -> Refl

trivialProof :: () :~: ()
trivialProof = Refl
