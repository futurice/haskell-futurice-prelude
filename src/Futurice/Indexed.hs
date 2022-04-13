{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}
module Futurice.Indexed (
    -- * Classes
    IxFunctor (..),
    IxApply (..),
    IxApplicative (..),
    -- * IxConst
    IxConst (..),
    -- * Free
    IxAp (..),
    liftIxAp,
    lowerIxAp,
    lowerIxAp_,
    )  where

import Control.Category (Category)
import Data.Kind (Type)
import Futurice.Prelude
import Prelude ()

import qualified Control.Category as Category

infixl 4 <<$>>
infixl 4 <<*>>

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

class IxFunctor f where
    (<<$>>) :: (a -> b) -> f j k a -> f j k b

class IxFunctor f => IxApply f where
    (<<*>>) :: f i j (a -> b) -> f j k a -> f i k b

class IxApply f => IxApplicative f where
    ipure :: a -> f i i a

-------------------------------------------------------------------------------
-- IxConst
-------------------------------------------------------------------------------

-- | 'IxConst' is to 'Category' as 'Const' is to 'Monoid'.
newtype IxConst cat i o a = IxConst { getIxConst :: cat i o }

instance IxFunctor (IxConst cat) where
    _ <<$>> IxConst x = IxConst x

-- | /Note:/ technically 'Semigroupoid' is enough.
instance Category cat => IxApply (IxConst cat) where
    IxConst f <<*>> IxConst x = IxConst (x Category.. f)

instance Category cat => IxApplicative (IxConst cat) where
    ipure _ = IxConst Category.id

-------------------------------------------------------------------------------
-- Free
-------------------------------------------------------------------------------

-- | Free 'IxApplicative'.
data IxAp :: (k -> k -> Type -> Type) -> k -> k -> Type -> Type where
    IxPure  ::  a                               -> IxAp f i i a
    IxAp    ::  f i j a -> IxAp f j k (a -> b)  -> IxAp f i k b

instance IxFunctor (IxAp f) where
    f <<$>> IxPure x  = IxPure (f x)
    f <<$>> IxAp x y  = IxAp x ((f .) <<$>> y)

instance IxApply (IxAp f) where
    IxPure f <<*>> z  = f <<$>> z
    IxAp x y <<*>> z  = IxAp x (flip <<$>> y <<*>> z)

instance IxApplicative (IxAp f) where
    ipure = IxPure

liftIxAp :: f i j a -> IxAp f i j a
liftIxAp x = IxAp x (IxPure id)

lowerIxAp :: IxApplicative g => (forall n m x. f n m x -> g n m x) -> IxAp f i j a -> g i j a
lowerIxAp _   (IxPure x)  = ipure x
lowerIxAp nt  (IxAp f x)  = flip id <<$>> nt f <<*>> lowerIxAp nt x

lowerIxAp_ :: Category cat => (forall n m x. f n m x -> cat n m) -> IxAp f i j b -> cat i j
lowerIxAp_ f = getIxConst . lowerIxAp (IxConst . f)
