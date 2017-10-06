{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.TypeTag (
    -- * TT
    TT (..),
    typeTags,
    typeTagDict,
    typeTagDict2,
    typeTagDict3,
    -- * SomeTT
    SomeTT (..),
    someTTToText,
    someTTFromText,
    someTTToInt,
    -- * Is
    Is,
    ) where

import Data.Aeson         (FromJSON (..), ToJSON (..), withText)
import Data.Constraint
import Data.GADT.Compare
       (GCompare (..), GEq (..), GOrdering (..), defaultCompare, defaultEq)
import Data.Type.Equality
import Data.Typeable      (typeRep)
import Futurice.Prelude
import Generics.SOP
import Prelude ()

import qualified Data.Map          as Map
import qualified Generics.SOP.Dict as SOP

-------------------------------------------------------------------------------
-- Is
-------------------------------------------------------------------------------

-- | Type-equality, non-infix name.
type Is = (:~:)

-- | Show refl with visible type-application
--
-- >>> showsPrecIs 0 (Refl :: Int :~: Int) ""
-- "Refl @Int"
showsPrecIs :: forall a b. Typeable a => Int -> Is b a -> ShowS
showsPrecIs d Refl = showParen (d > 10)
    $ showString "Refl @"
    . showsPrec 11 (typeRep (Proxy :: Proxy a))

-------------------------------------------------------------------------------
-- TT
-------------------------------------------------------------------------------

-- | Type tag. A closed alternative to 'Typeable'.
newtype TT (xs :: [k]) (a :: k) = TT { unTT :: NS (Is a) xs }

instance All Typeable xs => Show (TT xs a) where
    showsPrec d (TT t) = showParen (d > 10)
        $ showString "TT "
        . showsPrecNS (Proxy :: Proxy Typeable) showsPrecIs 11 t

instance NFData (TT xs a) where
    rnf (TT (Z Refl)) = rnf ()
    rnf (TT (S xs))   = rnf (TT xs)

instance GEq (TT xs) where
    TT (Z Refl) `geq` TT (Z Refl) = Just Refl
    TT (S a)    `geq` TT (S b)    = TT a `geq` TT b
    _                `geq` _                = Nothing

instance GCompare (TT xs) where
    TT (Z Refl) `gcompare` TT (Z Refl) = GEQ
    TT (S a)    `gcompare` TT (S b)    = TT a `gcompare` TT b
    TT (Z _)    `gcompare` TT (S _)    = GLT
    TT (S _)    `gcompare` TT (Z _)    = GGT

showsPrecNS
    :: All c xs
    => Proxy c
    -> (forall x. c x => Int -> f x -> ShowS)
    -> Int
    -> NS f xs
    -> ShowS
showsPrecNS _ f d (Z x)  = showParen (d > 10)
    $ showString "Z "
    . f 11 x
showsPrecNS p f d (S xs) = showParen (d > 10)
    $ showString "S "
    . showsPrecNS p f 11 xs

-- | List of 'TT's.
--
-- >>> typeTags :: NP (TT '[Int, String]) '[Int, String]
-- TT (Z (Refl @Int)) :* (TT (S (Z (Refl @[Char]))) :* Nil)
--
typeTags :: forall xs. SListI xs => NP (TT xs) xs
typeTags = case sList :: SList xs of
    SNil  -> Nil
    SCons -> TT (Z Refl) :* typeTags'
  where
    typeTags' :: forall y ys. SListI ys => NP (TT (y ': ys)) ys
    typeTags' = hmap (TT . S . unTT) typeTags

-- | Recover type-class dictionary.
typeTagDict :: forall c xs a. All c xs => Proxy c -> TT xs a -> Dict (c a)
typeTagDict _ (TT t) = hcollapse (hzipWith f SOP.hdicts t)
  where
    f :: SOP.Dict c x -> Is a x -> K (Dict (c a)) x
    f SOP.Dict Refl = K Dict

-- | Recover two type-class dictionaries simultaneously
typeTagDict2
    :: forall c1 c2 xs a. (All c1 xs, All c2 xs)
    => Proxy c1 -> Proxy c2
    -> TT xs a -> (Dict (c1 a), Dict (c2 a))
typeTagDict2 p1 p2 t = (typeTagDict p1 t, typeTagDict p2 t)

-- | Recover three type-class dictionaries simultaneously
typeTagDict3
    :: forall c1 c2 c3 xs a. (All c1 xs, All c2 xs, All c3 xs)
    => Proxy c1 -> Proxy c2 -> Proxy c3
    -> TT xs a -> (Dict (c1 a), Dict (c2 a), Dict (c3 a))
typeTagDict3 p1 p2 p3 t = (typeTagDict p1 t, typeTagDict p2 t, typeTagDict p3 t)

-------------------------------------------------------------------------------
-- SomeTT
-------------------------------------------------------------------------------

-- | Existential wrapper around 'TT'.
data SomeTT xs where
    SomeTT :: TT xs a -> SomeTT xs

deriving instance All Typeable xs => Show (SomeTT xs)

instance NFData (SomeTT xs) where
    rnf (SomeTT tt) = rnf tt

instance Eq (SomeTT xs) where
    SomeTT x == SomeTT y = defaultEq x y

instance Ord (SomeTT xs) where
    compare (SomeTT x) (SomeTT y) = defaultCompare x y

instance Hashable (SomeTT xs) where
    hashWithSalt salt stt = hashWithSalt salt (someTTToInt stt)

instance SListI xs => Binary (SomeTT xs) where
    put = put . someTTToInt
    get = do
        i <- get
        case someTTFromInt i of
            Just stt -> pure stt
            Nothing  -> fail $ "Invalid tag index " ++ show i

instance All Typeable xs => ToJSON (SomeTT xs) where
    toJSON     = toJSON . someTTToText
    toEncoding = toEncoding . someTTToText

instance All Typeable xs => FromJSON (SomeTT xs) where
    parseJSON = withText "SomeTT" $ \t ->
        case someTTFromText t of
            Just stt -> pure stt
            Nothing  -> fail $ "Cannot parse tag " ++ show t

-- | Construct 'SomeTT' from serialised name.
--
-- >>> someTTFromText "Int" :: Maybe (SomeTT '[Char, Int, Bool])
-- Just (SomeTT (TT (S (Z (Refl @Int)))))
--
-- >>> someTTFromText "Double" :: Maybe (SomeTT '[Char, Int, Bool])
-- Nothing
--
someTTFromText :: forall xs. All Typeable xs => Text -> Maybe (SomeTT xs)
someTTFromText n = Map.lookup n m
  where
    m :: Map Text (SomeTT xs)
    m = Map.fromList $ hcollapse $ hcmap (Proxy :: Proxy Typeable) f typeTags

    f :: forall x. Typeable x => TT xs x -> K (Text, SomeTT xs) x
    f tag = K (textShow (typeRep (Proxy :: Proxy x)), SomeTT tag)

-- |
--
-- >>> fmap someTTToText (someTTFromText "Int" :: Maybe (SomeTT '[Char, Int, Bool]))
-- Just "Int"
--
someTTToText :: forall xs. All Typeable xs => SomeTT xs -> Text
someTTToText (SomeTT (TT xs)) =
    hcollapse $ hcmap (Proxy :: Proxy Typeable) f xs
  where
    f :: forall x f. Typeable x => f x -> K Text x
    f _ = K (textShow (typeRep (Proxy :: Proxy x)))

-- |
--
-- >>> fmap someTTToInt (someTTFromText "Int" :: Maybe (SomeTT '[Char, Int, Bool]))
-- Just 1
--
someTTToInt :: SomeTT xs -> Int
someTTToInt (SomeTT (TT ns)) = hindex ns

-- |
--
-- >>> someTTFromInt 2 :: Maybe (SomeTT '[Char, Int, Bool])
-- Just (SomeTT (TT (S (S (Z (Refl @Bool))))))
--
-- >>> someTTFromInt 3 :: Maybe (SomeTT '[Char, Int, Bool])
-- Nothing
someTTFromInt :: SListI xs => Int -> Maybe (SomeTT xs)
someTTFromInt i = nth i (hcollapse (hmap (K . SomeTT) typeTags))

nth :: Int -> [a] -> Maybe a
nth n = listToMaybe . drop n

-------------------------------------------------------------------------------
-- Doctests
-------------------------------------------------------------------------------

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeOperators
