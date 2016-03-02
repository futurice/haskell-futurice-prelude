{-# LANGUAGE CPP                #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeOperators      #-}
module Futurice.Prelude (
    module Prelude.Compat,
    -- * Types
    Day,
    HashMap,
    HashSet,
    IntMap,
    IntSet,
    Map,
    Natural,
    Proxy(..),
    Set,
    Tagged, untag,
    Text,
    UTCTime,
    Vector,
    module Data.Int,
    module Data.Word,
    -- * Data Classes
    Binary,
    Generic,
    Hashable(..),
    NFData(..),
    genericRnf,
    Semigroup(..),
    Typeable,
    IsString(..),
    AnsiPretty,
    Exception,
    -- * Monad classes
    MonadIO(..),
    MonadError(..),
    MonadThrow(..),
    MonadCatch(..),
    -- * generics-sop
    deriveGeneric,
    -- * composition-extra
    (<$$>),
    -- * alternative
    Alternative(..), optional,
    -- * maybe
    fromMaybe,
    -- * foldable
    toList,
    -- * lens
    (^.), view,
    (.~),
    from,
    packed,
    strict, lazy,
    makeLenses, makePrisms,
    -- * Extras
    type (:$),
    ) where

import Prelude        ()
import Prelude.Compat

import Control.Applicative      (Alternative(..), optional)
import Control.Lens             ((^.), (.~), from, makeLenses, makePrisms, strict, lazy, view)
import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Control.Monad.Catch      (Exception, MonadCatch (..), MonadThrow (..))
import Control.Monad.IO.Class   (MonadIO (..))
import Data.Binary              (Binary)
import Data.Foldable            (toList)
import Data.Functor.Syntax      ((<$$>))
import Data.Hashable            (Hashable (..))
import Data.HashMap.Strict      (HashMap)
import Data.HashSet             (HashSet)
import Data.Int
import Data.IntMap.Strict       (IntMap)
import Data.IntSet              (IntSet)
import Data.Map.Strict          (Map)
import Data.Maybe               (fromMaybe)
import Data.Proxy               (Proxy (..))
import Data.Semigroup           (Semigroup (..))
import Data.Set                 (Set)
import Data.String              (IsString (..))
import Data.Tagged              (Tagged, untag)
import Data.Text                (Text)
import Data.Text.Lens           (packed)
import Data.Time                (UTCTime, Day)
import Data.Typeable            (Typeable)
import Data.Vector              (Vector)
import Data.Word
import Generics.SOP.TH          (deriveGeneric)
import GHC.Generics             (Generic)
import Numeric.Natural          (Natural)

import Text.PrettyPrint.ANSI.Leijen.AnsiPretty (AnsiPretty)

#if MIN_VERSION_mtl(2,2,0)
import Control.Monad.Except (MonadError (..))
#else
import Control.Monad.Error (MonadError (..))
#endif

-- Orphans
import Data.Binary.Orphans               ()
import Data.Hashable.Time                ()
import Data.Vector.Instances             ()
import Futurice.Prelude.Internal.Orphans ()

-- Own extras

infixr 0 :$

-- | Type level '$'
type (:$) (f :: k -> l) (x :: k) = f x
