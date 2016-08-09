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
    NominalDiffTime,
    Proxy(..),
    Set,
    Tagged (..), untag,
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
    MonadReader(..),
    MonadLogger,
    -- * monad-logger
    logDebug,
    logInfo,
    logWarn,
    logError,
    runStderrLoggingT,
    runNoLoggingT,
    -- * generics-sop
    deriveGeneric,
    -- * composition-extra
    (<$$>),
    -- * alternative
    Alternative(..), optional,
    -- * bifunctors
    first, second,
    -- * deepseq
    ($!!),
    -- * exception
    SomeException(..),
    evaluate,
    tryDeep,
    -- * Maybe
    fromMaybe,
    readMaybe,
    -- * Foldable
    fold,
    toList,
    traverse_,
    for_,
    -- ** Indexed
    itoList,
    -- * Monad
    void, join, forever, iterateM, foldM,
    -- * Function
    on, (&),
    -- * Lens
    Lens', lens,
    -- ** Operators
    (^.), (^..), view,
    (.~), (?~),
    from,
    -- ** Misc
    packed,
    strict, lazy,
    -- ** Common optics
    _Just, _Nothing, _Left, _Right,
    _1, _2,
    -- ** TH
    makeLenses, makePrisms,
    -- * List
    sort, sortBy, nub,
    shuffleM,
    -- * Extras
    type (:$),
    textShow,
    ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative      (Alternative (..), optional)
import Control.Concurrent.Async (waitCatch, withAsync)
import Control.DeepSeq          (NFData (..), ($!!))
import Control.DeepSeq.Generics (genericRnf)
import Control.Exception        (evaluate)
import Control.Lens
       (Lens', from, itoList, lazy, lens, makeLenses, makePrisms, strict, view,
       (&), (.~), (?~), (^.), (^..), _1, _2, _Just, _Left, _Nothing, _Right)
import Control.Monad.Catch
       (Exception, MonadCatch (..), MonadThrow (..), SomeException (..))
import Control.Monad.Compat     (foldM, forever, join, void)
import Control.Monad.IO.Class   (MonadIO (..))
import Control.Monad.Logger
       (MonadLogger, logDebug, logError, logInfo, logWarn, runNoLoggingT,
       runStderrLoggingT)
import Control.Monad.Reader     (MonadReader (..))
import Data.Bifunctor           (first, second)
import Data.Binary              (Binary)
import Data.Foldable            (fold, for_, toList, traverse_)
import Data.Function            (on)
import Data.Functor.Syntax      ((<$$>))
import Data.Hashable            (Hashable (..))
import Data.HashMap.Strict      (HashMap)
import Data.HashSet             (HashSet)
import Data.Int
import Data.IntMap.Strict       (IntMap)
import Data.IntSet              (IntSet)
import Data.List                (nub, sort, sortBy)
import Data.Map.Strict          (Map)
import Data.Maybe               (fromMaybe)
import Data.Proxy               (Proxy (..))
import Data.Semigroup           (Semigroup (..))
import Data.Set                 (Set)
import Data.String              (IsString (..))
import Data.Tagged              (Tagged (..), untag)
import Data.Text                (Text)
import Data.Text.Lens           (packed)
import Data.Time                (Day, NominalDiffTime, UTCTime)
import Data.Typeable            (Typeable)
import Data.Vector              (Vector)
import Data.Word
import Generics.SOP.TH          (deriveGeneric)
import GHC.Generics             (Generic)
import Numeric.Natural          (Natural)
import System.Random.Shuffle    (shuffleM)
import Text.Read                (readMaybe)

import Text.PrettyPrint.ANSI.Leijen.AnsiPretty (AnsiPretty)

#if MIN_VERSION_mtl(2,2,0)
import Control.Monad.Except (MonadError (..))
#else
import Control.Monad.Error (MonadError (..))
#endif

#if MIN_VERSION_monad_time(0,2,0)
import Control.Monad.Time ()
#else
import Control.Monad.Time.Instances ()
#endif

-- Orphans
import Data.Binary.Orphans ()
import Data.Hashable.Time ()
import Data.Orphans ()
import Data.Vector.Instances ()
import Futurice.Prelude.Internal.Orphans ()
import Test.QuickCheck.Instances ()

-------------------------------------------------------------------------------
-- Our additions
-------------------------------------------------------------------------------

-- | Type level '$'
type (:$) (f :: k -> l) (x :: k) = f x
infixr 0 :$

-- | Iterative version of 'forever'.
iterateM :: Monad m => (a -> m a) -> a -> m b
iterateM f = go
  where
    go x = f x >>= go

-- | @pack . show@.
textShow :: Show a => a -> Text
textShow = view packed . show

-- | Perform @IO@ action on background thread.
--
-- See <https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions>
tryDeep :: NFData a => IO a -> IO (Either SomeException a)
tryDeep action = flip withAsync waitCatch $ do
    x <- action
    evaluate $!! x
