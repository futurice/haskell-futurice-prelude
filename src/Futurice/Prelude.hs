{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
module Futurice.Prelude (
    module Prelude.Compat,
    -- * Types
    Day (..),
    HashMap,
    HashSet,
    IntMap,
    IntSet,
    LocalTime (..),
    Map,
    Natural,
    NominalDiffTime,
    Proxy(..),
    Scientific,
    Set,
    TZ,
    Tagged (..), untag,
    Text,
    These (..),
    UTCTime (..),
    UUID,
    Vector,
    module Data.Int,
    module Data.Word,
    -- ** Monoids
    Sum (..),
    UnionWith (..),
    -- * Data Classes
    Align (..),
    AlignWithKey (..),
    Binary,
    Generic,
    Hashable(..),
    NFData(..),
    Semigroup(..),
    Typeable,
    IsString(..),
    AnsiPretty,
    Zip (..),
    ZipWithKey (..),
    Exception,
    -- * Monad classes
    MonadIO(..),
    MonadCatch(..),
    MonadError(..),
    MonadLogger,
    MonadReader(..),
    MonadThrow(..),
    MonadTime (..),
    MonadTrans(..),
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
    traverse_, itraverse_,
    for_, ifor_,
    ifoldMap,
    -- * Traversable
    for,
    -- ** Indexed
    itoList,
    ifor, itraverse,
    -- * Monad
    void, join, forever, iterateM, foldM, guard, when,
    -- * Function
    on, (&),
    -- * Lens
    Lens', lens,
    -- ** Operators
    (^.), (^..), view,
    (^?),
    (.~), (?~),
    from,
    -- ** Named
    folded, ifolded,
    -- ** At
    ix, at,
    -- ** Text related
    packed,
    strict, lazy,
    -- ** _Empty
    isn't, _Empty,
    -- ** Common optics
    _Just, _Nothing, _Left, _Right,
    _1, _2,
    -- ** Constructors
    toMapOf,
    -- ** TH
    makeLenses, makePrisms, makeWrapped,
    -- * List
    sort, sortBy, nub,
    shuffleM,
    -- * Time
    -- ** TH
    mkUTCTime, mkDay,
    -- * Extras
    type (:$),
    bool,
    textShow,
    swapMapMap,
    -- ** Has classes
    HasUUID (..),
    -- ** Time
    currentDay,
    utcToHelsinkiTime,
    helsinkiTz,
    ) where

import Prelude ()
import Prelude.Compat hiding (zip, zipWith)

import Control.Applicative       (Alternative (..), optional)
import Control.Concurrent.Async  (waitCatch, withAsync)
import Control.DeepSeq           (NFData (..), ($!!))
import Control.Exception         (evaluate)
import Control.Lens
       (Lens', folded, from, ifolded, ifor, ifor_, isn't, itoList, itraverse,
       itraverse_, lazy, lens, makeLenses, makePrisms, makeWrapped, strict,
       view, (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _Empty, _Just, _Left,
       _Nothing, _Right)
import Control.Lens
       (At (..), IndexedGetting, Ixed (..), ifoldMap, ifoldMapOf, iviews,
       (<.>))
import Control.Monad.Catch
       (Exception, MonadCatch (..), MonadThrow (..), SomeException (..))
import Control.Monad.Compat      (foldM, forever, guard, join, void, when)
import Control.Monad.Except      (MonadError (..))
import Control.Monad.IO.Class    (MonadIO (..))
import Control.Monad.Logger
       (MonadLogger, logDebug, logError, logInfo, logWarn, runNoLoggingT,
       runStderrLoggingT)
import Control.Monad.Reader      (MonadReader (..))
import Control.Monad.Time        (MonadTime (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Align                (Align (..))
import Data.Align.Key            (AlignWithKey (..))
import Data.Bifunctor            (first, second)
import Data.Binary               (Binary)
import Data.Bool.Compat          (bool)
import Data.Foldable             (fold, for_, toList, traverse_)
import Data.Function             (on)
import Data.Functor.Syntax       ((<$$>))
import Data.Hashable             (Hashable (..))
import Data.HashMap.Strict       (HashMap)
import Data.HashSet              (HashSet)
import Data.Int
import Data.IntMap.Strict        (IntMap)
import Data.IntSet               (IntSet)
import Data.Key                  (Zip (..), ZipWithKey (..))
import Data.List                 (nub, sort, sortBy)
import Data.Map.Strict           (Map)
import Data.Maybe                (fromMaybe)
import Data.Proxy                (Proxy (..))
import Data.Scientific           (Scientific)
import Data.Semigroup            (Semigroup (..), Sum (..))
import Data.Semigroup.Union      (UnionWith (..))
import Data.Set                  (Set)
import Data.String               (IsString (..))
import Data.Tagged               (Tagged (..), untag)
import Data.Text                 (Text)
import Data.Text.Lens            (packed)
import Data.These                (These (..))
import Data.Time
       (Day (..), LocalTime (..), NominalDiffTime, UTCTime (..))
import Data.Time.TH              (mkDay, mkUTCTime)
import Data.Time.Zones           (TZ, utcToLocalTimeTZ)
import Data.Time.Zones.TH        (includeTZFromDB)
import Data.Traversable          (for)
import Data.Typeable             (Typeable)
import Data.UUID                 (UUID)
import Data.Vector               (Vector)
import Data.Word
import Generics.SOP.TH           (deriveGeneric)
import GHC.Generics              (Generic)
import Numeric.Natural           (Natural)
import System.Random.Shuffle     (shuffleM)
import Text.Read                 (readMaybe)

import Text.PrettyPrint.ANSI.Leijen.AnsiPretty (AnsiPretty)

import qualified Data.Map as Map

import Futurice.Prelude.Internal.Orphans ()

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

-- | Like 'swap', but for 'Map'.
swapMapMap :: (Ord k, Ord k') => Map k (Map k' v) -> Map k' (Map k v)
swapMapMap = getUnionWith . ifoldMapOf (ifolded <.> ifolded) f
  where
    f (k, k') v = UnionWith $ Map.singleton k' $ Map.singleton k v

-- | Construct a map from a 'IndexedGetter', 'Control.Lens.Fold.IndexedFold', 'Control.Lens.Traversal.IndexedTraversal' or 'Control.Lens.Lens.IndexedLens'
--
-- The construction is left-biased (see 'Data.Map.Lazy.union'), i.e. the first
-- occurences of keys in the fold or traversal order are preferred.
--
-- >>> toMapOf folded ["hello", "world"]
-- fromList [(0,"hello"),(1,"world")]
--
-- >>> toMapOf (folded <.> folded) ["foo", "bar"]
-- fromList [((0,0),'f'),((0,1),'o'),((0,2),'o'),((1,0),'b'),((1,1),'a'),((1,2),'r')]
--
-- >>> toMapOf ifolded $ Map.fromList [('a', "hello"), ('b', "world")]
-- fromList [('a',"hello"),('b',"world")]
--
-- @
-- 'toMapOf' ::          'IndexedGetter' i s a     -> s -> 'Map.Map' i a
-- 'toMapOf' :: 'Ord' i => 'IndexedFold' i s a       -> s -> 'Map.Map' i a
-- 'toMapOf' ::          'IndexedLens'' i s a      -> s -> 'Map.Map' i a
-- 'toMapOf' :: 'Ord' i => 'IndexedTraversal'' i s a -> s -> 'Map.Map' i a
-- @
--
-- See https://github.com/ekmett/lens/pull/676
toMapOf :: IndexedGetting i (Map i a) s a -> s -> Map i a
toMapOf l = iviews l Map.singleton

-------------------------------------------------------------------------------
-- UUID
-------------------------------------------------------------------------------

-- | Elements with has an 'UUID'
class HasUUID a where
    uuid :: Lens' a UUID

instance HasUUID UUID where
    uuid = id

-------------------------------------------------------------------------------
-- Time
-------------------------------------------------------------------------------

-- | Current day in Finland, @Europe/Helsinki@ timezone.
currentDay :: MonadTime m => m Day
currentDay = localDay . utcToHelsinkiTime <$> currentTime

utcToHelsinkiTime :: UTCTime -> LocalTime
utcToHelsinkiTime = utcToLocalTimeTZ helsinkiTz

helsinkiTz :: TZ
helsinkiTz = $(includeTZFromDB "Europe/Helsinki")
