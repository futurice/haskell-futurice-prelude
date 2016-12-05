{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
module Futurice.Prelude (
    module Prelude.Compat,
    -- * Types
    ByteString,
    Day (..),
    HashMap,
    HashSet,
    IntMap,
    IntSet,
    List,
    Pair,
    LazyText,
    LazyByteString,
    LocalTime (..),
    Map,
    Natural,
    NominalDiffTime,
    Proxy(..),
    Scientific,
    Set,
    StrictPair,
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
    AnsiPretty.AnsiPretty,
    Zip (..),
    ZipWithKey (..),
    Exception,
    -- * Functor classes
    module Data.Functor.Classes,
    showsTernaryWith,
    -- * Monad classes
    MonadIO(..),
    MonadCatch(..),
    MonadError(..),
    MonadLog,
    MonadReader(..),
    MonadThrow(..),
    MonadTime (..),
    MonadTrans(..),
    -- * log
    Logger,
    LogT,
    logAttention,
    logInfo,
    logTrace,
    logAttention_,
    logInfo_,
    logTrace_,
    logLocalData,
    logLocalDomain,
    runLogT,
    mkStderrLogger,
    withStderrLogger,
    -- * generics-sop
    deriveGeneric,
    -- * composition-extra
    (<$$>),
    -- * alternative
    Alternative(..), optional,
    -- * bifunctors
    bimap, first, second,
    -- * profunctors
    dimap, lmap, rmap,
    -- * contravariant
    contramap, (>$<), (>$),
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
    sequenceA_,
    -- * Traversable
    for,
    -- ** Indexed
    itoList,
    ifor, itraverse,
    -- * Monad
    void, join, forever, iterateM, foldM, guard, when,
    -- * Function
    on, (&),
    -- * SOP
    I (..), unI,
    K (..), unK,
    NP (..), NS (..),
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
    packed, unpacked,
    strict, lazy,
    -- ** _Empty
    isn't, _Empty,
    -- ** Common optics
    _Just, _Nothing, _Left, _Right,
    _1, _2, _3,
    -- ** Constructors
    toMapOf,
    -- ** TH
    makeLenses, makePrisms, makeWrapped,
    -- * List
    sort, sortBy, sortOn, nub,
    shuffleM,
    chunksOf,
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

import Control.Applicative        (Alternative (..), optional)
import Control.Concurrent.Async   (waitCatch, withAsync)
import Control.DeepSeq            (NFData (..), ($!!))
import Control.Exception          (evaluate)
import Control.Lens
       (Lens', folded, from, ifolded, ifor, ifor_, isn't, itoList, itraverse,
       itraverse_, lazy, lens, makeLenses, makePrisms, makeWrapped, strict,
       view, (&), (.~), (?~), (^.), (^..), (^?), _1, _2, _3, _Empty, _Just,
       _Left, _Nothing, _Right)
import Control.Lens
       (At (..), Ixed (..), ifoldMap, ifoldMapOf, (<.>))
import Control.Monad.Catch
       (Exception, MonadCatch (..), MonadThrow (..), SomeException (..))
import Control.Monad.Compat       (foldM, forever, guard, join, void, when)
import Control.Monad.Except       (MonadError (..))
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Reader       (MonadReader (..))
import Control.Monad.Time         (MonadTime (..))
import Control.Monad.Trans.Class  (MonadTrans (..))
import Data.Align                 (Align (..))
import Data.Align.Key             (AlignWithKey (..))
import Data.Bifunctor             (bimap, first, second)
import Data.Binary                (Binary)
import Data.Bool.Compat           (bool)
import Data.ByteString            (ByteString)
import Data.Foldable              (fold, for_, sequenceA_, toList, traverse_)
import Data.Function              (on)
import Data.Functor.Classes
import Data.Functor.Contravariant (contramap, (>$), (>$<))
import Data.Functor.Syntax        ((<$$>))
import Data.Hashable              (Hashable (..))
import Data.HashMap.Strict        (HashMap)
import Data.HashSet               (HashSet)
import Data.Int
import Data.IntMap.Strict         (IntMap)
import Data.IntSet                (IntSet)
import Data.Key                   (Zip (..), ZipWithKey (..))
import Data.List.Compat           (nub, sort, sortBy, sortOn)
import Data.List.Extra            (chunksOf)
import Data.Map.Lens              (toMapOf)
import Data.Map.Strict            (Map)
import Data.Maybe                 (fromMaybe)
import Data.Profunctor            (dimap, lmap, rmap)
import Data.Proxy                 (Proxy (..))
import Data.Scientific            (Scientific)
import Data.Semigroup             (Semigroup (..), Sum (..))
import Data.Semigroup.Union       (UnionWith (..))
import Data.Set                   (Set)
import Data.String                (IsString (..))
import Data.Tagged                (Tagged (..), untag)
import Data.Text                  (Text)
import Data.Text.Lens             (packed, unpacked)
import Data.These                 (These (..))
import Data.Time
       (Day (..), LocalTime (..), NominalDiffTime, UTCTime (..),
       defaultTimeLocale, formatTime)
import Data.Time.TH               (mkDay, mkUTCTime)
import Data.Time.Zones            (TZ, utcToLocalTimeTZ)
import Data.Time.Zones.TH         (includeTZFromDB)
import Data.Traversable           (for)
import Data.Typeable              (Typeable)
import Data.UUID                  (UUID)
import Data.Vector                (Vector)
import Data.Word
import Generics.SOP               (I (..), K (..), NP (..), NS (..), unI, unK)
import Generics.SOP.TH            (deriveGeneric)
import GHC.Generics               (Generic)
import Log
       (LogLevel (..), LogMessage (..), LogT, Logger, MonadLog, localData,
       localDomain, logAttention, logAttention_, logInfo, logInfo_, logTrace,
       logTrace_, mkBulkLogger, runLogT)
import Log.Internal.Logger        (withLogger)
import Numeric.Natural            (Natural)
import System.Console.ANSI ()
import System.IO                  (hFlush, stderr)
import System.Random.Shuffle      (shuffleM)
import Text.Read                  (readMaybe)

import qualified Data.Aeson.Types     as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map             as Map
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Lazy       as LT
import qualified Data.Tuple.Strict    as STuple
import qualified System.Console.ANSI  as ANSI

import qualified Text.PrettyPrint.ANSI.Leijen.AnsiPretty as AnsiPretty

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

-------------------------------------------------------------------------------
-- type aliases
-------------------------------------------------------------------------------

type Pair = (,)
type List = []
type LazyByteString = LBS.ByteString
type LazyText       = LT.Text
type StrictPair     = STuple.Pair

-------------------------------------------------------------------------------
-- Show
-------------------------------------------------------------------------------

showsTernaryWith
    :: (Int -> a -> ShowS)
    -> (Int -> b -> ShowS)
    -> (Int -> c -> ShowS)
    -> String -> Int -> a -> b -> c -> ShowS
showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10)
    $ showString name
    . showChar ' ' . sp1 11 x
    . showChar ' ' . sp2 11 y
    . showChar ' ' . sp3 11 z

-------------------------------------------------------------------------------
-- Logger
-------------------------------------------------------------------------------

mkStderrLogger :: IO Logger
mkStderrLogger = mkBulkLogger "ansi-stderr" (traverse_ log') (hFlush stderr)
  where
    log' lm@LogMessage { lmMessage = msg, lmData = data_ } = do
        -- Split multiline log messages, e.g. exception dumps
        for_ (T.lines msg) $ \l -> prefix lm >> T.putStrLn l

        when (data_ /= Aeson.emptyObject && data_ /= Aeson.Null) $ do
            let doc = AnsiPretty.nest 4 $ AnsiPretty.ansiPretty data_
            AnsiPretty.putDoc doc
            T.putStrLn ""

    prefix LogMessage {..} = do
        time lmTime
        T.putStr " "
        level lmLevel
        T.putStr " "
        withColour ANSI.Magenta $ T.putStr $ T.justifyLeft 25 ' ' $
            T.intercalate "/" $ lmComponent : lmDomain
        T.putStr " "

    level LogAttention = withColour ANSI.Red   (T.putStr "ERR")
    level LogInfo      = withColour ANSI.Green (T.putStr "INF")
    level LogTrace     = withColour ANSI.Cyan  (T.putStr "TRC")

    time t = withColour ANSI.Blue $
        putStr $ formatTime defaultTimeLocale "%F %T" t

    withColour :: ANSI.Color -> IO () -> IO ()
    withColour c m = do
        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid c]
        m
        ANSI.setSGR [ANSI.Reset]

withStderrLogger :: (Logger -> IO r) -> IO r
withStderrLogger act = do
    logger <- mkStderrLogger
    withLogger logger act

-- | Renamed 'Log.localData'.
logLocalData :: MonadLog m => [Aeson.Pair] -> m a -> m a
logLocalData = localData

-- | Renamed 'Log.localDomain'.
logLocalDomain :: MonadLog m => Text -> m a -> m a
logLocalDomain = localDomain
