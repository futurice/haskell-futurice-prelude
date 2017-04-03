-- | This is the internal prelude, which re-exports definitions from the
-- dependency packages.
--
-- You should import "Futurice.Prelude".
module Futurice.Prelude.Internal (
    -- * Prelude.Compat
    --
    -- | We use it as a basis. Yet we hide or generalise some definitions.
    -- E.g. 'zip'.
    --
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
    NonEmpty (..),
    Proxy(..),
    TimeSpec(..),
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
    -- ** Aeson
    Aeson.Value,
    AesonPair,
    -- ** Monoids
    Sum (..),
    UnionWith (..),
    -- * Data Classes
    Align (..),
    AlignWithKey (..),
    Binary (..),
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
    Identity (..),
    Const (..),
    Compose (..),
    module Data.Functor.Classes,
    -- * Monad classes
    --
    -- | Note, 'MonadState' members aren't exported.
    -- Use 'view', '%=', '.=', and '?='.
    MonadBase(..),
    MonadBaseControl(..),
    MonadCatch(..),
    MonadError(..),
    MonadFix(..),
    MonadIO(..),
    MonadLog,
    MonadPlus(..),
    MonadReader(..),
    MonadState,
    MonadThrow(..),
    MonadTime (..),
    MonadTrans(..),
    MonadTransControl(..),
    MonadWriter(..),
    -- * Monad transformers
    MaybeT(..),
    ReaderT(..),
    ExceptT(..),
    runExceptT,
    withExceptT,
    -- * log
    Logger,
    LogT,
    logAttention,
    logInfo,
    logTrace,
    logAttention_,
    logInfo_,
    logTrace_,
    runLogT,
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
    -- * Maybe
    fromMaybe,
    mapMaybe,
    catMaybes,
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
    void, join, forever, foldM, guard, when,
    -- * Function
    on, (&),
    -- * SOP
    I (..), unI,
    K (..), unK,
    NP (..), NS (..),
    -- * http-client
    Manager, newManager,
    tlsManagerSettings,
    -- * Lens
    Lens', Lens, lens,
    Prism', Prism, prism, prism',
    Traversal', Traversal,
    Iso', Iso, iso,
    -- ** Operators
    (^?), preview,
    (^.), view,
    (^..),
    (%~), over,
    (.~), (?~),
    from,
    -- ** State
    (%=), (?=),
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
    -- * semigroupoids
    Foldable1(..),
    Traversable1(..),
    -- * mmorph
    hoist,
    -- * Time
    -- ** TH
    mkUTCTime, mkDay,
    -- * Clock
    module Futurice.Clock,
    -- * Misc
    bool,
    ) where

import Prelude ()
import Prelude.Compat hiding (zip, zipWith)

import Control.Applicative         (Alternative (..), Const (..), optional)
import Control.DeepSeq             (NFData (..), ($!!))
import Control.Exception           (evaluate)
import Control.Lens
       (At (..), Iso, Iso', Ixed (..), Lens, Lens', Prism, Prism', Traversal,
       Traversal', folded, from, ifoldMap, ifolded, ifor, ifor_, isn't, iso,
       itoList, itraverse, itraverse_, lazy, lens, makeLenses, makePrisms,
       makeWrapped, over, preview, prism, prism', strict, view, (%=), (%~),
       (&), (.~), (?=), (?~), (^.), (^..), (^?), _1, _2, _3, _Empty, _Just,
       _Left, _Nothing, _Right)
import Control.Monad.Base          (MonadBase (..))
import Control.Monad.Catch
       (Exception, MonadCatch (..), MonadThrow (..), SomeException (..))
import Control.Monad.Compat
       (MonadPlus (..), foldM, forever, guard, join, void, when)
import Control.Monad.Except
       (ExceptT (..), MonadError (..), runExceptT, withExceptT)
import Control.Monad.Fix           (MonadFix (..))
import Control.Monad.IO.Class      (MonadIO (..))
import Control.Monad.Morph         (hoist)
import Control.Monad.Reader        (MonadReader (..), ReaderT (..))
import Control.Monad.State.Class   (MonadState (..))
import Control.Monad.Time          (MonadTime (..))
import Control.Monad.Trans.Class   (MonadTrans (..))
import Control.Monad.Trans.Control
       (MonadBaseControl (..), MonadTransControl (..))
import Control.Monad.Trans.Maybe   (MaybeT (..))
import Control.Monad.Writer.Class  (MonadWriter (..))
import Data.Align                  (Align (..))
import Data.Align.Key              (AlignWithKey (..))
import Data.Bifunctor              (bimap, first, second)
import Data.Binary                 (Binary (..))
import Data.Bool.Compat            (bool)
import Data.ByteString             (ByteString)
import Data.Foldable               (fold, for_, sequenceA_, toList, traverse_)
import Data.Function               (on)
import Data.Functor.Classes
import Data.Functor.Compose        (Compose (..))
import Data.Functor.Contravariant  (contramap, (>$), (>$<))
import Data.Functor.Identity       (Identity (..))
import Data.Functor.Syntax         ((<$$>))
import Data.Hashable               (Hashable (..))
import Data.HashMap.Strict         (HashMap)
import Data.HashSet                (HashSet)
import Data.Int
import Data.IntMap.Strict          (IntMap)
import Data.IntSet                 (IntSet)
import Data.Key                    (Zip (..), ZipWithKey (..))
import Data.List.Compat            (nub, sort, sortBy, sortOn)
import Data.List.Extra             (chunksOf)
import Data.List.NonEmpty          (NonEmpty (..))
import Data.Map.Lens               (toMapOf)
import Data.Map.Strict             (Map)
import Data.Maybe                  (catMaybes, fromMaybe, mapMaybe)
import Data.Profunctor             (dimap, lmap, rmap)
import Data.Proxy                  (Proxy (..))
import Data.Scientific             (Scientific)
import Data.Semigroup              (Semigroup (..), Sum (..))
import Data.Semigroup.Foldable     (Foldable1 (..))
import Data.Semigroup.Traversable  (Traversable1 (..))
import Data.Semigroup.Union        (UnionWith (..))
import Data.Set                    (Set)
import Data.String                 (IsString (..))
import Data.Tagged                 (Tagged (..), untag)
import Data.Text                   (Text)
import Data.Text.Lens              (packed, unpacked)
import Data.These                  (These (..))
import Data.Time
       (Day (..), LocalTime (..), NominalDiffTime, UTCTime (..))
import Data.Time.TH                (mkDay, mkUTCTime)
import Data.Time.Zones             (TZ)
import Data.Traversable            (for)
import Data.Typeable               (Typeable)
import Data.UUID                   (UUID)
import Data.Vector                 (Vector)
import Data.Word
import Futurice.Clock
import Generics.SOP                (I (..), K (..), NP (..), NS (..), unI, unK)
import Generics.SOP.TH             (deriveGeneric)
import GHC.Generics                (Generic)
import Log
       (LogT, Logger, MonadLog, logAttention, logAttention_, logInfo, logInfo_,
       logTrace, logTrace_, runLogT)
import Network.HTTP.Client         (Manager, newManager)
import Network.HTTP.Client.TLS     (tlsManagerSettings)
import Numeric.Natural             (Natural)
import System.Random.Shuffle       (shuffleM)
import Text.Read                   (readMaybe)

import qualified Data.Aeson.Types     as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy       as LT
import qualified Data.Tuple.Strict    as STuple

import qualified Text.PrettyPrint.ANSI.Leijen.AnsiPretty as AnsiPretty

-------------------------------------------------------------------------------
-- type aliases
-------------------------------------------------------------------------------

type Pair = (,)
type List = []
type LazyByteString = LBS.ByteString
type LazyText       = LT.Text
type StrictPair     = STuple.Pair
type AesonPair = Aeson.Pair
