{-# LANGUAGE CPP #-}
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
    module Prelude.Compat.Repl.Batteries,
    -- * Types
    ByteString,
    Day (..),
    HashMap,
    HashSet,
    IntMap,
    IntSet,
    LazyByteString,
    LazyText,
    List,
    LocalTime (..),
    Map,
    Natural,
    NominalDiffTime,
    NonEmpty (..),
    Only (..),
    Pair,
    Proxy(..),
    Scientific,
    Set,
    StrictPair,
    Tagged (..), untag,
    Text,
    ShortText,
    These (..),
    TimeOfDay (..),
    TimeSpec(..),
    TZ,
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
    Endo (..),
    UnionWith (..),
    -- * Data Classes
    Align (..),
    Semialign(..),
    SemialignWithIndex (..),
    Binary (..),
    Generic,
    Hashable(..),
    NFData(..),
    Semigroup(..),
    Typeable,
    IsString(..),
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
    -- * applicative
    liftA2,
    -- * alternative
    Alternative(..), optional,
    -- * bifunctors
    bimap, first, second,
    -- * profunctors
    Profunctor (..),
    -- * contravariant
    Contravariant (..), (>$<), phantom,
    -- * deepseq
    force,
    ($!!),
    -- * exception
    SomeException(..),
    evaluate,
    -- * Maybe
    fromMaybe,
    mapMaybe,
    catMaybes,
    readMaybe,
    listToMaybe,
    maybeToList,
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
    -- * Functor
    (<&>),
    -- * Monad
    void, join, forever, foldM, guard, when, unless,
    -- * Function
    on, (&),
    -- * SOP
    I (..), unI,
    K (..), unK,
    NP (..), NS (..),
    -- * TypeLits
    Symbol, KnownSymbol, symbolVal, sameSymbol,
#ifdef MIN_VERSION_http_client
    -- * http-client
    Manager, newManager,
    tlsManagerSettings,
#endif
    -- * Lens
    Lens', Lens, lens,
    Prism', Prism, prism, prism',
    Traversal', Traversal,
    Iso', Iso, iso,
    Getter,
    LensLike', LensLike,
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
    -- * nf
    NF,
    makeNF,
    getNF,
#ifdef MIN_VERSION_file_embed
    -- * file-embed
    embedFile,
    embedStringFile,
    makeRelativeToProject,
#endif
#ifdef MIN_VERSION_file_embed_lzma
    -- * file-embed-lzma
    embedText,
    embedByteString,
#endif
    -- * template-haskell
    Lift,
    -- * Coercible
    Coercible, coerce,
    -- * Time
    -- ** TH
    mkUTCTime, mkDay,
    -- * Clock
    module Futurice.Clock,
    -- * extra
    whenM,
    unlessM,
    -- * Misc
    bool,
    ) where

import Prelude ()
import Prelude.Compat.Repl.Batteries hiding (zip, zipWith)

import Control.Applicative
       (Alternative (..), Const (..), liftA2, optional)
import Control.DeepSeq             (NFData (..), force, ($!!))
import Control.Exception           (evaluate)
import Control.Lens
       (At (..), Getter, Iso, Iso', Ixed (..), Lens, Lens', LensLike, LensLike',
       Prism, Prism', Traversal, Traversal', _1, _2, _3, _Empty, _Just, _Left,
       _Nothing, _Right, folded, from, ifoldMap, ifolded, ifor, ifor_, isn't,
       iso, itoList, itraverse, itraverse_, lazy, lens, makeLenses, makePrisms,
       makeWrapped, over, preview, prism, prism', strict, view, (%=), (%~), (&),
       (.~), (<&>), (?=), (?~), (^.), (^..), (^?))
import Control.Monad
       (MonadPlus (..), foldM, forever, guard, join, unless, void, when)
import Control.Monad.Base          (MonadBase (..))
import Control.Monad.Catch
       (Exception, MonadCatch (..), MonadThrow (..), SomeException (..))
import Control.Monad.Except
       (ExceptT (..), MonadError (..), runExceptT, withExceptT)
import Control.Monad.Extra         (unlessM, whenM)
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
import Data.Align                  (Align (..), Semialign (..))
import Data.Bifunctor              (bimap, first, second)
import Data.Binary                 (Binary (..))
import Data.Bool                   (bool)
import Data.ByteString             (ByteString)
import Data.Coerce                 (Coercible, coerce)
import Data.Foldable               (fold, for_, sequenceA_, toList, traverse_)
import Data.Function               (on)
import Data.Functor.Classes
import Data.Functor.Compose        (Compose (..))
import Data.Functor.Contravariant  (Contravariant (..), phantom, (>$<))
import Data.Functor.Identity       (Identity (..))
import Data.Functor.Syntax         ((<$$>))
import Data.HashMap.Strict         (HashMap)
import Data.HashSet                (HashSet)
import Data.Hashable               (Hashable (..))
import Data.Int
import Data.IntMap.Strict          (IntMap)
import Data.IntSet                 (IntSet)
import Data.Key                    (Zip (..), ZipWithKey (..))
import Data.List                   (nub, sort, sortBy, sortOn)
import Data.List.Extra             (chunksOf)
import Data.List.NonEmpty          (NonEmpty (..))
import Data.Map.Lens               (toMapOf)
import Data.Map.Strict             (Map)
import Data.Maybe
       (catMaybes, fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.NF                     (NF, getNF, makeNF)
import Data.Profunctor             (Profunctor (..))
import Data.Proxy                  (Proxy (..))
import Data.Scientific             (Scientific)
import Data.Semialign.Indexed      (SemialignWithIndex (..))
import Data.Semigroup              (Endo (..), Semigroup (..), Sum (..))
import Data.Semigroup.Foldable     (Foldable1 (..))
import Data.Semigroup.Traversable  (Traversable1 (..))
import Data.Semigroup.Union        (UnionWith (..))
import Data.Set                    (Set)
import Data.String                 (IsString (..))
import Data.Tagged                 (Tagged (..), untag)
import Data.Text                   (Text)
import Data.Text.Lens              (packed, unpacked)
import Data.Text.Short             (ShortText)
import Data.These                  (These (..))
import Data.Time
       (Day (..), LocalTime (..), NominalDiffTime, TimeOfDay (..), UTCTime (..))
import Data.Time.TH                (mkDay, mkUTCTime)
import Data.Time.Zones             (TZ)
import Data.Traversable            (for)
import Data.Tuple.Only             (Only (..))
import Data.Typeable               (Typeable)
import Data.UUID.Types             (UUID)
import Data.Vector                 (Vector)
import Data.Word
import Futurice.Clock
import GHC.Generics                (Generic)
import GHC.TypeLits                (KnownSymbol, Symbol, sameSymbol, symbolVal)
import Generics.SOP                (I (..), K (..), NP (..), NS (..), unI, unK)
import Generics.SOP.TH             (deriveGeneric)
import Language.Haskell.TH.Lift    (Lift)
import Log
       (LogT, Logger, MonadLog, logAttention, logAttention_, logInfo, logInfo_,
       logTrace, logTrace_, runLogT)
import Numeric.Natural             (Natural)
import System.Random.Shuffle       (shuffleM)
import Text.Read                   (readMaybe)

#ifdef MIN_VERSION_file_embed
import Data.FileEmbed (embedFile, embedStringFile, makeRelativeToProject)
#endif
#ifdef MIN_VERSION_file_embed_lzma
import FileEmbedLzma (embedByteString, embedText)
#endif
#ifdef MIN_VERSION_http_client
import Network.HTTP.Client     (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
#endif

import qualified Data.Aeson.Types     as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Strict.Tuple    as STuple
import qualified Data.Text.Lazy       as LT

-------------------------------------------------------------------------------
-- type aliases
-------------------------------------------------------------------------------

type Pair = (,)
type List = []
type LazyByteString = LBS.ByteString
type LazyText       = LT.Text
type StrictPair     = STuple.Pair
type AesonPair = Aeson.Pair
