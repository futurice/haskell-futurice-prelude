{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
module Futurice.Prelude (
    -- * Re-exports
    module Futurice.Prelude.Internal,
    -- * Types
    Month,
    -- * lens
    getter,
    -- * log
    withStderrLogger,
    logLocalData,
    logLocalDomain,
    -- * Monad
    iterateM,
    -- * @exception@
    tryDeep,
    -- * Has classes
    HasUUID (..),
    -- * Text
    TE.encodeUtf8,
    decodeUtf8Lenient,
    canonicalize,
    -- * Time
    currentDay,
    currentMonth,
    firstDayOfMonth,
    lastDayOfMonth,
    formatHumanHelsinkiTime,
    utcToHelsinkiTime,
    helsinkiTimeToUtc,
    helsinkiTz,
    -- * Vector
    introsort,
    introsortBy,
    -- * monad-control
    ComposeSt,
    defaultLiftBaseWith,
    defaultRestoreM,
    RunDefault,
    defaultLiftWith,
    defaultRestoreT,
    RunDefault2,
    defaultLiftWith2,
    defaultRestoreT2,
    -- * Debug.Trace
    traceShow,
    traceShowId,
    -- * Misc extras
    type (:$),
    mcase,
    textShow,
    swapMapMap,
    showsTernaryWith,
    WrappedResponse (..),
    embedFromJSON,
    ) where

import Futurice.Prelude.Internal
import Prelude ()

import Control.Concurrent.Async    (waitCatch, withAsync)
import Control.Lens                (Optic', ifoldMapOf, (<.>), _Wrapped)
import Control.Monad.Trans.Control
       (ComposeSt, RunDefault, defaultLiftBaseWith, defaultLiftWith,
       defaultRestoreM, defaultRestoreT)
import Control.Monad.Writer.CPS    (Writer, execWriter)
import Data.Time                   (defaultTimeLocale, formatTime, timeZoneName)
import Data.Time.Zones
       (localTimeToUTCTZ, timeZoneForUTCTime, utcToLocalTimeTZ)
import Data.Time.Zones.TH          (includeTZFromDB)
import Futurice.Control
import Futurice.Time.Month
import Log
       (LogLevel (..), LogMessage (..), localData, localDomain, mkBulkLogger')
import Log.Internal.Logger         (withLogger)
import System.IO                   (hFlush, stderr)

import qualified Data.Aeson.Compat            as Aeson
import qualified Data.Aeson.Types             as Aeson
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.CaseInsensitive         as CI
import qualified Data.HashMap.Strict          as HM
import qualified Data.Map                     as Map
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Data.Text.Encoding.Error     as TE
import qualified Data.Text.IO                 as T
import qualified Data.Text.Lazy.Builder       as TB
import qualified Data.Text.Normalize          as TN
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Debug.Trace                  as DT
import qualified Language.Haskell.TH.Syntax   as TH
import qualified Network.HTTP.Client          as H
import qualified Network.HTTP.Types           as H
import qualified System.Console.ANSI          as ANSI
import qualified Text.PrettyPrint.Compact     as PC

import Futurice.Orphans ()

-------------------------------------------------------------------------------
-- Our additions
-------------------------------------------------------------------------------

-- | Type level '$'
--
-- >>> Refl :: (ReaderT Int :$ ExceptT () :$ IO) Char :~: ReaderT Int (ExceptT () IO) Char
-- Refl
--
type (:$) (f :: k -> l) (x :: k) = f x
infixr 0 :$

-- | Iterative version of 'forever'.
--
-- >>> runExceptT $ iterateM (\n -> liftIO (print n) >> if (n < 3) then pure (n + 1) else throwError ()) 0
-- 0
-- 1
-- 2
-- 3
-- Left ()
--
iterateM :: Monad m => (a -> m a) -> a -> m b
iterateM f = go
  where
    go x = f x >>= go

-- | @mcase m x f = maybe x f m@
--
-- Useful for defaulting single 'Maybe"
--
-- @
-- 'mcase' mfoo defaultBar $ \foo ->
--     fooToBar foo
-- @
--
-- >>> mcase Nothing 'A' toUpper
-- 'A'
--
-- >>> mcase (Just 'b') 'A' toUpper
-- 'B'
--
-- >>> mcase (Just 'b') True $ \c -> isLetter c || isSpace c
-- True
--
mcase :: Maybe a -> b -> (a -> b) -> b
mcase m x f = maybe x f m

-- | @pack . show@.
--
-- >>> textShow (1 :: Int)
-- "1"
--
-- >>> :t textShow (1 :: Int)
-- ... :: Text
--
-- /Note:/ we should move to use @text-show@ package.
-- OTOH this isn't a performance issue.
--
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
-- lens
-------------------------------------------------------------------------------

-- | Less ambigious alias for 'Control.Lens.to'
--
-- @
-- 'getter' :: (s -> a) -> 'Getter' s a
-- @
getter :: (Profunctor p, Contravariant f) => (s -> a) -> Optic' p f s a
getter k = dimap k (contramap k)

-------------------------------------------------------------------------------
-- UUID
-------------------------------------------------------------------------------

-- | Elements with has an 'UUID'
class HasUUID a where
    uuid :: Lens' a UUID

instance HasUUID UUID where
    uuid = id

-- | Unfortunately we cannot easily have polymorphic
-- @uuid :: Lens (Tagged tag a) (Tagged tag' a) UUID UUID@.
--
-- But we don't really need that either yet.
--
instance HasUUID a => HasUUID (Tagged tag a) where
    uuid = _Wrapped . uuid

-------------------------------------------------------------------------------
-- Time
-------------------------------------------------------------------------------

-- | Current day in Finland, @Europe/Helsinki@ timezone.
currentDay :: MonadTime m => m Day
currentDay = localDay . utcToHelsinkiTime <$> currentTime

-- | Current month in Finland, @Europe/Helsinki@ timezone.
currentMonth :: MonadTime m => m Month
currentMonth = dayToMonth <$> currentDay

-- | Convert time to local time in helsinki
--
-- >>> utcToHelsinkiTime $(mkUTCTime "2017-02-03T12:00:00.123456Z")
-- 2017-02-03 14:00:00.123456
--
utcToHelsinkiTime :: UTCTime -> LocalTime
utcToHelsinkiTime = utcToLocalTimeTZ helsinkiTz

-- | Convert local time in helsinki to UTC
--
-- >>> helsinkiTimeToUtc $ utcToHelsinkiTime $(mkUTCTime "2017-02-03T12:00:00.123456Z")
-- 2017-02-03 12:00:00.123456 UTC
--
-- >>> helsinkiTimeToUtc (LocalTime $(mkDay "2017-02-03") (TimeOfDay 2 30 0))
-- 2017-02-03 00:30:00 UTC
--
helsinkiTimeToUtc :: LocalTime -> UTCTime
helsinkiTimeToUtc = localTimeToUTCTZ helsinkiTz

-- | @Europe/Helsinki@ timezone.
helsinkiTz :: TZ
helsinkiTz = $(includeTZFromDB "Europe/Helsinki")

-- | Format time to display to humans.
--
-- >>> formatHumanHelsinkiTime $(mkUTCTime "2017-02-03T12:00:00Z")
-- "2017-02-03 14:00 EET"
--
formatHumanHelsinkiTime :: UTCTime -> Text
formatHumanHelsinkiTime t = view packed $
    formatTime defaultTimeLocale "%Y-%m-%d %H:%M " (utcToLocalTimeTZ tz t)
    ++ timeZoneName (timeZoneForUTCTime tz t)
  where
    tz = helsinkiTz

-------------------------------------------------------------------------------
-- Show
-------------------------------------------------------------------------------

-- | Next from 'showsUnaryWith' and 'showsBinaryWith'.
--
-- @
-- liftShowsPrec sp sl d (Three x y z) =
--      showsTernaryWith sp (liftShowsPrec sp sl) showsPrec
--      "Three" d x y z
-- @
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

-- | Create a @stderr@ bulk logger, which queue has at most 1000 messages
-- and pruned every 100ms. We should really log that much, ever.
--
-- Yet the limits are quite low to prevent non-drained @stderr@ to cause
-- space leak in the "to-be-logged" queue of log-messages.
--
mkStderrLogger :: IO Logger
mkStderrLogger = mkBulkLogger' 1000 100000 "ansi-stderr" (traverse_ log') (hFlush stderr)
  where
    log' lm@LogMessage { lmMessage = msg, lmData = data_ } = do
        -- Split multiline log messages, e.g. exception dumps
        let pfx = prefix lm
        for_ (T.lines msg) $ \l -> do
            T.hPutStr stderr pfx
            T.hPutStrLn stderr l

        -- JSON value
        when (data_ /= Aeson.emptyObject && data_ /= Aeson.Null) $ do
            let doc = prettiestJSON data_
            T.hPutStrLn stderr $ view strict $ TB.toLazyText $
                PC.renderWith pcOpts doc

        -- flush at the end
        hFlush stderr

    prefix :: LogMessage -> Text
    prefix LogMessage {..} = view strict $ TB.toLazyText $ execWriter $ do
        time lmTime
        tell $ TB.singleton ' '
        level lmLevel
        tell $ TB.singleton ' '
        component
        tell $ TB.singleton ' '
      where
        component = withColour ANSI.Magenta $ tell $
            TB.fromText $ T.justifyLeft 25 ' ' $
                T.intercalate "/" $ lmComponent : lmDomain

    level :: LogLevel -> WB ()
    level LogAttention = withColour ANSI.Red   $ tell "ERR"
    level LogInfo      = withColour ANSI.Green $ tell "INF"
    level LogTrace     = withColour ANSI.Cyan  $ tell "TRC"

    time :: UTCTime -> WB ()
    time t = withColour ANSI.Blue $
        tell $  TB.fromString $ formatTime defaultTimeLocale "%F %T" t

    withColour :: ANSI.Color -> WB () -> WB ()
    withColour c m = do
        tell $ TB.fromString $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid c]
        m
        tell $ TB.fromString $ ANSI.setSGRCode []

    pcOpts :: PC.Options [ANSI.SGR] TB.Builder
    pcOpts = PC.defaultOptions
        { PC.optsAnnotate = \sgr s -> TB.fromString $
            ANSI.setSGRCode sgr ++ s ++ ANSI.setSGRCode []
        , PC.optsPageWidth = 120
        }

type WB = Writer TB.Builder

-- |
--
-- >>> putStrLn $ PC.render $ prettiestJSON $ Aeson.Bool True
-- true
--
-- >>> let render = PC.renderWith PC.defaultOptions { PC.optsPageWidth = 20 }
-- >>> putStrLn $ render $ prettiestJSON $ Aeson.Array $ V.replicate 4 "foobar"
-- ["foobar"
-- ,"foobar"
-- ,"foobar"
-- ,"foobar"]
--
prettiestJSON :: Value -> PC.Doc [ANSI.SGR]
prettiestJSON = go where
    go (Aeson.Bool True)  = dullyellow $ PC.text "true"
    go (Aeson.Bool False) = dullyellow $ PC.text "false"
    go (Aeson.Object o)   =
          PC.encloseSep (dullgreen PC.lbrace) (dullgreen PC.rbrace) (dullgreen PC.comma) $
              map prettyKV $ HM.toList o
    go (Aeson.String s)   = PC.string (show s)
    go (Aeson.Array a)    =
        PC.encloseSep (dullgreen PC.lbracket) (dullgreen PC.rbracket) (dullgreen PC.comma) $
            map go $ toList a
    go Aeson.Null         = cyan (PC.text "null")
    go (Aeson.Number n)   = PC.text (show n)

    prettyKV (k,v)        = dullwhite (PC.text (show k)) PC.<> blue PC.colon PC.<+> go v

    blue       = PC.annotate [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
    cyan       = PC.annotate [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan]
    dullgreen  = PC.annotate [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
    dullwhite  = PC.annotate [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
    dullyellow = PC.annotate [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow]

{-
    vectorOfText :: Vector Value -> Maybe (Vector Text)
    vectorOfText = traverse $ \x -> case x of
        Aeson.String s | T.length s < 10 -> Just s
        _                                -> Nothing
-}

-- | We often need logger in small scripts:
--
-- @
-- main = withStderrLogger logger $ \logger -> do
--     ....
-- @
withStderrLogger :: (Logger -> IO r) -> IO r
withStderrLogger act = do
    logger <- mkStderrLogger
    withLogger logger act

-- | Renamed 'Log.localData'.
logLocalData :: MonadLog m => [AesonPair] -> m a -> m a
logLocalData = localData

-- | Renamed 'Log.localDomain'.
logLocalDomain :: MonadLog m => Text -> m a -> m a
logLocalDomain = localDomain

-------------------------------------------------------------------------------
-- Text
-------------------------------------------------------------------------------

decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TE.decodeUtf8With TE.lenientDecode

-- | @toLower . stripSpecials . normalize NFK@
--
-- >>> canonicalize "\196ITI" -- ÄITI
-- "aiti"
--
-- >>> canonicalize "олег" -- oleg
-- ""
--
canonicalize :: Text -> Text
canonicalize = T.toLower . T.filter p . TN.normalize TN.NFKD
  where
    p c = fromEnum c < 128

-------------------------------------------------------------------------------
-- Vector
-------------------------------------------------------------------------------

introsortBy :: (a -> a -> Ordering) -> Vector a -> Vector a
introsortBy cmp = V.modify (Intro.sortBy cmp)

-- | Sort vector using intro-sort
--
-- >>> introsort (V.fromList ("foobar" :: String))
-- "abfoor"
introsort :: Ord a => Vector a -> Vector a
introsort = introsortBy compare

-------------------------------------------------------------------------------
-- Logging response
-------------------------------------------------------------------------------

-- | A newtype to allow logging 'H.Response' (it has 'ToJSON' instance).
newtype WrappedResponse = WrapResponse
    { unwrapResponse :: H.Response LazyByteString
    }
  deriving Show

instance Aeson.ToJSON WrappedResponse where
    toJSON (WrapResponse r) = Aeson.object
        [ "status"  Aeson..= H.statusCode (H.responseStatus r)
        , "message" Aeson..= TE.decodeLatin1 (H.statusMessage (H.responseStatus r))
        , "headers" Aeson..= (headerToJSON <$> H.responseHeaders r)
        , "body"    Aeson..= decodeUtf8Lenient (LBS.take 500 (H.responseBody r) ^. strict)
        ]
      where
        headerToJSON (k, v) = (CI.map TE.decodeLatin1 k, TE.decodeLatin1 v)

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- |
--
-- @
-- myX :: X
-- myX = $(makeRelativeToProject "x.json" >>= embedFromJSON (Proxy :: Proxy X))
-- @
embedFromJSON :: forall a. (Aeson.FromJSON a, TH.Lift a) => Proxy a -> FilePath -> TH.Q TH.Exp
embedFromJSON _ fp = do
    TH.qAddDependentFile fp
    bs <- TH.runIO (LBS.readFile fp)
    x <- Aeson.decode bs :: TH.Q a
    TH.lift x

-------------------------------------------------------------------------------
-- Debug.Trace
-------------------------------------------------------------------------------

-- | The function is not referentially transparent: its type indicates that it
-- is a pure function but it has the side effect of outputting the trace
-- message.
--
-- See 'Debug.Trace.traceShow'.
traceShow :: Show a => a -> b -> b
traceShow = DT.traceShow
{-# DEPRECATED traceShow "Don't leave me in the code" #-}

-- | The function is not referentially transparent: its type indicates that it
-- is a pure function but it has the side effect of outputting the trace
-- message.
--
-- See 'Debug.Trace.traceShowId'.
traceShowId :: Show a => a -> a
traceShowId = DT.traceShowId
{-# DEPRECATED traceShowId "Don't leave me in the code" #-}

-------------------------------------------------------------------------------
-- Doctests
-------------------------------------------------------------------------------

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XTypeOperators
-- >>> import Data.Char (toUpper, isLetter, isSpace)
-- >>> import Data.Type.Equality
-- >>> import qualified Data.Vector as V
