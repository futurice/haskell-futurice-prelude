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
    -- * @log@
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
    -- * Time
    currentDay,
    currentMonth,
    firstDayOfMonth,
    lastDayOfMonth,
    formatHumanHelsinkiTime,
    utcToHelsinkiTime,
    helsinkiTz,
    -- * Misc extras
    type (:$),
    mcase,
    textShow,
    swapMapMap,
    showsTernaryWith,
    ) where

import Prelude ()
import Futurice.Prelude.Internal

import Control.Concurrent.Async (waitCatch, withAsync)
import Control.Lens             (_Wrapped)
import Control.Lens             (ifoldMapOf, (<.>))
import Data.Time                (defaultTimeLocale, formatTime, timeZoneName)
import Data.Time.Zones          (timeZoneForUTCTime, utcToLocalTimeTZ)
import Data.Time.Zones.TH       (includeTZFromDB)
import Futurice.Time.Month
import Log
       (LogLevel (..), LogMessage (..), localData, localDomain, mkBulkLogger)
import Log.Internal.Logger      (withLogger)
import System.IO                (hFlush, stderr)

import qualified Data.Aeson.Types                        as Aeson
import qualified Data.Map                                as Map
import qualified Data.Text                               as T
import qualified Data.Text.Encoding                      as TE
import qualified Data.Text.Encoding.Error                as TE
import qualified Data.Text.IO                            as T
import qualified System.Console.ANSI                     as ANSI
import qualified Text.PrettyPrint.ANSI.Leijen.AnsiPretty as AnsiPretty

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

-------------------------------------------------------------------------------
-- Doctests
-------------------------------------------------------------------------------

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XTypeOperators
-- >>> import Data.Char (toUpper, isLetter, isSpace)
-- >>> import Data.Type.Equality
