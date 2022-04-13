{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
#if __GLASGOW_HASKELL__ >= 800
-- a) THQ works on cross-compilers and unregisterised GHCs
-- b) may make compilation faster as no dynamic loading is ever needed (not sure about this)
-- c) removes one hindrance to have code inferred as SafeHaskell safe
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell       #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module defines some orphan instances for types and classes from
-- packages "Futurice.Prelude" depends upon.
--
-- TODO: Split into submodules
module Futurice.Orphans () where

import Futurice.Prelude.Internal
import Prelude ()

import Futurice.UUID

import Data.Binary.Orphans ()
import Data.Hashable.Time ()
import Data.Orphans ()
import Data.Vector.Instances ()
import Instances.TH.Lift ()
import System.Console.ANSI ()
import Test.QuickCheck.Instances ()
import Text.Parsec.String ()
import Text.Trifecta ()

import Codec.Picture                         (DynamicImage, Image, PixelRGBA8)
import Control.Monad.Trans.Resource          (MonadResource (..))
import Control.Monad.Trans.Resource.Internal (ResourceT (..))
import Control.Monad.Trans.State             (StateT)
import Data.Aeson.Compat
       (FromJSON (..), Parser, ToJSON (..), object, withArray, withObject, (.:),
       (.=))
import Data.Aeson.Types
       (FromJSON1 (..), FromJSONKey (..), FromJSONKeyFunction (..),
       ToJSON1 (..), ToJSONKey (..), coerceFromJSONKeyFunction,
       contramapToJSONKeyFunction, parseJSON1, toEncoding1, toJSON1)
#if MIN_VERSION_aeson(2,0,0)
import Control.Lens.At                       (Index, IxValue, At(..), Ixed(..))
import Data.Aeson.KeyMap                     (KeyMap)
import Data.Aeson.Key                        (Key)
import qualified Data.Aeson.KeyMap        as KM
#endif
import Data.Binary.Tagged                    (Structured)
import Data.Fixed                            (Fixed (..), HasResolution)
import Data.Time.Parsers                     (day, utcTime)
import Data.Kind (Type)
import Futurice.Control
import Generics.SOP                          (All)
import Lucid                                 (HtmlT, ToHtml (..), a_, href_)
import Numeric.Interval                      (Interval, inf, sup)
import System.Random                         (Random (..))
import Test.QuickCheck                       (Arbitrary (..))
import Text.Parsec                           (parse)

import qualified Data.Aeson.Encoding        as Aeson
import qualified Data.CaseInsensitive       as CI
import qualified Data.Csv                   as Csv
import qualified Data.Fixed                 as Fixed
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.Map                   as Map
import qualified Data.Scientific            as Scientific
import qualified Data.Strict.Tuple          as S
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Encoding.Error   as TE
import qualified Data.UUID.Types            as UUID
import qualified Data.Vector                as V
import qualified GHC.Exts                   as Exts
import qualified Generics.SOP               as SOP
import qualified Network.HTTP.Types.Status  as HTTP
import qualified Numeric.Interval.Kaucher   as Kaucher
import qualified Numeric.Interval.NonEmpty  as NonEmpty
import qualified System.Clock               as Clock

#ifndef __GHCJS__
import Control.Monad.CryptoRandom
       (CRandT (..), CRandom (..), MonadCRandom (..), runCRand)
import Data.Swagger               (NamedSchema (..), ToSchema (..))

import qualified Crypto.Random.DRBG.Hash              as DRBG
import qualified Data.Swagger                         as Swagger
import qualified Data.Swagger.Declare                 as Swagger
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.FromRow   as PG
import qualified Database.PostgreSQL.Simple.ToField   as PG
import qualified Database.PostgreSQL.Simple.ToRow     as PG
import qualified GitHub                               as GH
import qualified GitHub.Data.Name                     as GH
import qualified Network.HTTP.Client                  as HTTP
import qualified Network.Wai                          as Wai
import qualified Servant.Server                       as Servant
#endif

#ifndef  __GHCJS__
-- | Defined in 'Futurice.Prelude'.
instance MonadTransControl (CRandT g e) where
    type StT (CRandT g e) a = StT (ExceptT e) (StT (StateT g) a)
    liftWith f = defaultLiftWith2 CRandT unCRandT $ \x -> f x
    restoreT = defaultRestoreT2 CRandT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}
#endif

-- | Defined in 'Futurice.Prelude'.
--
-- <https://github.com/ekmett/intervals/issues/40>
instance Hashable a => Hashable (Interval a)
instance Hashable a => Hashable (Kaucher.Interval a)
instance Hashable a => Hashable (NonEmpty.Interval a)

-- | Defined in 'Futurice.Prelude'.
instance Hashable a => Hashable (I a)

-- | Defined in 'Futurice.Prelude'.
instance Hashable a => Hashable (K a b)

-- | Defined in 'Futurice.Prelude'.
instance NFData a => NFData (Interval a) where
    rnf a = rnf (sup a) `seq` rnf (inf a)
instance NFData a => NFData (Kaucher.Interval a) where
    rnf a = rnf (Kaucher.sup a) `seq` rnf (Kaucher.inf a)
instance NFData a => NFData (NonEmpty.Interval a) where
    rnf a = rnf (NonEmpty.sup a) `seq` rnf (NonEmpty.inf a)

-------------------------------------------------------------------------------
-- TypeableAt(..)At(..)
-------------------------------------------------------------------------------

deriving instance Typeable Image
deriving instance Typeable PixelRGBA8

-- | Defined in 'Futurice.Prelude'.
--
-- <https://github.com/Twinside/Juicy.Pixels/pull/126>
deriving instance Typeable DynamicImage

-------------------------------------------------------------------------------
-- QuickCheck
-------------------------------------------------------------------------------

#ifndef  __GHCJS__
instance Arbitrary (GH.Name entity) where
    arbitrary = GH.mkName Proxy <$> arbitrary -- TODO: maybe use some alphabet?
#endif

-------------------------------------------------------------------------------
-- Postgres
-------------------------------------------------------------------------------

#ifndef __GHCJS__
instance PG.FromField (GH.Name entity) where
    fromField f mbs = GH.N <$> PG.fromField f mbs

instance PG.ToField (GH.Name entity) where
    toField = PG.toField . GH.untagName
#endif

-------------------------------------------------------------------------------
-- cassava
-------------------------------------------------------------------------------

instance Csv.ToField Bool where
    toField True  = "true"
    toField False = "false"

instance Csv.FromField Bool where
    parseField "true"  = pure True
    parseField "false" = pure False
    parseField s       = fail $ "not a bool " ++ show s

instance Csv.ToField Day where
    toField = fromString . show

instance Csv.FromField Day where
    parseField s = either (fail . show) return $
        parse day "FromField Day" s

instance Csv.ToField UTCTime where
    toField = fromString . show

instance Csv.FromField UTCTime where
    parseField s = either (fail . show) return $
        parse utcTime "FromField UTCTime" s

-- | TODO: this instance is suspicious!
instance Csv.ToField (Map k v) where
    toField _ = "{}"

instance Csv.ToField UUID.UUID where
    toField = UUID.toASCIIBytes

instance Csv.FromField UUID.UUID where
    parseField = maybe (fail "invalid UUID") pure . UUID.fromASCIIBytes

instance HasResolution a => Csv.ToField (Fixed a) where
    toField = Csv.toField . Fixed.showFixed False

instance HasResolution a => Csv.FromField (Fixed a) where
    parseField bs = fromRational . scientificToSmallRational <$> Csv.parseField bs

scientificToSmallRational :: Scientific -> Rational
scientificToSmallRational s
    | c >= -100 && c < 100 = toRational s
    | otherwise            = 0
  where
    c = Scientific.coefficient s

instance All Csv.ToField xs => Csv.ToRecord (NP I xs) where
    toRecord
        = V.fromList
        . SOP.hcollapse
        . SOP.hcmap (Proxy :: Proxy Csv.ToField) (K . Csv.toField . unI)

instance All Csv.ToField xs => Csv.ToRecord (NP Maybe xs) where
    toRecord
        = V.fromList
        . SOP.hcollapse
        . SOP.hcmap (Proxy :: Proxy Csv.ToField) (K . maybe "" Csv.toField)

#ifndef  __GHCJS__
instance Csv.ToField (GH.Name a) where
    toField = Csv.toField . GH.untagName
#endif

-------------------------------------------------------------------------------
-- Swagger schemas
-------------------------------------------------------------------------------

#ifndef __GHCJS__

-- | /TODO:/ this is partly incorrect instance
instance ToSchema Value where
    declareNamedSchema _ = pure $ NamedSchema (Just "JSON Value") s
      where
        s = mempty

instance ToSchema (f (g a)) => ToSchema (Compose f g a) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (f (g a)))

instance ToSchema (GH.Name a) where
    declareNamedSchema _ = pure $ NamedSchema (Just "Github name") mempty

instance ToSchema GH.Language

instance ToSchema DynamicImage where
    declareNamedSchema _ = pure $ NamedSchema (Just "Image") mempty

instance ToSchema (Image a) where
    declareNamedSchema _ = pure $ NamedSchema (Just "Image") mempty

instance ToSchema1 NonEmpty.Interval where
    liftDeclareNamedSchema _ ns = do
        ref <- namedSchemaToRef ns
        pure $ NamedSchema (Just "NonEmpty.Interval") $ schema ref
      where
        schema s = mempty
          & Swagger.type_       .~ Just Swagger.SwaggerObject
          & Swagger.properties  .~ Exts.fromList
              [ ("inf", s)
              , ("sup", s)
              ]
          & Swagger.required    .~ ["sup", "inf"]

instance ToSchema a => ToSchema (NonEmpty.Interval a) where
    declareNamedSchema = declareNamedSchema1

class ToSchema1 (f :: Type -> Type) where
    liftDeclareNamedSchema
        :: proxy f
        -> NamedSchema   -- ^ schema of the element
        -> Swagger.Declare (Swagger.Definitions Swagger.Schema) NamedSchema

namedSchemaToRef
    :: NamedSchema
    -> Swagger.Declare (Swagger.Definitions Swagger.Schema) (Swagger.Referenced Swagger.Schema)
namedSchemaToRef (NamedSchema (Just name) schema) = do
    -- From 'declareSchemaRef'
    known <- Swagger.looks (InsOrdHashMap.member name)
    when (not known) $ do
        Swagger.declare $ InsOrdHashMap.fromList [(name, schema)]
    return $ Swagger.Ref (Swagger.Reference name)
namedSchemaToRef (NamedSchema Nothing schema) = pure $ Swagger.Inline schema

declareNamedSchema1
    :: forall f a proxy. (ToSchema1 f, ToSchema a)
    => proxy (f a)
    -> Swagger.Declare (Swagger.Definitions Swagger.Schema) NamedSchema
declareNamedSchema1 _ = do
    schema <- Swagger.declareNamedSchema (Proxy :: Proxy a)
    liftDeclareNamedSchema (Proxy :: Proxy f) schema

instance ToSchema a => ToSchema (I a)
instance ToSchema1 I where
    liftDeclareNamedSchema _ = pure

instance (ToSchema a, ToSchema b) => ToSchema (S.Pair a b)

instance (ToSchema a, ToSchema b) => ToSchema (These a b) where
    declareNamedSchema _ = do
        aSchema <- Swagger.declareSchemaRef (Proxy :: Proxy a)
        bSchema <- Swagger.declareSchemaRef (Proxy :: Proxy b)
        return $ NamedSchema (Just "These") $ mempty
            & Swagger.type_ .~ Just Swagger.SwaggerObject
            & Swagger.properties .~ InsOrdHashMap.fromList
                [ ("This", aSchema)
                , ("That", bSchema)
                ]
            -- At least 1 property, but both can be present!
            & Swagger.maxProperties ?~ 2
            & Swagger.minProperties ?~ 1

#endif

-------------------------------------------------------------------------------
-- NFData
-------------------------------------------------------------------------------

#if !MIN_VERSION_deepseq(1,4,3)
-- | https://github.com/haskell/deepseq/issues/31
instance NFData (a :~: b) where
    rnf Refl = ()
#endif

instance NFData HTTP.Status where
    rnf (HTTP.Status c m) = rnf c `seq` rnf m

#ifndef  __GHCJS__
instance NFData a => NFData (HTTP.Response a) where
    rnf res =
        rnf (HTTP.responseStatus res) `seq`
        HTTP.responseVersion res `seq`
        rnf (HTTP.responseHeaders res) `seq`
        rnf (HTTP.responseBody res) `seq`
        HTTP.responseCookieJar res `seq`  -- no NFData
        ()
#endif

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

#if MIN_VERSION_aeson(2,0,0)
type instance Index (KeyMap a) = Key
type instance IxValue (KeyMap a) = a

instance Ixed (KeyMap a) where
  ix k f m = case KM.lookup k m of
    Just v -> f v <&> \v' -> KM.insert k v' m
    Nothing -> pure m
  {-# INLINE ix #-}

instance At (KeyMap a) where
  at k f = KM.alterF f k
  {-# INLINE at #-}
#endif

-- TODO: ToJSON1 ?
instance ToJSON a => ToJSON (NonEmpty.Interval a) where
    toJSON i = object [ "inf" .= NonEmpty.inf i, "sup" .= NonEmpty.sup i ]

instance (Ord a, FromJSON a) => FromJSON (NonEmpty.Interval a) where
    parseJSON = withObject "NonEmpty.Interval" $ \obj -> (NonEmpty....)
        <$> obj .: "inf"
        <*> obj .: "sup"

#ifndef  __GHCJS__
-- | This istance is used in logging
instance ToJSON Wai.Request where
    toJSON r = object
        [ "method"         .= decodeUtf8Lenient (Wai.requestMethod r)
        , "rawPathInfo"    .= decodeUtf8Lenient (Wai.rawPathInfo r)
        , "rawQueryString" .= decodeUtf8Lenient (Wai.rawQueryString r)
        , "headers"        .= headers
        ]
      where
        headers
            = Map.fromList
            $ map (bimap (CI.map decodeUtf8Lenient) decodeUtf8Lenient)
            -- we filter headers
            . filter (goodHeader  . fst)
            $ Wai.requestHeaders r

        goodHeader h = notElem h ["authorization","cookie"]
        -- elem h ["Accept", "Content-Type", "remote-user"]

#endif

instance ToJSON a => ToJSON (CI.CI a) where
    toJSON     = toJSON . CI.foldedCase
    toEncoding = toEncoding . CI.foldedCase

instance ToJSONKey a => ToJSONKey (CI.CI a) where
    toJSONKey = contramapToJSONKeyFunction CI.foldedCase toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (map CI.foldedCase) toJSONKeyList

instance ToJSON Clock.TimeSpec
instance FromJSON Clock.TimeSpec

-------------------------------------------------------------------------------
-- Lucid
-------------------------------------------------------------------------------

#ifndef  __GHCJS__
class GHNameToHtml e where
    ghNameToHtml :: Monad m => GH.Name e -> HtmlT m ()

instance GHNameToHtml GH.User where
    ghNameToHtml n = a_ [ href_ $ "https://github.com/" <> n' ] $ toHtml n'
      where
        n' = GH.untagName n

instance GHNameToHtml e => ToHtml (GH.Name e) where
    toHtml    = ghNameToHtml
    toHtmlRaw = ghNameToHtml

instance ToSchema (HtmlT m a) where
    declareNamedSchema _ = pure $ NamedSchema (Just $ "some html") mempty
#endif

-------------------------------------------------------------------------------
-- aeson + generics-sop
-------------------------------------------------------------------------------

instance FromJSON1 I where
    liftParseJSON p _ a = I <$> p a
    {-# INLINE liftParseJSON #-}

    liftParseJSONList _ p a = fmap I <$> p a
    {-# INLINE liftParseJSONList #-}

instance (FromJSON a) => FromJSON (I a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

    parseJSONList = liftParseJSONList parseJSON parseJSONList
    {-# INLINE parseJSONList #-}

instance (FromJSONKey a) => FromJSONKey (I a) where
    fromJSONKey = coerceFromJSONKeyFunction (fromJSONKey :: FromJSONKeyFunction a)
    fromJSONKeyList = coerceFromJSONKeyFunction (fromJSONKeyList :: FromJSONKeyFunction [a])


instance ToJSON1 I where
    liftToJSON t _ (I a) = t a
    {-# INLINE liftToJSON #-}

    liftToJSONList _ tl xs = tl (map unI xs)
    {-# INLINE liftToJSONList #-}

    liftToEncoding t _ (I a) = t a
    {-# INLINE liftToEncoding #-}

    liftToEncodingList _ tl xs = tl (map unI xs)
    {-# INLINE liftToEncodingList #-}

instance (ToJSON a) => ToJSON (I a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toJSONList = liftToJSONList toJSON toJSONList
    {-# INLINE toJSONList #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

    toEncodingList = liftToEncodingList toEncoding toEncodingList
    {-# INLINE toEncodingList #-}

instance (ToJSONKey a) => ToJSONKey (I a) where
    toJSONKey = contramapToJSONKeyFunction unI toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (map unI) toJSONKeyList

-- NP

instance (ToJSON1 f, All ToJSON xs) => ToJSON (NP f xs) where
    toJSON
        = toJSON
        . SOP.hcollapse
        . SOP.hcmap (Proxy :: Proxy ToJSON) (K . toJSON1)

    toEncoding
        = Aeson.list id
        . SOP.hcollapse
        . SOP.hcmap (Proxy :: Proxy ToJSON) (K . toEncoding1)

instance (FromJSON1 f, All FromJSON xs) => FromJSON (NP f xs) where
    parseJSON = withArray "NP f xs" $ \arr -> case SOP.fromList (toList arr) of
        Nothing -> fail "Invalid dimension"
        Just np -> SOP.hsequence' (SOP.hcmap (Proxy :: Proxy FromJSON) f np)
      where
        f :: FromJSON a => K Value a -> (Parser SOP.:.: f) a
        f (K v) = SOP.Comp $ parseJSON1 v

-------------------------------------------------------------------------------
-- NFData for CryptoGen
-------------------------------------------------------------------------------

#ifndef __GHCJS__
instance NFData (DRBG.State a) where
    rnf s = s `seq` ()
#endif

-------------------------------------------------------------------------------
-- CRandom
-------------------------------------------------------------------------------

#ifndef __GHCJS__
instance (CRandom a, CRandom b) => CRandom (a, b) where
    crandom = runCRand $ do
        a <- getCRandom
        b <- getCRandom
        return (a, b)

instance (CRandom a, CRandom b, CRandom c) => CRandom (a, b, c) where
    crandom = runCRand $ do
        a <- getCRandom
        b <- getCRandom
        c <- getCRandom
        return (a, b, c)

instance (CRandom a, CRandom b, CRandom c, CRandom d) => CRandom (a, b, c, d) where
    crandom = runCRand $ do
        a <- getCRandom
        b <- getCRandom
        c <- getCRandom
        d <- getCRandom
        return (a, b, c, d)

instance CRandom UUID.UUID where
    crandom = runCRand $ view (from uuidWords) <$> getCRandom
#endif

-------------------------------------------------------------------------------
-- Random
-------------------------------------------------------------------------------

instance Random (Fixed a) where
    random g = first MkFixed $ random g
    randomR (MkFixed a, MkFixed b) g = first MkFixed $ randomR (a, b) g

-------------------------------------------------------------------------------
-- Binary
-------------------------------------------------------------------------------

instance (Binary a, Ord a) => Binary (NonEmpty.Interval a) where
    put i = put (NonEmpty.inf i) >> put (NonEmpty.sup i)
    get = (NonEmpty....) <$> get <*> get

#ifndef __GHCJS__
instance Binary (GH.GenRequest mt rw a) where
    get = undefined
    put (GH.Query ps qs) =
        put (0 :: Int) >> put ps >> put qs
    put (GH.PagedQuery ps qs c) =
        put (1 :: Int) >> put ps >> put qs >> put c
    put (GH.Command m ps bs) =
        put (2 :: Int) >> put m >> put ps >> put bs

instance Binary GH.CommandMethod where
    get = undefined
    put GH.Post   = put (0 :: Int)
    put GH.Patch  = put (1 :: Int)
    put GH.Put    = put (2 :: Int)
    -- skip number for just-in-case backward compatibility. May not be needed?
    put GH.Delete = put (4 :: Int)
#endif

-------------------------------------------------------------------------------
-- binary-tagged
-------------------------------------------------------------------------------

#ifndef __GHCJS__
instance Structured GH.Event
instance Structured GH.Invitation
instance Structured GH.InvitationRole
instance Structured GH.Issue
instance Structured GH.IssueLabel
instance Structured GH.IssueNumber
instance Structured GH.IssueState
instance Structured GH.Language
instance Structured GH.Milestone
instance Structured GH.Organization
instance Structured GH.Owner
instance Structured GH.OwnerType
instance Structured GH.Permission
instance Structured GH.Privacy
instance Structured GH.PullRequestReference
instance Structured GH.Repo
instance Structured GH.RepoRef
instance Structured GH.SimpleOrganization
instance Structured GH.SimpleOwner
instance Structured GH.SimpleTeam
instance Structured GH.SimpleUser
instance Structured GH.User
instance Structured GH.Team

instance (Typeable a) => Structured (GH.Name a)
instance (Typeable a) => Structured (GH.Id a)
instance Structured GH.URL
#endif

instance Structured a => Structured (Interval a)
instance Structured a => Structured (NonEmpty.Interval a)
instance Structured a => Structured (Kaucher.Interval a)

decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TE.decodeUtf8With TE.lenientDecode

-------------------------------------------------------------------------------
-- monad-time
-------------------------------------------------------------------------------

#ifndef __GHCJS__
instance MonadTime Servant.Handler where
    currentTime = liftIO currentTime
#endif

-------------------------------------------------------------------------------
-- postgresql-simple + NP
-------------------------------------------------------------------------------

#ifndef __GHCJS__
instance (All PG.ToField xs, f ~ I) => PG.ToRow (NP f xs) where
    toRow = SOP.hcollapse . SOP.hcmap (Proxy :: Proxy PG.ToField) (K . PG.toField . unI)

instance (All PG.FromField xs, f ~ I) => PG.FromRow (NP f xs) where
    fromRow = SOP.hsequence $ SOP.hcpure (Proxy :: Proxy PG.FromField) PG.field
#endif

-------------------------------------------------------------------------------
-- text-short
-------------------------------------------------------------------------------

instance ToSchema ShortText where
    declareNamedSchema  _ = declareNamedSchema (Proxy  :: Proxy Text)

-------------------------------------------------------------------------------
-- resourcet + unliftio-core
-------------------------------------------------------------------------------

instance MonadResource m => MonadResource (LogT m) where
    liftResourceT = lift . liftResourceT

#if MIN_VERSION_resourcet(1,2,0)
instance MonadBase b m => MonadBase b (ResourceT m) where
    liftBase = lift . liftBase

-- assuming monad-control ^>= 1.0.1.0
-- copied from resourcet-1.1.11
instance MonadTransControl ResourceT where
    type StT ResourceT a = a
    liftWith f = ResourceT $ \r -> f $ \(ResourceT t) -> t r
    restoreT = ResourceT . const

instance MonadBaseControl b m => MonadBaseControl b (ResourceT m) where
     type StM (ResourceT m) a = StM m a
     liftBaseWith f = ResourceT $ \reader' ->
         liftBaseWith $ \runInBase ->
             f $ runInBase . (\(ResourceT r) -> r reader'  )
     restoreM = ResourceT . const . restoreM
#endif

-- $setup
--
-- >>> import Data.Aeson
-- >>> import Data.Foldable (traverse_)
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS8
-- >>> import Servant.Client.Core (BaseUrl, parseBaseUrl)
