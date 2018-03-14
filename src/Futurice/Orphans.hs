{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
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

import Codec.Picture              (DynamicImage, Image, PixelRGBA8)
import Control.Monad.CryptoRandom
       (CRandT (..), CRandom (..), MonadCRandom (..), runCRand)
import Control.Monad.Trans.State  (StateT)
import Data.Aeson.Compat
       (FromJSON (..), Parser, ToJSON (..), object, withArray, withObject,
       (.:), (.=))
import Data.Aeson.Types
       (FromJSON1 (..), FromJSONKey (..), FromJSONKeyFunction, ToJSON1 (..),
       ToJSONKey (..), ToJSONKeyFunction, coerceFromJSONKeyFunction,
       contramapToJSONKeyFunction, parseJSON1, toEncoding1, toJSON1)
import Data.Binary.Tagged
       (HasSemanticVersion, HasStructuralInfo (..), StructuralInfo (..))
import Data.Fixed                 (Fixed (..), HasResolution)
import Data.Swagger               (NamedSchema (..), ToSchema (..))
import Data.Time.Parsers          (day, utcTime)
import Futurice.Control
import Generics.SOP               (All)
import Lucid                      (HtmlT, ToHtml (..), a_, href_)
import Numeric.Interval           (Interval, inf, sup)
import System.Random              (Random (..))
import Test.QuickCheck            (Arbitrary (..))
import Text.Parsec                (parse)

import qualified Crypto.Random.DRBG.Hash              as DRBG
import qualified Data.Aeson.Encoding                  as Aeson
import qualified Data.CaseInsensitive                 as CI
import qualified Data.Csv                             as Csv
import qualified Data.Fixed                           as Fixed
import qualified Data.HashMap.Strict.InsOrd           as InsOrdHashMap
import qualified Data.Map                             as Map
import qualified Data.Scientific                      as Scientific
import qualified Data.Swagger                         as Swagger
import qualified Data.Swagger.Declare                 as Swagger
import qualified Data.Text.Encoding                   as TE
import qualified Data.Text.Encoding.Error             as TE
import qualified Data.Tuple.Strict                    as S
import qualified Data.UUID                            as UUID
import qualified Data.Vector                          as V
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import qualified Generics.SOP                         as SOP
import qualified GHC.Exts                             as Exts
import qualified GitHub                               as GH
import qualified GitHub.Data.Name                     as GH
import qualified Language.Haskell.TH.Syntax           as TH
import qualified Network.HTTP.Client                  as HTTP
import qualified Network.HTTP.Types.Status            as HTTP
import qualified Network.Wai                          as Wai
import qualified Numeric.Interval.Kaucher             as Kaucher
import qualified Numeric.Interval.NonEmpty            as NonEmpty
import qualified Servant.Server                       as Servant
import qualified System.Clock                         as Clock

#if !MIN_VERSION_transformers_compat(0,5,0)
import Data.Functor.Identity (Identity (..))
#endif

#if !MIN_VERSION_deepseq(1,4,3)
import Data.Type.Equality
#endif

-- | Defined in 'Futurice.Prelude'
--
-- TODO: move into own package
instance (Hashable k, Hashable v) => Hashable (Map k v) where
    hashWithSalt salt = hashWithSalt salt . Map.toList

-- | Defined in 'Futurice.Prelude'.
instance MonadTransControl (CRandT g e) where
    type StT (CRandT g e) a = StT (ExceptT e) (StT (StateT g) a)
    liftWith = defaultLiftWith2 CRandT unCRandT
    restoreT = defaultRestoreT2 CRandT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

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
-- Typeable
-------------------------------------------------------------------------------

deriving instance Typeable Image
deriving instance Typeable PixelRGBA8

-- | Defined in 'Futurice.Prelude'.
--
-- <https://github.com/Twinside/Juicy.Pixels/pull/126>
deriving instance Typeable DynamicImage

-------------------------------------------------------------------------------
-- ansi-pretty instances
-------------------------------------------------------------------------------

instance AnsiPretty (GH.Name entity)
instance AnsiPretty GH.Language

-------------------------------------------------------------------------------
-- QuickCheck
-------------------------------------------------------------------------------

instance Arbitrary (GH.Name entity) where
    arbitrary = GH.mkName Proxy <$> arbitrary -- TODO: maybe use some alphabet?

-------------------------------------------------------------------------------
-- Postgres
-------------------------------------------------------------------------------

instance Postgres.FromField (GH.Name entity) where
    fromField f mbs = GH.N <$> Postgres.fromField f mbs

instance Postgres.ToField (GH.Name entity) where
    toField = Postgres.toField . GH.untagName

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

instance Csv.ToField (GH.Name a) where
    toField = Csv.toField . GH.untagName

-------------------------------------------------------------------------------
-- Swagger schemas
-------------------------------------------------------------------------------

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
          & Swagger.type_       .~ Swagger.SwaggerObject
          & Swagger.properties  .~ Exts.fromList
              [ ("inf", s)
              , ("sup", s)
              ]
          & Swagger.required    .~ ["sup", "inf"]

instance ToSchema a => ToSchema (NonEmpty.Interval a) where
    declareNamedSchema = declareNamedSchema1

-- | https://github.com/GetShopTV/swagger2/issues/93
instance ToSchema a => ToSchema (NonEmpty a) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy [a])

class ToSchema1 (f :: * -> *) where
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
            & Swagger.type_ .~ Swagger.SwaggerObject
            & Swagger.properties .~ InsOrdHashMap.fromList
                [ ("This", aSchema)
                , ("That", bSchema)
                ]
            -- At least 1 property, but both can be present!
            & Swagger.maxProperties ?~ 2
            & Swagger.minProperties ?~ 1

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

instance NFData a => NFData (HTTP.Response a) where
    rnf res =
        rnf (HTTP.responseStatus res) `seq`
        HTTP.responseVersion res `seq`
        rnf (HTTP.responseHeaders res) `seq`
        rnf (HTTP.responseBody res) `seq`
        HTTP.responseCookieJar res `seq`  -- no NFData
        ()

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance Contravariant ToJSONKeyFunction where
    contramap = contramapToJSONKeyFunction

-- TODO: ToJSON1 ?
instance ToJSON a => ToJSON (NonEmpty.Interval a) where
    toJSON i = object [ "inf" .= NonEmpty.inf i, "sup" .= NonEmpty.sup i ]

instance (Ord a, FromJSON a) => FromJSON (NonEmpty.Interval a) where
    parseJSON = withObject "NonEmpty.Interval" $ \obj -> (NonEmpty....)
        <$> obj .: "inf"
        <*> obj .: "sup"

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

instance NFData (DRBG.State a) where
    rnf s = s `seq` ()

-------------------------------------------------------------------------------
-- CRandom
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Random
-------------------------------------------------------------------------------

instance Random (Fixed a) where
    random g = first MkFixed $ random g
    randomR (MkFixed a, MkFixed b) g = first MkFixed $ randomR (a, b) g

-------------------------------------------------------------------------------
-- template-haskell
-------------------------------------------------------------------------------

instance TH.Lift a => TH.Lift (NonEmpty a) where
    lift (x :| xs) = [| x :| xs |]

-------------------------------------------------------------------------------
-- Binary
-------------------------------------------------------------------------------

instance (Binary a, Ord a) => Binary (NonEmpty.Interval a) where
    put i = put (NonEmpty.inf i) >> put (NonEmpty.sup i)
    get = (NonEmpty....) <$> get <*> get

instance Binary a => Binary (GH.Request k a) where
    get = undefined

    put (GH.SimpleQuery r)    =
        put (0 :: Int) >> put r
    put (GH.StatusQuery sm r) =
        put (1 :: Int) >> put sm >> put r
    put (GH.HeaderQuery hs r) =
        put (2 :: Int) >> put hs >> put r
    put (GH.RedirectQuery r) =
        put (3 :: Int) >> put r

instance Binary (GH.SimpleRequest k a) where
    get = undefined
    put (GH.Query ps qs) =
        put (0 :: Int) >> put ps >> put qs
    put (GH.PagedQuery ps qs c) =
        put (1 :: Int) >> put ps >> put qs >> put c
    put (GH.Command m ps bs) =
        put (2 :: Int) >> put m >> put ps >> put bs

instance Binary (GH.CommandMethod a) where
    get = undefined
    put GH.Post   = put (0 :: Int)
    put GH.Patch  = put (1 :: Int)
    put GH.Put    = put (2 :: Int)
    put GH.Put'   = put (3 :: Int)
    put GH.Delete = put (4 :: Int)

-------------------------------------------------------------------------------
-- binary-tagged
-------------------------------------------------------------------------------

instance HasStructuralInfo GH.Event
instance HasStructuralInfo GH.Invitation
instance HasStructuralInfo GH.InvitationRole
instance HasStructuralInfo GH.Issue
instance HasStructuralInfo GH.IssueLabel
instance HasStructuralInfo GH.IssueState
instance HasStructuralInfo GH.Language
instance HasStructuralInfo GH.Milestone
instance HasStructuralInfo GH.Organization
instance HasStructuralInfo GH.Owner
instance HasStructuralInfo GH.OwnerType
instance HasStructuralInfo GH.Permission
instance HasStructuralInfo GH.Privacy
instance HasStructuralInfo GH.PullRequestReference
instance HasStructuralInfo GH.Repo
instance HasStructuralInfo GH.RepoRef
instance HasStructuralInfo GH.SimpleOrganization
instance HasStructuralInfo GH.SimpleOwner
instance HasStructuralInfo GH.SimpleTeam
instance HasStructuralInfo GH.SimpleUser
instance HasStructuralInfo GH.User
instance HasStructuralInfo GH.Team

instance HasStructuralInfo (GH.Name a)
instance HasStructuralInfo (GH.Id a)
instance HasStructuralInfo GH.URL

instance HasSemanticVersion GH.Event
instance HasSemanticVersion GH.Invitation
instance HasSemanticVersion GH.InvitationRole
instance HasSemanticVersion GH.Issue
instance HasSemanticVersion GH.IssueLabel
instance HasSemanticVersion GH.IssueState
instance HasSemanticVersion GH.Language
instance HasSemanticVersion GH.Milestone
instance HasSemanticVersion GH.Organization
instance HasSemanticVersion GH.Owner
instance HasSemanticVersion GH.OwnerType
instance HasSemanticVersion GH.Permission
instance HasSemanticVersion GH.Privacy
instance HasSemanticVersion GH.PullRequestReference
instance HasSemanticVersion GH.Repo
instance HasSemanticVersion GH.RepoRef
instance HasSemanticVersion GH.SimpleOrganization
instance HasSemanticVersion GH.SimpleOwner
instance HasSemanticVersion GH.SimpleTeam
instance HasSemanticVersion GH.SimpleUser
instance HasSemanticVersion GH.Team
instance HasSemanticVersion GH.User

instance HasSemanticVersion (GH.Name a)
instance HasSemanticVersion (GH.Id a)
instance HasSemanticVersion GH.URL

instance HasStructuralInfo a => HasStructuralInfo (Interval a) where
    structuralInfo _ =
        NominalNewtype "Interval" $ structuralInfo (Proxy :: Proxy a)

instance HasStructuralInfo a => HasStructuralInfo (NonEmpty.Interval a) where
    structuralInfo _ =
        NominalNewtype "Interval.NonEmpty" $ structuralInfo (Proxy :: Proxy a)

instance HasStructuralInfo a => HasStructuralInfo (Kaucher.Interval a) where
    structuralInfo _ =
        NominalNewtype "Interval.Kaucher" $ structuralInfo (Proxy :: Proxy a)

decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TE.decodeUtf8With TE.lenientDecode

-------------------------------------------------------------------------------
-- monad-time
-------------------------------------------------------------------------------

instance MonadTime Servant.Handler where
    currentTime = liftIO currentTime
