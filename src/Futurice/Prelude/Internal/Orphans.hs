{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module defines some orphan instances for types and classes from
-- packages "Futurice.Prelude" depends upon.
--
-- TODO: Split into submodules
module Futurice.Prelude.Internal.Orphans () where

import Prelude ()
import Prelude.Compat

import Codec.Picture                (DynamicImage, Image, PixelRGBA8)
import Control.DeepSeq              (NFData (..))
import Control.Lens                 ((&), (.~), (^.))
import Control.Monad.Catch          (MonadCatch (..), MonadThrow (..))
import Control.Monad.CryptoRandom   (CRandT (..))
import Control.Monad.Logger         (MonadLogger (..))
import Control.Monad.Reader         (MonadReader (..))
import Control.Monad.Trans.Class    (lift)
import Data.Aeson.Compat
       (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.=))
import Data.Aeson.Extra             (M (..), ToJSONKey (..))
import Data.Binary                  (Binary (..))
import Data.Binary.Orphans ()
import Data.Binary.Tagged           (HasSemanticVersion, HasStructuralInfo)
import Data.Foldable                (toList)
import Data.Functor.Compose         (Compose (..))
import Data.Hashable                (Hashable (..))
import Data.Map                     (Map)
import Data.Proxy                   (Proxy (..))
import Data.Semigroup               (Semigroup (..))
import Data.String                  (fromString)
import Data.Swagger                 (NamedSchema (..), ToSchema (..))
import Data.Text.Lens               (packed)
import Data.Time                    (Day)
import Data.Time.Parsers            (day)
import Data.Typeable                (Typeable)
import Data.Vector                  (Vector)
import Generics.SOP                 (I (..))
import Lucid.Base                   (HtmlT (..))
import Numeric.Interval             (Interval, inf, sup)
import Text.Parsec                  (parse)
import Text.Parsec.String ()
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Text.PrettyPrint.ANSI.Leijen.AnsiPretty (AnsiPretty)

import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.Csv                             as Csv
import qualified Data.Swagger                         as Swagger
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import qualified GHC.Exts                             as Exts
import qualified GitHub                               as GH
import qualified GitHub.Data.Name                     as GH
import qualified Numeric.Interval.Kaucher             as Kaucher
import qualified Numeric.Interval.NonEmpty            as NonEmpty

#if !MIN_VERSION_transformers_compat(0,5,0)
import Data.Functor.Identity (Identity (..))
#endif

-- | Defined in 'Futurice.Prelude'.
instance Semigroup Doc where
    (<>) = mappend

-- | Defined in 'Futurice.Prelude'.
--
-- <https://github.com/tibbe/hashable/issues/108>
-- <https://github.com/ekmett/vector-instances/pull/4>
instance Hashable a => Hashable (Vector a) where
    hashWithSalt salt = hashWithSalt salt . toList

-- | Defined in 'Futurice.Prelude'.
instance Eq a => Eq (I a) where
    I a == I b = a == b

-- | Defined in 'Futurice.Prelude'.
--
-- <https://github.com/TomMD/monadcryptorandom/pull/10>
instance MonadThrow m => MonadThrow (CRandT g e m) where
    throwM = lift . throwM

-- | Defined in 'Futurice.Prelude'.
instance MonadCatch m => MonadCatch (CRandT g e m) where
    catch m h = CRandT $ catch (unCRandT m) (unCRandT . h)

-- | Defined in 'Futurice.Prelude'.
instance MonadLogger m => MonadLogger (CRandT g e m) where
    monadLoggerLog a b c d = lift $ monadLoggerLog a b c d

-- | Defined in 'Futurice.Prelude'.
--
-- <https://github.com/ekmett/intervals/issues/40>
instance Hashable a => Hashable (Interval a)
instance Hashable a => Hashable (Kaucher.Interval a)
instance Hashable a => Hashable (NonEmpty.Interval a)

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

#if !MIN_VERSION_transformers_compat(0,5,0)
deriving instance Typeable Identity
#endif

deriving instance Typeable Image
deriving instance Typeable PixelRGBA8

-- | Defined in 'Futurice.Prelude'.
--
-- <https://github.com/Twinside/Juicy.Pixels/pull/126>
deriving instance Typeable DynamicImage

-------------------------------------------------------------------------------
-- lucid
-------------------------------------------------------------------------------

-- | See <https://github.com/chrisdone/lucid/pull/53>
instance MonadReader r m => MonadReader r (HtmlT m) where
    ask = lift ask
    local f (HtmlT x) = HtmlT (local f x)

-------------------------------------------------------------------------------
-- ansi-pretty instances
-------------------------------------------------------------------------------

instance AnsiPretty (GH.Name entity)
instance AnsiPretty GH.Language

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

instance Csv.ToField Day where
    toField = fromString . show

instance Csv.FromField Day where
    parseField s = either (fail . show) return $
        parse day "FromField Day" s

instance Csv.ToField (M a) where
    toField _ = ""

-------------------------------------------------------------------------------
-- Swagger schemas
-------------------------------------------------------------------------------

-- | /TODO:/ this is partly incorrect instance
instance ToSchema Value where
    declareNamedSchema _ = pure $ NamedSchema (Just "JSON Value") s
      where
        s = mempty

instance ToJSONKey Day where
    toJSONKey a = show a ^. packed

instance ToSchema v => ToSchema (M (Map k v)) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Map String v))

instance ToSchema (f (g a)) => ToSchema (Compose f g a) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (f (g a)))

instance ToSchema (GH.Name a) where
    declareNamedSchema _ = pure $ NamedSchema (Just "Github name") mempty

instance ToSchema GH.Language

instance ToSchema DynamicImage where
    declareNamedSchema _ = pure $ NamedSchema (Just "Image") mempty

instance ToSchema (Image a) where
    declareNamedSchema _ = pure $ NamedSchema (Just "Image") mempty

instance ToSchema BS.ByteString where
    declareNamedSchema _ = pure $ NamedSchema (Just "Strict ByteString") mempty

instance ToSchema LBS.ByteString where
    declareNamedSchema _ = pure $ NamedSchema (Just "Lazy ByteString") mempty

instance ToSchema a => ToSchema (NonEmpty.Interval a) where
    declareNamedSchema _ = NamedSchema (Just "NonEmpty.Interval") . schema <$> propA
      where
        propA = Swagger.declareSchemaRef (Proxy :: Proxy a)
        schema prop = mempty
          & Swagger.type_       .~ Swagger.SwaggerObject
          & Swagger.properties  .~ Exts.fromList
              [ ("inf", prop)
              , ("sup", prop)
              ]
          & Swagger.required    .~ ["sup", "inf"]

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance ToJSON (f (g a)) => ToJSON (Compose f g a) where
    toJSON = toJSON . getCompose

instance FromJSON (f (g a)) => FromJSON (Compose f g a) where
    parseJSON = fmap Compose . parseJSON

instance ToJSON a => ToJSON (NonEmpty.Interval a) where
    toJSON i = object [ "inf" .= NonEmpty.inf i, "sup" .= NonEmpty.sup i ]

instance (Ord a, FromJSON a) => FromJSON (NonEmpty.Interval a) where
    parseJSON = withObject "NonEmpty.Interval" $ \obj -> (NonEmpty....)
        <$> obj .: "inf"
        <*> obj .: "sup"

-------------------------------------------------------------------------------
-- Binary
-------------------------------------------------------------------------------

instance Binary (GH.Request k a) where
    get = undefined

    put (GH.Query ps qs) =
        put (0 :: Int) >> put ps >> put qs
    put (GH.PagedQuery ps qs c) =
        put (1 :: Int) >> put ps >> put qs >> put c
    put (GH.Command m ps bs) =
        put (2 :: Int) >> put m >> put ps >> put bs
    put (GH.StatusQuery sm r) =
        put (3 :: Int) >> put sm >> put r
#if MIN_VERSION_github(0,15,0)
    put (GH.HeaderQuery hs r) =
        put (4 :: Int) >> put hs >> put r
#endif

instance Binary (GH.CommandMethod a) where
    get = undefined
    put GH.Post   = put (0 :: Int)
    put GH.Patch  = put (1 :: Int)
    put GH.Put    = put (2 :: Int)
    put GH.Delete = put (3 :: Int)

instance Binary (GH.StatusMap a) where
    get = undefined
    put GH.StatusOnlyOk = put (0 :: Int)
    put GH.StatusMerge  = put (1 :: Int)

-------------------------------------------------------------------------------
-- binary-tagged
-------------------------------------------------------------------------------

instance HasStructuralInfo GH.OwnerType
instance HasStructuralInfo GH.User
instance HasStructuralInfo (GH.Name a)
instance HasStructuralInfo (GH.Id a)

instance HasSemanticVersion GH.OwnerType
instance HasSemanticVersion GH.User
instance HasSemanticVersion (GH.Name a)
instance HasSemanticVersion (GH.Id a)
