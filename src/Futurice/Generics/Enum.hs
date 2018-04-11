{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Futurice.Generics.Enum (
    Enumica (..),
    TextEnum (..),
    -- * Explicit
    enumPrism,
    enumFromTextE,
    -- ** aeson
    enumToJSON,
    enumToEncoding,
    enumParseJSON,
    -- ** lucid
    enumToHtml,
    -- ** swagger2
    enumToParamSchema,
    enumDeclareNamedSchema,
    -- ** cassava
    enumCsvToField,
    enumCsvParseField,
    -- ** postgresql-simple
    enumPostgresToField,
    enumPostgresFromField,
    ) where

import Control.Monad    ((>=>))
import Data.Typeable    (typeRep)
import Futurice.Prelude
import Lucid            (HtmlT, ToHtml (..))
import Prelude ()
import Web.HttpApiData  (FromHttpApiData (..), ToHttpApiData (..))

import qualified Data.Aeson.Compat                    as Aeson
import qualified Data.Csv                             as Csv
import qualified Data.Map                             as Map
import qualified Data.Swagger                         as Swagger
import qualified Data.Swagger.Declare                 as Swagger
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import qualified Generics.SOP                         as SOP

-- | A helper newtype to use @DerivingVia@ extension.
newtype Enumica a = Enumica a

-------------------------------------------------------------------------------
-- TextEnum
-------------------------------------------------------------------------------

-- | Enums which can be converted to 'Text' and back.
class (Typeable a, Enum a, Bounded a) => TextEnum a where
    type TextEnumNames a :: [Symbol]

    enumToText :: a -> Text
    default enumToText
        :: (SOP.IsEnumType a, GTextEnumTo (TextEnumNames a) (SOP.Code a))
        => a -> Text
    enumToText = untag' . enumToText' . SOP.unSOP . SOP.from where
        untag' :: Tagged (TextEnumNames a) Text -> Text
        untag' = coerce

    enumFromText :: Text -> Maybe a
    default enumFromText
        :: (SOP.IsEnumType a, GTextEnumFrom (TextEnumNames a) (SOP.Code a))
        => Text -> Maybe a
    enumFromText t = SOP.to . SOP.SOP <$> Map.lookup (T.toLower t) (untag m) where
        m :: Tagged (TextEnumNames a) (Map Text (NS (NP I) (SOP.Code a)))
        m = enumMap

class GTextEnumTo (ks :: [Symbol]) (xss :: [[*]]) | ks -> xss where
    enumToText' :: NS (NP I) xss -> Tagged ks Text

instance GTextEnumTo '[] '[] where
    enumToText' ns = case ns of {} -- TODO: boring-0.1 absurd

instance
    forall sym ks xss. (KnownSymbol sym, GTextEnumTo ks xss)
    => GTextEnumTo (sym ': ks) ('[] ': xss)
  where
    enumToText' (S n) = coerce (enumToText' :: NS (NP I) xss -> Tagged ks Text) n
    enumToText' (Z Nil) = Tagged (symbolVal (Proxy :: Proxy sym) ^. packed)
#if __GLASGOW_HASKELL < 800
    enumToText' (Z x) = case x of
#endif

class GTextEnumFrom (ks :: [Symbol]) (xss :: [[*]]) | ks -> xss where
    enumMap :: Tagged ks (Map Text (NS (NP I) xss))

instance GTextEnumFrom '[] '[] where
    enumMap = Tagged Map.empty

instance
    forall sym ks xss. (KnownSymbol sym, GTextEnumFrom ks xss)
    => GTextEnumFrom (sym ': ks) ('[] ': xss)
  where
    enumMap = Tagged (Map.insert k (Z Nil) (Map.map S (untag m))) where
        k :: Text
        k = T.toLower $ symbolVal (Proxy :: Proxy sym) ^. packed

        m :: Tagged ks (Map Text (NS (NP I) xss))
        m = enumMap

enumPrism :: TextEnum a => Prism' Text a
enumPrism = prism' enumToText enumFromText

enumFromTextE :: forall a. (Typeable a, TextEnum a) => Text -> Either Text a
enumFromTextE t = maybe
    (Left $ "Invalid " <> name <> ": " <> t)
    Right
    (enumFromText t)
  where
    name = textShow $ typeRep (Proxy :: Proxy a)

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance TextEnum a => Aeson.ToJSON (Enumica a) where
    toJSON     = coerce (enumToJSON     :: a -> Aeson.Value)
    toEncoding = coerce (enumToEncoding :: a -> Aeson.Encoding)

instance TextEnum a => Aeson.FromJSON (Enumica a) where
    parseJSON = coerce (enumParseJSON :: Aeson.Value -> Aeson.Parser a)

enumToJSON :: TextEnum a => a -> Aeson.Value
enumToJSON = Aeson.String . enumToText

enumToEncoding :: TextEnum a => a -> Aeson.Encoding
enumToEncoding = Aeson.toEncoding . enumToText

enumParseJSON :: forall a. TextEnum a => Value -> Aeson.Parser a
enumParseJSON = Aeson.withText name $
    either (fail . view unpacked) pure . enumFromTextE
  where
    name = show $ typeRep (Proxy :: Proxy a)

-------------------------------------------------------------------------------
-- swagger2
-------------------------------------------------------------------------------

-- no Enumica instances yet. Doesn't work with GHC-8.2

enumToParamSchema
    :: forall a t proxy. TextEnum a
    => proxy a -> Swagger.ParamSchema t
enumToParamSchema _ = mempty
    & Swagger.type_ .~ Swagger.SwaggerString
    & Swagger.enum_ ?~ map enumToJSON [ minBound .. maxBound :: a ]

enumDeclareNamedSchema
    :: forall a proxy. TextEnum a
    => proxy a -> Swagger.Declare (Swagger.Definitions Swagger.Schema) Swagger.NamedSchema
enumDeclareNamedSchema _ = pure $ Swagger.NamedSchema (Just name) $ mempty
    & Swagger.paramSchema .~ enumToParamSchema (Proxy :: Proxy a)
  where
    name = textShow $ typeRep (Proxy :: Proxy a)

-------------------------------------------------------------------------------
-- http-api-data
-------------------------------------------------------------------------------

instance TextEnum a => FromHttpApiData (Enumica a) where
    parseUrlPiece = coerce (enumFromTextE :: Text -> Either Text a)

instance TextEnum a => ToHttpApiData (Enumica a) where
    toUrlPiece = coerce (enumToText :: a -> Text)

-------------------------------------------------------------------------------
-- lucid
-------------------------------------------------------------------------------

instance TextEnum a => ToHtml (Enumica a) where
    toHtmlRaw = toHtml
    toHtml (Enumica x) = enumToHtml x

enumToHtml :: (TextEnum a, Monad m) => a -> HtmlT m ()
enumToHtml = toHtml . enumToText

-------------------------------------------------------------------------------
-- cassava
-------------------------------------------------------------------------------

instance TextEnum a => Csv.ToField (Enumica a) where
    toField = coerce (enumCsvToField :: a -> Csv.Field)

instance TextEnum a => Csv.FromField (Enumica a) where
    parseField = coerce (enumCsvParseField :: Csv.Field -> Csv.Parser a)

enumCsvToField :: TextEnum a => a -> Csv.Field
enumCsvToField = Csv.toField . enumToText

enumCsvParseField :: TextEnum a => Csv.Field -> Csv.Parser a
enumCsvParseField = Csv.parseField >=>
    either (fail . view unpacked) pure . enumFromTextE

-------------------------------------------------------------------------------
-- postgresql-simple
-------------------------------------------------------------------------------

enumPostgresToField :: TextEnum a => a -> Postgres.Action
enumPostgresToField = Postgres.toField . enumToText

enumPostgresFromField  :: TextEnum a => Postgres.FieldParser a
enumPostgresFromField f mbs = Postgres.fromField f mbs >>=
    either (err . view unpacked) pure . enumFromTextE
  where
    err = Postgres.returnError Postgres.ConversionFailed f
