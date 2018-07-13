{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Futurice.Generics.Textual (
    Textica (..),
    Textual (..),
{-
    -- * Explicit
    textualPrism,
    textualFromTextE,
    -- ** aeson
    textualToJSON,
    textualToEncoding,
    textualParseJSON,
    -- ** lucid
    textualToHtml,
-}
    -- ** swagger2
    textualToParamSchema,
    textualDeclareNamedSchema,
{-
    -- * Explicit
    -- ** cassava
    textualCsvToField,
    textualCsvParseField,
    -- ** postgresql-simple
    textualPostgresToField,
    textualPostgresFromField,
-}
    ) where

import Control.Monad    ((>=>))
import Data.Typeable    (typeRep)
import Futurice.Prelude
import Lucid            (ToHtml (..))
import Prelude ()
import Web.HttpApiData  (FromHttpApiData (..), ToHttpApiData (..))

import qualified Data.Aeson.Compat                    as Aeson
import qualified Data.Csv                             as Csv
import qualified Data.Swagger                         as Swagger
import qualified Data.Swagger.Declare                 as Swagger
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres

-- | A helper newtype to use @DerivingVia@ extension.
newtype Textica a = Textica a

-------------------------------------------------------------------------------
-- Textual
-------------------------------------------------------------------------------

-- | A class of things like 'Text'.
class Textual a where
    textualToText   :: a -> Text
    textualFromText :: Text -> Either String a

texticaToText :: forall a. Textual a => Textica a -> Text
texticaToText = coerce (textualToText :: a -> Text)

texticaFromText :: forall a. Textual a => Text -> Either String (Textica a)
texticaFromText = coerce (textualFromText :: Text -> Either String a)

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance Textual a => Aeson.FromJSON (Textica a) where
    parseJSON v = do
        t <- Aeson.parseJSON v
        either fail pure $ texticaFromText t

instance Textual a => Aeson.ToJSON (Textica a) where
    toJSON     = Aeson.toJSON     . texticaToText
    toEncoding = Aeson.toEncoding . texticaToText

-------------------------------------------------------------------------------
-- http-api-data
-------------------------------------------------------------------------------

instance Textual a => ToHttpApiData (Textica a) where
    toUrlPiece = texticaToText

instance Textual a => FromHttpApiData (Textica a) where
    parseUrlPiece t = do
        t' <- parseUrlPiece t
        either (fail . view packed) pure $ texticaFromText t'

-------------------------------------------------------------------------------
-- swagger2
-------------------------------------------------------------------------------

textualToParamSchema
    :: forall a t proxy. Textual a
    => proxy a -> Swagger.ParamSchema t
textualToParamSchema _ = mempty
    & Swagger.type_  .~ Swagger.SwaggerString
    -- & Swagger.format ?~ "format?"

textualDeclareNamedSchema
    :: forall a proxy. (Textual a, Typeable a)
    => proxy a -> Swagger.Declare (Swagger.Definitions Swagger.Schema) Swagger.NamedSchema
textualDeclareNamedSchema p =
    pure $ Swagger.NamedSchema (Just name) $ mempty
        & Swagger.paramSchema .~ textualToParamSchema p
        -- TODO: example
        -- & Swagger.example ?~ toJSON (contentHashLBS "foobar")
  where
    name = textShow (typeRep (Proxy :: Proxy a))

instance Textual a => Swagger.ToParamSchema (Textica a) where
    toParamSchema _ = textualToParamSchema (Proxy :: Proxy a)

instance (Typeable a, Textual a) => Swagger.ToSchema (Textica a) where
    declareNamedSchema _ = textualDeclareNamedSchema (Proxy :: Proxy a)

-------------------------------------------------------------------------------
-- postgresql-simple
-------------------------------------------------------------------------------

instance Textual a => Postgres.ToField (Textica a) where
    toField = Postgres.toField . texticaToText

instance (Typeable a, Textual a) => Postgres.FromField (Textica a) where
    fromField f mbs = do
        t <- Postgres.fromField f mbs
        either (Postgres.returnError Postgres.ConversionFailed f) pure $
            texticaFromText t

-------------------------------------------------------------------------------
-- cassava
-------------------------------------------------------------------------------

instance Textual a => Csv.ToField (Textica a) where
    toField = Csv.toField . texticaToText

instance Textual a => Csv.FromField (Textica a) where
    parseField = Csv.parseField >=> either fail pure . texticaFromText

-------------------------------------------------------------------------------
-- Lucid
-------------------------------------------------------------------------------

instance Textual a => ToHtml (Textica a) where
    toHtmlRaw = toHtmlRaw . texticaToText
    toHtml    = toHtml    . texticaToText
