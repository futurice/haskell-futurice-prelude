{-# LANGUAGE CPP #-}
-- | "Generics.SOP" derivation for record types (i.e. products).
module Futurice.Generics (
    -- * Deriving Via
#if __GLASGOW_HASKELL__ >= 802
    deriveVia,
    Via,
#endif
    Sopica (..),
    Enumica (..),
    TextEnum (..),
    -- * Classes
    Arbitrary(..),
    ToHtml (..),
    ToNamedRecord(..),
    DefaultOrdered(..),
    FromRecord(..),
    ToJSON(..),
    FromJSON(..),
    ToSchema(..),
    ToParamSchema (..),
    ToHttpApiData (..),
    FromHttpApiData (..),
    -- * Empty
    emptyDeclareNamedSchema,
    -- * Newtype
    -- | We export this because they aren't GND deriviable.
    newtypeToParamSchema,
    newtypeDeclareNamedSchema,
    -- * Enum
    enumPrism,
    enumFromTextE,
    -- ** swagger2
    enumToParamSchema,
    enumDeclareNamedSchema,
    -- * SOP
    -- ** swagger2
    sopDeclareNamedSchema,
    ) where

import Data.Typeable    (typeRep)
import Futurice.Prelude
import Prelude ()

#if __GLASGOW_HASKELL__ >= 802
import Data.Deriving.Via         (Via, deriveVia)
#endif

import Futurice.Generics.Enum
import Futurice.Generics.Newtype
import Futurice.Generics.SOP

import Data.Aeson      (FromJSON (..), ToJSON (..))
import Data.Csv
       (DefaultOrdered (..), FromRecord (..), ToNamedRecord (..))
import Data.Swagger    (ToParamSchema (..), ToSchema (..))
import Lucid           (ToHtml (..))
import Test.QuickCheck (Arbitrary (..))
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

import qualified Data.Swagger         as Swagger
import qualified Data.Swagger.Declare as Swagger

-- | Declares with named but empty schema.
emptyDeclareNamedSchema
    :: forall a proxy. Typeable a => proxy a
    -> Swagger.Declare (Swagger.Definitions Swagger.Schema) Swagger.NamedSchema
emptyDeclareNamedSchema _ = pure $ Swagger.NamedSchema (Just name) mempty where
    name = textShow $ typeRep (Proxy :: Proxy a)
