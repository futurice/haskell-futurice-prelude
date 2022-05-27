{-# LANGUAGE CPP             #-}
{-# LANGUAGE ConstraintKinds #-}
-- | "Generics.SOP" derivation for record types (i.e. products).
module Futurice.Generics (
    Sopica (..),
    SopGeneric,
    GhcGeneric,
    SOP.HasDatatypeInfo,
    Enumica (..),
    TextEnum (..),
    Textica (..),
    Textual (..),
    -- * Classes
    Arbitrary(..),
    ToHtml (..),
    ToNamedRecord(..),
    DefaultOrdered(..),
    ToRecord(..),
    FromRecord(..),
    ToJSON(..),
    FromJSON(..),
#ifdef MIN_VERSION_swagger2
    ToSchema(..),
    ToParamSchema (..),
#endif
    ToHttpApiData (..),
    FromHttpApiData (..),
#ifdef MIN_VERSION_swagger2
    -- * Swagger2
    -- ** Empty
    emptyDeclareNamedSchema,
    -- *( Newtype
    newtypeToParamSchema,
    newtypeDeclareNamedSchema,
    -- **( swagger2
    enumToParamSchema,
    enumDeclareNamedSchema,
    -- ** Textual
    textualToParamSchema,
    textualDeclareNamedSchema,
    -- ** SOP
    sopDeclareNamedSchema,
#endif
    -- * Enum prisms
    enumPrism,
    enumFromTextE,
    ) where

import Data.Typeable    (typeRep)
import Futurice.Prelude
import Prelude ()

import Futurice.Generics.Enum
import Futurice.Generics.Newtype
import Futurice.Generics.SOP
import Futurice.Generics.Textual

import Data.Aeson      (FromJSON (..), ToJSON (..))
import Data.Csv
       (DefaultOrdered (..), FromRecord (..), ToNamedRecord (..),
       ToRecord (..))
import Lucid           (ToHtml (..))
import Test.QuickCheck (Arbitrary (..))
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

#ifdef MIN_VERSION_swagger2
import           Data.Swagger         (ToParamSchema (..), ToSchema (..))
import qualified Data.Swagger         as Swagger
import qualified Data.Swagger.Declare as Swagger
#endif

type SopGeneric = SOP.Generic
type GhcGeneric = GHC.Generic

#ifdef MIN_VERSION_swagger2
-- | Declares with named but empty schema.
emptyDeclareNamedSchema
    :: forall a proxy. Typeable a => proxy a
    -> Swagger.Declare (Swagger.Definitions Swagger.Schema) Swagger.NamedSchema
emptyDeclareNamedSchema _ = pure $ Swagger.NamedSchema (Just name) mempty where
    name = textShow $ typeRep (Proxy :: Proxy a)
#endif
