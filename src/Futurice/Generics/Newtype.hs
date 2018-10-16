{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.Generics.Newtype (
    -- * Deriving Via
    Newtypica (..),
    -- * Explicit
#ifdef MIN_VERSION_swagger2
    newtypeToParamSchema,
    newtypeDeclareNamedSchema,
#endif
    -- * Utilities
    repNewtype
    ) where

import Futurice.Prelude  hiding (Generic, from)
import Generics.SOP      hiding (constructorInfo, datatypeName)
import Generics.SOP.Lens
import Prelude ()

#ifdef MIN_VERSION_swagger2
import qualified Data.Swagger         as Swagger
import qualified Data.Swagger.Declare as Swagger
#endif

-------------------------------------------------------------------------------
-- Deriving via
-------------------------------------------------------------------------------

newtype Newtypica a = Newtypica a
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- swagger2
-------------------------------------------------------------------------------

#ifdef MIN_VERSION_swagger2
newtypeToParamSchema
    :: forall a r proxy t. (IsNewtype a r, Swagger.ToParamSchema r)
    => proxy a -> Swagger.ParamSchema t
newtypeToParamSchema _ = Swagger.toParamSchema (Proxy :: Proxy r)

newtypeDeclareNamedSchema
    :: forall a r proxy. (IsNewtype a r, Swagger.ToSchema r, HasDatatypeInfo a)
    => proxy a -> Swagger.Declare (Swagger.Definitions Swagger.Schema) Swagger.NamedSchema
newtypeDeclareNamedSchema _ = rename <$> Swagger.declareNamedSchema (Proxy :: Proxy r)
  where
    rename (Swagger.NamedSchema _ schema) = Swagger.NamedSchema (Just name) schema
    name = datatypeInfo (Proxy :: Proxy a) ^. datatypeName . packed
#endif

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

repNewtype :: IsNewtype a r => Iso' a r
repNewtype = iso coerce coerce
-- rep . unsop . unSingletonS . unSingletonP . uni
{-# INLINE repNewtype #-}
