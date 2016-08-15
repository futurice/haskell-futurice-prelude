{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- | "Generics.SOP" derivation for record types (i.e. products).
module Futurice.Generics (
    -- * QuickCheck
    QC.Arbitrary(..),
    sopArbitrary,
    sopShrink,
    -- * Cassava
    Csv.ToNamedRecord(..),
    sopToNamedRecord,
    Csv.DefaultOrdered(..),
    sopHeaderOrder,
    Csv.FromRecord(..),
    sopParseRecord,
    -- * Aeson
    Aeson.ToJSON(..),
    Aeson.FromJSON(..),
    sopToJSON,
    sopParseJSON,
    -- * Swagger
    Swagger.ToSchema(..),
    sopDeclareNamedSchema,
    ) where

import Futurice.Prelude hiding (Generic, from)
import Prelude ()

import Data.ByteString   (ByteString)
import Data.Char         (toLower)
import Generics.SOP
import Generics.SOP.Lens

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as Aeson
import qualified Data.Csv             as Csv
import qualified Data.Swagger         as Swagger
import qualified Data.Swagger.Declare as Swagger
import qualified Data.Vector          as V
import qualified GHC.Exts             as Exts
import qualified Test.QuickCheck      as QC

-------------------------------------------------------------------------------
-- QuickCheck
-------------------------------------------------------------------------------

-- | Works for non-recursive structures
sopArbitrary
    :: (Generic a, All2 QC.Arbitrary (Code a))
    => QC.Gen a
sopArbitrary = to <$> QC.oneof (map hsequence $ apInjs_POP popArbitrary)
  where
    popArbitrary :: All2 QC.Arbitrary xs => POP QC.Gen xs
    popArbitrary = hcpure (Proxy :: Proxy QC.Arbitrary) QC.arbitrary

-- | Works for non-recursive structures
sopShrink
    :: forall a. (Generic a, All2 QC.Arbitrary (Code a))
    => a -> [a]
sopShrink = map (to . SOP) . sopShrink' . unSOP . from
  where
    sopShrink'
      :: forall yss. All2 QC.Arbitrary yss
      => NS (NP I) yss -> [NS (NP I) yss]
    sopShrink' (Z x) = map Z (sopShrink'' x)
    sopShrink' (S x) = map S (sopShrink' x)

    sopShrink'' :: forall ys. All QC.Arbitrary ys => NP I ys -> [NP I ys]
    sopShrink''
        = hsequence
        . hcmap (Proxy :: Proxy QC.Arbitrary) (QC.shrink . unI)

-------------------------------------------------------------------------------
-- Cassava
-------------------------------------------------------------------------------

sopToNamedRecord
    :: forall a xs.
       (Generic a, HasDatatypeInfo a, All Csv.ToField xs, Code a ~ '[xs])
    => a
    -> Csv.NamedRecord
sopToNamedRecord
    = Csv.namedRecord
    . sopToNamedRecord' fieldInfos
    . (^. unsop . unSingletonS)
    . from
  where
    fieldInfos = datatypeInfo (Proxy :: Proxy a) ^.
        constructorInfo . unSingletonP . fieldInfo

sopToNamedRecord'
    :: All Csv.ToField xs => NP FieldInfo xs -> NP I xs
    -> [(ByteString, ByteString)]
sopToNamedRecord' fs' xs' = go fs' xs'
  where
    prefix :: String
    prefix = longestFieldInfoPrefix fs'

    go :: All Csv.ToField xs => NP FieldInfo xs -> NP I xs -> [(ByteString, ByteString)]
    go Nil Nil = []
    go (FieldInfo f :* fs) (I x :* xs) =
        Csv.namedField (fromString $ processFieldName prefix f) x : go fs xs
#if __GLASGOW_HASKELL__ < 800
    go _ _ = error "sopToNamedRecord' go: impossible happened"
#endif

sopHeaderOrder
    :: forall a xs.
       (Generic a, HasDatatypeInfo a, Code a ~ '[xs])
    => a  -- ^ Unused, only for compatibility with @cassava@'s 'headerOrder'
    -> Csv.Header
sopHeaderOrder _ = V.fromList (sopHeaderOrder' fieldInfos)
  where
    fieldInfos = datatypeInfo (Proxy :: Proxy a) ^.
        constructorInfo . unSingletonP . fieldInfo

sopHeaderOrder' :: SListI xs => NP FieldInfo xs -> [ByteString]
sopHeaderOrder' fs = hcollapse (hmap f fs)
  where
    prefix :: String
    prefix = longestFieldInfoPrefix fs

    f :: FieldInfo a -> K ByteString a
    f (FieldInfo n) = K . fromString . processFieldName prefix $ n

sopParseRecord
    :: forall a xs.
       (Generic a, All Csv.FromField xs, Code a ~ '[xs])
    => Csv.Record
    -> Csv.Parser a
sopParseRecord r
    | length r == lenXs = to . SOP . Z <$> sopParseRecord' r
    | otherwise         = fail $ "Cannot match field of length " ++ show (length r) ++ " with record of " ++ show lenXs ++ " fields"
  where
    lenXs = lengthSList (Proxy :: Proxy xs)

sopParseRecord' :: forall xs. (All Csv.FromField xs) => Csv.Record -> Csv.Parser (NP I xs)
sopParseRecord' r = go (sList :: SList xs) 0
  where
    go :: All Csv.FromField ys => SList ys -> Int -> Csv.Parser (NP I ys)
    go SNil  _ = pure Nil
    go SCons i = (\h t -> I h :* t) <$> r Csv..! i <*> go sList (i + 1)

-------------------------------------------------------------------------------
-- Aeson
-------------------------------------------------------------------------------

sopToJSON
    :: forall a xs.
       (Generic a, HasDatatypeInfo a, All Aeson.ToJSON xs, Code a ~ '[xs])
    => a
    -> Aeson.Value
sopToJSON
    = Aeson.object
    . sopToJSON' fieldInfos
    . (^. unsop . unSingletonS)
    . from
  where
    fieldInfos = datatypeInfo (Proxy :: Proxy a) ^.
        constructorInfo . unSingletonP . fieldInfo

sopToJSON'
    :: All Aeson.ToJSON xs => NP FieldInfo xs -> NP I xs
    -> [Aeson.Pair]
sopToJSON' fs' xs' = go fs' xs'
  where
    prefix :: String
    prefix = longestFieldInfoPrefix fs'

    go :: All Aeson.ToJSON xs => NP FieldInfo xs -> NP I xs -> [Aeson.Pair]
    go Nil Nil = []
    go (FieldInfo f :* fs) (I x :* xs) =
        (fromString $ processFieldName prefix f) Aeson..= x : go fs xs
#if __GLASGOW_HASKELL__ < 800
    go _ _ = error "sopToNamedRecord' go: impossible happened"
#endif

sopParseJSON
    :: forall a xs.
       (Generic a, HasDatatypeInfo a, All Aeson.FromJSON xs, Code a ~ '[xs])
    => Aeson.Value
    -> Aeson.Parser a
sopParseJSON = Aeson.withObject tName $ \obj ->
    to . SOP . Z <$> sopParseJSON' obj fieldInfos
  where
    dInfo = datatypeInfo (Proxy :: Proxy a)
    tName = dInfo ^. datatypeName
    fieldInfos = dInfo ^. constructorInfo . unSingletonP . fieldInfo

sopParseJSON'
    :: All Aeson.FromJSON xs
    => Aeson.Object -> NP FieldInfo xs -> Aeson.Parser (NP I xs)
sopParseJSON' obj fs' = go fs'
  where
    prefix :: String
    prefix = longestFieldInfoPrefix fs'

    go :: All Aeson.FromJSON ys => NP FieldInfo ys -> Aeson.Parser (NP I ys)
    go Nil  = pure Nil
    go (FieldInfo f :* fs) = (\h t -> I h :* t)
        <$> obj Aeson..: (fromString $ processFieldName prefix f)
        <*> go fs


-------------------------------------------------------------------------------
-- swagger
-------------------------------------------------------------------------------

type SwaggerM = Swagger.Declare (Swagger.Definitions Swagger.Schema)
type SwaggerPP = (Text, Swagger.Referenced Swagger.Schema)

sopDeclareNamedSchema
    :: forall a xs proxy.
       (HasDatatypeInfo a, All Swagger.ToSchema xs, Code a ~ '[xs])
    => proxy a
    -> SwaggerM Swagger.NamedSchema
sopDeclareNamedSchema _ = do
    props <- hsequenceK (hcmap (Proxy :: Proxy Swagger.ToSchema) prop fieldInfos) :: SwaggerM (NP (K SwaggerPP) xs)
    pure $ Swagger.NamedSchema (Just $ name ^. packed) $ schema (Exts.fromList . hcollapse $ props)
  where
    name = datatypeInfo proxy ^. datatypeName

    schema props = mempty
      & Swagger.type_ .~ Swagger.SwaggerObject
      & Swagger.properties .~ props
      & Swagger.required .~ hcollapse (hmap req fieldInfos)

    prefix :: String
    prefix = longestFieldInfoPrefix fieldInfos

    req :: forall y. FieldInfo y -> K Text y
    req (FieldInfo n) = K $ processFieldName prefix n ^. packed

    prop :: forall y. Swagger.ToSchema y => FieldInfo y -> K (SwaggerM SwaggerPP) y
    prop (FieldInfo n) = K $ (,) n' <$> s
      where
        n' = processFieldName prefix n ^. packed
        s = Swagger.declareSchemaRef (Proxy :: Proxy y)

    fieldInfos :: NP FieldInfo xs
    fieldInfos = datatypeInfo proxy ^. constructorInfo . unSingletonP . fieldInfo

    proxy :: Proxy a
    proxy = Proxy

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

longestCommonPrefix :: Eq a => [a] -> [a] -> [a]
longestCommonPrefix [] _ = []
longestCommonPrefix _ [] = []
longestCommonPrefix (x:xs) (y:ys)
    | x == y    = x : longestCommonPrefix xs ys
    | otherwise = []

longestFieldInfoPrefix :: NP FieldInfo xs -> String
longestFieldInfoPrefix Nil = ""
longestFieldInfoPrefix (FieldInfo _ :* Nil) = ""
longestFieldInfoPrefix (FieldInfo a :* FieldInfo b :* Nil) =
    longestCommonPrefix a b
longestFieldInfoPrefix (FieldInfo a :* xs) =
    longestCommonPrefix a (longestFieldInfoPrefix xs)

lowerFirst :: String -> String
lowerFirst []     = []
lowerFirst (c:cs) = toLower c : cs

processFieldName :: String -> String -> String
processFieldName pfx = lowerFirst . drop (length pfx)

-------------------------------------------------------------------------------
-- generics-sop-lens
-------------------------------------------------------------------------------

fieldInfo :: Lens' (ConstructorInfo xs) (NP FieldInfo xs)
fieldInfo = lens g s
  where
    g :: ConstructorInfo xs -> NP FieldInfo xs
    g (Record _ fs) = fs
    g _             = error "fieldInfo get: only record supported"

    s :: ConstructorInfo xs -> NP FieldInfo xs -> ConstructorInfo xs
    s (Record n _) fs = Record n fs
    s _ _             = error "fieldInfo set: only record supported"
