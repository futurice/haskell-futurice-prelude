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
module Futurice.Generics.SOP (
    Sopica (..),
    -- * Explicit
    -- ** QuickCheck
    sopArbitrary,
    sopShrink,
    -- ** cassava
    sopToNamedRecord,
    sopHeaderOrder,
    sopParseRecord,
    -- *( aeson
    sopToJSON,
    sopToEncoding,
    sopParseJSON,
#ifdef MIN_VERSION_swagger2
    -- ** swagger2
    sopDeclareNamedSchema,
#endif
    -- * Utilities
    sopRecordFieldNames,
    IsNewtype,
    IsProductType',
    processFieldName,
    longestFieldPrefix,
    fieldInfos,
    strippedFieldNames,
    strippedFieldNamesS,
    strippedFieldNamesT,
    ) where

import Data.Char                 (toLower)
import Futurice.Aeson            (withObjectDump)
import Futurice.IsMaybe
import Futurice.Prelude          hiding (Generic, from)
import Generics.SOP              hiding (constructorInfo, constructorName, datatypeName)
import Generics.SOP.Lens
import Prelude ()

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as Aeson
import qualified Data.Csv             as Csv
import qualified Data.HashMap.Strict  as HM
import qualified Data.Vector          as V
import qualified GHC.Exts             as Exts
import qualified Test.QuickCheck      as QC

#ifdef MIN_VERSION_swagger2
import qualified Data.Swagger         as Swagger
import qualified Data.Swagger.Declare as Swagger
#endif

-------------------------------------------------------------------------------
-- Deriving via
-------------------------------------------------------------------------------

newtype Sopica a = Sopica a
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Like 'IsProductType', but also requires that there are at least two fields.
type IsProductType' a xs y0 y1 ys =
    (IsProductType a xs, Code a ~ '[y0 ': y1 ': ys])

-------------------------------------------------------------------------------
-- QuickCheck
-------------------------------------------------------------------------------

-- | This instance works for non-recursive structures.
instance (Generic a, All2 QC.Arbitrary (Code a))
    => QC.Arbitrary (Sopica a)
  where
    arbitrary = coerce (sopArbitrary :: QC.Gen a)
    shrink    = coerce (sopShrink :: a -> [a])

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
sopShrink
    = map (to . SOP)
    . hsequence'
    . hcmap (Proxy :: Proxy (All QC.Arbitrary)) sopShrink'
    . unSOP
    . from
  where
    sopShrink' :: forall ys. All QC.Arbitrary ys => NP I ys -> ([] :.: NP I) ys
    sopShrink' Nil          = Comp $ []
    sopShrink' (I x :* Nil) = Comp $ (\x' -> I x' :* Nil) <$> QC.shrink x
    sopShrink' (I x :* xs)  = Comp $
        [ I x' :* xs  | x'  <- QC.shrink x ] ++
        [ I x  :* xs' | xs' <- unComp $ sopShrink' xs ]

-------------------------------------------------------------------------------
-- Cassava
-------------------------------------------------------------------------------

instance
    (Generic a, HasDatatypeInfo a, Code a ~ '[xs])
    => Csv.DefaultOrdered (Sopica a)
  where
    headerOrder = coerce (sopHeaderOrder :: a -> Csv.Header)

instance (HasDatatypeInfo a, IsProductType a xs, All Csv.ToField xs)
    => Csv.ToNamedRecord (Sopica a)
  where
    toNamedRecord = coerce (sopToNamedRecord :: a -> Csv.NamedRecord)

instance (IsProductType a xs, All Csv.ToField xs)
    => Csv.ToRecord (Sopica a)
  where
    toRecord = coerce (sopToRecord :: a -> Csv.Record)

sopToNamedRecord
    :: forall a xs. (IsProductType a xs, HasDatatypeInfo a, All Csv.ToField xs)
    => a
    -> Csv.NamedRecord
sopToNamedRecord
    = Csv.namedRecord
    . sopToNamedRecord' (longestFieldPrefix p) (fieldInfos p)
    . (^. unsop . unSingletonS)
    . from
  where
    p = Proxy :: Proxy a

sopToRecord
    :: forall a xs. (IsProductType a xs, All Csv.ToField xs)
    => a
    -> Csv.Record
sopToRecord
    = V.fromList
    . hcollapse
    . hcmap (Proxy :: Proxy Csv.ToField) (mapIK Csv.toField)
    . (^. unsop . unSingletonS)
    . from

sopToNamedRecord'
    :: All Csv.ToField xs
    => String -> NP FieldInfo xs -> NP I xs
    -> [(ByteString, ByteString)]
sopToNamedRecord' prefix = go
  where
    go :: All Csv.ToField xs => NP FieldInfo xs -> NP I xs -> [(ByteString, ByteString)]
    go Nil Nil = []
    go (FieldInfo f :* fs) (I x :* xs) =
        Csv.namedField (fromString $ processFieldName prefix f) x : go fs xs

sopHeaderOrder
    :: forall a xs.
       (Generic a, HasDatatypeInfo a, Code a ~ '[xs])
    => a  -- ^ Unused, only for compatibility with @cassava@'s 'headerOrder'
    -> Csv.Header
sopHeaderOrder _ = V.fromList . sopHeaderOrder' . fieldInfos $ (Proxy :: Proxy a)

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

instance
    (IsProductType' a xs y0 y1 ys, HasDatatypeInfo a, All Aeson.ToJSON xs, All IsMaybe xs)
    => Aeson.ToJSON (Sopica a)
  where
    toJSON     = coerce (sopToJSON :: a -> Aeson.Value)
    toEncoding = coerce (sopToEncoding :: a -> Aeson.Encoding)

instance
   (IsProductType' a xs y0 y1 ys, HasDatatypeInfo a, All Aeson.FromJSON xs, All IsMaybe xs)
    => Aeson.FromJSON (Sopica a)
  where
    parseJSON = coerce (sopParseJSON :: Value -> Aeson.Parser a)

-- | Derive generic 'toJSON'
--
-- >>> data R =  R { rFoo :: Int, rBarBar :: String }; $(deriveGeneric ''R)
-- >>> Aeson.encode $ sopToJSON R { rFoo = 42, rBarBar = "Answer" }
-- "{\"foo\":42,\"barBar\":\"Answer\"}"
--
-- /Note:/ single field data isn't supported. Use @'toJSON' . 'coerce'@
--
-- >>> newtype N = N { unN :: Int}; $(deriveGeneric ''N)
-- >>> Aeson.encode $ sopToJSON $ N 42
-- ...
-- ...Couldn't match type...
-- ...
--
-- >>> Aeson.encode $ Aeson.toJSON (coerce (N 42) :: Int)
-- "42"
--
-- /TODO:/ use 'hczipWith' to simplify implementations
sopToJSON
    :: forall a xs y0 y1 ys.
       (IsProductType' a xs y0 y1 ys, HasDatatypeInfo a, All Aeson.ToJSON xs, All IsMaybe xs)
    => a
    -> Aeson.Value
sopToJSON
    = Aeson.object
    . sopToJSON' (longestFieldPrefix p) (fieldInfos p)
    . (^. unsop . unSingletonS)
    . from
  where
    p = Proxy :: Proxy a

sopToJSON'
    :: (All Aeson.ToJSON xs, All IsMaybe xs)
    => String -> NP FieldInfo xs -> NP I xs
    -> [Aeson.Pair]
sopToJSON' prefix = go
  where
    go :: (All Aeson.ToJSON xs, All IsMaybe xs) => NP FieldInfo xs -> NP I xs -> [Aeson.Pair]
    go Nil Nil = []
    go (FieldInfo f :* fs) (I x :* xs) = maybe id (:) (single f x) $ go fs xs
#if __GLASGOW_HASKELL__ < 800
    go _ _ = error "sopToNamedRecord' go: impossible happened"
#endif

    single :: forall x. (Aeson.ToJSON x, IsMaybe x) => String -> x -> Maybe Aeson.Pair
    single f x = case sIsMaybe (Proxy :: Proxy x) of
        SIsMaybe _ -> case x of
            Nothing -> Nothing
            Just _  -> Just $ key Aeson..= x
        SIsNotMaybe -> Just $ key Aeson..= x
      where
        key :: Text
        key = fromString $ processFieldName prefix f

sopToEncoding
    :: forall a xs y0 y1 ys.
       (IsProductType' a xs y0 y1 ys, HasDatatypeInfo a, All Aeson.ToJSON xs, All IsMaybe xs)
    => a
    -> Aeson.Encoding
sopToEncoding
    = Aeson.pairs
    . sopToEncoding' (longestFieldPrefix p) (fieldInfos p)
    . (^. unsop . unSingletonS)
    . from
  where
    p = Proxy :: Proxy a

sopToEncoding'
    :: (All Aeson.ToJSON xs, All IsMaybe xs)
    => String -> NP FieldInfo xs -> NP I xs
    -> Aeson.Series
sopToEncoding' prefix = go
  where
    go :: (All Aeson.ToJSON xs, All IsMaybe xs) => NP FieldInfo xs -> NP I xs -> Aeson.Series
    go Nil Nil = mempty
    go (FieldInfo f :* fs) (I x :* xs) = single f x <> go fs xs
#if __GLASGOW_HASKELL__ < 800
    go _ _ = error "sopToNamedRecord' go: impossible happened"
#endif

    single :: forall x. (Aeson.ToJSON x, IsMaybe x) => String -> x -> Aeson.Series
    single f x = case sIsMaybe (Proxy :: Proxy x) of
        SIsMaybe _ -> case x of
            Nothing -> mempty
            Just _  -> key Aeson..= x
        SIsNotMaybe -> key Aeson..= x
      where
        key :: Text
        key = fromString $ processFieldName prefix f

sopParseJSON
    :: forall a xs y0 y1 ys.
       (IsProductType' a xs y0 y1 ys, HasDatatypeInfo a, All Aeson.FromJSON xs, All IsMaybe xs)
    => Aeson.Value
    -> Aeson.Parser a
sopParseJSON = withObjectDump tName $ \obj ->
    to . SOP . Z <$> sopParseJSON' obj (longestFieldPrefix p) (fieldInfos p)
  where
    p = Proxy :: Proxy a
    dInfo = datatypeInfo p
    tName = dInfo ^. datatypeName

sopParseJSON'
    :: (All Aeson.FromJSON xs, All IsMaybe xs)
    => Aeson.Object -> String -> NP FieldInfo xs
    -> Aeson.Parser (NP I xs)
sopParseJSON' obj prefix = go
  where
    go :: (All Aeson.FromJSON ys, All IsMaybe ys) => NP FieldInfo ys -> Aeson.Parser (NP I ys)
    go Nil  = pure Nil
    go (FieldInfo f :* fs) = (:*) <$> single f <*> go fs

    single :: forall y. (Aeson.FromJSON y, IsMaybe y) => String -> Aeson.Parser (I y)
    single f = I <$> case sIsMaybe (Proxy :: Proxy y) of
        SIsMaybe _  -> case HM.lookup key obj of
            Nothing -> pure Nothing
            _       -> obj Aeson..: key
            -- here we lookup key twice, cannot do better right now.
            -- We cannot deduce @ToJSON c@ from @ToJSON (Maybe c)@.
            -- One way is to encose ToJSON in IsMaybe, but let's not complicate
            -- things right now.
        SIsNotMaybe -> obj Aeson..: key
      where
        key :: Text
        key = fromString $ processFieldName prefix f

-------------------------------------------------------------------------------
-- swagger2
-------------------------------------------------------------------------------

#ifdef MIN_VERSION_swagger2
type SwaggerM = Swagger.Declare (Swagger.Definitions Swagger.Schema)
type SwaggerPP = (Text, Swagger.Referenced Swagger.Schema)

sopDeclareNamedSchema
    :: forall a xs proxy.
       (HasDatatypeInfo a, All Swagger.ToSchema xs, Code a ~ '[xs])
    => proxy a
    -> SwaggerM Swagger.NamedSchema
sopDeclareNamedSchema _ = do
    props <- hsequenceK (hcmap (Proxy :: Proxy Swagger.ToSchema) prop fis) :: SwaggerM (NP (K SwaggerPP) xs)
    pure $ Swagger.NamedSchema (Just $ name ^. packed) $ schema (Exts.fromList . hcollapse $ props)
  where
    name = datatypeInfo proxy ^. datatypeName

    schema props = mempty
      & Swagger.type_ .~ Swagger.SwaggerObject
      & Swagger.properties .~ props
      & Swagger.required .~ hcollapse (hmap req fis)

    prefix :: String
    prefix = longestFieldInfoPrefix fis

    req :: forall y. FieldInfo y -> K Text y
    req (FieldInfo n) = K $ processFieldName prefix n ^. packed

    prop :: forall y. Swagger.ToSchema y => FieldInfo y -> K (SwaggerM SwaggerPP) y
    prop (FieldInfo n) = K $ (,) n' <$> s
      where
        n' = processFieldName prefix n ^. packed
        s = Swagger.declareSchemaRef (Proxy :: Proxy y)

    fis :: NP FieldInfo xs
    fis = datatypeInfo proxy ^. constructorInfo . unSingletonP . fieldInfo

    proxy :: Proxy a
    proxy = Proxy
#endif

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

longestFieldPrefix
    :: forall a proxy xs. (IsProductType a xs, HasDatatypeInfo a)
    => proxy a -> String
longestFieldPrefix = longestFieldInfoPrefix . fieldInfos

fieldInfos
    :: forall a proxy xs. (IsProductType a xs, HasDatatypeInfo a)
    => proxy a -> NP FieldInfo xs
fieldInfos _ = datatypeInfo (Proxy :: Proxy a) ^.
    constructorInfo . unSingletonP . fieldInfo

-- | Get the field names of the record, with stripped prefixes.
--
-- >>> data Foo = Foo { fooCount :: Int, fooName :: Text }; deriveGeneric ''Foo
-- >>> strippedFieldNamesS (Proxy :: Proxy Foo)
-- K "count" :* K "name" :* Nil
--
strippedFieldNames
    :: forall a proxy xs s. (IsProductType a xs, HasDatatypeInfo a, IsString s)
    => proxy a -> NP (K s) xs
strippedFieldNames p = hmap f infos
  where
    f :: forall x. FieldInfo x -> K s x
    f (FieldInfo n) = K (fromString (processFieldName pfx n))

    infos = fieldInfos p
    pfx   = longestFieldInfoPrefix infos

-- | 'String' variant of 'strippedFieldNames'.
strippedFieldNamesS
    :: (IsProductType a xs, HasDatatypeInfo a)
    => proxy a -> NP (K String) xs
strippedFieldNamesS = strippedFieldNames

-- | 'Text' variant of 'strippedFieldNames'.
strippedFieldNamesT
    :: (IsProductType a xs, HasDatatypeInfo a)
    => proxy a -> NP (K String) xs
strippedFieldNamesT = strippedFieldNames



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

-- | /TODO/ use this in "Futurice.Generics"
sopRecordFieldNames
    :: forall a xs. (HasDatatypeInfo a, IsProductType a xs, SListI xs)
    => Proxy a
    -> NP (K Text) xs
sopRecordFieldNames proxy = hmap mk fis
  where
    mk :: forall x. FieldInfo x -> K Text x
    mk (FieldInfo n) = K $ processFieldName prefix n ^. packed

    prefix :: String
    prefix = longestFieldInfoPrefix fis

    fis :: NP FieldInfo xs
    fis = datatypeInfo proxy ^. constructorInfo . unSingletonP . fieldInfo

-------------------------------------------------------------------------------
-- generics-sop-lens
-------------------------------------------------------------------------------

fieldInfo :: Lens' (ConstructorInfo xs) (NP FieldInfo xs)
fieldInfo = lens g s
  where
    g :: ConstructorInfo xs -> NP FieldInfo xs
    g (Record _ fs) = fs
    g cis           = error $ "fieldInfo get: only record supported -- " ++ cis ^. constructorName

    s :: ConstructorInfo xs -> NP FieldInfo xs -> ConstructorInfo xs
    s (Record n _) fs = Record n fs
    s cis _           = error $ "fieldInfo set: only record supported -- " ++ cis ^. constructorName

-------------------------------------------------------------------------------
-- Doctest
-------------------------------------------------------------------------------

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XDataKinds
-- >>> :set -XTypeFamilies
