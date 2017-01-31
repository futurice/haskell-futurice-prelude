{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
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
    sopToEncoding,
    sopParseJSON,
    -- * Swagger
    Swagger.ToSchema(..),
    sopDeclareNamedSchema,
    -- * Internal
    sopRecordFieldNames,
    ) where

import Prelude ()
import Futurice.Prelude  hiding (Generic, from)
import Data.Char         (toLower)
import Futurice.Aeson    (withObjectDump)
import Futurice.IsMaybe
import Generics.SOP      hiding (constructorInfo, datatypeName)
import Generics.SOP.Lens

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as Aeson
import qualified Data.Csv             as Csv
import qualified Data.HashMap.Strict  as HM
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



-- | /TODO/ use hczipWith to simplify implemenetations
sopToJSON
    :: forall a xs.
       (Generic a, HasDatatypeInfo a, All Aeson.ToJSON xs, All IsMaybe xs, Code a ~ '[xs])
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
    :: (All Aeson.ToJSON xs, All IsMaybe xs) => NP FieldInfo xs -> NP I xs
    -> [Aeson.Pair]
sopToJSON' fs' xs' = go fs' xs'
  where
    prefix :: String
    prefix = longestFieldInfoPrefix fs'

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
    :: forall a xs.
       (Generic a, HasDatatypeInfo a, All Aeson.ToJSON xs, All IsMaybe xs, Code a ~ '[xs])
    => a
    -> Aeson.Encoding
sopToEncoding
    = Aeson.pairs
    . sopToEncoding' fieldInfos
    . (^. unsop . unSingletonS)
    . from
  where
    fieldInfos = datatypeInfo (Proxy :: Proxy a) ^.
        constructorInfo . unSingletonP . fieldInfo

sopToEncoding'
    :: (All Aeson.ToJSON xs, All IsMaybe xs) => NP FieldInfo xs -> NP I xs
    -> Aeson.Series
sopToEncoding' fs' xs' = go fs' xs'
  where
    prefix :: String
    prefix = longestFieldInfoPrefix fs'

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
    :: forall a xs.
       (Generic a, HasDatatypeInfo a, All Aeson.FromJSON xs, All IsMaybe xs, Code a ~ '[xs])
    => Aeson.Value
    -> Aeson.Parser a
sopParseJSON = withObjectDump tName $ \obj ->
    to . SOP . Z <$> sopParseJSON' obj fieldInfos
  where
    dInfo = datatypeInfo (Proxy :: Proxy a)
    tName = dInfo ^. datatypeName
    fieldInfos = dInfo ^. constructorInfo . unSingletonP . fieldInfo

sopParseJSON'
    :: (All Aeson.FromJSON xs, All IsMaybe xs)
    => Aeson.Object -> NP FieldInfo xs -> Aeson.Parser (NP I xs)
sopParseJSON' obj fs' = go fs'
  where
    prefix :: String
    prefix = longestFieldInfoPrefix fs'

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

-- | /TODO/ use this in "Futurice.Generics"
sopRecordFieldNames
    :: forall a xs. (HasDatatypeInfo a, Code a ~ '[xs], SListI xs)
    => Proxy a
    -> NP (K Text) xs
sopRecordFieldNames proxy = hmap mk fieldInfos
  where
    mk :: forall x. FieldInfo x -> K Text x
    mk (FieldInfo n) = K $ processFieldName prefix n ^. packed

    prefix :: String
    prefix = longestFieldInfoPrefix fieldInfos

    fieldInfos :: NP FieldInfo xs
    fieldInfos = datatypeInfo proxy ^. constructorInfo . unSingletonP . fieldInfo

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
