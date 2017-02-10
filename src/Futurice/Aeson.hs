module Futurice.Aeson (
    ParsedAsText (..),
    ParsedAsIntegral (..),
    withValueDump,
    withObjectDump,
    FromJSONField1 (..),
    fromJSONField1,
    module Data.Aeson.Compat,
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Aeson.Compat
import Data.Aeson.Internal (JSONPathElement (..), (<?>))
import Data.Aeson.Types    (modifyFailure, typeMismatch)
import Data.Foldable       (foldl')
import Data.Scientific     (floatingOrInteger)

import qualified Data.Attoparsec.Text as A
import qualified Data.HashMap.Strict  as HM

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Futurice.Prelude
-- >>> import Data.Aeson.Types (parseEither)
-- >>> import Data.Maybe       (fromJust)

-------------------------------------------------------------------------------
-- ParsedAs*
-------------------------------------------------------------------------------

-- | Parse integral values as text.
--
-- >>> eitherDecode "42" :: Either String Text
-- Left ...
--
-- >>> eitherDecode "\"42\"" :: Either String Text
-- Right "42"
--
-- >>> getParsedAsText <$> eitherDecode "42" :: Either String Text
-- Right "42"
--
-- >>> getParsedAsText <$> eitherDecode "\"42\"" :: Either String Text
-- Right "42"
--
newtype ParsedAsText a = ParsedAsText { getParsedAsText :: a }

instance FromJSON a => FromJSON (ParsedAsText a) where
    parseJSON (String s) = ParsedAsText <$> parseJSON (String s)
    parseJSON (Number n) = ParsedAsText <$> parseJSON (String s)
      where
        s = case floatingOrInteger n of
            Left  r -> textShow (r :: Double)
            Right i -> textShow (i :: Int64) -- TODO: should we do toBounded check?
    parseJSON v          = typeMismatch "String/Number as Text" v

instance ToJSON a => ToJSON (ParsedAsText a) where
    toJSON     = toJSON . getParsedAsText
    toEncoding = toEncoding . getParsedAsText

-- | Parse textual values as integral.
--
-- >>> eitherDecode "42" :: Either String Int
-- Right 42
--
-- >>> eitherDecode "\"42\"" :: Either String Int
-- Left ...
--
-- >>> getParsedAsIntegral <$> eitherDecode "42" :: Either String Int
-- Right 42
--
-- >>> getParsedAsIntegral <$> eitherDecode "\"42\"" :: Either String Int
-- Right 42
--
newtype ParsedAsIntegral a = ParsedAsIntegral { getParsedAsIntegral :: a }

instance FromJSON a => FromJSON (ParsedAsIntegral a) where
    parseJSON (Number n) = ParsedAsIntegral <$> parseJSON (Number n)
    parseJSON (String s)
        = either fail (fmap ParsedAsIntegral . parseJSON . Number)
        $ A.parseOnly (integralScientific <* A.endOfInput) s
    parseJSON v          = typeMismatch "Number/String as Integral" v

instance ToJSON a => ToJSON (ParsedAsIntegral a) where
    toJSON     = toJSON . getParsedAsIntegral
    toEncoding = toEncoding . getParsedAsIntegral

{-# INLINE integralScientific #-}
integralScientific :: A.Parser Scientific
integralScientific = fromInteger . snd  <$> A.runScanner 0 f
  where
    f n '0' = Just $ n * 10 + 0
    f n '1' = Just $ n * 10 + 1
    f n '2' = Just $ n * 10 + 2
    f n '3' = Just $ n * 10 + 3
    f n '4' = Just $ n * 10 + 4
    f n '5' = Just $ n * 10 + 5
    f n '6' = Just $ n * 10 + 6
    f n '7' = Just $ n * 10 + 7
    f n '8' = Just $ n * 10 + 8
    f n '9' = Just $ n * 10 + 9
    f _ _   = Nothing

-------------------------------------------------------------------------------
-- withValueDump
-------------------------------------------------------------------------------

-- | Amend error with value shallow dump.
--
-- >>> parseEither (withValueDump "Int" parseJSON) (fromJust $ decode "[1,2,3,[4,5]]") :: Either String Int
-- Left "Error in $: invalid json for Int: [1.0,2.0,3.0,[...]] -- expected Int, encountered Array"
--
-- /Note:/ prints only first 10 items of an array or an object.
withValueDump :: String -> (Value -> Parser a) -> Value -> Parser a
withValueDump n f v = modifyFailure modify (f v)
  where
    modify s
        = showString "invalid json for "
        . showString n
        . showString ": "
        . toplevel v
        . showString " -- "
        . showString s
        $ []

-- | A composition of 'withValueDump' with 'withObject'.
withObjectDump :: String -> (Object -> Parser a) -> Value -> Parser a
withObjectDump name f = withValueDump name $ withObject name f


toplevel :: Value -> String -> String
toplevel = go
  where
    go (Array v) = case take 10 $ toList v of
        []       -> showString "[]"
        (x : xs) ->
            showString "["
            . foldl' (\a b -> a . showString "," . go' b) (go' x) xs
            . showString "]"

    go (Object o) = case take 10 $ itoList o of
        []       -> showString "{}"
        (x : xs) ->
            showString "{"
            . foldl' (\a b -> a . showString "," . kv b) (kv x) xs
            . showString "}"

    go x = go' x

    kv (k, v) = showsPrec 0 k . showString ":" . go' v

    go' Null         = showString "null"
    go' (Number s)   = showsPrec 0 s
    go' (Bool True)  = showString "true"
    go' (Bool False) = showString "false"
    go' (String t)   = showsPrec 0 t
    go' (Array _)    = showString "[...]"
    go' (Object _)   = showString "{...}"

-------------------------------------------------------------------------------
-- FromJSONField
-------------------------------------------------------------------------------

class FromJSONField1 f where
    explicitFromJSONField1 :: Object -> Text -> (Value -> Parser a) -> Parser (f a)

instance FromJSONField1 Proxy where
    explicitFromJSONField1 _ _ _ = pure Proxy

instance FromJSONField1 I where
    explicitFromJSONField1 obj key p = case HM.lookup key obj of
        Nothing -> fail $ "key " ++ show key ++ " not present"
        Just v  -> I <$> p v <?> Key key

instance FromJSONField1 Identity where
    explicitFromJSONField1 obj key p = case HM.lookup key obj of
        Nothing -> fail $ "key " ++ show key ++ " not present"
        Just v  -> Identity <$> p v <?> Key key

-- | Non-presence and @null@ are decoded successfully to 'Nothing'.
instance FromJSONField1 Maybe where
    -- Note: we could use fmap, but this is clearer,
    -- as the layout follows the same pattern as above.
    explicitFromJSONField1 obj key p = case HM.lookup key obj of
        Nothing -> pure Nothing
        Just v  -> Just <$> p v <?> Key key

fromJSONField1
    :: (FromJSONField1 f, FromJSON a)
    => Object -> Text -> Parser (f a)
fromJSONField1 obj key = explicitFromJSONField1 obj key parseJSON
