module Futurice.Aeson (
    withValueDump,
    withObjectDump,
    FromJSONField1 (..),
    module Data.Aeson.Compat,
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Aeson.Compat
import Data.Aeson.Internal (JSONPathElement (..), (<?>))
import Data.Aeson.Types    (modifyFailure)
import Data.Foldable       (foldl')

import qualified Data.HashMap.Strict as HM

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
