module Futurice.Aeson (
    withValueDump,
    module Data.Aeson.Compat,
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Aeson.Compat
import Data.Foldable     (foldl')

-- | TODO: improve when https://github.com/bos/aeson/pull/483 is merged
--
-- >>> parseEither (withValueDump parseJSON) (fromJust $ decode "[1,2,3,[4,5]]") :: Either String Int
-- Left "Error in $: invalid json: [1.0,2.0,3.0,[...]]"
withValueDump :: (Value -> Parser a) -> Value -> Parser a
withValueDump f v = f v <|> fail ((++) "invalid json: " . toplevel v $ [])

toplevel :: Value -> String -> String
toplevel = go
  where
    go (Array v) = case toList v of
        []       -> showString "[]"
        (x : xs) ->
            showString "["
            . foldl' (\a b -> a . showString "," . go' b) (go' x) xs
            . showString "]"

    go (Object o) = case itoList o of
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
