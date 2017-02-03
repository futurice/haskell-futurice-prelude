-- | Extensions to "Data.UUID".
module Futurice.UUID (
    uuidWords,
    ) where

import Prelude ()
import Prelude.Compat ()

import           Control.Lens (Iso', iso)
import qualified Data.UUID    as UUID
import           Data.Word    (Word32)

-- $setup
-- >>> import Futurice.Prelude
-- >>> import qualified Data.UUID

-- | Isomorphism between 'UUID' and 'Word32' quadruple.
--
-- >>> Data.UUID.nil ^. uuidWords
-- (0,0,0,0)
--
uuidWords :: Iso' UUID.UUID (Word32, Word32, Word32, Word32)
uuidWords = iso UUID.toWords fromWords'
  where
    fromWords' :: (Word32, Word32, Word32, Word32) -> UUID.UUID
    fromWords' (a, b, c, d) = UUID.fromWords a b c d
