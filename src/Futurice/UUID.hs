-- | Extensions to "Data.UUID".
module Futurice.UUID (
    uuidWords,
    ) where

import Futurice.Prelude.Internal
import Prelude ()

import qualified Data.UUID.Types as UUID

-- $setup
-- >>> import Futurice.Prelude
-- >>> import qualified Data.UUID.Types

-- | Isomorphism between 'UUID' and 'Word32' quadruple.
--
-- >>> Data.UUID.Types.nil ^. uuidWords
-- (0,0,0,0)
--
uuidWords :: Iso' UUID.UUID (Word32, Word32, Word32, Word32)
uuidWords = iso UUID.toWords fromWords'
  where
    fromWords' :: (Word32, Word32, Word32, Word32) -> UUID.UUID
    fromWords' (a, b, c, d) = UUID.fromWords a b c d
