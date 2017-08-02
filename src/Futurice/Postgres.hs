{-# LANGUAGE TemplateHaskell #-}
module Futurice.Postgres (queryQQ) where

import Prelude ()
import Futurice.Prelude
import Language.SQL.SimpleSQL.Parser (parseQueryExpr, peFormattedError)
import Language.SQL.SimpleSQL.Syntax (Dialect (SQL2011))
import Language.Haskell.TH (ExpQ, Loc (..), location)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Database.PostgreSQL.Simple (Query)

-- | QuasiQuoter for 'Query', which validates query at the compile-time.
--
-- /Note:/ use only with SELECTs, INSERT aren't supported.
--
-- /Note:/ it would be cool to have variant which checks query against schema.
--
-- @
-- {-# LANGUAGE QuasiQuotes #-}
--
-- getRows :: Connection -> IO [Row]
-- getRows conn = query_ conn [queryQQ|
--      SELECT field1, field2
--      FROM row_table
--      WHERE foo >= 42
--      |]
-- @
queryQQ :: QuasiQuoter
queryQQ = QuasiQuoter
    { quoteExp  = queryExp
    , quotePat  = const $ fail "No quotePat defined for queryQQ"
    , quoteType = const $ fail "No quoteType defined for queryQQ"
    , quoteDec  = const $ fail "No quoteDec defined for queryQQ"
    }

queryExp :: String -> ExpQ
queryExp s = do
    l <- location
    case parseQueryExpr SQL2011 (loc_filename l) (Just (loc_start l)) s of
        Right _ -> [| fromString s :: Query |]
        Left pe -> fail (peFormattedError pe)
