{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
-- | Wrappers of "Data.PostgreSQL.Simple" query and execution
-- functions, to work with @'Pool' 'Database.PostgreSQL.Simple.Connection'@.
--
-- Note: do not use this with transactions; they won't work.
module Futurice.PostgresPool (
    -- * Class
    HasPostgresPool(..),
    -- * Query
    poolQuery,
    poolQuery_,
    -- * Execute
    poolExecute,
    poolExecute_,
    poolExecuteMany,
    -- * Transaction
    poolWithTransaction,
    ) where

import Prelude ()
import Futurice.Prelude

import           Data.Pool                  (Pool, withResource)
import qualified Database.PostgreSQL.Simple as Postgres

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

class HasPostgresPool a where
    postgresPool :: a -> Pool Postgres.Connection

instance conn ~ Postgres.Connection => HasPostgresPool (Pool conn) where
    postgresPool = id

-------------------------------------------------------------------------------
-- Execute
-------------------------------------------------------------------------------

poolExecute
    :: (Postgres.ToRow q, HasPostgresPool ctx, MonadBaseControl IO m)
    => ctx -> Postgres.Query -> q -> m Int64
poolExecute ctx query row = withResource (postgresPool ctx) $ \conn ->
    liftBase $ Postgres.execute conn query row

poolExecute_
    :: (HasPostgresPool ctx, MonadBaseControl IO m)
    => ctx -> Postgres.Query -> m Int64
poolExecute_ ctx query = withResource (postgresPool ctx) $ \conn ->
    liftBase $ Postgres.execute_ conn query

poolExecuteMany
    :: (Postgres.ToRow q, HasPostgresPool ctx, MonadBaseControl IO m)
    => ctx -> Postgres.Query -> [q] -> m Int64
poolExecuteMany ctx query rows = withResource (postgresPool ctx) $ \conn ->
    liftBase $ Postgres.executeMany conn query rows

-------------------------------------------------------------------------------
-- Query
-------------------------------------------------------------------------------

poolQuery
    :: (Postgres.ToRow q, Postgres.FromRow r, HasPostgresPool ctx, MonadBaseControl IO m)
    => ctx -> Postgres.Query -> q -> m [r]
poolQuery ctx query row = withResource (postgresPool ctx) $ \conn ->
    liftBase $ Postgres.query conn query row

poolQuery_
    :: (Postgres.FromRow r, HasPostgresPool ctx, MonadBaseControl IO m)
    => ctx -> Postgres.Query -> m [r]
poolQuery_ ctx query = withResource (postgresPool ctx) $ \conn ->
    liftBase $ Postgres.query_ conn query

-------------------------------------------------------------------------------
-- Transaction
-------------------------------------------------------------------------------

poolWithTransaction
    :: (HasPostgresPool ctx, MonadBaseControl IO m, StM m a ~ a)
    => ctx -> m a -> m a
poolWithTransaction ctx inner = withResource (postgresPool ctx) $ \conn ->
    liftBaseWith $ \runInBase ->
        Postgres.withTransaction conn (runInBase inner)
