{-# LANGUAGE RecordWildCards #-}

-- | WebSocket connection pooling for Plexus RPC
--
-- Maintains a pool of persistent WebSocket connections to avoid
-- the ~200ms overhead of creating a new connection for each RPC call.
--
-- Usage:
--
-- @
-- pool <- createConnectionPool config
-- withPooledConnection pool $ \conn -> do
--   items <- S.toList_ $ substrateRpc conn method params
--   pure items
-- @
module Plexus.Client.Pool
  ( ConnectionPool
  , createConnectionPool
  , destroyConnectionPool
  , withPooledConnection
  , defaultPlexusPoolConfig
  , PlexusPoolConfig(..)
  ) where

import Control.Exception (SomeException, catch, finally)
import Control.Monad (void)
import Data.Pool (Pool, newPool, defaultPoolConfig, setNumStripes, withResource, destroyAllResources)
import qualified Data.Pool as Pool
import Data.Time.Clock (NominalDiffTime)

import Plexus.Client
  ( SubstrateConfig
  , SubstrateConnection
  , connect
  , disconnect
  )

-- | Connection pool configuration
data PlexusPoolConfig = PlexusPoolConfig
  { poolStripes   :: Int              -- ^ Number of stripes (sub-pools)
  , poolMaxResources :: Int           -- ^ Max connections per stripe
  , poolIdleTime  :: NominalDiffTime  -- ^ How long to keep idle connections
  }
  deriving stock (Show, Eq)

-- | Default pool configuration
-- - 1 stripe (simple pool)
-- - 10 max connections
-- - 30 second idle timeout
defaultPlexusPoolConfig :: PlexusPoolConfig
defaultPlexusPoolConfig = PlexusPoolConfig
  { poolStripes = 1
  , poolMaxResources = 10
  , poolIdleTime = 30
  }

-- | Connection pool for WebSocket connections
type ConnectionPool = Pool SubstrateConnection

-- | Create a connection pool
--
-- The pool will maintain persistent WebSocket connections and reuse them
-- across multiple RPC calls, avoiding the 200ms connection setup overhead.
createConnectionPool :: SubstrateConfig -> PlexusPoolConfig -> IO ConnectionPool
createConnectionPool cfg PlexusPoolConfig{..} =
  newPool $ setNumStripes (Just poolStripes)
          $ Pool.defaultPoolConfig
              (connect cfg)                    -- Create connection
              disconnect                       -- Destroy connection
              (realToFrac poolIdleTime)        -- Idle timeout (convert to Double)
              poolMaxResources                 -- Max resources per stripe

-- | Destroy all connections in the pool
destroyConnectionPool :: ConnectionPool -> IO ()
destroyConnectionPool = destroyAllResources

-- | Use a connection from the pool
--
-- The connection is automatically returned to the pool when the action completes,
-- even if an exception is thrown.
withPooledConnection :: ConnectionPool -> (SubstrateConnection -> IO a) -> IO a
withPooledConnection = withResource
