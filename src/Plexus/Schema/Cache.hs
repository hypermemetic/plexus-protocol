-- | Schema caching for the dynamic CLI
--
-- Caches the PlexusSchema and enriched schemas to disk to avoid fetching on every invocation.
--
-- = Termination Guarantees
--
-- This module mixes pure and I/O functions:
--
-- * __Pure functions__ (isFresh, splitMethod, lookupMethodSchema): Provably terminate
-- * __File I/O__ (loadCache, saveCache): Bounded by OS file operations
-- * __Orchestration__ (loadSchemaWithCache): Delegates timeout to fetch functions
--
-- == Audit Status
--
-- ✅ Pure functions provably terminate
-- ✅ File I/O has exception handling
-- ✅ Fetch timeouts delegated to Transport layer
-- ✅ No unnecessary timeouts
--
module Plexus.Schema.Cache
  ( -- * Types
    CachedSchema(..)
  , CacheConfig(..)
    -- * Cache Operations
  , defaultCacheConfig
  , defaultCachePath
  , loadCache
  , saveCache
  , isFresh
  , loadSchemaWithCache
    -- * Enriched Schema Lookup
  , lookupMethodSchema
  ) where

import Control.Exception (SomeException, catch)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist, getXdgDirectory, XdgDirectory(..))
import System.FilePath ((</>), takeDirectory)

import qualified Plexus.Schema
import Plexus.Schema (PlexusSchema(..), ActivationInfo(..), EnrichedSchema, ActivationFullSchema(..), MethodSchemaInfo(..), MethodSchema(..), parseMethodSchemas, PlexusHash(..), PlexusHashEvent(..), extractHashEvent)
import Plexus.Types (PlexusStreamItem)

-- ============================================================================
-- Types
-- ============================================================================

-- | Cached schema with metadata
data CachedSchema = CachedSchema
  { cachedHash        :: Text                             -- ^ Plexus hash for invalidation
  , cachedSchema      :: PlexusSchema                     -- ^ The cached plexus schema
  , cachedFullSchemas :: Map Text ActivationFullSchema    -- ^ Full schemas by namespace
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CachedSchema where
  parseJSON = withObject "CachedSchema" $ \o ->
    CachedSchema
      <$> o .: "hash"
      <*> o .: "schema"
      <*> o .:? "full_schemas" .!= Map.empty

instance ToJSON CachedSchema where
  toJSON CachedSchema{..} = object
    [ "hash"         .= cachedHash
    , "schema"       .= cachedSchema
    , "full_schemas" .= cachedFullSchemas
    ]

-- | Cache configuration
data CacheConfig = CacheConfig
  { cachePath :: FilePath  -- ^ Path to cache file
  }
  deriving stock (Show, Eq)

-- ============================================================================
-- Configuration
-- ============================================================================

-- | Default cache configuration
-- Uses ~/.cache/symbols/schema.json on Linux/macOS
defaultCacheConfig :: IO CacheConfig
defaultCacheConfig = do
  path <- defaultCachePath
  pure CacheConfig
    { cachePath = path
    }

-- | Get the default cache file path
-- Respects XDG_CACHE_HOME on Linux/macOS
defaultCachePath :: IO FilePath
defaultCachePath = do
  cacheDir <- getXdgDirectory XdgCache "symbols"
  pure $ cacheDir </> "schema.json"

-- ============================================================================
-- Cache Operations
-- ============================================================================

-- | Load cached schema from disk
loadCache :: FilePath -> IO (Maybe CachedSchema)
loadCache path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      result <- (Just <$> LBS.readFile path) `catch` \(_ :: SomeException) -> pure Nothing
      case result of
        Nothing -> pure Nothing
        Just bs -> case eitherDecode bs of
          Left _err -> pure Nothing
          Right cached -> pure (Just cached)

-- | Save schema to cache
saveCache :: FilePath -> CachedSchema -> IO ()
saveCache path cached = do
  createDirectoryIfMissing True (takeDirectory path)
  LBS.writeFile path (encode cached)
    `catch` \(_ :: SomeException) -> pure ()  -- Silently fail on write errors

-- | Check if a cached schema is still fresh (hash matches current hash)
isFresh :: Text -> CachedSchema -> Bool
isFresh currentHash cached = cachedHash cached == currentHash

-- | Load schema with caching
--
-- 1. If forceRefresh is True, skip cache and fetch fresh
-- 2. Otherwise, fetch current hash and compare with cached hash
-- 3. If hashes match, return cached schema
-- 4. Otherwise, fetch fresh schema and cache it with new hash
loadSchemaWithCache
  :: Bool                                       -- ^ Force refresh (--refresh flag)
  -> CacheConfig                                -- ^ Cache configuration
  -> IO (Either Text Text)                      -- ^ Fetch plexus hash
  -> IO (Either Text PlexusSchema)              -- ^ Fetch plexus schema
  -> (Text -> IO (Maybe ActivationFullSchema))  -- ^ Fetch full schema for a namespace
  -> IO (Either Text CachedSchema)
loadSchemaWithCache forceRefresh config fetchHash fetchSchema fetchFullSchema = do
  if forceRefresh
    then fetchAndCache
    else do
      -- Fetch current hash
      hashResult <- fetchHash
      case hashResult of
        Left err -> do
          -- If we can't get hash, try to use cache anyway
          mCached <- loadCache (cachePath config)
          case mCached of
            Just cached -> pure $ Right cached
            Nothing -> pure $ Left err
        Right currentHash -> do
          -- Load cached schema
          mCached <- loadCache (cachePath config)
          case mCached of
            Nothing -> fetchAndCacheWithHash currentHash
            Just cached ->
              if isFresh currentHash cached
                then pure $ Right cached
                else fetchAndCacheWithHash currentHash
  where
    fetchAndCache = do
      hashResult <- fetchHash
      case hashResult of
        Left err -> pure $ Left err
        Right hash -> fetchAndCacheWithHash hash

    fetchAndCacheWithHash hash = do
      result <- fetchSchema
      case result of
        Left err -> do
          -- On fetch failure, try stale cache as fallback
          mCached <- loadCache (cachePath config)
          case mCached of
            Just cached -> pure $ Right cached  -- Use stale cache
            Nothing -> pure $ Left err  -- No cache available
        Right schema -> do
          -- Fetch full schemas for all activations
          fullSchemas <- fetchAllFullSchemas schema
          let cached = CachedSchema
                { cachedHash        = hash
                , cachedSchema      = schema
                , cachedFullSchemas = fullSchemas
                }
          saveCache (cachePath config) cached
          pure $ Right cached

    fetchAllFullSchemas schema = do
      let namespaces = map activationNamespace (schemaActivations schema)
      pairs <- mapM fetchPair namespaces
      pure $ Map.fromList [(ns, fs) | (ns, Just fs) <- pairs]

    fetchPair ns = do
      mSchema <- fetchFullSchema ns
      pure (ns, mSchema)

-- ============================================================================
-- Full Schema Lookup
-- ============================================================================

-- | Look up a method schema from the cached full schemas
-- Given "arbor_tree_create", looks up "arbor" full schema and finds "tree_create" method
lookupMethodSchema :: CachedSchema -> Text -> Maybe MethodSchemaInfo
lookupMethodSchema cached fullMethod = do
  -- Split "arbor_tree_create" into ("arbor", "tree_create")
  let (ns, method) = splitMethod fullMethod
  -- Look up the full schema for this namespace
  fullSchema <- Map.lookup ns (cachedFullSchemas cached)
  -- Find the matching method
  findMethod method (fullSchemaMethods fullSchema)

-- | Split "namespace_method" into (namespace, method)
-- e.g., "arbor_tree_create" -> ("arbor", "tree_create")
splitMethod :: Text -> (Text, Text)
splitMethod full =
  case T.break (== '_') full of
    (ns, rest) | not (T.null rest) -> (ns, T.drop 1 rest)
    _ -> (full, "")

-- | Find a method by name in a list of method schema infos
findMethod :: Text -> [MethodSchemaInfo] -> Maybe MethodSchemaInfo
findMethod name = find (\m -> methodInfoName m == name)
  where
    find f = foldr (\x acc -> if f x then Just x else acc) Nothing
