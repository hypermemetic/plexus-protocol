-- | Low-level transport for Substrate RPC calls
--
-- Pure IO functions for WebSocket communication with the Substrate backend.
-- All calls go through '<backend>.call' for routing.
module Plexus.Transport
  ( -- * RPC Calls (collected)
    rpcCall
  , rpcCallWith

    -- * RPC Calls (streaming)
  , rpcCallStreaming
  , invokeMethodStreaming

    -- * Schema Fetching
  , fetchSchemaAt
  , fetchMethodSchemaAt
  , extractSchema
  , extractSchemaResult

    -- * Method Invocation (collected)
  , invokeMethod
  , invokeRaw
  ) where

import Control.Exception (SomeException, catch)
import Data.Aeson hiding (Error)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Streaming.Prelude as S

import Plexus.Client (SubstrateConfig(..), connect, disconnect, substrateRpc, defaultConfig)
import Plexus.Types (PlexusStreamItem(..))
import Plexus.Schema.Recursive (PluginSchema, MethodSchema, SchemaResult(..), parsePluginSchema, parseSchemaResult)

-- | Low-level RPC call with default localhost config
rpcCall :: Text -> Text -> Value -> IO (Either Text [PlexusStreamItem])
rpcCall backend = rpcCallWith (defaultConfig backend)

-- | Low-level RPC call with custom config
rpcCallWith :: SubstrateConfig -> Text -> Value -> IO (Either Text [PlexusStreamItem])
rpcCallWith cfg method params = do
  result <- (Right <$> doCallInner cfg method params)
    `catch` \(e :: SomeException) ->
      pure $ Left $ T.pack $ "Connection error: " <> show e
  pure result

doCallInner :: SubstrateConfig -> Text -> Value -> IO [PlexusStreamItem]
doCallInner cfg method params = do
  conn <- connect cfg
  items <- S.toList_ $ substrateRpc conn method params
  disconnect conn
  pure items

-- | Streaming RPC call - invokes callback for each item as it arrives
rpcCallStreaming :: SubstrateConfig -> Text -> Value -> (PlexusStreamItem -> IO ()) -> IO (Either Text ())
rpcCallStreaming cfg method params onItem = do
  result <- (Right <$> doCallStreaming cfg method params onItem)
    `catch` \(e :: SomeException) ->
      pure $ Left $ T.pack $ "Connection error: " <> show e
  pure result

doCallStreaming :: SubstrateConfig -> Text -> Value -> (PlexusStreamItem -> IO ()) -> IO ()
doCallStreaming cfg method params onItem = do
  conn <- connect cfg
  S.mapM_ onItem $ substrateRpc conn method params
  disconnect conn

-- | Streaming method invocation
invokeMethodStreaming :: SubstrateConfig -> [Text] -> Text -> Value -> (PlexusStreamItem -> IO ()) -> IO (Either Text ())
invokeMethodStreaming cfg namespacePath method params onItem = do
  let backend = substrateBackend cfg
  let fullPath = if null namespacePath then [backend] else namespacePath
  let dotPath = T.intercalate "." (fullPath ++ [method])
  let callParams = object ["method" .= dotPath, "params" .= params]
  rpcCallStreaming cfg (backend <> ".call") callParams onItem

-- | Fetch schema at a specific path
-- Empty path = root (<backend>.schema)
-- Non-empty path = child schema (e.g., ["solar", "earth"] -> solar.earth.schema)
fetchSchemaAt :: SubstrateConfig -> [Text] -> IO (Either Text PluginSchema)
fetchSchemaAt cfg path = do
  let backend = substrateBackend cfg
  let schemaMethod = if null path
        then backend <> ".schema"
        else T.intercalate "." path <> ".schema"
  result <- rpcCallWith cfg (backend <> ".call") (object ["method" .= schemaMethod])
  case result of
    Left err -> pure $ Left err
    Right items -> pure $ extractSchema items

-- | Extract PluginSchema from stream items
extractSchema :: [PlexusStreamItem] -> Either Text PluginSchema
extractSchema items =
  case [dat | StreamData _ _ ct dat <- items, ".schema" `T.isSuffixOf` ct] of
    (dat:_) -> parsePluginSchema dat
    [] -> case [err | StreamError _ _ err _ <- items] of
      (err:_) -> Left err
      [] -> Left "No schema in response"

-- | Fetch a specific method's schema
-- Uses the parameter-based query: plugin.schema with {"method": "name"}
fetchMethodSchemaAt :: SubstrateConfig -> [Text] -> Text -> IO (Either Text MethodSchema)
fetchMethodSchemaAt cfg path methodName = do
  let backend = substrateBackend cfg
  let schemaMethod = if null path
        then backend <> ".schema"
        else T.intercalate "." path <> ".schema"
  result <- rpcCallWith cfg (backend <> ".call") (object
    [ "method" .= schemaMethod
    , "params" .= object ["method" .= methodName]
    ])
  case result of
    Left err -> pure $ Left err
    Right items -> case extractSchemaResult items of
      Left err -> pure $ Left err
      Right (SchemaMethod m) -> pure $ Right m
      Right (SchemaPlugin _) -> pure $ Left "Expected method schema, got plugin schema"

-- | Extract SchemaResult (plugin or method) from stream items
extractSchemaResult :: [PlexusStreamItem] -> Either Text SchemaResult
extractSchemaResult items =
  case [dat | StreamData _ _ ct dat <- items, ".schema" `T.isSuffixOf` ct] of
    (dat:_) -> parseSchemaResult dat
    [] -> case [err | StreamError _ _ err _ <- items] of
      (err:_) -> Left err
      [] -> Left "No schema in response"

-- | Invoke a method and return stream items
invokeMethod :: SubstrateConfig -> [Text] -> Text -> Value -> IO (Either Text [PlexusStreamItem])
invokeMethod cfg namespacePath method params = do
  let backend = substrateBackend cfg
  let fullPath = if null namespacePath then [backend] else namespacePath
  let dotPath = T.intercalate "." (fullPath ++ [method])
  let callParams = object ["method" .= dotPath, "params" .= params]
  rpcCallWith cfg (backend <> ".call") callParams

-- | Invoke with raw method path
invokeRaw :: SubstrateConfig -> Text -> Value -> IO (Either Text [PlexusStreamItem])
invokeRaw cfg method params = do
  let backend = substrateBackend cfg
  let callParams = object ["method" .= method, "params" .= params]
  rpcCallWith cfg (backend <> ".call") callParams
