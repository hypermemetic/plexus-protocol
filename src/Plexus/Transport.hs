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

    -- * Bidirectional Response
  , sendBidirectionalResponse
  ) where

import Control.Exception (SomeException, catch)
import Data.Aeson hiding (Error)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Streaming.Prelude as S

import Plexus.Client (SubstrateConfig(..), connect, disconnect, substrateRpc, defaultConfig)
import Plexus.Types (PlexusStreamItem(..), StandardResponse, TransportError(..))
import Plexus.Schema.Recursive (PluginSchema, MethodSchema, SchemaResult(..), parsePluginSchema, parseSchemaResult)

-- | Low-level RPC call with default localhost config
rpcCall :: Text -> Text -> Value -> IO (Either TransportError [PlexusStreamItem])
rpcCall backend = rpcCallWith (defaultConfig backend)

-- | Low-level RPC call with custom config
rpcCallWith :: SubstrateConfig -> Text -> Value -> IO (Either TransportError [PlexusStreamItem])
rpcCallWith cfg method params = do
  result <- (Right <$> doCallInner cfg method params)
    `catch` \(e :: SomeException) ->
      pure $ Left $ NetworkError $ T.pack $ "Connection error: " <> show e
  pure result

doCallInner :: SubstrateConfig -> Text -> Value -> IO [PlexusStreamItem]
doCallInner cfg method params = do
  conn <- connect cfg
  items <- S.toList_ $ substrateRpc conn method params
  disconnect conn
  pure items

-- | Streaming RPC call - invokes callback for each item as it arrives
rpcCallStreaming :: SubstrateConfig -> Text -> Value -> (PlexusStreamItem -> IO ()) -> IO (Either TransportError ())
rpcCallStreaming cfg method params onItem = do
  result <- (Right <$> doCallStreaming cfg method params onItem)
    `catch` \(e :: SomeException) ->
      pure $ Left $ NetworkError $ T.pack $ "Connection error: " <> show e
  pure result

doCallStreaming :: SubstrateConfig -> Text -> Value -> (PlexusStreamItem -> IO ()) -> IO ()
doCallStreaming cfg method params onItem = do
  conn <- connect cfg
  S.mapM_ onItem $ substrateRpc conn method params
  disconnect conn

-- | Streaming method invocation
invokeMethodStreaming :: SubstrateConfig -> [Text] -> Text -> Value -> (PlexusStreamItem -> IO ()) -> IO (Either TransportError ())
invokeMethodStreaming cfg namespacePath method params onItem = do
  let backend = substrateBackend cfg
  let fullPath = if null namespacePath then [backend] else namespacePath
  let dotPath = T.intercalate "." (fullPath ++ [method])
  let callParams = object ["method" .= dotPath, "params" .= params]
  rpcCallStreaming cfg (backend <> ".call") callParams onItem

-- | Fetch schema at a specific path
-- Empty path = root (<backend>.schema)
-- Non-empty path = child schema (e.g., ["solar", "earth"] -> solar.earth.schema)
fetchSchemaAt :: SubstrateConfig -> [Text] -> IO (Either TransportError PluginSchema)
fetchSchemaAt cfg path = do
  let backend = substrateBackend cfg
  let schemaMethod = if null path
        then backend <> ".schema"
        else T.intercalate "." path <> ".schema"
  result <- rpcCallWith cfg (backend <> ".call") (object ["method" .= schemaMethod])
  case result of
    Left err -> pure $ Left err
    Right items -> case extractSchema items of
      Left errMsg -> pure $ Left $ ProtocolError errMsg
      Right schema -> pure $ Right schema

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
fetchMethodSchemaAt :: SubstrateConfig -> [Text] -> Text -> IO (Either TransportError MethodSchema)
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
      Left err -> pure $ Left $ ProtocolError err
      Right (SchemaMethod m) -> pure $ Right m
      Right (SchemaPlugin _) -> pure $ Left $ ProtocolError "Expected method schema, got plugin schema"

-- | Extract SchemaResult (plugin or method) from stream items
extractSchemaResult :: [PlexusStreamItem] -> Either Text SchemaResult
extractSchemaResult items =
  case [dat | StreamData _ _ ct dat <- items, ".schema" `T.isSuffixOf` ct] of
    (dat:_) -> parseSchemaResult dat
    [] -> case [err | StreamError _ _ err _ <- items] of
      (err:_) -> Left err
      [] -> Left "No schema in response"

-- | Invoke a method and return stream items
invokeMethod :: SubstrateConfig -> [Text] -> Text -> Value -> IO (Either TransportError [PlexusStreamItem])
invokeMethod cfg namespacePath method params = do
  let backend = substrateBackend cfg
  let fullPath = if null namespacePath then [backend] else namespacePath
  let dotPath = T.intercalate "." (fullPath ++ [method])
  let callParams = object ["method" .= dotPath, "params" .= params]
  rpcCallWith cfg (backend <> ".call") callParams

-- | Invoke with raw method path
invokeRaw :: SubstrateConfig -> Text -> Value -> IO (Either TransportError [PlexusStreamItem])
invokeRaw cfg method params = do
  let backend = substrateBackend cfg
  let callParams = object ["method" .= method, "params" .= params]
  rpcCallWith cfg (backend <> ".call") callParams

-- | Send a bidirectional response back to the backend
sendBidirectionalResponse :: SubstrateConfig -> Text -> StandardResponse -> IO (Either TransportError ())
sendBidirectionalResponse cfg requestId response = do
  let backend = substrateBackend cfg
  let respondMethod = backend <> ".respond"
  let params = object
        [ "request_id" .= requestId
        , "response" .= response
        ]
  result <- (Right <$> doCallInner cfg respondMethod params)
    `catch` \(e :: SomeException) -> do
      -- Parse the exception to create a proper TransportError
      let errMsg = T.pack $ show e
      pure $ Left $ NetworkError errMsg
  case result of
    Left err -> pure $ Left err
    Right _ -> pure $ Right ()
