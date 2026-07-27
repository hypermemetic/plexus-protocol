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
  , invokeMethodStreamingCancellable

    -- * Bidirectional Response
  , sendBidirectionalResponse

    -- * Schema Fetching
  , fetchSchemaAt
  , fetchMethodSchemaAt
  , extractSchema
  , extractSchemaResult

    -- * Connectome Fetching (PLX-121 / PLX-142)
  , fetchConnectomeDocument
  , connectomeMethodName

    -- * Method Invocation (collected)
  , invokeMethod
  , invokeRaw
  ) where

import Control.Exception (SomeException, IOException, catch, fromException)
import qualified Control.Exception as E
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Data.Aeson hiding (Error)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Streaming.Prelude as S
import qualified Network.Socket as NS
import System.IO.Unsafe (unsafePerformIO)

import Plexus.Client (SubstrateConfig(..), CancelTurn(..), connect, disconnect, substrateRpc, substrateRpcCancellable, defaultConfig)
import Plexus.Client.Pool (ConnectionPool, createConnectionPool, withPooledConnection, defaultPlexusPoolConfig)
import Plexus.Types (PlexusStreamItem(..), TransportError(..), Response(..), StandardResponse)
import Data.List (find)
import Plexus.Schema.Recursive (PluginSchema, MethodSchema, SchemaResult(..), parsePluginSchema, parseSchemaResult, psMethods, methodName)

-- | Global connection pool cache
-- One pool per unique SubstrateConfig to enable connection reuse across calls
{-# NOINLINE poolCache #-}
poolCache :: MVar (Map SubstrateConfig ConnectionPool)
poolCache = unsafePerformIO (newMVar Map.empty)

-- | Get or create a connection pool for the given config
getOrCreatePool :: SubstrateConfig -> IO ConnectionPool
getOrCreatePool cfg = modifyMVar poolCache $ \pools ->
  case Map.lookup cfg pools of
    Just pool -> pure (pools, pool)
    Nothing -> do
      pool <- createConnectionPool cfg defaultPlexusPoolConfig
      pure (Map.insert cfg pool pools, pool)

-- | Low-level RPC call with default localhost config
rpcCall :: Text -> Text -> Value -> IO (Either TransportError [PlexusStreamItem])
rpcCall backend = rpcCallWith (defaultConfig backend)

-- | Low-level RPC call with custom config
rpcCallWith :: SubstrateConfig -> Text -> Value -> IO (Either TransportError [PlexusStreamItem])
rpcCallWith cfg method params = do
  result <- (Right <$> doCallInner cfg method params)
    `catch` categorizeException cfg
  pure result

-- | Categorize exceptions into typed TransportError
categorizeException :: SubstrateConfig -> SomeException -> IO (Either TransportError a)
categorizeException cfg e
  -- Check for connection refused (most common)
  | Just ioErr <- fromException e :: Maybe IOException = do
      let errMsg = show ioErr
      let host = T.pack $ substrateHost cfg
      let port = substratePort cfg
      pure $ Left $ if "refused" `isInfixOf` errMsg || "ECONNREFUSED" `isInfixOf` errMsg
        then ConnectionRefused host port
        else if "timeout" `isInfixOf` errMsg || "ETIMEDOUT" `isInfixOf` errMsg
          then ConnectionTimeout host port
          else NetworkError (T.pack errMsg)

  -- Catch protocol/parse errors
  | otherwise =
      let errMsg = show e
          host = T.pack $ substrateHost cfg
          port = substratePort cfg
      in pure $ Left $ if "protocol" `isInfixOf` errMsg || "parse" `isInfixOf` errMsg
        then ProtocolError (T.pack errMsg)
        else NetworkError (T.pack errMsg)

  where
    isInfixOf = T.isInfixOf `on` (T.toLower . T.pack)
    on f g x y = f (g x) (g y)

doCallInner :: SubstrateConfig -> Text -> Value -> IO [PlexusStreamItem]
doCallInner cfg method params = do
  pool <- getOrCreatePool cfg
  withPooledConnection pool $ \conn ->
    S.toList_ $ substrateRpc conn method params

-- | Streaming RPC call - invokes callback for each item as it arrives
rpcCallStreaming :: SubstrateConfig -> Text -> Value -> (PlexusStreamItem -> IO ()) -> IO (Either TransportError ())
rpcCallStreaming cfg method params onItem = do
  result <- (Right <$> doCallStreaming cfg method params onItem)
    `catch` categorizeException cfg
  pure result

doCallStreaming :: SubstrateConfig -> Text -> Value -> (PlexusStreamItem -> IO ()) -> IO ()
doCallStreaming cfg method params onItem =
  doCallStreamingCancellable (\_ -> pure ()) cfg method params onItem

doCallStreamingCancellable
  :: (CancelTurn -> IO ()) -> SubstrateConfig -> Text -> Value
  -> (PlexusStreamItem -> IO ()) -> IO ()
doCallStreamingCancellable onCancel cfg method params onItem = do
  pool <- getOrCreatePool cfg
  withPooledConnection pool $ \conn ->
    S.mapM_ onItem $ substrateRpcCancellable onCancel conn method params

-- | Streaming method invocation
invokeMethodStreaming :: SubstrateConfig -> [Text] -> Text -> Value -> (PlexusStreamItem -> IO ()) -> IO (Either TransportError ())
invokeMethodStreaming cfg namespacePath method params onItem = do
  let backend = substrateBackend cfg
  let fullPath = if null namespacePath then [backend] else namespacePath
  let dotPath = T.intercalate "." (fullPath ++ [method])
  let callParams = object ["method" .= dotPath, "params" .= params]
  rpcCallStreaming cfg (backend <> ".call") callParams onItem

-- | PLX-123: 'invokeMethodStreaming', plus the cancel handle for the
-- subscription this call opens. See 'Plexus.Client.substrateRpcCancellable'
-- for the channel decision and exactly what it does not assert.
invokeMethodStreamingCancellable
  :: (CancelTurn -> IO ())
  -> SubstrateConfig -> [Text] -> Text -> Value
  -> (PlexusStreamItem -> IO ()) -> IO (Either TransportError ())
invokeMethodStreamingCancellable onCancel cfg namespacePath method params onItem = do
  let backend = substrateBackend cfg
  let fullPath = if null namespacePath then [backend] else namespacePath
  let dotPath = T.intercalate "." (fullPath ++ [method])
  let callParams = object ["method" .= dotPath, "params" .= params]
  (Right <$> doCallStreamingCancellable onCancel cfg (backend <> ".call") callParams onItem)
    `catch` categorizeException cfg

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
    Left transportErr -> pure $ Left transportErr
    Right items -> case extractSchema items of
      Left parseErr -> pure $ Left $ ProtocolError parseErr  -- Parse errors are protocol errors
      Right schema -> pure $ Right schema

-- | Extract PluginSchema from stream items
-- Returns Either Text for application-level errors (not transport)
extractSchema :: [PlexusStreamItem] -> Either Text PluginSchema
extractSchema items =
  case [dat | StreamData _ _ ct dat <- items, ".schema" `T.isSuffixOf` ct] of
    (dat:_) -> parsePluginSchema dat
    [] -> case [err | StreamError _ _ err _ <- items] of
      (err:_) -> Left err
      [] -> Left "No schema in response"

-- | Fetch a specific method's schema.
--
-- PROT schema unification (PLX-13): the server no longer returns a bare
-- MethodSchema (the @SchemaMethod@ result variant is gone). We fetch the unified
-- 'PluginSchema' at the path and drill into 'psMethods' for the requested method.
fetchMethodSchemaAt :: SubstrateConfig -> [Text] -> Text -> IO (Either TransportError MethodSchema)
fetchMethodSchemaAt cfg path wantMethod = do
  let backend = substrateBackend cfg
  let schemaMethod = if null path
        then backend <> ".schema"
        else T.intercalate "." path <> ".schema"
  result <- rpcCallWith cfg (backend <> ".call") (object
    [ "method" .= schemaMethod
    , "params" .= object []
    ])
  case result of
    Left transportErr -> pure $ Left transportErr
    Right items -> case extractSchemaResult items of
      Left parseErr -> pure $ Left $ ProtocolError parseErr
      Right (SchemaPlugin plugin) ->
        case find ((== wantMethod) . methodName) (psMethods plugin) of
          Just m  -> pure $ Right m
          Nothing -> pure $ Left $ ProtocolError
            ("Method '" <> wantMethod <> "' not found in unified schema")

-- | The wire method that serves a CONNECTOME RFC 002 document (PLX-142).
--
-- It is registered once per hub as @{hubNamespace}.connectome@ — there is no
-- per-activation @{ns}.connectome@ and no separate @child_connectome@ method;
-- the lazy child fetch is the SAME method with a @namespace@ parameter.
-- Verified against a live substrate: @solar.connectome@ answers @-32601 Method
-- not found@, @substrate.connectome {"namespace":"solar"}@ answers the subtree.
connectomeMethodName :: SubstrateConfig -> Text
connectomeMethodName cfg = substrateBackend cfg <> ".connectome"

-- | Fetch a Connectome document.
--
-- @Nothing@ fetches the hub's own whole-tree document; @Just ns@ fetches the
-- child at that namespace as a document in its own right (RFC §5.1's lazy
-- Dynamic-edge fetch).
--
-- Returns the raw 'Value' rather than a decoded document on purpose:
-- plexus-protocol owns the wire, and @plexus-connectome@ owns the model. This
-- package does not depend on the latter, so the ONE document model is not
-- forked here.
fetchConnectomeDocument
  :: SubstrateConfig
  -> Maybe Text
  -> IO (Either TransportError Value)
fetchConnectomeDocument cfg mNamespace = do
  let method = connectomeMethodName cfg
      params = case mNamespace of
        Nothing -> object []
        Just ns -> object ["namespace" .= ns]
  result <- rpcCallWith cfg method params
  case result of
    Left transportErr -> pure $ Left transportErr
    Right items -> pure $ extractConnectome method items

-- | Pull the document out of the subscription frames.
--
-- The server tags the frame with @content_type = {ns}.connectome@ (deliberately
-- NOT @*.schema@, so nothing tries to decode it as a legacy PluginSchema).
extractConnectome :: Text -> [PlexusStreamItem] -> Either TransportError Value
extractConnectome method items =
  case [dat | StreamData _ _ ct dat <- items, ct == method] of
    (dat:_) -> Right dat
    [] -> case [err | StreamError _ _ err _ <- items] of
      (err:_) -> Left $ ProtocolError err
      [] -> Left $ ProtocolError $
        "no " <> method <> " frame in response"

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

-- | Send a response back to the server for a bidirectional request
-- Uses {backend}.respond RPC method
sendBidirectionalResponse :: SubstrateConfig -> Text -> Response Value -> IO (Either TransportError ())
sendBidirectionalResponse cfg requestId response = do
  let backend = substrateBackend cfg
  let respondParams = object
        [ "request_id" .= requestId
        , "response_data" .= response
        ]
  result <- rpcCallWith cfg (backend <> ".respond") respondParams
  case result of
    Left transportErr -> pure $ Left transportErr
    Right items -> case [err | StreamError _ _ err _ <- items] of
      (err:_) -> pure $ Left $ ProtocolError err
      [] -> pure $ Right ()
