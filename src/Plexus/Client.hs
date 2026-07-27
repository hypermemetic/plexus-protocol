-- | Low-level WebSocket client for Substrate
--
-- The core primitive is:
--
-- @
-- substrateRpc :: SubstrateConnection -> Value -> Stream (Of PlexusStreamItem) IO ()
-- @
--
-- This establishes a subscription and yields stream items until completion.
module Plexus.Client
  ( -- * Connection
    SubstrateConnection
  , connect
  , disconnect

    -- * Core RPC primitive
  , substrateRpc
  , substrateRpcCancellable
  , CancelTurn(..)

    -- * Configuration
  , SubstrateConfig(..)
  , defaultConfig
  , cookieHeader
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async, async, cancel, wait)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch)
import Control.Monad (forever, void)
import System.Timeout (timeout)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Network.WebSockets (Connection)
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Streaming
import qualified Streaming.Prelude as Str

import Plexus.Types

-- | Substrate connection configuration
data SubstrateConfig = SubstrateConfig
  { substrateHost    :: String
  , substratePort    :: Int
  , substratePath    :: String
  , substrateBackend :: Text    -- ^ Backend name (e.g., "plexus")
  , substrateHeaders :: WS.Headers  -- ^ Extra HTTP upgrade headers (e.g., Cookie)
  }
  deriving stock (Show, Eq, Ord)

-- | Default configuration for local development (requires backend)
defaultConfig :: Text -> SubstrateConfig
defaultConfig backend = SubstrateConfig
  { substrateHost    = "127.0.0.1"
  , substratePort    = 4444
  , substratePath    = "/"
  , substrateBackend = backend
  , substrateHeaders = []
  }

-- | Build a Cookie header from a JWT token
cookieHeader :: Text -> WS.Headers
cookieHeader tok = [(CI.mk "Cookie", "access_token=" <> BS8.pack (T.unpack tok))]

-- | Pending request with its queue, waiting for subscription ID
data PendingRequest = PendingRequest
  { prQueue    :: TQueue Value        -- ^ Queue to receive notifications
  , prResponse :: TMVar RpcResponse   -- ^ Response channel
  }

-- | Active connection to substrate
data SubstrateConnection = SubstrateConnection
  { scConnection    :: Connection
  , scNextId        :: IORef Int
  , scSubscriptions :: TVar (Map SubscriptionId (TQueue Value))
  , scPendingReqs   :: TVar (Map RequestId PendingRequest)
  , scReaderThread  :: Async ()
  }

-- | Connect to substrate
-- Throws an exception if connection fails
connect :: SubstrateConfig -> IO SubstrateConnection
connect SubstrateConfig{..} = do
  -- Initialize state
  nextId <- newIORef 1
  subs   <- newTVarIO Map.empty
  pendingReqs <- newTVarIO Map.empty
  resultVar <- newEmptyTMVarIO  -- Either error message or (conn, reader)

  -- Run WebSocket client in a background thread
  -- Catch exceptions inside the thread to prevent them from being printed
  void $ forkIO $
    (WS.runClientWith substrateHost substratePort substratePath
        WS.defaultConnectionOptions substrateHeaders $ \conn -> do
      reader <- async $ readerLoop conn subs pendingReqs
      -- Signal success
      atomically $ putTMVar resultVar (Right (conn, reader))
      -- Keep alive until reader exits
      void (wait reader) `catch` \(_ :: SomeException) -> pure ()
    ) `catch` \(e :: SomeException) ->
      -- Signal failure (don't print, just capture)
      atomically $ putTMVar resultVar (Left $ show e)

  -- Wait for connection result (with 5s timeout instead of hardcoded 200ms sleep)
  mResult <- timeout 5000000 $ atomically $ takeTMVar resultVar

  case mResult of
    Nothing -> error "Connection timeout (5s)"
    Just (Left err) -> error $ "Connection failed: " <> err
    Just (Right (conn, reader)) ->
      pure SubstrateConnection
        { scConnection    = conn
        , scNextId        = nextId
        , scSubscriptions = subs
        , scPendingReqs   = pendingReqs
        , scReaderThread  = reader
        }

-- | Disconnect from substrate
disconnect :: SubstrateConnection -> IO ()
disconnect SubstrateConnection{..} = do
  cancel scReaderThread
  WS.sendClose scConnection ("bye" :: Text) `catch` \(_ :: SomeException) -> pure ()

-- | Reader loop - dispatches incoming messages to the right handler
readerLoop
  :: Connection
  -> TVar (Map SubscriptionId (TQueue Value))
  -> TVar (Map RequestId PendingRequest)
  -> IO ()
readerLoop conn subs pendingReqs = forever $ do
  msg <- WS.receiveData conn
  case eitherDecode msg of
    Left err -> putStrLn $ "Failed to decode message: " <> err
    Right val -> dispatch val
  where
    dispatch :: Value -> IO ()
    dispatch val = case val of
      Object o
        -- Check if it's a subscription notification (has "params.subscription" but no "id")
        | Just (Object params) <- KM.lookup "params" o
        , Just _ <- KM.lookup "subscription" params
        , Nothing <- KM.lookup "id" o
        -> handleNotification val

        -- Otherwise it's a response (has "id")
        | Just _ <- KM.lookup "id" o
        -> handleResponse val

      _ -> putStrLn $ "Unknown message format: " <> show val

    handleNotification :: Value -> IO ()
    handleNotification val =
      case fromJSON val of
        Success (SubscriptionNotification _ _ params) -> do
          let subId = subParamsSubscription params
              result = subParamsResult params
          mQueue <- Map.lookup subId <$> readTVarIO subs
          case mQueue of
            Just queue -> atomically $ writeTQueue queue result
            Nothing    -> putStrLn $ "Unknown subscription: " <> show subId
        Error err -> putStrLn $ "Failed to parse notification: " <> err

    handleResponse :: Value -> IO ()
    handleResponse val =
      case fromJSON val of
        Success resp -> do
          let rid = rpcRespId resp
          mPending <- Map.lookup rid <$> readTVarIO pendingReqs
          case mPending of
            Just (PendingRequest queue respVar) -> do
              -- For successful responses, register the subscription queue BEFORE
              -- signaling completion. This prevents race with notifications.
              case resp of
                RpcSuccess _ result ->
                  case fromJSON result of
                    Success subId -> atomically $ do
                      modifyTVar' subs $ Map.insert subId queue
                      putTMVar respVar resp
                    Error _ ->
                      -- Can't parse subId, just signal response
                      atomically $ putTMVar respVar resp
                RpcError{} ->
                  atomically $ putTMVar respVar resp
            Nothing -> putStrLn $ "Unknown request id: " <> show rid
        Error err -> putStrLn $ "Failed to parse response: " <> err

-- | The core RPC primitive
--
-- @substrateRpc conn method params@ sends a subscription request and returns a stream
-- of 'PlexusStreamItem' values. The stream completes when a 'StreamDone' or
-- 'StreamError' item is received.
--
-- Example:
--
-- @
-- import qualified Streaming.Prelude as S
--
-- main = do
--   conn <- connect defaultConfig
--   S.print $ substrateRpc conn "bash_execute" (toJSON ["echo hello"])
-- @
substrateRpc
  :: SubstrateConnection
  -> Text              -- ^ Method name (e.g., "bash_execute")
  -> Value             -- ^ Parameters
  -> Stream (Of PlexusStreamItem) IO ()
substrateRpc = substrateRpcCancellable (\_ -> pure ())

-- | PLX-123 / M3·D, decision gate 2 — the cancellation channel.
--
-- RFC 002 §10.2 leaves the cancellation channel deliberately unspecified:
-- "cancellation is therefore transport-defined and is NOT projectable the way
-- callbacks are." This is where synapse defines it, and the definition is
-- deliberately the smallest one that already exists on the wire:
--
-- __the cancellation channel is the JSON-RPC subscription this very call
-- opened, on the same multiplexed connection, addressed by its
-- 'SubscriptionId'.__
--
-- No second socket, no second port, no out-of-band identity. The substrate
-- already registers an unsubscribe method alongside every subscription
-- (@\<ns\>.call@ is paired with @\<ns\>.call_unsub@ — see
-- @plexus-core@'s @arc_into_rpc_module@), and the subscription id is the only
-- per-invocation identity the server has ever put on this wire. The turn id
-- would be a better address, because it is what @LiveTurns::cancel_turn@ takes
-- — but the server does not advertise it except inside a callback's
-- @request_id@, so addressing a turn directly is a server-supply change and
-- not a client one. That gap is recorded in the PLX-84 amendment.
--
-- The caller is handed a pre-bound @IO ()@ that sends the unsubscribe. It is
-- an @IO ()@ rather than the id itself so that no caller can be tempted to
-- address the connection instead: the connection is __pooled and shared__, so
-- a cancel must be per-subscription or it would tear down someone else's call.
substrateRpcCancellable
  :: (CancelTurn -> IO ())  -- ^ Handed the cancel action once the substrate confirms the subscription.
  -> SubstrateConnection
  -> Text
  -> Value
  -> Stream (Of PlexusStreamItem) IO ()
substrateRpcCancellable onCancelHandle conn method params = do
  -- Get next request ID
  rid <- liftIO $ atomicModifyIORef' (scNextId conn) $ \n -> (n + 1, RequestId n)

  -- Create queue for this subscription and response channel
  queue <- liftIO newTQueueIO
  respVar <- liftIO newEmptyTMVarIO

  -- Register pending request (includes queue so reader can register subscription)
  liftIO $ atomically $ modifyTVar' (scPendingReqs conn) $
    Map.insert rid (PendingRequest queue respVar)

  -- Send subscription request
  let req = mkSubscribeRequest rid method params
  liftIO $ WS.sendTextData (scConnection conn) (encode req)

  -- Wait for subscription confirmation
  resp <- liftIO $ atomically $ takeTMVar respVar

  -- Clean up pending request
  liftIO $ atomically $ modifyTVar' (scPendingReqs conn) $ Map.delete rid

  case resp of
    RpcError _ err -> do
      -- Emit error as StreamError so caller can handle it properly
      Str.yield $ StreamError
        { itemPlexusHash = ""  -- No hash available for subscription errors
        , itemProvenance = Provenance ["substrate"]
        , itemError = T.pack $ "Subscription error: " <> show err
        , itemRecoverable = False
        }

    RpcSuccess _ result -> do
      -- The reader loop already registered the queue in scSubscriptions
      -- when it processed the response, so we just need the subId for cleanup
      case fromJSON result of
        Error err -> do
          liftIO $ putStrLn $ "Failed to parse subscription id: " <> err
          pure ()

        Success subId -> do
          -- Hand the caller its cancel action now that the subscription
          -- exists and is addressable, and BEFORE the first item is pulled:
          -- a cancel that only becomes possible after the first update
          -- cannot cancel a turn that is slow to produce one.
          liftIO $ onCancelHandle (CancelTurn (sendUnsubscribe conn method subId))
          -- Stream items until done, cleanup when finished
          streamItems queue subId <* liftIO (cleanup subId)
  where
    streamItems :: TQueue Value -> SubscriptionId -> Stream (Of PlexusStreamItem) IO ()
    streamItems queue subId = do
      val <- liftIO $ atomically $ readTQueue queue
      case fromJSON val of
        Error err -> do
          liftIO $ putStrLn $ "Failed to parse stream item: " <> err
          streamItems queue subId

        Success item -> do
          Str.yield item
          case item of
            StreamDone{}  -> pure ()  -- End of stream
            StreamError{} -> pure ()  -- Error terminates stream
            _             -> streamItems queue subId

    cleanup :: SubscriptionId -> IO ()
    cleanup subId = atomically $ modifyTVar' (scSubscriptions conn) $
      Map.delete subId

-- | The cancel action for one live subscription. Opaque on purpose — see
-- 'substrateRpcCancellable' for why the caller gets an action and not an id.
newtype CancelTurn = CancelTurn { runCancelTurn :: IO () }

-- | Send the JSON-RPC unsubscribe paired with @subMethod@.
--
-- jsonrpsee names the pair at registration time; @plexus-core@ spells it
-- @\<sub-method\>_unsub@, so @substrate.call@ is cancelled by
-- @substrate.call_unsub@. This is fire-and-forget by design: the reply is a
-- bare boolean on a request id nothing is waiting on, and the caller has
-- already stopped consuming.
--
-- __What this does and does not assert.__ It tears down the subscription. It
-- does __not__ reach @LiveTurns::cancel_turn@, so it does not deliver RFC 002
-- §6.8's cooperative signal to the turn, and the server does not answer with a
-- @Cancelled@ terminal. Callers must not render this as a server-confirmed
-- cancellation; synapse renders it as @synapse:client_cancelled@ and says so.
sendUnsubscribe :: SubstrateConnection -> Text -> SubscriptionId -> IO ()
sendUnsubscribe conn subMethod subId = do
  rid <- atomicModifyIORef' (scNextId conn) $ \n -> (n + 1, RequestId n)
  let req = mkUnsubscribeRequest rid (subMethod <> "_unsub") subId
  WS.sendTextData (scConnection conn) (encode req)
    `catch` \(_ :: SomeException) -> pure ()
