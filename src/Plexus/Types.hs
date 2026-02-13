-- | JSON-RPC types for communicating with Plexus RPC servers
module Plexus.Types
  ( -- * JSON-RPC Protocol
    RpcRequest(..)
  , RpcResponse(..)
  , RpcError(..)
  , SubscriptionNotification(..)
  , SubNotifParams(..)
  , RequestId(..)
  , SubscriptionId(..)

    -- * Plexus Stream Types
  , PlexusStreamItem(..)
  , Provenance(..)
  , StreamMetadata(..)
  , GuidanceErrorType(..)
  , GuidanceSuggestion(..)
  , TransportError(..)

    -- * Bidirectional Types
  , SelectOption(..)
  , Request(..)
  , StandardRequest
  , Response(..)
  , StandardResponse

    -- * Transport Errors
  , TransportError(..)

    -- * Bidirectional Request/Response Types
  , StandardRequest(..)
  , StandardResponse(..)
  , SelectOption(..)

    -- * Helpers
  , mkSubscribeRequest
  , mkUnsubscribeRequest
  , getPlexusHash
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)

-- | Request ID for JSON-RPC calls
newtype RequestId = RequestId { unRequestId :: Int }
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

-- | Subscription ID returned by the server (can be string or number)
newtype SubscriptionId = SubscriptionId { unSubscriptionId :: Text }
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON)

instance FromJSON SubscriptionId where
  parseJSON (String s) = pure $ SubscriptionId s
  parseJSON v =
    -- Store raw JSON representation as the ID
    pure $ SubscriptionId $ T.decodeUtf8 $ LBS.toStrict $ encode v

-- | JSON-RPC 2.0 request
data RpcRequest = RpcRequest
  { rpcReqJsonrpc :: Text           -- ^ Always "2.0"
  , rpcReqMethod  :: Text           -- ^ Method name (e.g., "bash_execute")
  , rpcReqParams  :: Value          -- ^ Parameters (array or object)
  , rpcReqId      :: RequestId      -- ^ Request identifier
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RpcRequest where
  toJSON RpcRequest{..} = object
    [ "jsonrpc" .= rpcReqJsonrpc
    , "method"  .= rpcReqMethod
    , "params"  .= rpcReqParams
    , "id"      .= rpcReqId
    ]

-- | JSON-RPC 2.0 response
data RpcResponse
  = RpcSuccess
      { rpcRespId     :: RequestId
      , rpcRespResult :: Value
      }
  | RpcError
      { rpcRespId    :: RequestId
      , rpcRespError :: RpcError
      }
  deriving stock (Show, Eq, Generic)

instance FromJSON RpcResponse where
  parseJSON = withObject "RpcResponse" $ \o -> do
    rid <- o .: "id"
    mResult <- o .:? "result"
    mError  <- o .:? "error"
    case (mResult, mError) of
      (Just r, Nothing) -> pure $ RpcSuccess rid r
      (Nothing, Just e) -> pure $ RpcError rid e
      _ -> fail "Expected either 'result' or 'error' field"

-- | JSON-RPC error object
data RpcError = RpcErrorObj
  { errCode    :: Int
  , errMessage :: Text
  , errData    :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON RpcError where
  parseJSON = withObject "RpcError" $ \o -> RpcErrorObj
    <$> o .: "code"
    <*> o .: "message"
    <*> o .:? "data"

-- | Subscription notification from server
-- This is what we receive for each stream item
data SubscriptionNotification = SubscriptionNotification
  { subNotifJsonrpc :: Text
  , subNotifMethod  :: Text           -- ^ Usually "subscription"
  , subNotifParams  :: SubNotifParams
  }
  deriving stock (Show, Eq, Generic)

data SubNotifParams = SubNotifParams
  { subParamsSubscription :: SubscriptionId
  , subParamsResult       :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SubscriptionNotification where
  parseJSON = withObject "SubscriptionNotification" $ \o -> SubscriptionNotification
    <$> o .: "jsonrpc"
    <*> o .: "method"
    <*> o .: "params"

instance FromJSON SubNotifParams where
  parseJSON = withObject "SubNotifParams" $ \o -> SubNotifParams
    <$> o .: "subscription"
    <*> o .: "result"

-- | Provenance tracking nested calls through activations
-- Now just a list of namespace segments (no wrapper object)
newtype Provenance = Provenance { segments :: [Text] }
  deriving stock (Show, Eq, Generic)

instance FromJSON Provenance where
  parseJSON v = case v of
    -- New format: just an array ["plexus", "echo"]
    Array arr -> Provenance <$> mapM parseJSON (foldr (:) [] arr)
    -- Legacy format: {"segments": [...]}
    Object o -> Provenance <$> o .: "segments"
    _ -> fail "Provenance: expected array or object"

instance ToJSON Provenance where
  toJSON (Provenance segs) = toJSON segs

-- | Metadata wrapper for stream results
data StreamMetadata = StreamMetadata
  { metaProvenance :: Provenance
  , metaPlexusHash :: Text
  , metaTimestamp  :: Integer
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON StreamMetadata where
  parseJSON = withObject "StreamMetadata" $ \o -> StreamMetadata
    <$> o .: "provenance"
    <*> o .: "plexus_hash"
    <*> o .: "timestamp"

instance ToJSON StreamMetadata where
  toJSON StreamMetadata{..} = object
    [ "provenance"  .= metaProvenance
    , "plexus_hash" .= metaPlexusHash
    , "timestamp"   .= metaTimestamp
    ]

-- | Error type from guidance events
data GuidanceErrorType
  = ActivationNotFound
      { errorActivation :: Text
      }
  | MethodNotFound
      { errorActivation :: Text
      , errorMethod     :: Text
      }
  | InvalidParams
      { errorMethod :: Text
      , errorReason :: Text
      }
  | TransportErrorGuidance
      { errorTransport :: TransportError
      }
  deriving stock (Show, Eq, Generic)

instance FromJSON GuidanceErrorType where
  parseJSON = withObject "GuidanceErrorType" $ \o -> do
    kind <- o .: "error_kind" :: Parser Text
    case kind of
      "activation_not_found" -> ActivationNotFound <$> o .: "activation"
      "method_not_found" -> MethodNotFound <$> o .: "activation" <*> o .: "method"
      "invalid_params" -> InvalidParams <$> o .: "method" <*> o .: "reason"
      "transport_error" -> TransportErrorGuidance <$> parseJSON (Object o)
      _ -> fail $ "Unknown error_kind: " <> T.unpack kind

instance ToJSON GuidanceErrorType where
  toJSON (ActivationNotFound activation) = object
    [ "error_kind" .= ("activation_not_found" :: Text)
    , "activation" .= activation
    ]
  toJSON (MethodNotFound activation method) = object
    [ "error_kind" .= ("method_not_found" :: Text)
    , "activation" .= activation
    , "method" .= method
    ]
  toJSON (InvalidParams method reason) = object
    [ "error_kind" .= ("invalid_params" :: Text)
    , "method" .= method
    , "reason" .= reason
    ]
  toJSON (TransportErrorGuidance transportErr) =
    -- Merge the transport error JSON with error_kind
    case toJSON transportErr of
      Object o -> Object o
      _ -> object ["error_kind" .= ("transport_error" :: Text)]

-- | Suggestion for next action from guidance events
data GuidanceSuggestion
  = CallPlexusSchema
  | CallActivationSchema
      { suggestionNamespace :: Text
      }
  | TryMethod
      { suggestionMethod       :: Text
      , suggestionExampleParams :: Maybe Value
      }
  | CustomGuidance
      { suggestionMessage :: Text
      }
  deriving stock (Show, Eq, Generic)

instance FromJSON GuidanceSuggestion where
  parseJSON = withObject "GuidanceSuggestion" $ \o -> do
    action <- o .: "action" :: Parser Text
    case action of
      "call_plexus_schema" -> pure CallPlexusSchema  -- Legacy: maps to {backend}.schema
      "call_activation_schema" -> CallActivationSchema <$> o .: "namespace"
      "try_method" -> TryMethod <$> o .: "method" <*> o .:? "example_params"
      "custom" -> CustomGuidance <$> o .: "message"
      _ -> fail $ "Unknown action: " <> T.unpack action

instance ToJSON GuidanceSuggestion where
  toJSON CallPlexusSchema = object ["action" .= ("call_plexus_schema" :: Text)]  -- Legacy: represents {backend}.schema
  toJSON (CallActivationSchema namespace) = object
    [ "action" .= ("call_activation_schema" :: Text)
    , "namespace" .= namespace
    ]
  toJSON (TryMethod method exampleParams) = object $
    [ "action" .= ("try_method" :: Text)
    , "method" .= method
    ] <> catMaybes [("example_params" .=) <$> exampleParams]
  toJSON (CustomGuidance message) = object
    [ "action" .= ("custom" :: Text)
    , "message" .= message
    ]

-- ============================================================================
-- Transport Errors
-- ============================================================================

-- | Strongly-typed transport-level errors
-- These occur before reaching the backend (connection, network, protocol issues)
data TransportError
  = ConnectionRefused
      { transportHost :: Text
      , transportPort :: Int
      }
  | ConnectionTimeout
      { transportHost :: Text
      , transportPort :: Int
      }
  | ProtocolError
      { transportMessage :: Text
      }
  | NetworkError
      { transportMessage :: Text
      }
  deriving stock (Show, Eq, Generic)

instance ToJSON TransportError where
  toJSON (ConnectionRefused host port) = object
    [ "error_kind" .= ("connection_refused" :: Text)
    , "host" .= host
    , "port" .= port
    ]
  toJSON (ConnectionTimeout host port) = object
    [ "error_kind" .= ("connection_timeout" :: Text)
    , "host" .= host
    , "port" .= port
    ]
  toJSON (ProtocolError msg) = object
    [ "error_kind" .= ("protocol_error" :: Text)
    , "message" .= msg
    ]
  toJSON (NetworkError msg) = object
    [ "error_kind" .= ("network_error" :: Text)
    , "message" .= msg
    ]

instance FromJSON TransportError where
  parseJSON = withObject "TransportError" $ \o -> do
    kind <- o .: "error_kind" :: Parser Text
    case kind of
      "connection_refused" -> ConnectionRefused <$> o .: "host" <*> o .: "port"
      "connection_timeout" -> ConnectionTimeout <$> o .: "host" <*> o .: "port"
      "protocol_error" -> ProtocolError <$> o .: "message"
      "network_error" -> NetworkError <$> o .: "message"
      _ -> fail $ "Unknown transport error_kind: " <> T.unpack kind

-- ============================================================================
-- Bidirectional Types (Request/Response)
-- ============================================================================

-- | Option for select requests, parameterized over value type
data SelectOption t = SelectOption
  { optionValue       :: t
  , optionLabel       :: Text
  , optionDescription :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance (FromJSON t) => FromJSON (SelectOption t) where
  parseJSON = withObject "SelectOption" $ \o -> SelectOption
    <$> o .: "value"
    <*> o .: "label"
    <*> o .:? "description"

instance (ToJSON t) => ToJSON (SelectOption t) where
  toJSON SelectOption{..} = object $
    [ "value" .= optionValue, "label" .= optionLabel ]
    <> catMaybes [("description" .=) <$> optionDescription]

-- | Parameterized request type for bidirectional communication
data Request t
  = Confirm  { confirmMessage :: Text, confirmDefault :: Maybe Bool }
  | Prompt   { promptMessage :: Text, promptDefault :: Maybe t, promptPlaceholder :: Maybe Text }
  | Select   { selectMessage :: Text, selectOptions :: [SelectOption t], selectMulti :: Bool }
  | CustomRequest { customRequestData :: t }
  deriving (Show, Eq, Generic)

-- | Type alias for backwards compatibility: requests with JSON values
type StandardRequest = Request Value

instance (FromJSON t) => FromJSON (Request t) where
  parseJSON = withObject "Request" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "confirm" -> Confirm
        <$> o .: "message"
        <*> o .:? "default"
      "prompt" -> Prompt
        <$> o .: "message"
        <*> o .:? "default"
        <*> o .:? "placeholder"
      "select" -> Select
        <$> o .: "message"
        <*> o .: "options"
        <*> o .:? "multi_select" .!= False
      "custom" -> CustomRequest <$> o .: "data"
      _ -> fail $ "Unknown request type: " <> T.unpack typ

instance (ToJSON t) => ToJSON (Request t) where
  toJSON (Confirm msg def) = object $
    [ "type" .= ("confirm" :: Text), "message" .= msg ]
    <> catMaybes [("default" .=) <$> def]
  toJSON (Prompt msg def placeholder) = object $
    [ "type" .= ("prompt" :: Text), "message" .= msg ]
    <> catMaybes [("default" .=) <$> def, ("placeholder" .=) <$> placeholder]
  toJSON (Select msg opts multi) = object
    [ "type" .= ("select" :: Text), "message" .= msg
    , "options" .= opts, "multi_select" .= multi ]
  toJSON (CustomRequest d) = object ["type" .= ("custom" :: Text), "data" .= d]

-- | Parameterized response type for bidirectional communication
--
-- Note: The 'Value' constructor corresponds to JSON @"type":"text"@ for
-- wire compatibility with the Rust implementation.
data Response t
  = Confirmed { confirmedValue :: Bool }
  | Value     { responseValue :: t }    -- ^ wire tag: "text"
  | Selected  { selectedValues :: [t] }
  | CustomResponse { customResponseData :: t }
  | Cancelled
  deriving (Show, Eq, Generic)

-- | Type alias for backwards compatibility: responses with JSON values
type StandardResponse = Response Value

instance (FromJSON t) => FromJSON (Response t) where
  parseJSON = withObject "Response" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "confirmed" -> Confirmed <$> o .: "value"
      "text"      -> Value <$> o .: "value"
      "selected"  -> Selected <$> o .: "values"
      "custom"    -> CustomResponse <$> o .: "data"
      "cancelled" -> pure Cancelled
      _ -> fail $ "Unknown response type: " <> T.unpack typ

instance (ToJSON t) => ToJSON (Response t) where
  toJSON (Confirmed v)       = object ["type" .= ("confirmed" :: Text), "value" .= v]
  toJSON (Value v)           = object ["type" .= ("text" :: Text), "value" .= v]
  toJSON (Selected vs)       = object ["type" .= ("selected" :: Text), "values" .= vs]
  toJSON (CustomResponse d)  = object ["type" .= ("custom" :: Text), "data" .= d]
  toJSON Cancelled           = object ["type" .= ("cancelled" :: Text)]

-- ============================================================================
-- Plexus Stream Items
-- ============================================================================

-- | Unified stream item from Plexus RPC
--
-- New format uses metadata wrapper:
-- @{"type":"data","metadata":{...},"content_type":"...","content":{...}}@
data PlexusStreamItem
  = StreamProgress
      { itemPlexusHash  :: Text
      , itemProvenance  :: Provenance
      , itemMessage     :: Text
      , itemPercentage  :: Maybe Double
      }
  | StreamData
      { itemPlexusHash  :: Text
      , itemProvenance  :: Provenance
      , itemContentType :: Text
      , itemData        :: Value
      }
  | StreamGuidance
      { itemPlexusHash        :: Text
      , itemProvenance        :: Provenance
      , itemErrorType         :: GuidanceErrorType
      , itemSuggestion        :: GuidanceSuggestion
      , itemAvailableMethods  :: Maybe [Text]
      , itemMethodSchema      :: Maybe Value
      }
  | StreamError
      { itemPlexusHash  :: Text
      , itemProvenance  :: Provenance
      , itemError       :: Text
      , itemRecoverable :: Bool
      }
  | StreamRequest
      { itemPlexusHash  :: Text
      , itemProvenance  :: Provenance
      , itemRequestId   :: Text
      , itemRequestData :: StandardRequest
      , itemTimeout     :: Maybe Int
      }
  | StreamDone
      { itemPlexusHash :: Text
      , itemProvenance :: Provenance
      }
  | StreamRequest
      { itemPlexusHash  :: Text
      , itemProvenance  :: Provenance
      , itemRequestId   :: Text
      , itemRequestData :: Request Value
      , itemTimeoutMs   :: Int
      }
  deriving stock (Show, Eq, Generic)

instance FromJSON PlexusStreamItem where
  parseJSON = withObject "PlexusStreamItem" $ \o -> do
    typ <- o .: "type" :: Parser Text
    -- New format: metadata is a nested object
    mMeta <- o .:? "metadata" :: Parser (Maybe StreamMetadata)
    case mMeta of
      Just meta -> parseWithMetadata typ meta o
      Nothing -> parseLegacy typ o

    where
      -- New format: metadata wrapper
      parseWithMetadata :: Text -> StreamMetadata -> Object -> Parser PlexusStreamItem
      parseWithMetadata typ meta o = do
        let hash = metaPlexusHash meta
            prov = metaProvenance meta
        case typ of
          "progress" -> StreamProgress hash prov
            <$> o .: "message"
            <*> o .:? "percentage"
          "data" -> StreamData hash prov
            <$> o .: "content_type"
            <*> o .: "content"
          "guidance" -> StreamGuidance hash prov
            <$> o .: "error_type"
            <*> o .: "suggestion"
            <*> o .:? "available_methods"
            <*> o .:? "method_schema"
          "error" -> StreamError hash prov
            <$> o .: "message"
            <*> o .:? "recoverable" .!= False
          "request" -> StreamRequest hash prov
            <$> o .: "request_id"
            <*> o .: "request"
            <*> o .:? "timeout"
          "done" -> pure $ StreamDone hash prov
          "request" -> StreamRequest hash prov
            <$> o .: "request_id"
            <*> o .: "request"
            <*> o .: "timeout_ms"
          _ -> fail $ "Unknown event type: " <> T.unpack typ

      -- Legacy format: flat structure
      parseLegacy :: Text -> Object -> Parser PlexusStreamItem
      parseLegacy typ o = do
        hash <- o .: "plexus_hash"
        case typ of
          "progress" -> StreamProgress hash
            <$> o .: "provenance"
            <*> o .: "message"
            <*> o .:? "percentage"
          "data" -> StreamData hash
            <$> o .: "provenance"
            <*> o .: "content_type"
            <*> o .: "data"
          "guidance" -> StreamGuidance hash
            <$> o .: "provenance"
            <*> o .: "error_type"
            <*> o .: "suggestion"
            <*> o .:? "available_methods"
            <*> o .:? "method_schema"
          "error" -> StreamError hash
            <$> o .: "provenance"
            <*> o .: "error"
            <*> o .: "recoverable"
          "request" -> StreamRequest hash
            <$> o .: "provenance"
            <*> o .: "request_id"
            <*> o .: "request"
            <*> o .:? "timeout"
          "done" -> StreamDone hash
            <$> o .: "provenance"
          "request" -> StreamRequest hash
            <$> o .: "provenance"
            <*> o .: "request_id"
            <*> o .: "request"
            <*> o .: "timeout_ms"
          _ -> fail $ "Unknown event type: " <> T.unpack typ

instance ToJSON PlexusStreamItem where
  toJSON (StreamProgress hash prov msg pct) = object
    [ "type" .= ("progress" :: Text)
    , "metadata" .= StreamMetadata prov hash 0
    , "message" .= msg
    , "percentage" .= pct
    ]
  toJSON (StreamData hash prov ct dat) = object
    [ "type" .= ("data" :: Text)
    , "metadata" .= StreamMetadata prov hash 0
    , "content_type" .= ct
    , "content" .= dat
    ]
  toJSON (StreamGuidance hash prov errorType suggestion availMethods methodSchema) = object $
    [ "type" .= ("guidance" :: Text)
    , "metadata" .= StreamMetadata prov hash 0
    , "error_type" .= errorType
    , "suggestion" .= suggestion
    ] <> catMaybes
    [ ("available_methods" .=) <$> availMethods
    , ("method_schema" .=) <$> methodSchema
    ]
  toJSON (StreamError hash prov err rec) = object
    [ "type" .= ("error" :: Text)
    , "metadata" .= StreamMetadata prov hash 0
    , "message" .= err
    , "recoverable" .= rec
    ]
  toJSON (StreamRequest hash prov reqId reqData timeout) = object $
    [ "type" .= ("request" :: Text)
    , "metadata" .= StreamMetadata prov hash 0
    , "request_id" .= reqId
    , "request" .= reqData
    ] <> catMaybes
    [ ("timeout" .=) <$> timeout
    ]
  toJSON (StreamDone hash prov) = object
    [ "type" .= ("done" :: Text)
    , "metadata" .= StreamMetadata prov hash 0
    ]
  toJSON (StreamRequest hash prov reqId reqData timeout) = object
    [ "type" .= ("request" :: Text)
    , "metadata" .= StreamMetadata prov hash 0
    , "request_id" .= reqId
    , "request" .= reqData
    , "timeout_ms" .= timeout
    ]

-- | Create a subscription request
mkSubscribeRequest :: RequestId -> Text -> Value -> RpcRequest
mkSubscribeRequest rid method params = RpcRequest
  { rpcReqJsonrpc = "2.0"
  , rpcReqMethod  = method
  , rpcReqParams  = params
  , rpcReqId      = rid
  }

-- | Create an unsubscribe request
mkUnsubscribeRequest :: RequestId -> Text -> SubscriptionId -> RpcRequest
mkUnsubscribeRequest rid unsubMethod subId = RpcRequest
  { rpcReqJsonrpc = "2.0"
  , rpcReqMethod  = unsubMethod
  , rpcReqParams  = toJSON [unSubscriptionId subId]
  , rpcReqId      = rid
  }

-- | Extract the Plexus RPC hash from a stream item
getPlexusHash :: PlexusStreamItem -> Text
getPlexusHash = itemPlexusHash

-- ============================================================================
-- Transport Errors
-- ============================================================================

-- | Strongly-typed transport errors
data TransportError
  = ConnectionRefused
      { transportErrorHost :: Text
      , transportErrorPort :: Int
      }
  | ConnectionTimeout
      { transportErrorHost :: Text
      , transportErrorPort :: Int
      }
  | ProtocolError
      { transportErrorMessage :: Text
      }
  | NetworkError
      { transportErrorMessage :: Text
      }
  deriving stock (Show, Eq, Generic)

-- ============================================================================
-- Bidirectional Request/Response Types
-- ============================================================================

-- | Standard request types for bidirectional communication
data StandardRequest
  = ConfirmRequest
      { confirmMessage :: Text
      , confirmDefault :: Maybe Bool
      }
  | PromptRequest
      { promptMessage     :: Text
      , promptDefault     :: Maybe Text
      , promptPlaceholder :: Maybe Text
      }
  | SelectRequest
      { selectMessage :: Text
      , selectOptions :: [SelectOption]
      }
  deriving stock (Show, Eq, Generic)

instance FromJSON StandardRequest where
  parseJSON = withObject "StandardRequest" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "confirm" -> ConfirmRequest
        <$> o .: "message"
        <*> o .:? "default"
      "prompt" -> PromptRequest
        <$> o .: "message"
        <*> o .:? "default"
        <*> o .:? "placeholder"
      "select" -> SelectRequest
        <$> o .: "message"
        <*> o .: "options"
      _ -> fail $ "Unknown request type: " <> T.unpack typ

instance ToJSON StandardRequest where
  toJSON (ConfirmRequest msg def) = object
    [ "type" .= ("confirm" :: Text)
    , "message" .= msg
    , "default" .= def
    ]
  toJSON (PromptRequest msg def placeholder) = object $
    [ "type" .= ("prompt" :: Text)
    , "message" .= msg
    ] <> catMaybes
    [ ("default" .=) <$> def
    , ("placeholder" .=) <$> placeholder
    ]
  toJSON (SelectRequest msg opts) = object
    [ "type" .= ("select" :: Text)
    , "message" .= msg
    , "options" .= opts
    ]

-- | Standard response types for bidirectional communication
data StandardResponse
  = ConfirmedResponse Bool
  | TextResponse Text
  | SelectedResponse [Text]
  | CancelledResponse
  deriving stock (Show, Eq, Generic)

instance FromJSON StandardResponse where
  parseJSON = withObject "StandardResponse" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "confirmed" -> ConfirmedResponse <$> o .: "value"
      "text" -> TextResponse <$> o .: "value"
      "selected" -> SelectedResponse <$> o .: "values"
      "cancelled" -> pure CancelledResponse
      _ -> fail $ "Unknown response type: " <> T.unpack typ

instance ToJSON StandardResponse where
  toJSON (ConfirmedResponse b) = object
    [ "type" .= ("confirmed" :: Text)
    , "value" .= b
    ]
  toJSON (TextResponse t) = object
    [ "type" .= ("text" :: Text)
    , "value" .= t
    ]
  toJSON (SelectedResponse vals) = object
    [ "type" .= ("selected" :: Text)
    , "values" .= vals
    ]
  toJSON CancelledResponse = object
    [ "type" .= ("cancelled" :: Text)
    ]

-- | Select option for select requests
data SelectOption = SelectOption
  { optionValue       :: Text
  , optionLabel       :: Text
  , optionDescription :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SelectOption where
  parseJSON = withObject "SelectOption" $ \o -> SelectOption
    <$> o .: "value"
    <*> o .: "label"
    <*> o .:? "description"

instance ToJSON SelectOption where
  toJSON SelectOption{..} = object $
    [ "value" .= optionValue
    , "label" .= optionLabel
    ] <> catMaybes
    [ ("description" .=) <$> optionDescription
    ]
