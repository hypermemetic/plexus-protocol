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
  deriving stock (Show, Eq, Generic)

instance FromJSON GuidanceErrorType where
  parseJSON = withObject "GuidanceErrorType" $ \o -> do
    kind <- o .: "error_kind" :: Parser Text
    case kind of
      "activation_not_found" -> ActivationNotFound <$> o .: "activation"
      "method_not_found" -> MethodNotFound <$> o .: "activation" <*> o .: "method"
      "invalid_params" -> InvalidParams <$> o .: "method" <*> o .: "reason"
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
      "call_plexus_schema" -> pure CallPlexusSchema
      "call_activation_schema" -> CallActivationSchema <$> o .: "namespace"
      "try_method" -> TryMethod <$> o .: "method" <*> o .:? "example_params"
      "custom" -> CustomGuidance <$> o .: "message"
      _ -> fail $ "Unknown action: " <> T.unpack action

instance ToJSON GuidanceSuggestion where
  toJSON CallPlexusSchema = object ["action" .= ("call_plexus_schema" :: Text)]
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
  | StreamDone
      { itemPlexusHash :: Text
      , itemProvenance :: Provenance
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
          "done" -> pure $ StreamDone hash prov
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
          "done" -> StreamDone hash
            <$> o .: "provenance"
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
  toJSON (StreamDone hash prov) = object
    [ "type" .= ("done" :: Text)
    , "metadata" .= StreamMetadata prov hash 0
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
