-- | Shallow plugin schema for Plexus RPC
--
-- = Design
--
-- The schema is shallow: children are summaries (namespace, description, hash),
-- not full schemas. Full child schemas are fetched on demand when navigating.
--
-- This matches the coalgebraic design:
-- - Rust side: unfolds plugin structure on demand (anamorphism)
-- - Wire format: one layer at a time (shallow schema)
-- - Haskell side: folds/consumes structure (catamorphism over fetched data)
--
-- = The Functor (Conceptual)
--
-- @
-- F : Set → Set
-- F(X) = Namespace × Version × Description × Hash × [Method] × Maybe [X]
-- @
--
-- On the wire, X = ChildSummary (a reference). Resolution is lazy.
--
-- = Category Properties
--
-- The plugin system forms a free category:
-- - Objects: Schemas (identified by hash)
-- - Morphisms: Paths (sequences of child references)
-- - Identity: Empty path
-- - Composition: Path concatenation
module Plexus.Schema.Recursive
  ( -- * Core Types
    PluginSchema(..)
  , MethodSchema(..)
  , ChildSummary(..)
  , PluginHash
  , SchemaResult(..)

    -- * Deprecation Metadata (IR-5)
  , DeprecationInfo(..)
  , ParamSchema(..)
  , MethodRole(..)

    -- * Queries
  , isHubActivation
  , isLeafActivation
  , pluginMethods
  , pluginChildren
  , childNamespaces

    -- * JSON Parsing
  , parsePluginSchema
  , parseSchemaResult
  ) where

import Control.Applicative ((<|>))

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- ============================================================================
-- Deprecation Metadata (IR-5)
-- ============================================================================

-- | Structured deprecation metadata attached to activations, methods, and
--   parameter fields.
--
--   All three text fields use JSON snake_case on the wire
--   (@since@, @removed_in@, @message@). Producers emitted before IR-2
--   may omit the outer field entirely; consumers should use @.:?@ so a
--   missing value deserializes as 'Nothing' on the containing record.
data DeprecationInfo = DeprecationInfo
  { depSince     :: Text   -- ^ Version the surface became deprecated.
  , depRemovedIn :: Text   -- ^ Version at which the surface will be removed.
  , depMessage   :: Text   -- ^ Human-readable migration guidance.
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON DeprecationInfo where
  parseJSON = withObject "DeprecationInfo" $ \o -> DeprecationInfo
    <$> o .:  "since"
    <*> o .:  "removed_in"
    <*> o .:  "message"

instance ToJSON DeprecationInfo where
  toJSON DeprecationInfo{..} = object
    [ "since"      .= depSince
    , "removed_in" .= depRemovedIn
    , "message"    .= depMessage
    ]

-- | Semantic role of a method (for future catalog/UX use).
--
--   Parsed permissively: any unknown string becomes 'MethodRoleOther'.
data MethodRole
  = MethodRoleQuery
  | MethodRoleCommand
  | MethodRoleStream
  | MethodRoleOther Text
  deriving stock (Show, Eq, Generic)

instance FromJSON MethodRole where
  parseJSON = withText "MethodRole" $ \t -> pure $ case t of
    "query"   -> MethodRoleQuery
    "command" -> MethodRoleCommand
    "stream"  -> MethodRoleStream
    other     -> MethodRoleOther other

instance ToJSON MethodRole where
  toJSON MethodRoleQuery       = String "query"
  toJSON MethodRoleCommand     = String "command"
  toJSON MethodRoleStream      = String "stream"
  toJSON (MethodRoleOther t)   = String t

-- | Shallow parameter schema with optional per-field deprecation.
--
--   Not every producer emits this; synapse treats 'Nothing' as "no
--   per-field metadata available" and falls back to the raw
--   'methodParams' 'Value' for legacy rendering.
data ParamSchema = ParamSchema
  { paramName         :: Text
  , paramDescription  :: Maybe Text
  , paramRequired     :: Bool
  , paramDeprecation  :: Maybe DeprecationInfo
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ParamSchema where
  parseJSON = withObject "ParamSchema" $ \o -> ParamSchema
    <$> o .:  "name"
    <*> o .:? "description"
    <*> o .:? "required" .!= False
    <*> o .:? "deprecation"

instance ToJSON ParamSchema where
  toJSON ParamSchema{..} = object
    [ "name"        .= paramName
    , "description" .= paramDescription
    , "required"    .= paramRequired
    , "deprecation" .= paramDeprecation
    ]

-- ============================================================================
-- Core Types
-- ============================================================================

-- | Content hash for cache invalidation
type PluginHash = Text

-- | Summary of a child plugin (shallow - no methods or nested children)
--
-- This is a reference to a child, not the full schema. To get the full
-- schema, fetch it via @{path}.schema@ RPC call.
data ChildSummary = ChildSummary
  { csNamespace   :: Text
  , csDescription :: Text
  , csHash        :: PluginHash
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ChildSummary where
  parseJSON = withObject "ChildSummary" $ \o -> ChildSummary
    <$> o .: "namespace"
    <*> o .: "description"
    <*> o .: "hash"

instance ToJSON ChildSummary where
  toJSON ChildSummary{..} = object
    [ "namespace"   .= csNamespace
    , "description" .= csDescription
    , "hash"        .= csHash
    ]

-- | Schema for a single method
data MethodSchema = MethodSchema
  { methodName            :: Text
  , methodDescription     :: Text
  , methodHash            :: PluginHash
  , methodParams          :: Maybe Value  -- ^ JSON Schema for params
  , methodReturns         :: Maybe Value  -- ^ JSON Schema for return events
  , methodStreaming        :: Bool         -- ^ True if method streams multiple events
  , methodBidirectional   :: Bool         -- ^ True if method uses a bidirectional channel
  , methodRequestType     :: Maybe Value  -- ^ JSON Schema for the server→client request type (when bidirectional)
  , methodResponseType    :: Maybe Value  -- ^ JSON Schema for the client→server response type (when bidirectional)
  , methodDeprecation     :: Maybe DeprecationInfo  -- ^ Deprecation info, if any (IR-5)
  , methodParamSchemas    :: Maybe [ParamSchema]    -- ^ Optional structured param info with per-field deprecations (IR-5)
  , methodRole            :: Maybe MethodRole       -- ^ Semantic role (query/command/stream), if declared (IR-5)
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON MethodSchema where
  parseJSON = withObject "MethodSchema" $ \o -> MethodSchema
    <$> o .: "name"
    <*> o .: "description"
    <*> o .: "hash"
    <*> o .:? "params"
    <*> o .:? "returns"
    <*> o .:? "streaming"      .!= False
    <*> o .:? "bidirectional"  .!= False
    <*> o .:? "request_type"
    <*> o .:? "response_type"
    <*> o .:? "deprecation"
    <*> o .:? "param_schemas"
    <*> o .:? "role"

instance ToJSON MethodSchema where
  toJSON MethodSchema{..} = object
    [ "name"           .= methodName
    , "description"    .= methodDescription
    , "hash"           .= methodHash
    , "params"         .= methodParams
    , "returns"        .= methodReturns
    , "streaming"      .= methodStreaming
    , "bidirectional"  .= methodBidirectional
    , "request_type"   .= methodRequestType
    , "response_type"  .= methodResponseType
    , "deprecation"    .= methodDeprecation
    , "param_schemas"  .= methodParamSchemas
    , "role"           .= methodRole
    ]

-- | Shallow plugin schema (what we receive from {backend}.schema)
--
-- Children are summaries only - fetch full schema on-demand when navigating.
-- This is the wire format: one layer of observation at a time.
data PluginSchema = PluginSchema
  { psNamespace       :: Text
  , psVersion         :: Text
  , psDescription     :: Text
  , psLongDescription :: Maybe Text  -- ^ Extended description (no word limit)
  , psHash            :: PluginHash
  , psMethods         :: [MethodSchema]
  , psChildren        :: Maybe [ChildSummary]  -- ^ Nothing = leaf, Just = hub activation
  , psDeprecation     :: Maybe DeprecationInfo -- ^ Activation-level deprecation (IR-5)
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PluginSchema where
  parseJSON = withObject "PluginSchema" $ \o -> PluginSchema
    <$> o .: "namespace"
    <*> o .: "version"
    <*> o .: "description"
    <*> o .:? "long_description"
    <*> o .: "hash"
    <*> o .:? "methods" .!= []
    <*> o .:? "children"
    <*> o .:? "deprecation"

instance ToJSON PluginSchema where
  toJSON PluginSchema{..} = object
    [ "namespace"        .= psNamespace
    , "version"          .= psVersion
    , "description"      .= psDescription
    , "long_description" .= psLongDescription
    , "hash"             .= psHash
    , "methods"          .= psMethods
    , "children"         .= psChildren
    , "deprecation"      .= psDeprecation
    ]

-- | Result of a schema query - can be either a full plugin or just a method
data SchemaResult
  = SchemaPlugin PluginSchema
  | SchemaMethod MethodSchema
  deriving stock (Show, Eq)

instance FromJSON SchemaResult where
  parseJSON v =
    -- Try PluginSchema first (has "namespace" field)
    (SchemaPlugin <$> parseJSON v) <|>
    -- Fall back to MethodSchema (has "name" field)
    (SchemaMethod <$> parseJSON v)

instance ToJSON SchemaResult where
  toJSON (SchemaPlugin p) = toJSON p
  toJSON (SchemaMethod m) = toJSON m

-- ============================================================================
-- Basic Queries
-- ============================================================================

-- | Is this a hub activation (has children)?
isHubActivation :: PluginSchema -> Bool
isHubActivation = maybe False (not . null) . psChildren

-- | Is this a leaf activation (no children)?
isLeafActivation :: PluginSchema -> Bool
isLeafActivation = not . isHubActivation

-- | Get methods (alias for psMethods)
pluginMethods :: PluginSchema -> [MethodSchema]
pluginMethods = psMethods

-- | Get child summaries (empty list if leaf)
pluginChildren :: PluginSchema -> [ChildSummary]
pluginChildren = fromMaybe [] . psChildren

-- | Get child namespace names
childNamespaces :: PluginSchema -> [Text]
childNamespaces = map csNamespace . pluginChildren

-- ============================================================================
-- JSON Parsing Helpers
-- ============================================================================

-- | Parse a PluginSchema from the schema event content
parsePluginSchema :: Value -> Either Text PluginSchema
parsePluginSchema val = case fromJSON val of
  Success schema -> Right schema
  Error err -> Left $ T.pack err

-- | Parse a SchemaResult (plugin or method) from schema event content
parseSchemaResult :: Value -> Either Text SchemaResult
parseSchemaResult val = case fromJSON val of
  Success result -> Right result
  Error err -> Left $ T.pack err
