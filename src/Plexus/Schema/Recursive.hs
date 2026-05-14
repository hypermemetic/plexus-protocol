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

    -- * Credential Metadata (AUTHZ-CRED-IR-1)
  , CredentialFieldDecl(..)
  , CredentialMetadata(..)
  , CredentialKind(..)
  , AttachmentSite(..)
  , CredentialIssuer(..)
  , RequiredCredential(..)

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
import Data.Aeson.Types (Parser)
import Data.Maybe (catMaybes, fromMaybe)
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

-- | Structural role of a method in the activation graph (IR-2 / IR-3).
--
--   Mirrors the Rust @plexus_core::MethodRole@ enum. Tagged on the wire
--   with a @kind@ discriminator and @snake_case@ variant names:
--
--   @
--   {"kind": "rpc"}
--   {"kind": "static_child"}
--   {"kind": "dynamic_child", "list_method": "planet_names", "search_method": null}
--   @
--
--   Defaults to 'MethodRoleRpc' when absent so pre-IR servers deserialize
--   cleanly (see 'MethodSchema.methodRole' parser).
data MethodRole
  = MethodRoleRpc
  | MethodRoleStaticChild
  | MethodRoleDynamicChild
      { listMethod   :: Maybe Text
      , searchMethod :: Maybe Text
      }
  deriving stock (Show, Eq, Generic)

instance FromJSON MethodRole where
  parseJSON = withObject "MethodRole" $ \o -> do
    kind <- o .: "kind" :: Parser Text
    case kind of
      "rpc"           -> pure MethodRoleRpc
      "static_child"  -> pure MethodRoleStaticChild
      "dynamic_child" -> MethodRoleDynamicChild
        <$> o .:? "list_method"
        <*> o .:? "search_method"
      other -> fail $ "Unknown MethodRole kind: " <> T.unpack other

instance ToJSON MethodRole where
  toJSON MethodRoleRpc         = object ["kind" .= ("rpc" :: Text)]
  toJSON MethodRoleStaticChild = object ["kind" .= ("static_child" :: Text)]
  toJSON (MethodRoleDynamicChild lm sm) =
    object $ ("kind" .= ("dynamic_child" :: Text)) : catMaybes
      [ ("list_method"   .=) <$> lm
      , ("search_method" .=) <$> sm
      ]

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
-- Credential Metadata (AUTHZ-CRED-IR-1)
-- ============================================================================
--
-- These types mirror the Rust 'plexus_core::plexus::schema::{CredentialFieldDecl,
-- RequiredCredential}' and the underlying 'plexus_auth_core::{CredentialMetadata,
-- CredentialKind, AttachmentSite, CredentialIssuer}' types pinned by
-- AUTHZ-CRED-CORE-1 / AUTHZ-CRED-CORE-3.
--
-- The wire format is verified against the producer-side serde derives in
-- 'plexus-core/src/plexus/schema.rs' (see the AUTHZ-CRED-IR-1 run-notes for
-- the JSON dump used to pin the shape). Field names are snake_case for
-- back-compat with the existing IR convention.
--
-- Pre-CRED-CORE-3 producers omit 'credentials' and 'requires_credential'
-- entirely; the parsers default them to empty/Nothing.

-- | What kind of credential this is. Mirrors the Rust closed enum
--   @plexus_auth_core::CredentialKind@.
--
--   Tagged on the wire with a @kind@ discriminator and @snake_case@ variant
--   names (matches @#[serde(tag = "kind", rename_all = "snake_case")]@):
--
--   @
--   {"kind": "bearer"}
--   {"kind": "oauth_access"}
--   {"kind": "other", "name": "my_custom"}
--   @
data CredentialKind
  = CredKindBearer
  | CredKindCookie
  | CredKindOauthAccess
  | CredKindOauthRefresh
  | CredKindOidcId
  | CredKindAwsSts
  | CredKindMacaroon
  | CredKindOther { credKindOtherName :: Text }
  deriving stock (Show, Eq, Generic)

instance FromJSON CredentialKind where
  parseJSON = withObject "CredentialKind" $ \o -> do
    k <- o .: "kind" :: Parser Text
    case k of
      "bearer"        -> pure CredKindBearer
      "cookie"        -> pure CredKindCookie
      "oauth_access"  -> pure CredKindOauthAccess
      "oauth_refresh" -> pure CredKindOauthRefresh
      "oidc_id"       -> pure CredKindOidcId
      "aws_sts"       -> pure CredKindAwsSts
      "macaroon"      -> pure CredKindMacaroon
      "other"         -> CredKindOther <$> o .: "name"
      other           -> fail $ "Unknown CredentialKind kind: " <> T.unpack other

instance ToJSON CredentialKind where
  toJSON CredKindBearer        = object ["kind" .= ("bearer" :: Text)]
  toJSON CredKindCookie        = object ["kind" .= ("cookie" :: Text)]
  toJSON CredKindOauthAccess   = object ["kind" .= ("oauth_access" :: Text)]
  toJSON CredKindOauthRefresh  = object ["kind" .= ("oauth_refresh" :: Text)]
  toJSON CredKindOidcId        = object ["kind" .= ("oidc_id" :: Text)]
  toJSON CredKindAwsSts        = object ["kind" .= ("aws_sts" :: Text)]
  toJSON CredKindMacaroon      = object ["kind" .= ("macaroon" :: Text)]
  toJSON (CredKindOther n)     = object ["kind" .= ("other" :: Text), "name" .= n]

-- | Where the credential is attached on the wire when sent on subsequent
--   calls. Mirrors the Rust closed enum @plexus_auth_core::AttachmentSite@.
--
--   Tagged on the wire with a @site@ discriminator and @snake_case@ variant
--   names (matches @#[serde(tag = "site", rename_all = "snake_case")]@):
--
--   @
--   {"site": "header", "name": "authorization"}
--   {"site": "cookie", "name": "plexus_session"}
--   {"site": "first_frame", "setup_method": "auth.setup", "param": "token"}
--   {"site": "in_rpc_param", "param": "auth_token"}
--   @
data AttachmentSite
  = AttachHeader      { attachHeaderName :: Text }
  | AttachCookie      { attachCookieName :: Text }
  | AttachFirstFrame  { attachSetupMethod :: Text, attachParam :: Text }
  | AttachInRpcParam  { attachInRpcParam :: Text }
  deriving stock (Show, Eq, Generic)

instance FromJSON AttachmentSite where
  parseJSON = withObject "AttachmentSite" $ \o -> do
    s <- o .: "site" :: Parser Text
    case s of
      "header"        -> AttachHeader <$> o .: "name"
      "cookie"        -> AttachCookie <$> o .: "name"
      "first_frame"   -> AttachFirstFrame <$> o .: "setup_method" <*> o .: "param"
      "in_rpc_param"  -> AttachInRpcParam <$> o .: "param"
      other           -> fail $ "Unknown AttachmentSite site: " <> T.unpack other

instance ToJSON AttachmentSite where
  toJSON (AttachHeader n)         = object ["site" .= ("header" :: Text), "name" .= n]
  toJSON (AttachCookie n)         = object ["site" .= ("cookie" :: Text), "name" .= n]
  toJSON (AttachFirstFrame sm p)  = object
    [ "site"          .= ("first_frame" :: Text)
    , "setup_method"  .= sm
    , "param"         .= p
    ]
  toJSON (AttachInRpcParam p)     = object ["site" .= ("in_rpc_param" :: Text), "param" .= p]

-- | Identity of the issuing party: the Origin the credential was issued from
--   and the method that issued it. Mirrors the Rust
--   @plexus_auth_core::CredentialIssuer@.
--
--   Wire shape: both fields serialize transparently as bare strings (the Rust
--   side uses @#[serde(transparent)]@ on the @Origin@ and @MethodPath@
--   newtypes).
data CredentialIssuer = CredentialIssuer
  { credIssuerOrigin :: Text  -- ^ Backend Origin (e.g., @"ws://localhost:4444"@)
  , credIssuerMethod :: Text  -- ^ Method path (e.g., @"auth.login"@)
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CredentialIssuer where
  parseJSON = withObject "CredentialIssuer" $ \o -> CredentialIssuer
    <$> o .: "origin"
    <*> o .: "method"

instance ToJSON CredentialIssuer where
  toJSON CredentialIssuer{..} = object
    [ "origin" .= credIssuerOrigin
    , "method" .= credIssuerMethod
    ]

-- | Per-credential metadata. Mirrors the Rust
--   @plexus_auth_core::CredentialMetadata@.
--
--   All fields are wire-typed exactly per the Rust serde derives:
--
--   * @scheme@, @expires_at@, @refresh_via@, @revoke_via@: optional, omitted
--     from the wire when 'Nothing'.
--   * @scopes@: always present; empty list when no scopes.
--   * @sensitive@: always 'True' (the Rust side enforces this); carried for
--     symmetry with the runtime metadata.
--   * @expires_at@: ISO 8601 timestamp as a string; the IR decoder does NOT
--     parse it into a 'DateTime' — downstream consumers do.
data CredentialMetadata = CredentialMetadata
  { cmKind       :: CredentialKind
  , cmAttachAs   :: AttachmentSite
  , cmScheme     :: Maybe Text         -- ^ e.g., @"Bearer "@; @Nothing@ when omitted
  , cmScopes     :: [Text]             -- ^ Capability identifiers; empty when unscoped
  , cmExpiresAt  :: Maybe Text         -- ^ ISO 8601 timestamp; the IR does not parse this
  , cmRefreshVia :: Maybe Text         -- ^ Refresh method path; @Nothing@ when none
  , cmRevokeVia  :: Maybe Text         -- ^ Revoke method path; @Nothing@ when none
  , cmIssuer     :: CredentialIssuer
  , cmSensitive  :: Bool               -- ^ Always 'True'; preserved for symmetry
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CredentialMetadata where
  parseJSON = withObject "CredentialMetadata" $ \o -> CredentialMetadata
    <$> o .:  "kind"
    <*> o .:  "attach_as"
    <*> o .:? "scheme"
    <*> o .:? "scopes"      .!= []
    <*> o .:? "expires_at"
    <*> o .:? "refresh_via"
    <*> o .:? "revoke_via"
    <*> o .:  "issuer"
    <*> o .:? "sensitive"   .!= True

instance ToJSON CredentialMetadata where
  toJSON CredentialMetadata{..} = object $
    [ "kind"      .= cmKind
    , "attach_as" .= cmAttachAs
    , "scopes"    .= cmScopes
    , "issuer"    .= cmIssuer
    , "sensitive" .= cmSensitive
    ] ++ catMaybes
    [ ("scheme"       .=) <$> cmScheme
    , ("expires_at"   .=) <$> cmExpiresAt
    , ("refresh_via"  .=) <$> cmRefreshVia
    , ("revoke_via"   .=) <$> cmRevokeVia
    ]

-- | One credential-bearing field declaration. Mirrors the Rust
--   @plexus_core::plexus::schema::CredentialFieldDecl@.
--
--   * 'cfdFieldPath' locates the credential within the method's return type
--     (one entry per object-field path segment, no array indices).
--   * 'cfdVariantTag' is set when the return type is an enum and the
--     credential lives on one variant; 'Nothing' for struct returns.
--   * 'cfdMetadata' is the full per-credential metadata.
data CredentialFieldDecl = CredentialFieldDecl
  { cfdFieldPath  :: [Text]            -- ^ Object-field path within the return type
  , cfdVariantTag :: Maybe Text        -- ^ Enum-variant tag; 'Nothing' for struct returns
  , cfdMetadata   :: CredentialMetadata
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CredentialFieldDecl where
  parseJSON = withObject "CredentialFieldDecl" $ \o -> CredentialFieldDecl
    <$> o .:  "field_path"
    <*> o .:? "variant_tag"
    <*> o .:  "metadata"

instance ToJSON CredentialFieldDecl where
  toJSON CredentialFieldDecl{..} = object $
    [ "field_path" .= cfdFieldPath
    , "metadata"   .= cfdMetadata
    ] ++ catMaybes
    [ ("variant_tag" .=) <$> cfdVariantTag
    ]

-- | What credential a method requires on input (when implicit-derived from
--   scope tagging or refresh/revoke linkage). Mirrors the Rust
--   @plexus_core::plexus::schema::RequiredCredential@.
--
--   Wire-compact shape:
--
--   * @kind@ and @site_hint@ omitted when 'Nothing' (matches
--     @#[serde(skip_serializing_if = "Option::is_none")]@).
--   * @scopes@ omitted when empty (matches
--     @#[serde(skip_serializing_if = "Vec::is_empty")]@).
data RequiredCredential = RequiredCredential
  { rcKind     :: Maybe CredentialKind  -- ^ Specific kind required; 'Nothing' for "any kind matching scope"
  , rcScopes   :: [Text]                -- ^ Required scope set; wildcard-matched against candidate scopes
  , rcSiteHint :: Maybe AttachmentSite  -- ^ Advisory preferred attach site
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON RequiredCredential where
  parseJSON = withObject "RequiredCredential" $ \o -> RequiredCredential
    <$> o .:? "kind"
    <*> o .:? "scopes"     .!= []
    <*> o .:? "site_hint"

instance ToJSON RequiredCredential where
  toJSON RequiredCredential{..} = object $ catMaybes
    [ ("kind"      .=) <$> rcKind
    , if null rcScopes then Nothing else Just ("scopes" .= rcScopes)
    , ("site_hint" .=) <$> rcSiteHint
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
  , methodRole            :: MethodRole             -- ^ Structural role in the activation graph (IR-2 / IR-3); defaults to 'MethodRoleRpc'
  , methodCredentials     :: [CredentialFieldDecl]
    -- ^ Credential-bearing fields in this method's return type
    --   (AUTHZ-CRED-IR-1). One entry per @#[plexus::credential(...)]@-
    --   annotated field, in declaration order. Empty when the return type
    --   contains no credentials. Pre-CRED-CORE-3 producers omit the key
    --   entirely; the parser defaults it to @[]@.
  , methodRequiresCredential :: Maybe RequiredCredential
    -- ^ Implicit-derived required-credential filter for this method
    --   (AUTHZ-CRED-IR-1). 'Nothing' for @public@ methods or methods with
    --   no scope-derived requirement. Pre-CRED-CORE-3 producers omit the
    --   key entirely; the parser defaults it to 'Nothing'.
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
    <*> o .:? "role" .!= MethodRoleRpc
    <*> o .:? "credentials"          .!= []
    <*> o .:? "requires_credential"

instance ToJSON MethodSchema where
  toJSON MethodSchema{..} =
    -- Emit pre-IR-1 fields verbatim; the two AUTHZ-CRED-IR-1 fields are
    -- elided when empty/Nothing to match the producer-side
    -- 'skip_serializing_if' behaviour and preserve wire back-compat for
    -- methods with no credential metadata.
    object $
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
      ] ++ catMaybes
      [ if null methodCredentials
          then Nothing
          else Just ("credentials" .= methodCredentials)
      , ("requires_credential" .=) <$> methodRequiresCredential
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
  , psRequest         :: Maybe Value           -- ^ JSON Schema of the activation's PlexusRequest struct (REQ-4/REQ-5)
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
    <*> o .:? "request"

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
    , "request"          .= psRequest
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
