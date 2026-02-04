# plexus-protocol

**Haskell types for Plexus RPC** — shared between Synapse and other Plexus RPC clients.

## Layers

```
┌─────────────────────────────────────────┐
│  Plexus.Types                           │  Stream items, provenance
│  Plexus.Schema.Recursive                │  PluginSchema, MethodSchema
├─────────────────────────────────────────┤
│  Substrate.Transport                    │  RPC calls, schema fetching
│  Substrate.Client                       │  WebSocket connection
└─────────────────────────────────────────┘
```

**Substrate** = transport layer modules (WebSocket, JSON-RPC)
**Plexus** = Plexus RPC protocol types (schemas, stream items)

## Usage

```haskell
import Plexus
import qualified Streaming.Prelude as S

main :: IO ()
main = do
  conn <- connect defaultConfig
  S.print $ substrateRpc conn "health.check" (object [])
  disconnect conn
```

### Schema Fetching

```haskell
import Substrate.Transport

-- Fetch plugin schema
Right schema <- fetchSchemaAt defaultConfig ["echo"]
print $ psNamespace schema  -- "echo"
print $ psMethods schema    -- [MethodSchema...]

-- Fetch method schema directly
Right method <- fetchMethodSchemaAt defaultConfig ["echo"] "once"
print $ methodName method   -- "once"
print $ methodParams method -- Just (JSON Schema)
```

## Types

### Stream Items

```haskell
data PlexusStreamItem
  = StreamProgress Provenance PluginHash Text (Maybe Int)
  | StreamData Provenance PluginHash Text Value
  | StreamError Provenance PluginHash Text Bool
  | StreamDone Provenance PluginHash
```

### Schema Types

```haskell
data PluginSchema = PluginSchema
  { psNamespace       :: Text
  , psVersion         :: Text
  , psDescription     :: Text
  , psLongDescription :: Maybe Text
  , psHash            :: PluginHash
  , psMethods         :: [MethodSchema]
  , psChildren        :: Maybe [ChildSummary]
  }

data MethodSchema = MethodSchema
  { methodName        :: Text
  , methodDescription :: Text
  , methodHash        :: PluginHash
  , methodParams      :: Maybe Value  -- JSON Schema
  , methodReturns     :: Maybe Value  -- JSON Schema
  }

data SchemaResult
  = SchemaPlugin PluginSchema
  | SchemaMethod MethodSchema
```

## Structure

```
src/
├── Plexus.hs                   # Re-exports
├── Plexus/
│   ├── Types.hs                # PlexusStreamItem, Provenance
│   ├── Schema.hs               # Schema re-exports
│   └── Schema/
│       ├── Recursive.hs        # PluginSchema, MethodSchema, SchemaResult
│       └── Cache.hs            # Schema caching
└── Substrate/
    ├── Client.hs               # WebSocket connection
    └── Transport.hs            # RPC calls, schema fetching
```

## Build

```bash
cabal build
cabal test
```

## License

MIT
