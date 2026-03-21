{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Regression tests for validation bugs in plexus-protocol
--
-- These tests currently FAIL, proving that bugs exist.
-- After integrating synapse-types, these tests should PASS.
--
-- Each test represents a bug that needs to be fixed:
-- - Test expects REJECTION of invalid data (Left)
-- - Currently gets ACCEPTANCE (Right) - TEST FAILS
-- - After fix, gets REJECTION (Left) - TEST PASSES
module CurrentBugsSpec where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Plexus.Types (StreamMetadata(..), PlexusStreamItem(..), Provenance(..))

spec :: Spec
spec = do
  describe "REGRESSION: Invalid hash length" $ do
    it "should reject short hash" $ do
      let json = "{\"provenance\":[\"s\"],\"plexus_hash\":\"short\",\"timestamp\":1735052400}"
      case eitherDecode json :: Either String StreamMetadata of
        Left _ -> pure ()  -- Expected: rejection
        Right meta -> expectationFailure $
          "BUG: Accepted invalid hash (too short): " <> show (metaPlexusHash meta)

    it "should reject long hash" $ do
      let json = "{\"provenance\":[\"s\"],\"plexus_hash\":\"abc123def4567890toolong\",\"timestamp\":1735052400}"
      case eitherDecode json :: Either String StreamMetadata of
        Left _ -> pure ()  -- Expected: rejection
        Right meta -> expectationFailure $
          "BUG: Accepted invalid hash (too long): " <> show (metaPlexusHash meta)

    it "should only accept exactly 16 chars" $ do
      let json = "{\"provenance\":[\"s\"],\"plexus_hash\":\"abc123def4567890\",\"timestamp\":1735052400}"
      case eitherDecode json :: Either String StreamMetadata of
        Right meta -> T.length (metaPlexusHash meta) `shouldBe` 16
        Left err -> expectationFailure $ "Valid hash rejected: " <> err

  describe "REGRESSION: Invalid hash characters" $ do
    it "should reject uppercase hex" $ do
      let json = "{\"provenance\":[\"s\"],\"plexus_hash\":\"ABC123DEF4567890\",\"timestamp\":1735052400}"
      case eitherDecode json :: Either String StreamMetadata of
        Left _ -> pure ()  -- Expected: rejection
        Right meta -> expectationFailure $
          "BUG: Accepted uppercase hex: " <> show (metaPlexusHash meta)

    it "should reject non-hex characters" $ do
      let json = "{\"provenance\":[\"s\"],\"plexus_hash\":\"xyz123def4567890\",\"timestamp\":1735052400}"
      case eitherDecode json :: Either String StreamMetadata of
        Left _ -> pure ()  -- Expected: rejection
        Right meta -> expectationFailure $
          "BUG: Accepted non-hex characters: " <> show (metaPlexusHash meta)

    it "should reject SQL injection attempt" $ do
      let json = "{\"provenance\":[\"s\"],\"plexus_hash\":\"'; DROP TABLE u\",\"timestamp\":1735052400}"
      case eitherDecode json :: Either String StreamMetadata of
        Left _ -> pure ()  -- Expected: rejection
        Right meta -> expectationFailure $
          "BUG: Accepted SQL injection string: " <> show (metaPlexusHash meta)

    it "should reject mixed case hex" $ do
      let json = "{\"provenance\":[\"s\"],\"plexus_hash\":\"Abc123Def4567890\",\"timestamp\":1735052400}"
      case eitherDecode json :: Either String StreamMetadata of
        Left _ -> pure ()  -- Expected: rejection
        Right meta -> expectationFailure $
          "BUG: Accepted mixed case hex: " <> show (metaPlexusHash meta)

  describe "REGRESSION: Empty provenance" $ do
    it "should reject empty provenance array" $ do
      let json = "{\"provenance\":[],\"plexus_hash\":\"abc123def4567890\",\"timestamp\":1735052400}"
      case eitherDecode json :: Either String StreamMetadata of
        Left _ -> pure ()  -- Expected: rejection
        Right meta -> do
          let Provenance segs = metaProvenance meta
          expectationFailure $
            "BUG: Accepted empty provenance (length " <> show (length segs) <> ")"

    it "should accept non-empty provenance" $ do
      let json = "{\"provenance\":[\"substrate\"],\"plexus_hash\":\"abc123def4567890\",\"timestamp\":1735052400}"
      case eitherDecode json :: Either String StreamMetadata of
        Right meta -> do
          let Provenance segs = metaProvenance meta
          length segs `shouldBe` 1
        Left err -> expectationFailure $ "Valid provenance rejected: " <> err

  describe "REGRESSION: Percentage out of range" $ do
    it "should reject percentage > 100" $ do
      let json = "{\"type\":\"progress\",\"metadata\":{\"provenance\":[\"s\"],\"plexus_hash\":\"abc123def4567890\",\"timestamp\":1735052400},\"message\":\"test\",\"percentage\":150}"
      case eitherDecode json :: Either String PlexusStreamItem of
        Left _ -> pure ()  -- Expected: rejection
        Right (StreamProgress _ _ _ (Just pct)) -> expectationFailure $
          "BUG: Accepted percentage > 100: " <> show pct
        Right other -> expectationFailure $ "Wrong item type: " <> show other

    it "should reject negative percentage" $ do
      let json = "{\"type\":\"progress\",\"metadata\":{\"provenance\":[\"s\"],\"plexus_hash\":\"abc123def4567890\",\"timestamp\":1735052400},\"message\":\"test\",\"percentage\":-50}"
      case eitherDecode json :: Either String PlexusStreamItem of
        Left _ -> pure ()  -- Expected: rejection
        Right (StreamProgress _ _ _ (Just pct)) -> expectationFailure $
          "BUG: Accepted negative percentage: " <> show pct
        Right other -> expectationFailure $ "Wrong item type: " <> show other

    it "should accept percentage 0-100" $ do
      let json = "{\"type\":\"progress\",\"metadata\":{\"provenance\":[\"s\"],\"plexus_hash\":\"abc123def4567890\",\"timestamp\":1735052400},\"message\":\"test\",\"percentage\":50}"
      case eitherDecode json :: Either String PlexusStreamItem of
        Right (StreamProgress _ _ _ (Just pct)) -> pct `shouldBe` 50
        Right other -> expectationFailure $ "Wrong item type: " <> show other
        Left err -> expectationFailure $ "Valid percentage rejected: " <> err

  describe "REGRESSION: No format validation" $ do
    it "should reject arbitrary text as hash" $ do
      let json = "{\"provenance\":[\"s\"],\"plexus_hash\":\"this is not a hash at all!\",\"timestamp\":1735052400}"
      case eitherDecode json :: Either String StreamMetadata of
        Left _ -> pure ()  -- Expected: rejection
        Right meta -> expectationFailure $
          "BUG: Accepted arbitrary text: " <> show (metaPlexusHash meta)

  describe "REGRESSION: Buffer overflow (DoS vector)" $ do
    it "should reject extremely long hash" $ do
      let longHash = Prelude.replicate 10000 'a' :: String
      let json = "{\"provenance\":[\"s\"],\"plexus_hash\":\"" <> longHash <> "\",\"timestamp\":1735052400}"
      case eitherDecode (LBS8.pack json) :: Either String StreamMetadata of
        Left _ -> pure ()  -- Expected: rejection
        Right meta -> expectationFailure $
          "BUG: Accepted " <> show (T.length $ metaPlexusHash meta) <> "-character hash (DoS vector)"

  describe "BASELINE: Float timestamps (Aeson rejects these)" $ do
    -- These tests document that Aeson's Int64 parser rejects floats
    -- They should PASS now and continue to PASS after integration
    it "correctly rejects float timestamp" $ do
      let json = "{\"provenance\":[\"substrate\"],\"plexus_hash\":\"abc123def4567890\",\"timestamp\":1735052400.789}"
      case eitherDecode json :: Either String StreamMetadata of
        Left err -> err `shouldContain` "parsing Int64 failed"
        Right _ -> expectationFailure "Should have rejected float timestamp"

    it "correctly rejects float in stream item" $ do
      let json = "{\"type\":\"data\",\"metadata\":{\"provenance\":[\"s\"],\"plexus_hash\":\"abc123def4567890\",\"timestamp\":1735052400.5},\"content_type\":\"test\",\"content\":{}}"
      case eitherDecode json :: Either String PlexusStreamItem of
        Left err -> err `shouldContain` "parsing Int64 failed"
        Right _ -> expectationFailure "Should have rejected float timestamp"

  describe "ERROR MESSAGES: Poor UX" $ do
    it "gives cryptic error for camelCase fields" $ do
      let json = "{\"provenance\":[\"s\"],\"plexusHash\":\"abc123def4567890\",\"timestamp\":1735052400}"
      case eitherDecode json :: Either String StreamMetadata of
        Left err -> do
          -- Currently just says "plexus_hash" not found
          err `shouldContain` "plexus_hash"
          -- After integration, should explain camelCase vs snake_case
        Right _ -> expectationFailure "Should have failed on missing field"
