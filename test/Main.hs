module Main (main) where

import Test.Hspec
import qualified CurrentBugsSpec

main :: IO ()
main = do
  putStrLn ""
  putStrLn "=========================================="
  putStrLn "  plexus-protocol Regression Test Suite"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn "CURRENT STATUS: Tests are expected to FAIL"
  putStrLn "  - Each FAILING test proves a bug exists"
  putStrLn "  - plexus-protocol accepts invalid data"
  putStrLn ""
  putStrLn "AFTER INTEGRATION: Tests should PASS"
  putStrLn "  - Passing tests prove validation works"
  putStrLn "  - synapse-types catches all invalid data"
  putStrLn ""
  putStrLn "Run this after integrating synapse-types to verify fixes!"
  putStrLn "=========================================="
  putStrLn ""

  hspec $ do
    describe "Validation Bugs (should fail now, pass after fix)" CurrentBugsSpec.spec
