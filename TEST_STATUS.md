# plexus-protocol Regression Test Status

**Date**: 2025-03-20
**Command**: `cabal test plexus-protocol`

## Current Status: 11/17 Tests FAILING ❌

```
Total Tests:     17
FAILING:         11  (bugs exist)
PASSING:          6  (correct behavior)
```

## What This Means

### ❌ 11 FAILING Tests = 11 BUGS CONFIRMED

These tests **expect** plexus-protocol to reject invalid data.
Currently, plexus-protocol **accepts** invalid data → **TEST FAILS**.

After integrating synapse-types, plexus-protocol will **reject** invalid data → **TEST PASSES**.

### ✓ 6 PASSING Tests = Correct Behavior

These tests verify that:
- Valid data is accepted
- Aeson already rejects float timestamps
- Error messages exist (even if cryptic)

## Bug Breakdown

### REGRESSION: Invalid Hash Length (2 bugs)

```
❌ should reject short hash
   BUG: Accepted invalid hash (too short): "short"

❌ should reject long hash
   BUG: Accepted invalid hash (too long): "abc123def4567890toolong"

✓ should only accept exactly 16 chars
   (Valid hash correctly accepted)
```

### REGRESSION: Invalid Hash Characters (4 bugs)

```
❌ should reject uppercase hex
   BUG: Accepted uppercase hex: "ABC123DEF4567890"

❌ should reject non-hex characters
   BUG: Accepted non-hex characters: "xyz123def4567890"

❌ should reject SQL injection attempt
   BUG: Accepted SQL injection string: "'; DROP TABLE u"

❌ should reject mixed case hex
   BUG: Accepted mixed case hex: "Abc123Def4567890"
```

### REGRESSION: Empty Provenance (1 bug)

```
❌ should reject empty provenance array
   BUG: Accepted empty provenance (length 0)

✓ should accept non-empty provenance
   (Valid provenance correctly accepted)
```

### REGRESSION: Percentage Out of Range (2 bugs)

```
❌ should reject percentage > 100
   BUG: Accepted percentage > 100: 150.0

❌ should reject negative percentage
   BUG: Accepted negative percentage: -50.0

✓ should accept percentage 0-100
   (Valid percentage correctly accepted)
```

### REGRESSION: No Format Validation (1 bug)

```
❌ should reject arbitrary text as hash
   BUG: Accepted arbitrary text: "this is not a hash at all!"
```

### REGRESSION: Buffer Overflow (1 bug)

```
❌ should reject extremely long hash
   BUG: Accepted 10000-character hash (DoS vector)
```

### BASELINE: Float Timestamps (2 tests - already working!)

```
✓ correctly rejects float timestamp
   (Aeson's Integer parser rejects floats)

✓ correctly rejects float in stream item
   (Aeson's Integer parser rejects floats)
```

### ERROR MESSAGES: Poor UX (1 test)

```
✓ gives cryptic error for camelCase fields
   (Error exists, just not helpful)
```

## After Integration

When we integrate synapse-types into plexus-protocol:

```
Expected: 17/17 PASSING ✓
  - All invalid data rejected
  - All valid data accepted
  - Better error messages
```

## Test Execution

```bash
# Run regression tests
cd /workspace/hypermemetic/synapse
cabal test plexus-protocol

# Current output:
# 17 examples, 11 failures ← Proves 11 bugs exist

# After integrating synapse-types:
# 17 examples, 0 failures ← Proves all bugs fixed
```

## Bug Severity

| Severity | Count | Examples |
|----------|-------|----------|
| Critical | 2 | SQL injection, DoS (10k chars) |
| High | 7 | Invalid hash format, empty provenance |
| Medium | 2 | Percentage out of range |
| Low | 0 | - |

## What Each Failure Proves

1. **No hash length validation** - Accepts 5-char and 24-char hashes
2. **No hash format validation** - Accepts non-hex, uppercase, mixed case
3. **No security validation** - Accepts SQL injection strings
4. **No DoS protection** - Accepts 10k-character hashes
5. **No provenance validation** - Accepts empty arrays
6. **No bounds checking** - Accepts percentages > 100 and < 0

## Comparison

| Test | Current (FAILS) | With synapse-types (PASSES) |
|------|-----------------|----------------------------|
| Short hash | Accepts ❌ | Rejects ✓ |
| Long hash | Accepts ❌ | Rejects ✓ |
| Uppercase hex | Accepts ❌ | Rejects ✓ |
| Non-hex chars | Accepts ❌ | Rejects ✓ |
| SQL injection | Accepts ❌ | Rejects ✓ |
| Empty provenance | Accepts ❌ | Rejects ✓ |
| Percentage 150 | Accepts ❌ | Rejects ✓ |
| Percentage -50 | Accepts ❌ | Rejects ✓ |
| Arbitrary text | Accepts ❌ | Rejects ✓ |
| 10k-char hash | Accepts ❌ | Rejects ✓ |
| Float timestamp | Rejects ✓ | Rejects ✓ |

## Conclusion

**11 failing tests prove plexus-protocol has serious validation gaps.**

These aren't theoretical bugs - they're **demonstrated with concrete test cases**:
- SQL injection strings accepted
- DoS vectors (unbounded strings) accepted
- Protocol violations (empty provenance, invalid hashes) accepted

The tests are now correctly structured:
- **FAIL now** = Bug exists
- **PASS after fix** = Bug fixed

This is the standard regression test pattern.
