# Property Testing Guide

This document explains the property testing setup for the copybook-rs project and provides guidance on running and writing property tests.

## Overview

Property testing is a testing technique that verifies that certain properties (invariants) hold true for a wide range of randomly generated inputs. Unlike traditional unit tests that check specific examples, property tests check general rules across many automatically generated test cases.

The copybook-rs project uses the [proptest](https://docs.rs/proptest/) framework for property-based testing.

## Why Property Testing?

Property testing provides several benefits:

1. **Edge Case Discovery**: Randomly generated inputs often uncover edge cases that manual test design might miss
2. **Regression Prevention**: Property tests catch regressions across a wide range of inputs, not just specific examples
3. **Shrinking**: When a test fails, proptest automatically finds the minimal failing case
4. **Confidence**: Testing with hundreds of random cases provides higher confidence than a few manual examples

## Running Property Tests

### Run All Property Tests

```bash
# Run all property tests in the workspace
cargo test --test proptest

# Run with verbose output
cargo test --test proptest -- --nocapture

# Run with specific number of test cases
PROPTEST_CASES=1024 cargo test --test proptest
```

### Run Property Tests for a Specific Crate

```bash
# Run property tests for copybook-core
cargo test -p copybook-core --test proptest

# Run property tests for copybook-codec
cargo test -p copybook-codec --test proptest
```

### Run a Specific Property Test

```bash
# Run a specific test
cargo test --test proptest prop_ascii_roundtrip

# Run tests matching a pattern
cargo test --test proptest roundtrip
```

### Configuration Options

Property tests can be configured via environment variables:

| Variable | Description | Default |
|-----------|-------------|----------|
| `PROPTEST_CASES` | Number of test cases to run | 256 |
| `PROPTEST_SEED` | Random seed for reproducibility | Random |
| `PROPTEST_NUMBER_OF_THREADS` | Number of threads to use | CPU count |
| `PROPTEST_TIMEOUT` | Timeout per test case in seconds | 60 |

### Reproducible Testing

To reproduce a failing property test with the same inputs:

```bash
# Use a fixed seed
PROPTEST_SEED="copybook-rs-test-123" cargo test --test proptest

# The seed will be printed in test output
# Use that seed to reproduce the exact same test cases
```

## Property Test Structure

Property tests in copybook-rs are organized in the `tests/proptest/` directory:

```
tests/proptest/
├── mod.rs              # Module declarations and configuration
├── generators.rs        # Custom proptest strategies
├── roundtrip.rs        # Round-trip encoding/decoding tests
├── parsing.rs          # Copybook parsing invariant tests
├── numeric.rs          # Numeric field handling tests
├── arrays.rs           # OCCURS/ODO handling tests
├── pic_clauses.rs      # PIC clause property tests
└── redefines.rs        # REDEFINES clause property tests
```

## Property Test Invariants

### Round-Trip Tests (`roundtrip.rs`)

These tests verify that `encode(decode(data)) == data` for various data types:

- **ASCII zoned decimal round-trip**: Ensures ASCII zoned decimals preserve data
- **EBCDIC zoned decimal round-trip**: Ensures EBCDIC zoned decimals preserve data
- **Signed zoned decimal round-trip**: Ensures signed values preserve sign
- **Alphanumeric round-trip**: Ensures text data is preserved
- **Multi-field round-trip**: Ensures records with multiple fields round-trip correctly

### Parsing Tests (`parsing.rs`)

These tests verify that parsed copybooks maintain structural invariants:

- **Valid field hierarchy**: Ensures level numbers are valid and properly nested
- **Monotonic offsets**: Ensures field offsets increase monotonically
- **Consistent total size**: Ensures total record size matches field layout
- **PIC clause idempotency**: Ensures PIC clauses parse consistently
- **Unique field names**: Ensures field names are unique within each level

### Numeric Tests (`numeric.rs`)

These tests verify numeric field handling:

- **Field size matches PIC**: Ensures field sizes match PIC specifications
- **Value preservation**: Ensures numeric values round-trip correctly
- **Decimal precision**: Ensures decimal values preserve precision
- **Zero values**: Ensures zero values round-trip correctly
- **Maximum values**: Ensures maximum values round-trip correctly
- **Negative values**: Ensures negative values preserve sign
- **COMP-3 encoding**: Ensures packed decimal encoding preserves values
- **Binary encoding**: Ensures binary encoding preserves values

### Array Tests (`arrays.rs`)

These tests verify OCCURS and ODO handling:

- **Element size consistency**: Ensures OCCURS elements have consistent sizes
- **Array round-trip**: Ensures arrays round-trip all elements
- **OCCURS TO bounds**: Ensures OCCURS with TO clause maintains bounds
- **ODO size relationship**: Ensures ODO maintains size relationships
- **ODO varying sizes**: Ensures ODO works with varying array sizes
- **Nested OCCURS**: Ensures nested OCCURS maintain structure

### PIC Clause Tests (`pic_clauses.rs`)

These tests verify PIC clause parsing:

- **9(n) size**: Ensures numeric PIC clauses produce correct sizes
- **X(n) size**: Ensures alphanumeric PIC clauses produce correct sizes
- **S9(n) size**: Ensures signed numeric PIC clauses produce correct sizes
- **Decimal PIC size**: Ensures decimal PIC clauses produce correct sizes
- **COMP-3 size**: Ensures COMP-3 PIC clauses produce correct sizes
- **Binary PIC size**: Ensures binary PIC clauses produce correct sizes
- **Edited format size**: Ensures edited PIC formats produce correct sizes

### REDEFINES Tests (`redefines.rs`)

These tests verify REDEFINES clause handling:

- **Same offset**: Ensures REDEFINES fields have the same offset
- **Different sizes**: Ensures REDEFINES fields can have different sizes
- **Multiple REDEFINES**: Ensures multiple fields can redefine the same field
- **Different types**: Ensures REDEFINES works with different data types
- **Offset consistency**: Ensures REDEFINES chains maintain consistent offsets
- **Total size**: Ensures REDEFINES doesn't affect total record size

## Writing New Property Tests

### Basic Structure

```rust
#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_core::parse_copybook;
use copybook_codec::{Codepage, DecodeOptions, EncodeOptions, RecordFormat};
use proptest::prelude::*;

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,  // Number of test cases
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_my_test_case(
        input in prop::collection::vec(0u8..=255, 1..=100)
    ) {
        // Your test logic here
        prop_assert!(/* some condition */);
        prop_assert_eq!(/* expected */, /* actual */);
    }
}
```

### Using Custom Generators

The `generators.rs` module provides custom strategies for generating copybook syntax and data:

```rust
use crate::proptest::generators::*;

// Generate a simple copybook
let copybook = simple_copybook_strategy();

// Generate PIC clause patterns
let pic = pic_clause_strategy();

// Generate ASCII zoned decimal data
let data = ascii_zoned_strategy(length);

// Generate JSON data for encoding
let json = json_object_strategy();
```

### Best Practices

1. **Use meaningful property names**: Names should describe what invariant is being tested
2. **Keep tests focused**: Each test should verify one specific invariant
3. **Use appropriate case counts**: More complex tests may need more cases
4. **Handle errors gracefully**: Use `prop_assert!` for conditions that may fail
5. **Document invariants**: Add comments explaining what property is being tested

### Test Configuration

Use the configuration constants from `config` module:

```rust
use crate::proptest::config::*;

proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,      // 256 cases
        // cases: QUICK_CASES,        // 64 cases (CI)
        // cases: COMPREHENSIVE_CASES, // 1024 cases (slow)
        ..ProptestConfig::default()
    })]
    // ...
}
```

## Regression Tracking

When a property test fails, proptest automatically:

1. **Shrinks the input**: Finds the minimal failing case
2. **Saves the regression**: Stores the failing case in `proptest-regressions/`
3. **Replays on future runs**: Uses saved regressions to prevent regressions

### Managing Regressions

Regression files are stored in:

- `tests/proptest-regressions/` - Integration test regressions
- `copybook-core/tests/proptest-regressions/` - Core crate regressions
- `copybook-codec/tests/proptest-regressions/` - Codec crate regressions

### Clearing Regressions

After fixing a failing test, you can clear the regression files:

```bash
# Clear all regression files
find . -name "*.proptest-regressions" -type d -exec rm -rf {} +

# Or clear specific directory
rm -rf tests/proptest-regressions/*
```

**Note**: Only clear regressions after fixing the underlying issue, otherwise the test will fail again.

## CI Integration

Property tests run automatically in CI via [`.github/workflows/ci-proptest.yml`](../.github/workflows/ci-proptest.yml):

- **Trigger**: On push to main/develop, pull requests, and weekly schedule
- **Matrix**: Tests run on multiple OS and Rust version combinations
- **Cases**: 256 cases for normal runs, 1024 for scheduled runs
- **Artifacts**: Failing regressions are uploaded as artifacts

### CI Configuration

Property tests in CI use these settings:

```yaml
env:
  PROPTEST_CASES: ${{ github.event_name == 'schedule' && '1024' || '256' }}
  PROPTEST_SEED: "copybook-rs-proptest"
```

## Troubleshooting

### Test Fails Intermittently

If a test fails intermittently:

1. **Check for non-determinism**: Ensure the test doesn't depend on external state
2. **Use a fixed seed**: Run with `PROPTEST_SEED` to reproduce
3. **Increase timeout**: Set `PROPTEST_TIMEOUT` if tests are slow

### Test Times Out

If tests timeout:

1. **Reduce case count**: Use `QUICK_CASES` instead of `DEFAULT_CASES`
2. **Simplify generators**: Make the input strategy less complex
3. **Increase timeout**: Set `PROPTEST_TIMEOUT` to a higher value

### Shrinking Takes Too Long

If shrinking is slow:

1. **Disable shrinking**: Set `shrink = false` in `ProptestConfig`
2. **Limit shrink iterations**: Set `max_shrink_iters` in `proptest.toml`

## Additional Resources

- [proptest documentation](https://docs.rs/proptest/)
- [Property-Based Testing in Rust](https://blog.panicsoftware.com/property-based-testing-in-rust/)
- [The proptest book](https://altf4.gitlab.io/proptest-book/proptest/getting-started.html)

## Feature Flags

Property tests can be enabled/disabled via feature flags:

```bash
# Run property tests with specific features
cargo test --test proptest --features comp3_fast

# Run property tests with audit feature
cargo test --test proptest --features audit
```

## Performance Considerations

Property tests can be computationally expensive. To balance coverage and runtime:

- **Quick runs**: Use `QUICK_CASES` (64 cases) for rapid feedback
- **Normal runs**: Use `DEFAULT_CASES` (256 cases) for development
- **Comprehensive runs**: Use `COMPREHENSIVE_CASES` (1024 cases) for CI/nightly

## Contributing

When adding new property tests:

1. **Add to appropriate module**: Place tests in the relevant `tests/proptest/*.rs` file
2. **Use existing generators**: Leverage strategies from `generators.rs` when possible
3. **Add regression directory**: Ensure `proptest-regressions/` exists for the test file
4. **Update documentation**: Document new invariants in this file
5. **Test locally**: Run tests locally before pushing to ensure they pass
