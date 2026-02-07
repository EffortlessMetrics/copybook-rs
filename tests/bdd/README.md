# BDD Tests for copybook-rs

This directory contains Behavior Driven Development (BDD) tests for the copybook-rs project using the [Cucumber](https://github.com/cucumber-rs/cucumber) framework and Gherkin syntax.

## Overview

BDD tests describe the expected behavior of the copybook-rs library in human-readable Gherkin syntax. These tests serve as both executable tests and living documentation of the system's behavior.

## Structure

```
tests/bdd/
├── Cargo.toml              # BDD test dependencies
├── bdd.rs                  # Main test runner and step definitions
├── features/               # Gherkin feature files
│   ├── copybook_parsing.feature
│   ├── encode_decode.feature
│   └── error_handling.feature
└── README.md              # This file
```

## Running BDD Tests

### Run all BDD tests

```bash
cargo test -p copybook-bdd
```

### Run specific feature file

```bash
# Run only copybook parsing tests
cargo test -p copybook-bdd -- features/copybook_parsing.feature

# Run only encode/decode tests
cargo test -p copybook-bdd -- features/encode_decode.feature

# Run only error handling tests
cargo test -p copybook-bdd -- features/error_handling.feature
```

### Run with verbose output

```bash
cargo test -p copybook-bdd -- --nocapture
```

### Run specific scenario

```bash
cargo test -p copybook-bdd -- "Parse a simple copybook with a single field"
```

## Feature Files

### copybook_parsing.feature

Tests for copybook parsing functionality:
- Simple copybooks with single fields
- Numeric fields (packed, binary, zoned)
- OCCURS clauses and ODO (OCCURS DEPENDING ON)
- REDEFINES clauses
- Level-88 condition values
- Strict vs. tolerant parsing modes
- Inline comments
- Field offset and length calculations
- Nested group structures

### encode_decode.feature

Tests for encode and decode operations:
- ASCII and EBCDIC codepage support
- Binary to JSON decoding
- JSON to binary encoding
- Round-trip lossless conversion
- Numeric field handling
- Array field handling
- Nested group structures
- Metadata emission
- Number coercion

### error_handling.feature

Tests for error handling and edge cases:
- Syntax errors in copybooks
- Invalid OCCURS clauses
- Invalid PIC clauses
- Binary data that is too short
- Invalid encoding data
- Missing required fields in JSON
- Invalid field types in JSON
- Duplicate field names
- Invalid level numbers
- Unterminated strings
- Invalid REDEFINES
- Circular REDEFINES
- ODO count validation
- Level-88 validation
- Empty data handling
- Malformed JSONL
- Unsupported edited PIC clauses
- Nested ODO restrictions

## Writing New BDD Tests

### 1. Add a new scenario to an existing feature file

Edit one of the `.feature` files in the `features/` directory:

```gherkin
Scenario: Your new scenario description
  Given some precondition
  When some action occurs
  Then some expected outcome
```

### 2. Create a new feature file

Create a new `.feature` file in the `features/` directory:

```gherkin
Feature: Your feature name

  As a role
  I want a feature
  So that I get a benefit

  Scenario: First scenario
    Given a precondition
    When an action
    Then an outcome
```

### 3. Add new step definitions

If you need new steps that aren't already defined in [`bdd.rs`](bdd.rs), add them to the `steps` module:

```rust
#[given(expr = "your step expression")]
async fn your_step(world: &mut CopybookWorld, param: String) {
    // Your step implementation
}

#[when(expr = "another step expression")]
async fn another_step(world: &mut CopybookWorld) {
    // Your step implementation
}

#[then(expr = "expected outcome")]
async fn expected_outcome(world: &mut CopybookWorld, expected: String) {
    // Your step implementation
}
```

Then register your new steps in the `main` function:

```rust
let runner = CopybookWorld::init(&[
    // ... existing steps
    steps::your_step,
    steps::another_step,
    steps::expected_outcome,
]);
```

## Step Patterns

The BDD framework supports various step patterns:

- **Exact match**: `expr = "exact text"`
- **String parameter**: `expr = "a {string} parameter"`
- **Integer parameter**: `expr = "{int} items"`
- **Float parameter**: `expr = "{float} value"`
- **Regex**: `expr = "a pattern with (.*)"`

## Best Practices

1. **Keep scenarios independent**: Each scenario should be able to run independently of others
2. **Use descriptive names**: Scenario names should clearly describe what is being tested
3. **Focus on behavior**: Test the behavior, not the implementation details
4. **Use Given-When-Then structure**: Follow the BDD pattern consistently
5. **Reuse steps**: Define reusable steps that can be used across multiple scenarios
6. **Keep scenarios simple**: Each scenario should test one specific behavior
7. **Use table data**: For multiple similar test cases, consider using Gherkin tables

## CI Integration

BDD tests are automatically run as part of the CI workflow. See the `.github/workflows/ci.yml` file for details.

## Troubleshooting

### Tests fail to compile

Make sure all step definitions are registered in the `main` function in [`bdd.rs`](bdd.rs).

### Step not found

If you see "Step not found" errors, check that:
1. The step expression in the feature file matches the `expr` parameter in the step definition
2. The step function is registered in the `main` function
3. The step function has the correct signature

### Tests pass but should fail

Check that your assertions in the step definitions are correct and match the expected behavior.

## Resources

- [Cucumber for Rust](https://github.com/cucumber-rs/cucumber)
- [Gherkin Syntax](https://cucumber.io/docs/gherkin/)
- [BDD Best Practices](https://cucumber.io/docs/bdd/)
- [copybook-rs Documentation](https://docs.rs/copybook-core)
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../../LICENSE).
