<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# BDD Testing in copybook-rs

## Overview

copybook-rs uses Behavior Driven Development (BDD) testing to describe and verify the expected behavior of the library in human-readable Gherkin syntax. These tests serve as both executable tests and living documentation.

> **See Also**: [Testing Integration Summary](TESTING_INTEGRATION_SUMMARY.md) for comprehensive documentation on how BDD integrates with all testing methodologies.

## Why BDD?

BDD testing provides several benefits for the copybook-rs project:

1. **Living Documentation**: Feature files describe the expected behavior in plain English, making it easy for non-technical stakeholders to understand what the system does.

2. **Shared Understanding**: BDD encourages collaboration between developers, QA, and business stakeholders by focusing on behavior rather than implementation details.

3. **Executable Specifications**: Feature files are not just documentation—they're executable tests that verify the system behaves as described.

4. **Better Test Organization**: BDD organizes tests by feature and scenario, making it easier to find and understand test coverage.

5. **Regression Prevention**: BDD tests catch regressions early by verifying that the system continues to behave as expected after changes.

## Framework Choice

copybook-rs uses the **[Cucumber](https://github.com/cucumber-rs/cucumber)** framework for BDD testing in Rust. Cucumber was chosen because:

- **Mature and Well-Maintained**: Cucumber for Rust is actively maintained and has a stable API.
- **Gherkin Support**: Full support for standard Gherkin syntax (Given-When-Then).
- **Async Support**: Built-in async support for complex test scenarios.
- **Integration**: Works well with Rust's testing ecosystem and cargo.

## BDD Test Structure

```
tests/bdd/
├── Cargo.toml              # BDD test dependencies
├── bdd.rs                  # Main test runner and step definitions
├── features/               # Gherkin feature files
│   ├── copybook_parsing.feature
│   ├── encode_decode.feature
│   └── error_handling.feature
└── README.md              # BDD test documentation
```

### Feature Files

Feature files are written in Gherkin syntax and describe the expected behavior of the system:

```gherkin
Feature: Copybook Parsing

  As a developer working with COBOL copybooks
  I want to parse copybook definitions into a structured schema
  So that I can understand and validate the data structure

  Scenario: Parse a simple copybook with a single field
    Given a simple copybook with a single field
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)
```

### Step Definitions

Step definitions are Rust functions that connect Gherkin steps to actual code:

```rust
#[given(expr = "a simple copybook with a single field")]
async fn given_simple_copybook(world: &mut CopybookWorld) {
    world.copybook_text = Some("01 TEST-RECORD.\n    05 TEST-FIELD PIC X(10).".to_string());
}

#[when(expr = "the copybook is parsed")]
async fn when_copybook_is_parsed(world: &mut CopybookWorld) {
    let copybook_text = world.copybook_text.as_ref().expect("Copybook text not set");
    world.schema = match parse_copybook(copybook_text) {
        Ok(schema) => Some(schema),
        Err(e) => {
            world.error = Some(e);
            None
        }
    };
}

#[then(expr = "the schema should be successfully parsed")]
async fn then_schema_successfully_parsed(world: &mut CopybookWorld) {
    assert!(world.schema.is_some(), "Schema should be parsed successfully");
    assert!(world.error.is_none(), "No error should occur");
}
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

## BDD Test Coverage

The BDD test suite covers the following areas:

### Copybook Parsing (`copybook_parsing.feature`)

- Simple copybooks with single fields
- Numeric fields (packed, binary, zoned)
- OCCURS clauses and ODO (OCCURS DEPENDING ON)
- REDEFINES clauses
- Level-88 condition values
- Strict vs. tolerant parsing modes
- Inline comments
- Field offset and length calculations
- Nested group structures

### Encode/Decode Operations (`encode_decode.feature`)

- ASCII and EBCDIC codepage support
- Binary to JSON decoding
- JSON to binary encoding
- Round-trip lossless conversion
- Numeric field handling
- Array field handling
- Nested group structures
- Metadata emission
- Number coercion

### Error Handling (`error_handling.feature`)

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

Edit one of the `.feature` files in the `tests/bdd/features/` directory:

```gherkin
Scenario: Your new scenario description
  Given some precondition
  When some action occurs
  Then some expected outcome
```

### 2. Create a new feature file

Create a new `.feature` file in the `tests/bdd/features/` directory:

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

If you need new steps that aren't already defined in `tests/bdd/bdd.rs`, add them to the `steps` module:

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

## Best Practices

1. **Keep scenarios independent**: Each scenario should be able to run independently of others
2. **Use descriptive names**: Scenario names should clearly describe what is being tested
3. **Focus on behavior**: Test behavior, not implementation details
4. **Use Given-When-Then structure**: Follow the BDD pattern consistently
5. **Reuse steps**: Define reusable steps that can be used across multiple scenarios
6. **Keep scenarios simple**: Each scenario should test one specific behavior
7. **Use table data**: For multiple similar test cases, consider using Gherkin tables

## CI Integration

BDD tests are automatically run as part of the CI workflow. The BDD test job is defined in `.github/workflows/ci.yml`:

```yaml
bdd-tests:
  name: BDD Tests
  runs-on: ubuntu-latest
  steps:
  - uses: actions/checkout@v6
  - uses: dtolnay/rust-toolchain@master
    with:
      toolchain: stable
  - uses: Swatinem/rust-cache@v2
  - name: Run BDD tests
    run: cargo test -p copybook-bdd -- --nocapture
```

## Relationship to Other Tests

BDD tests complement the existing test suite in copybook-rs:

- **Unit Tests**: Test individual functions and methods in isolation
- **Integration Tests**: Test the interaction between multiple components
- **Property-Based Tests**: Test properties that should hold for all inputs
- **Golden Fixture Tests**: Verify consistent outputs for known inputs
- **BDD Tests**: Describe and verify system behavior in human-readable terms

BDD tests are not a replacement for other types of tests, but rather an additional layer of testing that focuses on behavior and documentation.

## Troubleshooting

### Tests fail to compile

Make sure all step definitions are registered in the `main` function in `tests/bdd/bdd.rs`.

### Step not found

If you see "Step not found" errors, check that:
1. The step expression in the feature file matches the `expr` parameter in the step definition
2. The step function is registered in the `main` function
3. The step function has the correct signature

### Tests pass but should fail

Check that your assertions in the step definitions are correct and match the expected behavior.

### CI failures

Check the CI logs for detailed error messages. BDD tests run with `--nocapture` to provide detailed output.

## Resources

- [Cucumber for Rust](https://github.com/cucumber-rs/cucumber)
- [Gherkin Syntax](https://cucumber.io/docs/gherkin/)
- [BDD Best Practices](https://cucumber.io/docs/bdd/)
- [copybook-rs BDD README](../tests/bdd/README.md)
- [copybook-rs Documentation](https://docs.rs/copybook-core)
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
