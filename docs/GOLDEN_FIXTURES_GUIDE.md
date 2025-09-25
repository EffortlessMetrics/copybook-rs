# Golden Fixtures Usage Guide

## Overview

The Golden Fixtures framework in copybook-rs provides comprehensive structural validation for enterprise COBOL scenarios. This guide covers how to use, create, and maintain golden fixtures for production-ready validation.

## What are Golden Fixtures?

Golden fixtures are test cases that validate COBOL copybook parsing and data processing against known-good reference outputs using SHA-256 cryptographic verification. They ensure consistent behavior across different environments, versions, and configurations.

### Key Features

- **Structural Validation**: ODO (Occurs Depending On), Level-88 condition values, REDEFINES interactions
- **Enterprise Scenarios**: Real-world mainframe patterns from banking, insurance, retail, manufacturing
- **Performance Integration**: Automated performance regression detection with baselines
- **SHA-256 Verification**: Cryptographic validation of test outputs for consistency
- **Comprehensive Coverage**: 458+ tests including edge cases and enterprise production scenarios

## Running Golden Fixtures

### Basic Usage

```bash
# Run all golden fixtures
cargo test --workspace --test "*golden*"

# Run comprehensive golden fixture validation
cargo test --test golden_fixtures_comprehensive

# Run specific fixture categories
cargo test --test golden_fixtures_odo                    # ODO structural validation
cargo test --test golden_fixtures_level88               # Level-88 condition values
cargo test --test golden_fixtures_redefines            # REDEFINES interactions
cargo test --test golden_fixtures_enterprise           # Enterprise production scenarios
```

### Performance Testing Integration

```bash
# Performance integration testing
cargo test --test golden_fixtures_ac6_performance_integration

# Performance benchmarking with golden fixtures
PERF=1 cargo bench --package copybook-bench -- golden_fixtures

# Run with performance validation enabled
GOLDEN_FIXTURES=1 cargo test --workspace
```

## Fixture Categories

### 1. ODO Structural Validation (`golden_fixtures_odo`)

Tests OCCURS DEPENDING ON structural constraints and positioning rules.

**Scenarios Covered:**
- ODO arrays at tail position (valid)
- ODO arrays with Level-88 conditions following (valid)
- ODO arrays with storage siblings following (invalid)
- ODO counter field validation
- ODO bounds enforcement during processing

**Example Test Cases:**
```
AC2: Level-88 After ODO - Validates non-storage condition values following variable arrays
AC3: Child Inside ODO - Validates internal structure of variable arrays
AC4: Sibling After ODO - Validates rejection of storage fields after ODO (negative test)
```

### 2. Level-88 Condition Values (`golden_fixtures_level88`)

Tests Level-88 condition name placement, VALUE clause parsing, and semantic validation.

**Scenarios Covered:**
- Level-88 conditions after ODO arrays
- Level-88 conditions within nested groups
- Level-88 with REDEFINES interactions
- VALUE clause parsing (single, multiple, ranges)
- Condition value validation

### 3. REDEFINES Interactions (`golden_fixtures_redefines`)

Tests REDEFINES clause interactions with other structural elements.

**Scenarios Covered:**
- REDEFINES with ODO arrays
- REDEFINES with Level-88 conditions
- Memory layout validation
- Size constraint validation
- Nested REDEFINES hierarchies

### 4. Enterprise Production Scenarios (`golden_fixtures_enterprise`)

Real-world mainframe record layouts and patterns from production environments.

**Industry Domains:**
- Banking: Customer accounts, transaction records, loan processing
- Insurance: Policy records, claims processing, actuarial data
- Retail: Product catalogs, sales transactions, inventory management
- Manufacturing: Production schedules, quality metrics, supply chain data

## Creating New Golden Fixtures

### Using the Library API

```rust
use copybook_gen::golden::{GoldenTest, GoldenTestSuite, TestConfig};

// Create a golden test with specific configuration
let config = TestConfig {
    codepage: "cp037".to_string(),
    record_format: "fixed".to_string(),
    json_number_mode: "lossless".to_string(),
    flags: vec!["level88".to_string(), "odo".to_string()],
};

let golden_test = GoldenTest::new_with_config(
    "enterprise_banking_customer_record",
    &copybook_text,
    &test_data,
    config
);

// Create test suite for enterprise scenarios
let mut suite = GoldenTestSuite::new(
    "enterprise_banking_tests",
    "Production banking mainframe scenarios"
);
suite.add_test(golden_test);

// Validate fixture outputs with SHA-256 verification
let is_valid = golden_test.validate_string_output("json", &output_json);
```

### Command Line Generation

```bash
# Generate new golden fixtures (when implemented)
cargo run --package copybook-gen -- generate-golden-fixtures \
    --enterprise \
    --output fixtures/enterprise/ \
    --categories odo,level88,redefines
```

## Golden Fixture Structure

### File Organization

```
/fixtures/
├── enterprise/              # Enhanced enterprise patterns
│   ├── copybooks/          # Real-world COBOL structures
│   ├── data/               # Corresponding binary samples
│   ├── golden/             # Expected JSON outputs with metadata
│   └── metadata/           # Structural validation metadata
├── copybooks/              # Basic test patterns
├── data/                   # Basic binary samples
├── golden/                 # Basic expected outputs
└── schemas/                # JSON meta-schemas
```

### Fixture Metadata

Each fixture includes comprehensive metadata:

```json
{
  "fixture_id": "ac2_level88_after_odo_basic",
  "category": "level_88",
  "structural_elements": ["ODO", "LEVEL_88"],
  "validation_rules": [
    {
      "rule_id": "level88_after_odo",
      "description": "Level-88 conditions allowed after ODO arrays",
      "expected_result": "PASS",
      "error_code": null
    }
  ],
  "performance_criteria": {
    "min_throughput_mbps": 100,
    "max_memory_mb": 256,
    "max_variance_percent": 2.0
  },
  "enterprise_context": {
    "industry_domain": "banking",
    "record_type": "customer_account",
    "complexity_level": "INTERMEDIATE"
  }
}
```

## Validation and Error Handling

### Error Code Coverage

Golden fixtures comprehensively test structural error conditions:

- `CBKP021_ODO_NOT_TAIL`: ODO array positioning validation
- `CBKS121_COUNTER_NOT_FOUND`: ODO counter field validation
- `CBKS301_ODO_CLIPPED`: ODO bounds enforcement
- `CBKS302_ODO_RAISED`: ODO minimum value validation
- Plus comprehensive coverage of parsing, schema, and data validation error codes

### SHA-256 Verification

All fixtures use cryptographic hashing to ensure consistent outputs:

```rust
// Example validation in test
let actual_hash = GoldenTest::hash_string(&output_json);
let expected_hash = fixture.expected_outputs.get("json").unwrap();
assert_eq!(actual_hash, *expected_hash, "Output hash mismatch for fixture: {}", fixture.name);
```

## Performance Standards

### Throughput Targets

| Fixture Category | Min Throughput | Performance Variance |
|------------------|----------------|---------------------|
| ODO Structural | 80 MB/s | <3% |
| Level-88 | 100 MB/s | <2% |
| REDEFINES | 75 MB/s | <4% |
| Complex Scenarios | 60 MB/s | <5% |

### Memory Constraints

- **Steady State**: <256 MiB for processing multi-GB fixture sets
- **Peak Memory**: <512 MiB during fixture generation
- **Memory Growth**: <2% per 1000 fixtures processed

### Regression Detection

Performance baselines are automatically established and monitored:

```rust
// Example performance validation
let baseline = monitor.establish_baseline(&previous_metrics);
let regression_report = monitor.detect_regression(&current_metrics, &baseline);

if regression_report.has_regression() {
    panic!("Performance regression detected: {:?}", regression_report);
}
```

## Integration with CI/CD

### Quality Gates

Golden fixtures integrate with the standard quality gates:

```bash
# Format, lint, build, test pipeline with golden fixtures
cargo fmt --all --check
cargo clippy --workspace -- -D warnings -W clippy::pedantic
cargo build --workspace --release
cargo test --workspace  # Includes all golden fixtures
```

### Performance Monitoring

```bash
# Continuous performance validation
PERF=1 cargo bench --package copybook-bench -- golden_fixtures

# Performance regression detection in CI
if cargo bench --package copybook-bench -- golden_fixtures | grep "regression"; then
    echo "Performance regression detected in golden fixtures"
    exit 1
fi
```

## Best Practices

### Creating Effective Fixtures

1. **Use Real-World Data**: Base fixtures on actual mainframe record layouts
2. **Cover Edge Cases**: Include boundary conditions and error scenarios
3. **Document Context**: Provide clear descriptions and industry context
4. **Performance Aware**: Include performance criteria appropriate for the scenario
5. **Comprehensive Coverage**: Test all relevant structural interactions

### Maintaining Fixtures

1. **Regular Updates**: Keep fixtures current with schema changes
2. **Performance Monitoring**: Monitor for regressions in CI/CD
3. **Hash Verification**: Always validate output consistency
4. **Documentation**: Maintain clear fixture documentation and metadata
5. **Categorization**: Organize fixtures by structural elements and complexity

### Debugging Fixture Failures

```bash
# Run specific failing fixture with verbose output
cargo test --test golden_fixtures_odo -- --nocapture ac2_level88_after_odo

# Compare actual vs expected outputs
cargo test --test golden_fixtures_comprehensive -- --nocapture | grep "hash mismatch"

# Check performance regressions
PERF=1 cargo bench --package copybook-bench -- golden_fixtures | grep -A5 -B5 "regression"
```

## Enterprise Deployment

### Production Validation

Golden fixtures provide enterprise deployment confidence through:

- **Structural Integrity**: Validates all COBOL structural constraints
- **Performance Assurance**: Ensures enterprise throughput targets are met
- **Error Handling**: Validates comprehensive error taxonomy
- **Regression Prevention**: Prevents structural and performance regressions
- **Industry Patterns**: Tests against real-world mainframe scenarios

### Compliance and Auditing

- **Cryptographic Verification**: SHA-256 hashes provide audit trails
- **Deterministic Results**: Consistent outputs across environments
- **Comprehensive Coverage**: 458+ tests covering all production scenarios
- **Performance Baselines**: Documented performance characteristics
- **Enterprise Patterns**: Validated against industry-standard record layouts

The Golden Fixtures framework ensures copybook-rs maintains enterprise-grade reliability and performance while providing comprehensive validation of all structural COBOL elements.