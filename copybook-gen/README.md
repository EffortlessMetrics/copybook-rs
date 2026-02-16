# `copybook-gen`

`copybook-gen` generates synthetic COBOL copybooks, synthetic binary payloads, and golden test artifacts for copybook-rs.

## Purpose

- Deterministic fixture generation for copybook parser and codec tests.
- Positive and negative dataset synthesis for edge-case coverage.
- Golden test suite builders for repeatable regression checks.
- Enterprise-style synthetic fixture generation for production-like scenarios.

## Features

- Copybook generators for:
  - Simple records
  - REDEFINES and OCCURS structures
  - OCCURS DEPENDING ON variants
  - Invalid / malformed fixtures
- Data generation strategies for realistic, corrupted, and targeted test data.
- Golden test suite helpers with tagging and metadata.
- Deterministic output with explicit random seed support.

## Key Types

- `GeneratorConfig`
- `generate_complete_test_suite`
- `generate_comprehensive_test_suite`
- `generate_performance_test_suite`
- `generate_negative_test_suite`
- `generate_data`, `generate_data_with_strategy`
- `generate_invalid_copybooks`
- `generate_corrupted_data`

## Quick Start

```bash
cargo test -p copybook-gen
```

Run the example generator:

```bash
cargo run --example roundtrip_repro -p copybook-gen
```

## Example

```rust
use copybook_gen::{GeneratorConfig, generate_copybook, generate_data_with_strategy};
use copybook_gen::data::DataStrategy;
use copybook_core::parse_copybook;

let config = GeneratorConfig {
    seed: 2024,
    record_count: 10,
    include_edge_cases: true,
    include_invalid_data: false,
};

let copybook_text = generate_copybook(&config);
let schema = parse_copybook(&copybook_text).expect("parse copybook");
let data = generate_data_with_strategy(&schema, &config, DataStrategy::Normal);

println!("records: {}", data.len());
```

## Test Fixtures

The crate ships internal test sources under `tests/` and examples under `examples/`.

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
