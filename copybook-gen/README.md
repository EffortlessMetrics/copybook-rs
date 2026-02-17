# `copybook-gen`

Deterministic fixture generation utilities for `copybook-rs` tests.

Use `copybook-gen` to create synthetic copybooks, payloads, and golden test suites for parser/codec validation.

## What it does

- Generate valid copybooks with REDEFINES and ODO shapes for stress coverage.
- Produce record payloads with deterministic pseudo-random seeds.
- Create targeted invalid and corrupted fixtures to test error paths.
- Build tagged golden test suites for reproducible regression checks.

## Quick start

```bash
cargo test -p copybook-gen
cargo run --example roundtrip_repro -p copybook-gen
```

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

let copybook = generate_copybook(&config);
let schema = parse_copybook(&copybook)?;
let payloads = generate_data_with_strategy(&schema, &config, DataStrategy::Normal);

println!("generated payloads: {}", payloads.len());
```

## License

Part of `copybook-rs`, licensed under `AGPL-3.0-or-later`.
