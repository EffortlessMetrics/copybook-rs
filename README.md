<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-rs

[![CI Quick](https://github.com/EffortlessMetrics/copybook-rs/actions/workflows/ci-quick.yml/badge.svg)](https://github.com/EffortlessMetrics/copybook-rs/actions/workflows/ci-quick.yml)
[![codecov](https://codecov.io/gh/EffortlessMetrics/copybook-rs/branch/main/graph/badge.svg)](https://codecov.io/gh/EffortlessMetrics/copybook-rs)
[![Security Audit](https://github.com/EffortlessMetrics/copybook-rs/workflows/Weekly%20Security%20Scan/badge.svg)](https://github.com/EffortlessMetrics/copybook-rs/actions/workflows/security-scan.yml)
[![Dependency Review](https://img.shields.io/badge/dependencies-Dependabot-blue.svg)](https://github.com/EffortlessMetrics/copybook-rs/blob/main/.github/dependabot.yml)
[![MSRV](https://img.shields.io/badge/MSRV-1.95-blue.svg)](https://www.rust-lang.org)
[![License: AGPL-3.0-or-later](https://img.shields.io/badge/License-AGPL--3.0--or--later-blue.svg)](LICENSE)

Rust toolkit for COBOL copybook parsing and fixed-record data conversion. Deterministic, memory-safe, zero `unsafe` in public APIs.

## Status

Engineering Preview (v0.5.0). Stable CLI and library APIs; feature completeness is preview-level. See [ROADMAP.md](docs/ROADMAP.md) for adoption guidance and known limitations.

<!-- TEST_STATUS:BEGIN -->
**conformance:** 9968/9968  • **roundtrip:** N/A  • **negative:** N/A  • **skipped:** 0  • **leaks:** 0  
_Source: CI receipts (nextest/junit). This block is updated automatically._
<!-- TEST_STATUS:END -->

## Try It Now

Install the CLI from crates.io and decode an EBCDIC file to JSON:

```bash
cargo install copybook-cli@0.5.0 --locked

# Fetch the example fixtures (or use your own copybook + data)
curl -LO https://github.com/EffortlessMetrics/copybook-rs/raw/v0.5.0/fixtures/copybooks/simple.cpy
curl -LO https://github.com/EffortlessMetrics/copybook-rs/raw/v0.5.0/fixtures/data/simple.bin

# Decode EBCDIC fixture to JSON
copybook decode simple.cpy simple.bin \
  --format fixed --codepage cp037 \
  --output demo.jsonl

# View the result
cat demo.jsonl
# {"CUSTOMER-ID":"123456","CUSTOMER-NAME":"John Smith",...,"ACCOUNT-BALANCE":"12345.67",...}
```

The included `simple.cpy` copybook and `simple.bin` data file demonstrate EBCDIC-to-JSON conversion with COMP-3 packed decimal fields.

For Rust library use, depend on the canonical facade:

```toml
[dependencies]
copybook = "=0.5.0"
```

```rust
use copybook::core::parse_copybook;
use copybook::codec::{decode_record, DecodeOptions};
```

(`copybook-rs` is a redirect/search alias for the same API; `copybook-core`/`copybook-codec` remain available as intentional granular crates.)

To build from source instead, clone the repo, `git checkout v0.5.0`, and `cargo build --release`; the binary is `./target/release/copybook`.

## What It Supports

### Supported

- **Data types**: DISPLAY, Zoned Decimal, COMP-3, BINARY, COMP-1/COMP-2, Edited PIC
- **Structure**: REDEFINES, OCCURS (fixed), ODO (tail position), Level-88, RENAMES (R1-R3)
- **Formats**: Fixed-length and RDW records; CP037/CP273/CP500/CP1047/CP1140
- **Features**: Field projection (`--select`), Dialect lever (`--dialect`), Deterministic round-trip

### Not Supported (by design)

- Nested ODO (O5/O6), ODO over REDEFINES
- RENAMES with REDEFINES/OCCURS (R4-R6)
- EXTERNAL / GLOBAL clauses

See [COBOL_SUPPORT_MATRIX.md](docs/reference/COBOL_SUPPORT_MATRIX.md) for the full feature matrix.

## Documentation

| Document | Description |
|----------|-------------|
| [Getting Started](docs/tutorials/getting-started.md) | Tutorial with bundled fixtures |
| [Documentation Start](docs/START_HERE.md) | Hand-maintained documentation entry point |
| [CLI Reference](docs/CLI_REFERENCE.md) | Command-line interface documentation |
| [Library API](docs/reference/LIBRARY_API.md) | Rust library API reference |
| [Error Codes](docs/reference/ERROR_CODES.md) | Error taxonomy (10 families, 63 codes) |
| [Support Matrix](docs/reference/COBOL_SUPPORT_MATRIX.md) | COBOL feature coverage |
| [Engineering Report](docs/REPORT.md) | Readiness and current engineering status |
| [Design Docs](docs/design/) | Feature and design contracts |
| [Internal Docs](docs/internal/) | Internal implementation and spec references |
| [Stability Guarantees](docs/STABILITY_GUARANTEES.md) | API stability contract and versioning policy |
| [Support Policy](docs/SUPPORT_POLICY.md) | Release support windows and response times |
| [API Freeze Audit](docs/API_FREEZE_AUDIT.md) | Checklist for pre-v1.0 stability freeze completion |
| [Roadmap](docs/ROADMAP.md) | Project status and what's next |

## Stability and support posture

- **Stability contract:** see [Stability Guarantees](docs/STABILITY_GUARANTEES.md)
- **Freeze rules:** see [API Freeze](docs/API_FREEZE.md)
- **Support window:** see [Support Policy](docs/SUPPORT_POLICY.md)
- **Freeze audit checklist:** see [API Freeze Audit](docs/API_FREEZE_AUDIT.md)

## Exit Codes

| Code | Tag  | Meaning (1-liner) | Test |
|----:|:----:|--------------------|------|
| 2 | CBKD | Data quality failure | exit_code_mapping::exit_code_cbkd_is_2 |
| 3 | CBKE | Encode/validation failure | exit_code_mapping::exit_code_cbke_is_3 |
| 4 | CBKF | Record format/RDW failure | exit_code_mapping::exit_code_cbkf_is_4 |
| 5 | CBKI | Internal orchestration error | exit_code_mapping::exit_code_cbki_is_5 |

## Development

```bash
# Full validation pipeline
cargo build --workspace --release && cargo test --workspace && cargo clippy --workspace -- -D warnings -W clippy::pedantic && cargo fmt --all --check

# Quick test
cargo test --workspace

# Benchmarks
cargo bench --package copybook-bench
```

See [CONTRIBUTING.md](CONTRIBUTING.md) for the full development workflow.

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).

