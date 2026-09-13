<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
<h1 align="center">copybook-rs</h1>

<p align="center">
  <em>Deterministic COBOL copybook parsing and mainframe record conversion.</em>
</p>

<p align="center">
  <a href="https://github.com/EffortlessMetrics/copybook-rs/actions/workflows/ci-quick.yml"><img src="https://github.com/EffortlessMetrics/copybook-rs/actions/workflows/ci-quick.yml/badge.svg" alt="CI Quick" /></a>
  <a href="https://codecov.io/gh/EffortlessMetrics/copybook-rs"><img src="https://codecov.io/gh/EffortlessMetrics/copybook-rs/branch/main/graph/badge.svg" alt="Codecov" /></a>
  <a href="https://github.com/EffortlessMetrics/copybook-rs/actions/workflows/security-scan.yml"><img src="https://github.com/EffortlessMetrics/copybook-rs/workflows/Weekly%20Security%20Scan/badge.svg" alt="Security Audit" /></a>
  <a href="https://github.com/EffortlessMetrics/copybook-rs/blob/main/.github/dependabot.yml"><img src="https://img.shields.io/badge/dependencies-Dependabot-blue.svg" alt="Dependabot" /></a>
</p>

<p align="center">
  <a href="https://github.com/EffortlessMetrics/copybook-rs/releases/latest"><img src="https://img.shields.io/github/v/release/EffortlessMetrics/copybook-rs?sort=semver&label=release" alt="GitHub release" /></a>
  <a href="https://crates.io/crates/copybook-cli"><img src="https://img.shields.io/crates/d/copybook-cli.svg?label=crates.io%20downloads" alt="crates.io downloads" /></a>
  <a href="https://docs.rs/copybook"><img src="https://docs.rs/copybook/badge.svg" alt="docs.rs" /></a>
</p>

<p align="center">
  <a href="https://doc.rust-lang.org/cargo/reference/manifest.html#the-rust-version-field"><img src="https://img.shields.io/badge/MSRV-1.95-blue.svg" alt="MSRV 1.95" /></a>
  <a href="#license"><img src="https://img.shields.io/badge/License-AGPL--3.0--or--later-blue.svg" alt="License: AGPL-3.0-or-later" /></a>
</p>

**copybook-rs turns COBOL copybooks and fixed-length or RDW mainframe records into JSON you can trust — byte-for-byte deterministic.**

It does not run COBOL. It makes mainframe data reviewable outside the mainframe.

The first useful run should feel small:

```text
one copybook
-> one record file
-> one JSONL line per record
-> the same bytes every time
```

## Prerequisites

- **Rust ≥ 1.95** (2024 edition). Check with `rustc --version`; update with `rustup update stable`.
- A COBOL copybook (`.cpy`) and a fixed-length or RDW record file. No mainframe access needed.

## The first useful run

Install the CLI from crates.io and decode an EBCDIC file to JSON:

```bash
cargo install copybook-cli@0.6.0 --locked

# Fetch the example fixtures (or use your own copybook + data)
curl -LO https://github.com/EffortlessMetrics/copybook-rs/raw/v0.6.0/fixtures/copybooks/simple.cpy
curl -LO https://github.com/EffortlessMetrics/copybook-rs/raw/v0.6.0/fixtures/data/simple.bin

# Decode EBCDIC fixture to JSON
copybook decode simple.cpy simple.bin \
  --format fixed --codepage cp037 \
  --output demo.jsonl

# View the result
cat demo.jsonl
# {"CUSTOMER-ID":"123456","CUSTOMER-NAME":"John Smith",...,"ACCOUNT-BALANCE":"12345.67",...}
```

The bundled `simple.cpy` / `simple.bin` pair demonstrates EBCDIC-to-JSON conversion with COMP-3 packed-decimal fields.

You work in five key terms. Everything else in this README and the reference
docs expands on them:

| Term | One-line meaning |
| --- | --- |
| **copybook** | the COBOL record description — the schema source of truth |
| **layout** | the resolved byte map: offsets, lengths, REDEFINES, OCCURS |
| **record** | one fixed-length (or RDW-framed) byte slice decoded against a layout |
| **codepage** | the EBCDIC/ASCII mapping (CP037/CP273/CP500/CP1047/CP1140) applied to text |
| **round-trip** | decode-then-encode reproducing the input bytes exactly |

For Rust library use, depend on the canonical facade:

```toml
[dependencies]
copybook = "=0.6.0"
```

```rust
use copybook::core::parse_copybook;
use copybook::codec::{decode_record, DecodeOptions};
```

(`copybook-rs` is a redirect/search alias for the same API; `copybook-core` /
`copybook-codec` remain available as intentional granular crates.)

To build from source instead, clone the repo, `git checkout v0.6.0`, and
`cargo build --release`; the binary is `./target/release/copybook`.

## Status

Engineering Preview (v0.6.0). Stable CLI and library APIs; feature completeness
is preview-level. See [ROADMAP.md](docs/ROADMAP.md) for adoption guidance and
known limitations.

## How copybook-rs works (reference)

> Internal vocabulary and capability detail live here and below. The first
> screen above is all a new user needs to start.

`copybook-core` parses the copybook into a schema and resolves it to a byte
layout; `copybook-codec` decodes each record slice against that layout
(charset conversion, COMP-3/zoned/overpunch numerics, edited PIC, ODO and
REDEFINES handling) and emits canonical JSONL. `copybook-cli` orchestrates the
pipeline: `parse`, `inspect`, `decode`, `encode`, `verify`, `determinism`,
`support`, and `audit`.

## Where it fits

```text
generic ETL:     moves bytes; schema is your problem
copybook-rs:     the copybook IS the schema, bytes round-trip exactly
COBOL runtime:   executes programs; needs the mainframe
```

`copybook-rs` is offline and read-only by default: no network, no mainframe
connection, no source edits. Raw-capture modes embed record bytes as base64 —
treat outputs as sensitive when inputs are.

## What it supports

### Supported

- **Data types**: DISPLAY, Zoned Decimal, COMP-3, BINARY, COMP-1/COMP-2, Edited PIC
- **Structure**: REDEFINES, OCCURS (fixed), ODO (tail position), Level-88, RENAMES (R1-R3)
- **Formats**: Fixed-length and RDW records; CP037/CP273/CP500/CP1047/CP1140
- **Features**: Field projection (`--select`), Dialect lever (`--dialect`), Deterministic round-trip

### Not supported (by design)

- Nested ODO (O5/O6), ODO over REDEFINES
- RENAMES with REDEFINES/OCCURS (R4-R6)
- EXTERNAL / GLOBAL clauses

See [COBOL_SUPPORT_MATRIX.md](docs/reference/COBOL_SUPPORT_MATRIX.md) for the full feature matrix.

## Exit codes

| Code | Tag | Meaning (1-liner) | Test |
|----:|:----:|--------------------|------|
| 2 | CBKD | Data quality failure | exit_code_mapping::exit_code_cbkd_is_2 |
| 3 | CBKE | Encode/validation failure | exit_code_mapping::exit_code_cbke_is_3 |
| 4 | CBKF | File read or record format/RDW failure | exit_code_mapping::exit_code_cbkf_is_4 |
| 5 | CBKI | Internal orchestration error | exit_code_mapping::exit_code_cbki_is_5 |

## Documentation

| Document | Description |
|----------|-------------|
| [Getting Started](docs/tutorials/getting-started.md) | Tutorial with bundled fixtures |
| [Documentation Start](docs/START_HERE.md) | Hand-maintained documentation entry point |
| [CLI Reference](docs/CLI_REFERENCE.md) | Command-line interface documentation |
| [Library API](docs/reference/LIBRARY_API.md) | Rust library API reference |
| [Error Codes](docs/reference/ERROR_CODES.md) | Error taxonomy |
| [Support Matrix](docs/reference/COBOL_SUPPORT_MATRIX.md) | COBOL feature coverage |
| [Engineering Report](docs/REPORT.md) | Readiness and current engineering status |
| [Stability Guarantees](docs/STABILITY_GUARANTEES.md) | API stability contract and versioning policy |
| [Support Policy](docs/SUPPORT_POLICY.md) | Release support windows and response times |
| [Roadmap](docs/ROADMAP.md) | Project status and what's next |

## Development

```bash
just build    # cargo build --workspace
just test     # cargo nextest run
just lint     # clippy, pedantic
just fmt      # rustfmt
```

See [CONTRIBUTING.md](CONTRIBUTING.md) for the full development workflow.

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
