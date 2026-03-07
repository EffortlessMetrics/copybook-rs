<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# CLAUDE.md

Maintainer and operator guidance for Claude Code working in this repository.

## Project Identity

- Rust workspace, Edition 2024, MSRV 1.92
- COBOL copybook parsing and data conversion (EBCDIC/ASCII to JSON and back)
- Engineering Preview v0.4.3
- 36 publishable crates under `crates/`, 3 dev-only under `tools/`, 3 test suites under `tests/`

## Canonical Sources

Do not restate feature behavior in this file. Read the canonical source instead.

| Concern | Canonical file |
|---|---|
| Feature support | `docs/reference/COBOL_SUPPORT_MATRIX.md` |
| Error codes | `docs/reference/ERROR_CODES.md` |
| CLI flags/examples | `docs/CLI_REFERENCE.md` |
| Library API | `docs/reference/LIBRARY_API.md` |
| Performance policy | `docs/PERFORMANCE_GOVERNANCE.md` |
| Perf receipts | `scripts/bench/perf.json` |
| Project status | `docs/ROADMAP.md` |
| Dialect behavior | `docs/internal/features/d0_dialect_lever_contract.md` |
| Edited PIC behavior | `docs/internal/features/e3_edited_pic_encode_contract.md` |
| ODO design | `docs/design/NESTED_ODO_BEHAVIOR.md` |
| RENAMES design | `docs/design/RENAMES_NESTED_GROUPS.md` |
| Release process | `docs/RELEASE_PROCESS.md` |
| Stability contract | `docs/STABILITY_GUARANTEES.md` |
| Support policy | `docs/SUPPORT_POLICY.md` |
| User docs entry | `docs/START_HERE.md` |

## Workspace Layout

```
crates/                           # 36 publishable crates
  Parser:     copybook-core, copybook-lexer
  Codec:      copybook-codec, copybook-codec-memory, copybook-codepage,
              copybook-charset, copybook-overpunch, copybook-zoned-format
  CLI:        copybook-cli, copybook-cli-determinism, copybook-options
  Framing:    copybook-fixed, copybook-rdw, copybook-rdw-predicates,
              copybook-record-io
  Schema:     copybook-dialect, copybook-determinism, copybook-support-matrix
  Governance: copybook-contracts, copybook-governance,
              copybook-governance-contracts, copybook-governance-grid,
              copybook-governance-runtime
  Safety:     copybook-error, copybook-error-reporter, copybook-overflow,
              copybook-safe-index, copybook-safe-ops, copybook-safe-text,
              copybook-utils
  Quality:    copybook-corruption, copybook-corruption-detectors,
              copybook-corruption-predicates, copybook-corruption-rdw
  Other:      copybook-arrow, copybook-sequence-ring
tools/                            # 3 dev-only (publish = false)
  copybook-bench, copybook-gen, xtask
tests/                            # Test-only workspace members
  bdd, e2e, proptest
```

## Required Commands

```bash
# Full validation pipeline
cargo build --workspace --release && cargo test --workspace && cargo clippy --workspace -- -D warnings -W clippy::pedantic && cargo fmt --all --check

# Quick test
cargo test --workspace

# Benchmarks
cargo bench --package copybook-bench

# Golden fixtures
cargo test --workspace --test "*golden*"
```

## Processing Flow

1. **copybook-core**: Parses COBOL copybook text -> Schema AST
2. **copybook-codec**: Schema -> encode/decode binary data <-> JSON
3. **copybook-cli**: User-friendly command orchestration

## Key Data Types

- `Schema`: Parsed copybook structure with field lookup methods and Level-88 condition support
- `Field` / `FieldKind`: COBOL field definitions with JSON processing and condition value handling
- `DecodeOptions` / `EncodeOptions`: Configuration (codepage, JSON modes, metadata)
- `ScratchBuffers`: Reusable memory buffers for performance
- `RecordFormat`: Fixed-length vs RDW formats
- `Codepage`: EBCDIC variants (CP037, CP273, CP500, CP1047, CP1140)
- `GoldenTest` / `GoldenTestSuite`: Structural validation fixtures with SHA-256 verification

## Error Taxonomy

10 families, 61 stable codes. See `docs/reference/ERROR_CODES.md` for the full list.

| Prefix | Domain |
|--------|--------|
| CBKP | Parse errors (syntax, unsupported features) |
| CBKS | Schema validation (ODO, projection) |
| CBKD | Data errors (decimals, truncation, edited PIC decode) |
| CBKE | Encoding errors (type mismatch, bounds, edited PIC encode) |
| CBKR | Record format errors (RDW, fixed-length) |
| CBKC | Character set errors (unmappable characters) |
| CBKF | File/format errors (I/O, framing) |
| CBKI | Infrastructure errors (internal/operational) |
| CBKA | Audit errors (audit trail) |
| CBKW | Arrow/Writer errors (Apache Arrow integration) |

## Invariants

- Zero `unsafe` in public APIs
- Clippy pedantic clean across the workspace
- Error codes stable across minor versions
- Round-trip determinism: same input -> byte-identical output across runs and worker configurations
- FILLER naming convention: `_filler_00000XXX` (byte offset)
- Perf receipts canonical location: `scripts/bench/perf.json`

## Raw Data Capture Convention

- `__raw_b64` is the canonical raw-payload field emitted when raw mode is enabled
- `RawMode::Record` stores only record payload bytes
- `RawMode::RecordRDW` stores RDW header + payload bytes
- `RawMode::Field` stores field-level raw values in `<FIELD_NAME>__raw_b64`
- `RawMode::Off` stores no raw payload
- For RDW records, `RecordRDW` preserves reserved bytes; encode recomputes length when payload changes

## Docs Alignment Rule

When feature behavior changes, update in this order:

1. **Canonical source** (from table above)
2. **CLAUDE.md invariants** (only if an invariant changed)
3. **README.md** (only if supported/not-supported lists changed)
4. **ROADMAP.md** (only if a "Next" item completed or new gap emerged)
