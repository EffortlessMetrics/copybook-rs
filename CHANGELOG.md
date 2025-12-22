# Changelog

All notable changes to copybook-rs are documented here. This root file is the canonical source used by release workflows; `docs/CHANGELOG.md` now defers to it.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.4.0] — 2025-12-18

> Minor release with projection support and edited PIC decode.

### Added

- **Field Projection** (`--select` CLI flag) for decode/encode/verify commands
  - Comma-separated or multiple `--select` flags for field selection
  - Automatic ODO counter inclusion when selecting variable arrays
  - RENAMES alias resolution (R1-R3 scenarios) with alias-aware schema lookup
  - Parent group preservation to maintain JSON hierarchical structure
  - Projection API: `copybook_core::project_schema()` for programmatic use
  - Error codes: CBKS701 (invalid ODO), CBKS702 (unresolved alias), CBKS703 (field not found)
  - Test coverage: 24+ projection tests across CLI and core modules

- **Edited PIC Phase E1** (Parse + Schema): Parse edited PICTURE clauses into schema
  - New FieldKind: `EditedNumeric` with editing pattern metadata
  - Supports editing symbols: Z (zero suppress), $ (currency), comma, +/- (sign), CR/DB (credit/debit), * (check protect)
  - Available via `copybook inspect` and `copybook parse` commands
  - Test coverage: 15 parse tests in `edited_pic_e1_tests.rs`

- **Edited PIC Phase E2** (Decode): Decode edited numeric fields to JSON
  - Well-chosen subset: ZZZ9, $ZZ,ZZZ.99, +/- sign editing, CR/DB, asterisk fill
  - BLANK WHEN ZERO handling with CBKD423 informational warning
  - JSON output via JsonNumberMode (lossless/native) for precision preservation
  - Error codes: CBKD421 (invalid format), CBKD422 (sign mismatch), CBKD423 (blank when zero)
  - Test coverage: 28 decode tests in `edited_pic_decode_e2_tests.rs`

- **Enhanced RENAMES Test Coverage** (R1-R3): Fixed test coverage for RENAMES scenarios
  - Fixed boundary test for non-standard level numbers (level 77 detection)
  - Validated alias-aware schema methods: `find_field_or_alias`, `resolve_alias_to_target`
  - Test coverage: 8 tests in `schema_alias_lookup_tests.rs`

### Changed

- **Schema Fingerprint**: Now includes edited PIC metadata for cache invalidation
- **CLI Help Text**: Updated with field projection examples and edited PIC phase notes
- **Error Reporter**: Integrated new error codes (CBKS701-703, CBKD421-423) with contextual formatting

### Fixed

- **RENAMES Boundary Test**: Fixed test assertion for non-standard level number detection (level 77)
- **Projection Validation**: Enhanced error messages for ODO counter and alias resolution failures; parent groups now materialize before ODO counter inference to avoid CBKS701 when selecting nested leaves; fixed-format LRECL is preserved during projection even when tail ODO metadata is present
- **Nested ODO / REDEFINES Guardrails**: Reject nested ODO and ODO-over-REDEFINES scenarios with explicit parse errors (`CBKP022_NESTED_ODO`, `CBKP023_ODO_REDEFINES`)
- **Edited PIC Parser**: Proper handling of complex editing patterns with multiple symbols
- **SIGN Clause Handling**: SIGN LEADING/TRAILING now rejects with CBKP051 until decode semantics are implemented to avoid tokenizer failures
- **Edited PIC BLANK WHEN ZERO**: CBKD423 downgraded to a warning and emitted when fields are blankified

### Performance

- **Field Projection**: Minimal overhead for schema projection (<5% impact)
- **Edited PIC Decode**: Lockstep algorithm with zero-copy optimization where possible
- **Canonical Performance Truth**: Established via [`scripts/bench/perf.json`](scripts/bench/perf.json) with complete environment metadata
  - Baseline: DISPLAY 205 MiB/s, COMP-3 58 MiB/s (commit 1fa63633, 2025-09-30)
  - Advisory floors: DISPLAY ≥80 MiB/s, COMP-3 ≥40 MiB/s
  - See [`docs/HISTORICAL_PERFORMANCE.md`](docs/HISTORICAL_PERFORMANCE.md) for archived historical claims
- **Receipt Validation**: Automated integrity checks via [`scripts/validate-perf-receipt.sh`](scripts/validate-perf-receipt.sh)
- **Governance Policy**: All performance claims must reference canonical receipts ([`docs/PERFORMANCE_GOVERNANCE.md`](docs/PERFORMANCE_GOVERNANCE.md))

### Documentation

- **CLAUDE.md**: Added comprehensive Field Projection and Edited PIC sections with CLI/API examples
- **ERROR_CODES.md**: Documented CBKS701-703 (projection) and CBKD421-423 (edited PIC) error codes
- **COBOL_SUPPORT_MATRIX.md**: Updated feature matrix with E1/E2 status and projection support

## [0.3.2] — 2025-09-23

### Fixed

- Aligned workspace `rust-version` with the CI-tested toolchain (Rust 1.90) and refreshed MSRV messaging.
- Marked the audit feature as experimental scaffolding; CLI audit commands now emit stubbed reports instead of hardcoded "pass" metrics.
- Standardised performance receipts on `scripts/bench/perf.json` and demoted legacy `test_perf.json` to a clearly-labelled sample.
- Synced documentation status/performance messaging with `docs/ROADMAP.md` (engineering preview) and corrected security scanning policy to match CI (cargo-deny always; cargo-audit on lockfile diffs or scheduled runs).
- Regenerated performance receipts via `scripts/bench.sh` and updated `PERFORMANCE_VALIDATION_FINAL.md`. See canonical receipts in `scripts/bench/perf.json`.

### Added

- First crates.io publish for `copybook-core`, `copybook-codec`, and `copybook-cli` with pinned internal dependency versions.
- Bench receipt tooling (`bench-report`) with JSON output, baseline management, and validation commands (Issue #52).
- Expanded golden fixtures for ODO/RENAMES validation and documentation navigation refresh for Diátaxis structure.

### Changed

- Release runbook updated with version-pinned dependency guidance and publish dry-run steps.
- Roadmap and readiness docs consolidated around engineering preview status.

### Performance

- DISPLAY 4–5 GiB/s; COMP-3 50–70 MiB/s (SLOs far exceeded)

## [0.3.1] — 2025-09-22

### Added

- `--strict` flag for `inspect`/`parse` commands: normative ODO bounds/ordering; REDEFINES ambiguity becomes error.
- Comprehensive CLI integration tests for ODO lenient/strict modes, REDEFINES processing, edited PIC handling, and fixed-form parsing.
- Enhanced documentation with validation modes, binary widths clarification, and strict mode examples.

### Performance

- DISPLAY 205 MiB/s, COMP-3 58 MiB/s (unchanged from v0.3.1)

## [0.3.0] — 2025-09-19

### Added

- Initial release of copybook-rs workspace
- COBOL copybook parser with comprehensive field type support
- High-performance codec for encoding/decoding mainframe data formats
- CLI tool with parse, inspect, decode, encode, and verify commands
- Support for fixed-length and RDW record formats
- Multiple EBCDIC codepage support (CP037, CP273, CP500, CP1047, CP1140)
- Performance benchmarks achieving 4+ GiB/s for DISPLAY data and 500+ MiB/s for COMP-3 data

## [0.2.0] - 2025-09-19

### Added

- Decode/verify parity for ODO: >max→CBKS301, <min→CBKS302, short→CBKD301
- Counter-type twins (zoned/binary/packed) + verify twins
- Structural tail rule (last storage sibling), allows children inside ODO

### Performance

- DISPLAY 4–5 GiB/s; COMP-3 50–70 MiB/s (SLOs far exceeded)

## [0.1.0] - 2025-09-02

### Added

- Initial release of copybook-rs workspace
- COBOL copybook parser with comprehensive field type support
- High-performance codec for encoding/decoding mainframe data formats
- CLI tool with parse, inspect, decode, encode, and verify commands
- Support for fixed-length and RDW record formats
- Multiple EBCDIC codepage support (CP037, CP273, CP500, CP1047, CP1140)
- Performance benchmarks achieving 4+ GiB/s for DISPLAY data and 500+ MiB/s for COMP-3 data