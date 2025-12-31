# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

copybook-rs is a Rust workspace for enterprise mainframe data processing. Provides comprehensive COBOL copybook parsing and data conversion with focus on correctness and safety.

**Status**: **Engineering Preview** (v0.4.0) - See [ROADMAP.md](docs/ROADMAP.md) for adoption guidance
**Performance**: Baseline established (DISPLAY: 205 MiB/s, COMP-3: 58 MiB/s; 2025-09-30, commit 1fa63633)
**Quality**: 840+ tests passing (24 skipped for external tool requirements), zero unsafe code, clippy pedantic compliance, comprehensive error taxonomy (40+ error codes), 85+ workspace-inherited dependencies

**Adoption Guidance**: Suitable for teams that validate copybooks against supported features (see Known Limitations & Roadmap below). Production deployment requires pilot validation on representative workloads.

**MSRV**: Rust 1.90 enforced at workspace level and validated in CI (see .github/workflows/ci.yml).

See [REPORT.md](docs/REPORT.md) for detailed readiness assessment and [copybook-bench/BASELINE_METHODOLOGY.md](copybook-bench/BASELINE_METHODOLOGY.md) for performance baseline details.

## Workspace Structure

5 crates:
- **copybook-core**: COBOL parsing (lexer, parser, AST, layout) with Level-88 condition value support
- **copybook-codec**: Data encoding/decoding, character conversion with structural validation and determinism validation
- **copybook-cli**: CLI with subcommands (parse, inspect, decode, encode, verify, determinism)
- **copybook-gen**: Test fixture generation with golden fixture framework
- **copybook-bench**: Performance benchmarks with regression detection

## Development Commands

```bash
# Build and test
cargo build --workspace --release
cargo test --workspace
cargo clippy --workspace -- -D warnings -W clippy::pedantic
cargo fmt --all

# Benchmarks (output to canonical location: scripts/bench/perf.json)
cargo bench --package copybook-bench
PERF=1 cargo bench -p copybook-bench -- --output-format json > target/perf.json
mkdir -p scripts/bench && cp target/perf.json scripts/bench/perf.json

# Benchmark reporting (Issue #52)
cargo run --bin bench-report -p copybook-bench -- validate scripts/bench/perf.json       # Validate performance JSON
cargo run --bin bench-report -p copybook-bench -- baseline promote scripts/bench/perf.json  # Promote to baseline
cargo run --bin bench-report -p copybook-bench -- baseline show            # Show current baseline
cargo run --bin bench-report -p copybook-bench -- compare scripts/bench/perf.json        # Check for regressions
cargo run --bin bench-report -p copybook-bench -- summary                  # Show performance status

# Golden fixture testing
cargo test --test golden_fixtures_comprehensive    # All golden fixtures
cargo test --test golden_fixtures_odo             # ODO-specific fixtures
cargo test --test golden_fixtures_level88         # Level-88 fixtures
cargo test --test golden_fixtures_enterprise      # Enterprise scenarios

# Full validation pipeline
cargo build --workspace --release && cargo test --workspace && cargo clippy --workspace -- -D warnings -W clippy::pedantic && cargo fmt --all --check
```

## CLI Usage

```bash
# CLI access
cargo run --bin copybook -- [SUBCOMMAND]

# Key commands
cargo run --bin copybook -- parse copybook.cpy                    # Parse to schema JSON
cargo run --bin copybook -- inspect copybook.cpy                  # Human-readable layout
cargo run --bin copybook -- verify --format fixed --codepage cp037 copybook.cpy data.bin  # Validate data

# Decode COBOL data to JSONL
cargo run --bin copybook -- decode customer.cpy data.bin --output data.jsonl --format fixed --codepage cp037 --threads 4

# High-performance decode with options
cargo run --bin copybook -- decode file.cpy data.bin --output data.jsonl --format fixed --codepage cp037 --json-number lossless --emit-meta --threads 8

# Encode JSONL to binary
cargo run --bin copybook -- encode --format fixed --codepage cp037 copybook.cpy input.jsonl output.bin

# Encode with error handling options
cargo run --bin copybook -- encode --format fixed --codepage cp037 --fail-fast --max-errors 5 copybook.cpy input.jsonl output.bin

# Determinism validation (Issue #112)
cargo run --bin copybook -- determinism decode --format fixed --codepage cp037 schema.cpy data.bin           # Verify decode determinism
cargo run --bin copybook -- determinism encode --format fixed --codepage cp037 schema.cpy input.jsonl        # Verify encode determinism
cargo run --bin copybook -- determinism round-trip --format fixed --codepage cp037 schema.cpy data.bin      # Verify round-trip determinism

# Determinism with JSON output for CI integration
cargo run --bin copybook -- determinism decode --output json --format fixed --codepage cp037 schema.cpy data.bin

# Determinism exit codes: 0=deterministic, 2=non-deterministic, 3=error

# Field projection (select specific fields)
cargo run --bin copybook -- decode customer.cpy data.bin --output selected.jsonl --format fixed --codepage cp037 --select "CUSTOMER-ID,BALANCE"

# Multiple --select flags supported
cargo run --bin copybook -- decode customer.cpy data.bin --output selected.jsonl --format fixed --codepage cp037 --select "CUSTOMER-ID" --select "BALANCE"

# Auto-includes ODO counters
cargo run --bin copybook -- decode schema.cpy data.bin --output data.jsonl --format fixed --codepage cp037 --select "TRANSACTIONS"  # Counter field included automatically

# RENAMES alias resolution (R1-R3)
cargo run --bin copybook -- decode schema.cpy data.bin --output data.jsonl --format fixed --codepage cp037 --select "CUSTOMER-HEADER"  # Resolves RENAMES alias

# Dialect lever (controls ODO min_count interpretation)
cargo run --bin copybook -- decode schema.cpy data.bin --output data.jsonl --format fixed --codepage cp037 --dialect 0  # Zero-tolerant (IBM Enterprise)
cargo run --bin copybook -- parse schema.cpy --dialect 1  # One-tolerant (Micro Focus)
COPYBOOK_DIALECT=0 cargo run --bin copybook -- verify schema.cpy data.bin --format fixed  # Environment variable
```

## Library API

### Core Functions

```rust
use copybook_core::parse_copybook;
use copybook_codec::{decode_record, DecodeOptions, Codepage, JsonNumberMode};

// Parse copybook
let schema = parse_copybook(&copybook_text)?;

// Parse copybook with custom options (supports Level-88 condition values)
use copybook_core::{parse_copybook_with_options, ParseOptions};
let parse_options = ParseOptions {
    allow_inline_comments: false, // Disable COBOL-2002 inline comments (*>)
    ..ParseOptions::default()
};
let schema = parse_copybook_with_options(&copybook_text, &parse_options)?;

// Configure decoder options
let options = DecodeOptions::new()
    .with_codepage(Codepage::CP037)
    .with_json_number_mode(JsonNumberMode::Lossless)
    .with_emit_meta(true);

// Decode record
let json_value = decode_record(&schema, &record_data, &options)?;

// High-performance with scratch buffers
use copybook_codec::{decode_record_with_scratch, memory::ScratchBuffers};
let mut scratch = ScratchBuffers::new();
let json_value = decode_record_with_scratch(&schema, &record_data, &options, &mut scratch)?;

// Streaming iterator
use copybook_codec::iter_records_from_file;
let iterator = iter_records_from_file("data.bin", &schema, &options)?;
for record_result in iterator {
    let json_value = record_result?;
    // Process record
}

// File processing with metrics
use copybook_codec::decode_file_to_jsonl;
let summary = decode_file_to_jsonl(&schema, input, output, &options)?;

// Golden fixture testing with structural validation
use copybook_gen::golden::{GoldenTest, GoldenTestSuite, TestConfig};

// Create golden test with specific configuration
let config = TestConfig {
    codepage: "cp037".to_string(),
    record_format: "fixed".to_string(),
    json_number_mode: "lossless".to_string(),
    flags: vec!["level88".to_string(), "odo".to_string()],
};
let golden_test = GoldenTest::new_with_config("test_name", &copybook_text, &test_data, config);

// Create test suite for enterprise scenarios
let mut suite = GoldenTestSuite::new("enterprise_tests", "Production mainframe scenarios");
suite.add_test(golden_test);

// Validate fixture outputs with SHA-256 verification
let is_valid = golden_test.validate_string_output("json", &output_json);

// Field projection API for programmatic use
use copybook_core::project_schema;

// Select specific fields from schema
let selected_fields = vec!["CUSTOMER-ID".to_string(), "BALANCE".to_string()];
let projected_schema = project_schema(&schema, &selected_fields)?;

// Projected schema includes parent groups and auto-includes ODO counters
let json_value = decode_record(&projected_schema, &record_data, &options)?;
```

## Architecture

### Processing Flow
1. **copybook-core**: Parses COBOL copybook text → Schema AST
2. **copybook-codec**: Schema → encode/decode binary data ↔ JSON
3. **copybook-cli**: User-friendly command orchestration

### Key Data Types
- `Schema`: Parsed copybook structure with field lookup methods and Level-88 condition support
- `Field`/`FieldKind`: COBOL field definitions with JSON processing and condition value handling
- `DecodeOptions`/`EncodeOptions`: Configuration (codepage, JSON modes, metadata)
- `ScratchBuffers`: Reusable memory buffers for performance
- `RecordFormat`: Fixed-length vs RDW formats
- `Codepage`: EBCDIC variants (CP037, CP273, CP500, CP1047, CP1140)
- `GoldenTest`/`GoldenTestSuite`: Structural validation fixtures with SHA-256 verification

### Error Handling
Structured error taxonomy with stable codes:
- `CBKP*`: Parse errors (syntax, unsupported features)
- `CBKS*`: Schema validation (ODO counters, record limits, projection errors)
- `CBKD*`: Data errors (invalid decimals, truncated records, edited PIC decode errors)
- `CBKE*`: Encoding errors (type mismatches, bounds, edited PIC encode errors)
- `CBKR*`: Record format errors (RDW processing, fixed-length records)

**Projection Error Codes** (CBKS7xx):
- `CBKS701_PROJECTION_INVALID_ODO`: Selected field with ODO but counter not accessible
- `CBKS702_PROJECTION_UNRESOLVED_ALIAS`: RENAMES alias spans unselected fields
- `CBKS703_PROJECTION_FIELD_NOT_FOUND`: Selected field name does not exist

**Edited PIC Error Codes** (CBKD4xx, CBKE4xx):
- `CBKD421_EDITED_PIC_INVALID_FORMAT`: Data doesn't match edited PICTURE pattern
- `CBKD422_EDITED_PIC_SIGN_MISMATCH`: Sign character mismatch in editing
- `CBKD423_EDITED_PIC_BLANK_WHEN_ZERO`: Field is all blanks (BLANK WHEN ZERO)

### Performance Features
- Scratch buffer optimization for hot paths
- Streaming I/O with bounded memory (<256 MiB for multi-GB files)
- Parallel processing with deterministic output
- Zero-copy operations where possible
- FILLER fields named by byte offset (`_filler_00000XXX`)

## Requirements

- Rust 1.90+ (MSRV), Edition 2024
- Workspace dependencies for consistent versions

## Performance

**CI Gating**: Perf workflow enforces throughput floors (DISPLAY ≥ 80 MiB/s, COMP-3 ≥ 40 MiB/s); baseline comparisons remain advisory.

**Targets vs Achieved**:
- DISPLAY-heavy: ≥80 MB/s → **205 MiB/s (2.56x exceeded)**
- COMP-3-heavy: ≥40 MB/s → **58 MiB/s (1.45x exceeded)**
- Memory: <256 MiB steady-state for multi-GB files
- Variance: ~5% across benchmark runs (WSL2 environment)
- Overhead budget: 20% reserved for enterprise security/audit features

**Baseline Established**: 2025-09-30 (Commit 1fa63633)
- Measurement Environment: WSL2 on AMD Ryzen 9 9950X3D (32 threads, 196 GiB RAM)
- Sample Count: 5 independent runs with statistical analysis
- See `copybook-bench/HARDWARE_SPECS.md` for complete hardware specifications
- See `copybook-bench/BASELINE_METHODOLOGY.md` for measurement procedures

**Benchmarking**:
```bash
cargo bench --package copybook-bench           # All benchmarks
cargo bench --package copybook-bench -- slo_validation  # SLO validation
PERF=1 cargo bench -p copybook-bench          # Performance mode

# Baseline management (Issue #49)
cargo run --bin bench-report -p copybook-bench -- baseline show
cargo run --bin bench-report -p copybook-bench -- baseline promote perf.json
```

**Performance Notes**:
- Measurements taken in WSL2 environment; native Linux may show 5-15% improvement
- DISPLAY-heavy workloads benefit from parallel processing (up to 177 MiB/s on 8 threads)
- COMP-3 decoding performance limited by packed decimal conversion complexity
- Baseline provides foundation for regression detection and performance tracking

**Benchmark Container** (Issue #113):
```bash
# Pull pre-built container from GHCR
docker pull ghcr.io/effortlessmetrics/copybook-rs/bench:latest
docker run -v $(pwd)/output:/workspace/output ghcr.io/effortlessmetrics/copybook-rs/bench:latest
cat output/perf.json

# Or build locally
docker build -t copybook-rs-bench .
docker run -v $(pwd)/output:/workspace/output copybook-rs-bench
```

Provides reproducible benchmark environment with:
- Rust 1.90 toolchain (MSRV)
- All benchmark dependencies pre-installed
- Automated perf.json receipt generation
- Performance summary with SLO comparison
- See [Operator Runbook](docs/perf/OPERATOR_RUNBOOK.md) for detailed usage

## Golden Fixtures System

The Golden Fixtures framework provides comprehensive structural validation for enterprise COBOL scenarios with SHA-256 verification and performance regression detection.

### Key Features

- **Structural Validation**: ODO (Occurs Depending On), Level-88 condition values, REDEFINES interactions
- **Enterprise Scenarios**: Production mainframe patterns from banking, insurance, retail, manufacturing
- **Performance Integration**: Automated performance regression detection with baselines
- **SHA-256 Verification**: Cryptographic validation of test outputs for consistency
- **Comprehensive Coverage**: Golden fixtures form a substantial subset of the 840+ tests in the workspace, covering ODO, Level-88, REDEFINES, and enterprise layouts

### Usage Patterns

```bash
# Run comprehensive golden fixture validation
cargo test --workspace --test "*golden*"

# Run specific fixture categories
cargo test --test golden_fixtures_odo                    # ODO structural validation
cargo test --test golden_fixtures_level88               # Level-88 condition values
cargo test --test golden_fixtures_redefines            # REDEFINES interactions
cargo test --test golden_fixtures_enterprise           # Enterprise production scenarios
cargo test --test golden_fixtures_comprehensive        # Full comprehensive suite

# Performance integration testing
cargo test --test golden_fixtures_ac6_performance_integration
PERF=1 cargo bench --package copybook-bench -- golden_fixtures

# Generate new golden fixtures
cargo run --package copybook-gen -- generate-golden-fixtures --enterprise --output fixtures/enterprise/
```

### Structural Validation Coverage

- **ODO Tail Validation**: Ensures ODO arrays are properly positioned as tail elements
- **Level-88 After ODO**: Validates non-storage condition values following variable arrays
- **Child Inside ODO**: Validates internal structure of variable arrays
- **Sibling After ODO**: Validates rejection of storage fields after ODO (negative test)
- **REDEFINES + Level-88**: Complex interactions between memory redefinition and conditions
- **Enterprise Patterns**: Real-world mainframe record layouts with authentic constraints

### Error Code Coverage

The golden fixtures comprehensively test structural error conditions:

- `CBKP021_ODO_NOT_TAIL`: ODO array positioning validation
- `CBKS121_COUNTER_NOT_FOUND`: ODO counter field validation
- `CBKS301_ODO_CLIPPED`: ODO bounds enforcement
- `CBKS302_ODO_RAISED`: ODO minimum value validation
- Plus comprehensive coverage of parsing, schema, and data validation error codes

### Performance Standards

Golden fixtures maintain strict performance requirements aligned with established baselines:

- **DISPLAY-heavy**: 205 MiB/s baseline (2.56x enterprise target of 80 MiB/s)
- **COMP-3-heavy**: 58 MiB/s baseline (1.45x enterprise target of 40 MiB/s)
- **Memory**: <256 MiB steady-state for multi-GB fixture sets
- **Variance**: ~5% (DISPLAY), ~8% (COMP-3) across benchmark runs
- **Regression Detection**: Automated baseline comparison with <2% tolerance

Baseline established 2025-09-30 (commit 1fa63633) on WSL2/AMD Ryzen 9 9950X3D. See [copybook-bench/BASELINE_METHODOLOGY.md](copybook-bench/BASELINE_METHODOLOGY.md) for measurement procedures and [docs/REPORT.md](docs/REPORT.md) for complete performance analysis.

## Field Projection (`--select`)

The `--select` flag allows selective field decoding/encoding for performance optimization and data minimization:

### Features

- **Comma-separated or multiple flags**: `--select "ID,NAME"` or `--select "ID" --select "NAME"`
- **Automatic ODO counter inclusion**: When selecting ODO arrays, counter fields are auto-included
- **RENAMES alias resolution (R1-R3)**: Aliases resolve to their storage fields automatically
- **Parent group preservation**: JSON structure maintains hierarchical relationships
- **Error codes**: CBKS701-703 for validation failures

### CLI Examples

```bash
# Decode specific fields only
cargo run --bin copybook -- decode schema.cpy data.bin \
  --output selected.jsonl \
  --format fixed --codepage cp037 \
  --select "CUSTOMER-ID,BALANCE"

# Multiple field selection with ODO (counter auto-included)
cargo run --bin copybook -- decode schema.cpy data.bin \
  --output selected.jsonl \
  --format fixed --codepage cp037 \
  --select "CUSTOMER-ID" --select "TRANSACTIONS"

# RENAMES alias projection (R1-R3)
cargo run --bin copybook -- decode schema.cpy data.bin \
  --output selected.jsonl \
  --format fixed --codepage cp037 \
  --select "CUSTOMER-HEADER"  # Resolves to aliased fields

# Encode with field projection
cargo run --bin copybook -- encode --format fixed --codepage cp037 \
  --select "CUSTOMER-ID,BALANCE" \
  schema.cpy input.jsonl output.bin

# Verify with field projection
cargo run --bin copybook -- verify --format fixed --codepage cp037 \
  --select "CUSTOMER-ID,BALANCE" \
  schema.cpy data.bin
```

### Library API

```rust
use copybook_core::{parse_copybook, project_schema};
use copybook_codec::{decode_record, DecodeOptions};

// Parse full schema
let schema = parse_copybook(&copybook_text)?;

// Project to selected fields
let selected_fields = vec!["CUSTOMER-ID".to_string(), "BALANCE".to_string()];
let projected_schema = project_schema(&schema, &selected_fields)?;

// Decode with projected schema (only selected fields in output)
let options = DecodeOptions::new()
    .with_codepage(Codepage::Cp037);
let json_value = decode_record(&projected_schema, &record_data, &options)?;
```

### Projection Behavior

- **Parent groups included**: Schema hierarchy preserved in JSON output
- **ODO auto-dependency**: Counter fields automatically selected when ODO array is selected
- **RENAMES resolution**: Aliases resolve to underlying storage fields (R1-R3 scenarios)
- **Error on invalid selection**: `CBKS703_PROJECTION_FIELD_NOT_FOUND` if field doesn't exist
- **Error on incomplete ODO**: `CBKS701_PROJECTION_INVALID_ODO` if counter not accessible
- **Error on partial alias**: `CBKS702_PROJECTION_UNRESOLVED_ALIAS` if alias spans unselected fields

## Dialect Lever (ODO min_count Interpretation)

The dialect lever controls how `min_count` is interpreted for `OCCURS DEPENDING ON` (ODO) arrays. Different COBOL dialects have different requirements for the minimum bound in ODO declarations.

### Dialect Modes

| Mode | CLI Flag | Description | Behavior |
|------|----------|-------------|----------|
| **Normative** (default) | `--dialect n` | Strict enforcement | `min_count` is enforced as declared in copybook |
| **Zero-Tolerant** | `--dialect 0` | IBM Enterprise mode | `min_count` is ignored (always treated as 0) |
| **One-Tolerant** | `--dialect 1` | Micro Focus mode | `min_count` is clamped to 1 (minimum ≥ 1) |

### When to Use

- **Normative (`n`)**: Default behavior, suitable for most use cases where copybooks follow ANSI COBOL standards
- **Zero-Tolerant (`0`)**: For IBM Enterprise COBOL copybooks where ODO arrays should always allow zero occurrences
- **One-Tolerant (`1`)**: For Micro Focus COBOL copybooks where minimum count is always at least 1

### Configuration

**Precedence Order**:
1. CLI `--dialect` flag (highest priority)
2. `COPYBOOK_DIALECT` environment variable
3. Default value (`n` - Normative)

### CLI Examples

```bash
# Use normative dialect (default) - min_count enforced as declared
cargo run --bin copybook -- parse schema.cpy --dialect n

# Use zero-tolerant dialect for IBM Enterprise COBOL
cargo run --bin copybook -- decode schema.cpy data.bin \
  --format fixed --codepage cp037 \
  --dialect 0 \
  --output data.jsonl

# Use one-tolerant dialect for Micro Focus COBOL
cargo run --bin copybook -- encode --format fixed --codepage cp037 \
  --dialect 1 \
  schema.cpy data.jsonl output.bin

# Environment variable configuration
export COPYBOOK_DIALECT=0
cargo run --bin copybook -- verify schema.cpy data.bin --format fixed

# CLI flag overrides environment variable
COPYBOOK_DIALECT=0 cargo run --bin copybook -- parse schema.cpy --dialect 1  # Uses one-tolerant

# Inspect with dialect
cargo run --bin copybook -- inspect schema.cpy --dialect 0
```

### Library API

```rust
use copybook_core::{parse_copybook_with_options, ParseOptions};
use copybook_core::dialect::Dialect;

// Parse with specific dialect
let options = ParseOptions {
    dialect: Dialect::ZeroTolerant,  // IBM Enterprise mode
    ..ParseOptions::default()
};
let schema = parse_copybook_with_options(&copybook_text, &options)?;

// Available dialects
let normative = Dialect::Normative;     // Default: min_count enforced
let zero_tolerant = Dialect::ZeroTolerant;  // min_count ignored
let one_tolerant = Dialect::OneTolerant;   // min_count clamped to 1
```

### COBOL Copybook Examples

**Example 1: ODO with min_count > 0**
```cobol
       01  RECORD.
           05  COUNTER      PIC 9(3).
           05  ITEMS        OCCURS 1 TO 10 DEPENDING ON COUNTER
                            PIC X(10).
```

Behavior by dialect:
- `--dialect n`: Counter must be 1-10 (min_count=1 enforced)
- `--dialect 0`: Counter can be 0-10 (min_count ignored)
- `--dialect 1`: Counter must be 1-10 (min_count=1 enforced)

**Example 2: ODO with min_count = 0**
```cobol
       01  RECORD.
           05  COUNTER      PIC 9(3).
           05  ITEMS        OCCURS 0 TO 10 DEPENDING ON COUNTER
                            PIC X(10).
```

Behavior by dialect:
- `--dialect n`: Counter can be 0-10 (min_count=0 allowed)
- `--dialect 0`: Counter can be 0-10 (min_count=0 allowed)
- `--dialect 1`: Counter must be 1-10 (min_count raised to 1)

### Available on All Commands

The `--dialect` flag is supported on all copybook-processing commands:
- `parse` - Affects schema parsing and layout validation
- `inspect` - Affects layout display and field offset calculation
- `decode` - Affects ODO bounds checking during data decoding
- `encode` - Affects ODO bounds validation during encoding
- `verify` - Affects data validation against schema

### Design Documentation

See `docs/internal/features/d0_dialect_lever_contract.md` for complete specification and implementation details.

## Edited PIC Support (Phase E1/E2/E3)

copybook-rs provides full parse, decode, and encode support for edited numeric PICTURE clauses.

### Implementation Status

**Phase E1 (Parse + Schema)**: ✅ Complete
- Parses edited PICTURE clauses (Z, $, comma, +, -, CR, DB, *)
- Creates `EditedNumeric` FieldKind in schema AST
- Available via `copybook inspect` command for layout inspection

**Phase E2 (Decode)**: ✅ Complete
- Decodes edited numeric fields to JSON numeric values
- Supports: zero suppression (Z), currency ($), sign (+, -, CR, DB), asterisk (*)
- JSON output via `JsonNumberMode` (lossless/native)
- Error codes: CBKD421-423 for decode failures

**Phase E3 (Encode)**: ✅ Complete
- Encodes JSON numeric values to edited EBCDIC/ASCII formats
- Supports: zero suppression (Z), currency ($), sign (+/-/CR/DB), asterisk (*), comma, slash
- Note: Space (B) insertion not yet supported
- Error codes: CBKE4xx for encode failures

### Supported Edited PIC Patterns (Phase E2)

| Pattern | Example | Description | Test Evidence |
|---------|---------|-------------|---------------|
| ZZZ9 | `PIC ZZZ9` | Basic zero suppression | `test_e2_simple_z_editing_zzz9` |
| ZZZ9.99 | `PIC ZZZ9.99` | With decimal point | `test_e2_zero_suppression_with_decimal` |
| $ZZ,ZZZ.99 | `PIC $ZZ,ZZZ.99` | Currency with comma and decimal | `test_e2_currency_dollar_zz_zzz_99` |
| +/-/CR/DB | `PIC +ZZZ9`, `PIC -ZZZ9` | Sign editing | `test_e2_sign_editing_*` |
| \*\*\*9 | `PIC ***9` | Check protect (asterisk fill) | `test_e2_check_protect_asterisk` |
| BLANK WHEN ZERO | `PIC ZZZ9 BLANK WHEN ZERO` | All-blank for zero | `test_e2_blank_when_zero` |

### CLI Examples

```bash
# Decode edited PIC fields (Phase E2)
cargo run --bin copybook -- decode schema.cpy data.bin \
  --output data.jsonl \
  --format fixed --codepage cp037 \
  --json-number lossless  # Preserve edited numeric precision

# Inspect edited PIC layout (Phase E1)
cargo run --bin copybook -- inspect schema.cpy
# Output shows EditedNumeric field kind with pattern details

# Parse edited PIC to schema JSON (Phase E1)
cargo run --bin copybook -- parse schema.cpy --output schema.json
# Schema JSON includes EditedNumeric kind with editing metadata
```

### COBOL Copybook Examples

```cobol
      * Phase E1: Parsed into EditedNumeric schema
05 AMOUNT        PIC $ZZ,ZZZ.99.
05 QUANTITY      PIC ZZZ9.
05 BALANCE       PIC +ZZ,ZZZ.99-.
05 CHECK-AMT     PIC ***,**9.99.
05 DISCOUNT      PIC ZZ9 BLANK WHEN ZERO.

      * Phase E2: Decode EBCDIC edited format → JSON numeric
      * Decode converts: " $1,234.56" → "1234.56" (lossless)
      *                  "    0" → "0" (BLANK WHEN ZERO)

      * Phase E3: Encode JSON → EBCDIC edited format
      * Encode converts: "1234.56" → " $1,234.56" (with pattern)
```

### Error Codes

- **CBKD421_EDITED_PIC_INVALID_FORMAT**: Data doesn't match edited PICTURE pattern
- **CBKD422_EDITED_PIC_SIGN_MISMATCH**: Sign character mismatch (expected +/-, CR, or DB)
- **CBKD423_EDITED_PIC_BLANK_WHEN_ZERO**: Field is all blanks (informational warning)

### Design Documentation

- See `docs/design/EDITED_PIC_PHASES.md` for complete phase breakdown (E1/E2/E3)
- See `copybook-codec/tests/edited_pic_decode_e2_tests.rs` for 28 comprehensive decode tests
- See `copybook-core/tests/edited_pic_e1_tests.rs` for 15 parse tests

## Known Limitations & Roadmap

**Current Status**: Engineering Preview (v0.4.0) - ROADMAP.md is the canonical status source.

### Unsupported COBOL Features
- COMP-1/COMP-2 floating-point types (rare in practice)
- SIGN LEADING/TRAILING SEPARATE directives
- Edited PIC Space (B) insertion – not yet implemented
- Nested OCCURS DEPENDING ON (O5: ODO inside ODO) – rejected by design; see Issue #164
- ODO over REDEFINES (O6) – rejected by design; see Issue #164
- RENAMES interactions with REDEFINES/OCCURS (R4-R6) – out of scope; see docs/design/RENAMES_NESTED_GROUPS.md

**Note**:
- **OCCURS/ODO support** (O1-O4): Simple tail ODO ✅, tail ODO with DYNAMIC ✅, group-with-ODO tail ✅, ODO with sibling after 🚫 (CBKP021_ODO_NOT_TAIL). See docs/design/NESTED_ODO_BEHAVIOR.md and docs/reference/COBOL_SUPPORT_MATRIX.md for complete O1-O7 scenario breakdown.
- **RENAMES support** (R1-R3): Same-scope fields ✅, group alias ✅, nested groups ✅ with alias-aware Schema API. See docs/design/RENAMES_NESTED_GROUPS.md for complete specification.
- **Level-88 condition values**: Fully supported (parse + codec + structural validation). See COBOL_SUPPORT_MATRIX.md for detailed test evidence.
- **Edited PIC support** (E1/E2/E3): Parse ✅, decode ✅, encode ✅. See Edited PIC section above for details.
- **Field projection**: Fully supported (`--select` flag) with ODO auto-dependency and RENAMES alias resolution (R1-R3).

### Performance Considerations
- Baseline established at 205 MiB/s (DISPLAY) and 58 MiB/s (COMP-3) on reference hardware (commit 1fa63633)
- WSL2 environment introduces 10-30% overhead versus native Linux
- Current measurements show variance (66-95 MiB/s DISPLAY, 18-25 MiB/s COMP-3) based on system load
- Advisory-only performance policy active (Issues #74, #75); historic SLOs (4.1 GiB/s DISPLAY, 560 MiB/s COMP-3) replaced with empirical baseline
- CI enforces realistic throughput floors (DISPLAY ≥80 MiB/s, COMP-3 ≥40 MiB/s)
- See [docs/ROADMAP.md](docs/ROADMAP.md) for performance improvement plans

### Outstanding Work
- `bench-report` CLI tool available in copybook-bench for baseline management (Issue #52)
- Benchmark automation includes: baseline promote/show, compare, validate, summary commands
- Performance tracking infrastructure active; regression detection advisory-only (Issues #74, #75)
- See [docs/ROADMAP.md](docs/ROADMAP.md) for v0.5.0 dialect features and v1.0.0 stability plans

### Adoption Recommendations
- Validate representative copybooks against supported feature set before production deployment
- Run pilot workloads to verify throughput meets requirements
- Review [docs/REPORT.md](docs/REPORT.md) for detailed readiness assessment
- Consult [docs/ROADMAP.md](docs/ROADMAP.md) for upcoming features and release timeline
