# copybook-rs

[![CI Quick](https://github.com/EffortlessMetrics/copybook-rs/actions/workflows/ci-quick.yml/badge.svg)](https://github.com/EffortlessMetrics/copybook-rs/actions/workflows/ci-quick.yml)
[![Security Audit](https://github.com/EffortlessMetrics/copybook-rs/workflows/Weekly%20Security%20Scan/badge.svg)](https://github.com/EffortlessMetrics/copybook-rs/actions/workflows/security-scan.yml)
[![Dependency Review](https://img.shields.io/badge/dependencies-Dependabot-blue.svg)](https://github.com/EffortlessMetrics/copybook-rs/blob/main/.github/dependabot.yml)

Rust toolkit for COBOL copybook parsing and fixed-record data conversion that prioritizes correctness, safety, and transparent validation evidence. We focus on sharing what the tooling supports today, how the test suite exercises it, how to benchmark on your own hardware, and where the roadmap is headed next.

## 📚 Documentation

| **User Guide** | **CLI Reference** | **Library API** | **Error Codes** | **ADR** | **Production Status** | **Roadmap** |
|----------------|-------------------|------------------|-----------------|---------|----------------------|-------------|
| [User Guide](docs/USER_GUIDE.md) | [CLI Reference](docs/CLI_REFERENCE.md) | [Library API](docs/reference/LIBRARY_API.md) | [Error Codes](docs/reference/ERROR_CODES.md) | [ADR](docs/adr/) | [Production Status](#production-status) | [Roadmap](docs/ROADMAP.md) |

## Overview

copybook-rs delivers deterministic COBOL copybook parsing, schema inspection, and record encoding/decoding in Rust. The project focus is on predictable behaviour, detailed error reporting, and memory safety. Performance is validated internally with CI receipts published in PR artifacts and `scripts/bench/perf.json` (policy: accuracy-first). See [copybook-bench/BASELINE_METHODOLOGY.md](copybook-bench/BASELINE_METHODOLOGY.md) for measurement procedures and [docs/REPORT.md](docs/REPORT.md) for comprehensive analysis. Latest automated evidence available in `integrative_gate_summary.md` and perf workflow artifacts.

### Design Priorities

- **Correctness first**: Detailed error taxonomy, deterministic encoders/decoders, and zero `unsafe` blocks in public APIs
- **Transparent evidence**: CI reports 615/615 tests passing (54 skipped); raw data captured in `integrative_gate_summary.md`
- **Schema insight**: CLI and library APIs expose rich metadata for copybook inspection and validation workflows
- **Round-trip fidelity**: Binary↔JSON conversions preserve layout information to keep downstream audits reproducible
- **Sustainable maintenance**: Clean room Rust implementation with clippy pedantic and edition 2024 compliance

## Production Features

### **Core Capabilities**
- **COBOL schema parsing**: Lexer, parser, and AST with layout resolution for REDEFINES, ODO, and SYNCHRONIZED
- **Encoding/decoding**: Deterministic conversion between COBOL records and JSONL/structured data
- **Enterprise error handling**: Stable error taxonomy (CBKP*, CBKS*, CBKD*, CBKE*) with contextual metadata
- **Memory-aware streaming**: Streaming I/O architecture with bounded memory; real-world telemetry stays below 256 MiB during decode/encode runs

### **Quality Signals**
- **Deterministic output**: Byte-identical results across runs and worker configurations
- **Round-trip fidelity**: Zoned decimal metadata preserved to maintain copybook semantics
- **Memory safety**: Zero `unsafe` in public APIs; pedantic lints enforced across the workspace
- **Test coverage**: Hundreds of unit/integration tests plus nextest orchestration; one legacy performance assertion remains flaky (see `integrative_gate_summary.md`)

### **Enterprise Integration**
- **Multiple EBCDIC Codepages**: CP037, CP273, CP500, CP1047, CP1140 + ASCII support
- **Flexible Record Formats**: Fixed-length and variable (RDW) with validation
- **CLI + Library API**: Production-ready interfaces; Engineering Preview for feature completeness (see [ROADMAP.md](docs/ROADMAP.md) for known limitations: #44, #51, #72, #86)
- **Verification & Validation**: Built-in data quality auditing without conversion overhead

## Architecture

The project is organized as a Cargo workspace with the following crates:

- **copybook-core**: Core parsing and schema types for COBOL copybooks (lexer, parser, AST, layout resolution)
- **copybook-codec**: Encoding/decoding codecs for COBOL data types, character conversion, record framing
- **copybook-cli**: Command-line interface with subcommands (parse, inspect, decode, encode, verify, support)
- **copybook-gen**: Test fixture and synthetic data generation utilities
- **copybook-bench**: Performance benchmarks and testing harness

## Quick Start

### Installation

```bash
# Install from crates.io
cargo install copybook-cli

# Or build from source
git clone https://github.com/EffortlessMetrics/copybook-rs.git
cd copybook-rs
cargo build --release
```

### Basic Usage

```bash
# Parse a copybook and output schema JSON
copybook parse customer.cpy --output customer-schema.json

# Inspect copybook layout with human-readable table
copybook inspect customer.cpy --codepage cp037

# Decode binary data to JSONL
copybook decode customer.cpy customer-data.bin \
  --output customer-data.jsonl \
  --format fixed \
  --codepage cp037

# Encode JSONL back to binary (round-trip)
copybook encode customer.cpy customer-data.jsonl \
  --output customer-data-new.bin \
  --format fixed \
  --codepage cp037

# Verify data file integrity against schema
copybook verify customer.cpy customer-data.bin \
  --format fixed \
  --codepage cp037 \
  --report validation-report.json

# Check COBOL feature support
copybook support                        # Display support matrix table
copybook support --json                 # Machine-readable JSON output
copybook support --check level-88       # Validate specific feature (exit 0 if supported)
```

### CLI Exit Codes

When multiple error families surface, the CLI returns the highest-precedence code (`CBKI` > `CBKF` > `CBKE` > `CBKD`).

| Code | Tag  | Meaning (1-liner) | Test |
|----:|:----:|--------------------|------|
| 2 | CBKD | Data quality failure | exit_code_mapping::exit_code_cbkd_is_2 |
| 3 | CBKE | Encode/validation failure | exit_code_mapping::exit_code_cbke_is_3 |
| 4 | CBKF | Record format/RDW failure | exit_code_mapping::exit_code_cbkf_is_4 |
| 5 | CBKI | Internal orchestration error | exit_code_mapping::exit_code_cbki_is_5 |

#### Structured failure log

Every fatal path also emits a machine-parseable line so dashboards and CI can
reason about precedence:

```text
code_tag=CBKE code=3 family=policy precedence_rank=2 op=decode path=missing.bin errno=null err_kind=NotFound copybook CLI terminated with an error
```

- **Policy controls:** Precedence is `--strict-policy` → `--no-strict-policy` → `COPYBOOK_STRICT_POLICY`.

### Example: Decode to JSONL (stable schema)

```bash
# Decode an RDW file into JSONL with the versioned envelope
copybook decode \
  --codepage cp037 \
  --format rdw \
  --preferred-zoned-encoding preferred \
  --input ./data/customers.rdw \
  --output ./out/customers.jsonl
```

Each line is a standalone JSON object containing `"schema":"copybook.v1"`,
`record_index`, `codepage`, and a nested `fields` object. See
`docs/jsonl-schema.md` for the full schema and worked examples.

### Telemetry (optional, feature-gated)

Build the CLI with metrics and expose Prometheus `/metrics`:

```bash
cargo build -p copybook-cli --features metrics --release
./target/release/copybook decode --metrics-listen 0.0.0.0:9300 ...
# http://localhost:9300/metrics
```

Exported series:

- `copybook_records_total{format,codepage,zero_policy}`
- `copybook_bytes_total{format,codepage,zero_policy}`
- `copybook_decode_errors_total{family}`
- `copybook_decode_seconds{format,codepage}`
- `copybook_throughput_mibps{format,codepage}`

Short error runs can finish before a scrape lands; use `--metrics-grace-ms 2000` (or another small hold period) so Prometheus always sees the HELP/TYPE lines and the final error counter sample.

The library stays recorder-agnostic. When compiled with `--features metrics` **and** a global recorder is installed,
counters are emitted; otherwise metrics calls are no-ops.

## Operations & Observability

- **Telemetry rollout / launch readiness**: see [`docs/telemetry-launch-checklist.md`](docs/telemetry-launch-checklist.md) for the end-to-end operator flow (staging build/deploy, Prometheus/Grafana, alerts, perf receipts, shadow/soak, roadmap flip).
- **Feature flags**: telemetry is feature-gated (`--features metrics`) and off by default; the exporter binds only with `--metrics-listen <addr>`.

## Detailed Usage Examples

### Working with Fixed-Length Records

```bash
# Decode fixed-length mainframe data
copybook decode schema.cpy data.bin \
  --format fixed \
  --codepage cp037 \
  --output data.jsonl \
  --emit-meta \
  --threads 4

# Strict mode - stop on first error (including truncated records)
copybook decode schema.cpy data.bin \
  --format fixed \
  --strict \
  --output data.jsonl

# Lenient mode with error limit (continues after recoverable errors)
copybook decode schema.cpy data.bin \
  --format fixed \
  --max-errors 100 \
  --output data.jsonl

# Handle truncated records with detailed error reporting
copybook decode schema.cpy potentially-corrupt.bin \
  --format fixed \
  --output data.jsonl \
  --verbose  # Shows detailed CBKD301_RECORD_TOO_SHORT errors
```

### Working with Variable-Length Records (RDW)

```bash
# Decode RDW (Record Descriptor Word) format with enhanced validation
copybook decode schema.cpy data.bin \
  --format rdw \
  --codepage cp037 \
  --output data.jsonl

# Preserve raw RDW headers for round-trip (includes reserved byte warnings)
copybook decode schema.cpy data.bin \
  --format rdw \
  --emit-raw record+rdw \
  --output data.jsonl \
  --verbose  # Shows CBKR211_RDW_RESERVED_NONZERO warnings

# Use raw data for encoding (preserves reserved bytes, avoids warnings)
copybook encode schema.cpy data.jsonl \
  --format rdw \
  --use-raw \
  --output data-roundtrip.bin

# Strict RDW processing (fail on reserved byte violations)
copybook decode schema.cpy data.bin \
  --format rdw \
  --strict \
  --output data.jsonl  # Treats reserved bytes as fatal errors
```

### Character Encoding Options

```bash
# Different EBCDIC code pages
copybook decode schema.cpy data.bin --codepage cp037   # US/Canada
copybook decode schema.cpy data.bin --codepage cp273   # Germany/Austria
copybook decode schema.cpy data.bin --codepage cp500   # International
copybook decode schema.cpy data.bin --codepage cp1047  # Open Systems
copybook decode schema.cpy data.bin --codepage cp1140  # US/Canada Euro

# ASCII data with full overpunch support for signed fields
copybook decode schema.cpy data.bin --codepage ascii

# Handle unmappable characters
copybook decode schema.cpy data.bin \
  --codepage cp037 \
  --on-decode-unmappable replace  # or error, skip
```

> **Zoned policy chain**  
> `override → preserved → preferred`  
> If no override is set and no format was preserved from decode, the codec defaults
> to **preferred-zero** for EBCDIC (F-zero). ASCII uses **positive** zero.

### Zoned policy examples

- Preferred (default)

  ```bash
  copybook encode schema.cpy data.jsonl \
    --format rdw \
    --preferred-zoned-encoding preferred \
    --output data.bin
  ```

- Preserve (round-trip)

  ```bash
  copybook decode schema.cpy data.bin \
    --format rdw \
    --preserve-zoned-encoding \
    --output data.jsonl
  ```

- Override (force)

  ```bash
  copybook encode schema.cpy data.jsonl \
    --format rdw \
    --zoned-encoding-override ascii \
    --output data.bin
  ```

### Advanced Options

```bash
# Include FILLER fields in output (named by byte offset: _filler_00000XXX)
# FILLER fields use consistent byte-offset naming across parsing sessions
copybook decode schema.cpy data.bin \
  --emit-filler \
  --output data.jsonl

# Add metadata (schema fingerprint, record index, byte offsets)
copybook decode schema.cpy data.bin \
  --emit-meta \
  --output data.jsonl

# Control JSON number representation
copybook decode schema.cpy data.bin \
  --json-number lossless \  # strings for decimals (default, preserves precision)
  --output data.jsonl

copybook decode schema.cpy data.bin \
  --json-number native \    # JSON numbers where possible (integers, small decimals)
  --output data.jsonl

# BLANK WHEN ZERO encoding policy
copybook encode schema.cpy data.jsonl \
  --bwz-encode on \         # emit spaces for zero values
  --output data.bin

# Error handling options for encoding
copybook encode schema.cpy data.jsonl \
  --output data.bin \
  --fail-fast \             # stop on first error (default: true)
  --max-errors 10          # or allow up to N errors before stopping
```

### Performance and Parallel Processing

```bash
# Parallel processing (maintains deterministic output)
copybook decode schema.cpy large-data.bin \
  --threads 8 \
  --output data.jsonl

# Performance monitoring
copybook decode schema.cpy data.bin \
  --output data.jsonl \
  --verbose  # shows throughput and memory usage
```

### Data Validation and Verification

The `verify` command validates data files against copybook schemas without generating output files, making it ideal for data quality audits and integrity checks.

```bash
# Basic verification - check data file structure
copybook verify schema.cpy data.bin \
  --format fixed \
  --codepage cp037

# Generate detailed JSON verification report
copybook verify schema.cpy data.bin \
  --format fixed \
  --codepage cp037 \
  --report validation-report.json
# Report schema: docs/VERIFY_REPORT.schema.json

# Verify RDW format data
copybook verify schema.cpy data.bin \
  --format rdw \
  --codepage cp1047

# Verify multiple files with different formats
copybook verify schema.cpy fixed-data.bin --format fixed --report fixed-report.json
copybook verify schema.cpy rdw-data.bin --format rdw --report rdw-report.json
```

#### Verification Features

- **Record-Level Validation**: Detects truncated records, invalid numeric formats, and field type mismatches
- **Comprehensive Error Reporting**: Provides error codes, record indices, field paths, and byte offsets
- **JSON Report Generation**: Machine-readable reports for integration with data quality pipelines
- **Exit Code Compliance**: Returns 0 for valid files, 3 for validation errors, 2 for fatal errors (IO/schema)
- **Performance Optimized**: Validation-only processing without JSON conversion overhead

## JSON Output Quality

copybook-rs produces clean, properly typed JSON output with comprehensive COBOL field processing:

- **Proper Field Values**: COBOL fields are decoded to their correct string or numeric representations (no unintended null values)
- **Numeric Precision**: Zoned and packed decimals maintain precision with proper sign handling
- **Character Conversion**: EBCDIC and ASCII character data converted to UTF-8 strings
- **Hierarchical Structure**: Group fields create nested JSON objects matching copybook structure
- **REDEFINES Support**: Enhanced JSON encoding with proper REDEFINES field resolution and precedence rules
- **Schema-Aware Processing**: Full schema context enables advanced field lookups and ODO counter management

### Example JSON Output

For a COBOL record with various field types:

```json
{
  "CUSTOMER-ID": "00123",
  "CUSTOMER-NAME": "JOHN DOE",
  "ACCOUNT-BALANCE": "-1234.56",
  "LAST-PAYMENT": "890.12",
  "ORDER-COUNT": "3",
  "CUSTOMER-ADDRESS": {
    "STREET": "123 MAIN ST",
    "CITY": "ANYTOWN",
    "ZIP-CODE": "12345"
  }
}
```

## Library API Usage

### COBOL→JSON Processing Example

The library provides comprehensive COBOL record decoding with complete field processing:

```rust
use copybook_core::parse_copybook;
use copybook_codec::{
    decode_record, decode_file_to_jsonl, DecodeOptions, Codepage, RecordFormat,
    JsonNumberMode, RawMode, UnmappablePolicy, ZonedEncodingFormat
};
use std::path::Path;

// Parse copybook
let copybook_text = std::fs::read_to_string("customer.cpy")?;
let schema = parse_copybook(&copybook_text)?;

// Configure decode options for complete JSON output with zoned encoding preservation
let opts = DecodeOptions {
    codepage: Codepage::Cp037,
    format: RecordFormat::Fixed,
    json_number_mode: JsonNumberMode::Lossless, // Preserve decimal precision
    strict_mode: false,
    max_errors: Some(100),
    emit_filler: false, // When true, FILLER fields named as _filler_00000XXX (by computed byte offset)
    emit_meta: true,
    emit_raw: RawMode::Off,
    on_decode_unmappable: UnmappablePolicy::Error,
    threads: 1,
    preserve_zoned_encoding: true, // Enable binary round-trip fidelity
    preferred_zoned_encoding: ZonedEncodingFormat::Auto, // Auto-detect with EBCDIC fallback
};

// Single record decoding with complete field processing
let record_data = std::fs::read("single-record.bin")?;
let json_value = decode_record(&schema, &record_data, &opts)?;
// json_value now contains properly typed COBOL fields, not nulls
println!("Record: {}", serde_json::to_string_pretty(&json_value)?);

// Complete file processing with performance metrics
let input = std::fs::File::open("data.bin")?;
let output = std::fs::File::create("output.jsonl")?;
let summary = decode_file_to_jsonl(&schema, input, output, &opts)?;

println!("Processed {} records with {} errors at {:.2} MB/s",
         summary.records_processed, summary.records_with_errors, summary.throughput_mbps);
```

### Zoned Decimal Encoding Configuration

```rust
use copybook_codec::{DecodeOptions, EncodeOptions, ZonedEncodingFormat};

// Configure decode options for encoding preservation
let decode_opts = DecodeOptions::new()
    .with_codepage(Codepage::Cp037)
    .with_preserve_zoned_encoding(true) // Enable format detection and preservation
    .with_preferred_zoned_encoding(ZonedEncodingFormat::Ebcdic); // Fallback for ambiguous cases

// Configure encode options with explicit format override
let encode_opts = EncodeOptions::new()
    .with_codepage(Codepage::Cp037)
    .with_zoned_encoding_override(Some(ZonedEncodingFormat::Ascii)); // Force ASCII zones

// Or use preserved format from decode metadata
let encode_opts = EncodeOptions::new()
    .with_codepage(Codepage::Cp037)
    .with_zoned_encoding_override(None); // Respect preserved formats

// Round-trip with encoding preservation
let json_value = decode_record(&schema, &record_data, &decode_opts)?;
// json_value now contains _encoding_metadata for zoned decimal fields

let encoded_data = encode_record(&schema, &json_value, &encode_opts)?;
// encoded_data preserves original zoned decimal encoding format
```

### High-Performance Streaming Processing

```rust
use copybook_codec::{RecordIterator, DecodeOptions, iter_records_from_file, 
                     decode_record_with_scratch, memory::ScratchBuffers};

// Create iterator for processing records one at a time with complete field processing
// Note: Fixed format now requires LRECL to be specified in schema for truncation detection
let mut iter = iter_records_from_file("data.bin", &schema, &opts)?;

for (record_idx, record_result) in iter.enumerate() {
    match record_result {
        Ok(json_value) => {
            // Process individual record with properly typed COBOL fields
            // Example: {"CUSTOMER-ID": "00123", "CUSTOMER-NAME": "JOHN DOE", "BALANCE": "-1234.56"}
            println!("{}", serde_json::to_string(&json_value)?);
        }
        Err(e) => {
            // Enhanced error handling with specific error codes
            match e.code {
                ErrorCode::CBKD301_RECORD_TOO_SHORT => {
                    eprintln!("Truncated record detected: {}", e);
                    // Handle truncated records - could continue or stop based on requirements
                }
                ErrorCode::CBKD411_ZONED_BAD_SIGN => {
                    eprintln!("Invalid sign zone (check codepage/overpunch): {}", e);
                }
                _ => {
                    eprintln!("Record error: {}", e);
                }
            }
        }
    }
}

// For maximum performance, use scratch buffers to minimize allocations
let mut scratch = ScratchBuffers::new();
for record_data in records {
    let json_value = decode_record_with_scratch(&schema, &record_data, &opts, &mut scratch)?;
    // Process records with optional parallel workers
    process_record(json_value);
    // scratch buffers automatically cleared for next iteration
}
```

#### Enhanced RecordIterator API

- **LRECL Requirement**: Fixed-format processing now requires `schema.lrecl_fixed` to be set for proper truncation detection
- **Fail-Fast Validation**: RecordIterator constructor validates LRECL availability early
- **Enhanced Error Messages**: Precise byte counts and record indexing for truncation errors
- **Performance Optimized**: Enhanced validation with minimal performance impact

### Data Verification API

```rust
use copybook_codec::{iter_records_from_file, DecodeOptions, RecordFormat, Codepage};
use copybook_core::{parse_copybook, Error};

// Parse copybook
let copybook_text = std::fs::read_to_string("customer.cpy")?;
let schema = parse_copybook(&copybook_text)?;

// Configure verification options (no output needed)
let opts = DecodeOptions::new()
    .with_format(RecordFormat::Fixed)
    .with_codepage(Codepage::Cp037);

// Collect validation errors without generating output
let mut errors: Vec<Error> = Vec::new();
let mut iter = iter_records_from_file("data.bin", &schema, &opts)?;

while let Some(result) = iter.next() {
    if let Err(e) = result {
        let idx = iter.current_record_index();
        errors.push(e.with_record(idx));
    }
}

println!("Validation complete: {} errors found in {} records",
         errors.len(), iter.current_record_index());

// Process errors for reporting
for error in errors {
    if let Some(ctx) = &error.context {
        println!("Record {}: {} - {}",
                 ctx.record_index.unwrap_or(0),
                 error.code,
                 error.message);
    }
}
```

## Security & Compliance

copybook-rs implements enterprise-grade security scanning and compliance infrastructure:

- **Continuous Vulnerability Monitoring**: cargo-audit on every PR + weekly proactive scans
- **Supply Chain Security**: Enhanced deny.toml policies (no yanked crates, no wildcards, trusted sources only)
- **Automated Dependency Updates**: Dependabot with grouped security/routine updates
- **Regulatory Compliance**: SOX, HIPAA, GDPR, PCI DSS audit trail (90-day artifact retention)

See [SECURITY.md](SECURITY.md) for security scanning infrastructure and vulnerability response procedures.

## Enterprise Audit System

copybook-rs includes a comprehensive enterprise audit system for regulatory compliance, security monitoring, and data lineage tracking. The audit system is disabled by default for optimal performance and can be enabled when enterprise features are required.

### Enabling Enterprise Audit Features

The audit system is available as an optional feature that can be enabled during build:

```bash
# Enable audit features for CLI
cargo build --features audit

# Install CLI with audit capabilities
cargo install copybook-cli --features audit

# Build library with audit support
cargo build --workspace --features audit
```

### Supported Compliance Frameworks

- **SOX (Sarbanes-Oxley)**: Financial data integrity, audit trails, executive certification
- **HIPAA**: Protected health information processing, breach detection, safeguards validation
- **GDPR**: Personal data processing records, data subject rights, cross-border monitoring
- **PCI DSS**: Payment card data protection, encryption validation, access controls

### Enterprise Audit CLI Commands

When the audit feature is enabled, the CLI provides comprehensive audit capabilities:

```bash
# Validate compliance against multiple frameworks
copybook audit validate --compliance sox,gdpr,hipaa \
  --output compliance-report.json \
  financial-schema.cpy financial-data.bin

# Generate comprehensive audit reports
copybook audit report --include-performance \
  --include-lineage --include-security \
  schema.cpy data.bin --output audit-report.json

# Analyze data lineage and transformation impact
copybook audit lineage schema.cpy \
  --source-system mainframe \
  --target-system data-warehouse \
  --output lineage-analysis.json

# Performance audit with baseline validation
copybook audit performance schema.cpy data.bin \
  --baseline performance-baseline.json \
  --output performance-audit.json

# Security audit and access pattern analysis
copybook audit security schema.cpy \
  --monitor-access-patterns \
  --output security-audit.json

# Audit trail health and integrity monitoring
copybook audit health \
  --verify-integrity \
  --output health-report.json
```

### Enterprise Library API

```rust
#[cfg(feature = "audit")]
use copybook_core::audit::*;

#[cfg(feature = "audit")]
async fn enterprise_audit_example() -> Result<(), Box<dyn std::error::Error>> {
    // Create audit context with compliance requirements
    let audit_context = AuditContext::new()
        .with_operation_id("financial_data_processing_2024")
        .with_user("data_processor")
        .with_compliance_profiles(&[ComplianceProfile::SOX, ComplianceProfile::GDPR])
        .with_security_classification(SecurityClassification::Confidential);

    // Initialize compliance engine
    let compliance_engine = ComplianceEngine::new()
        .with_sox_validation(true)
        .with_gdpr_validation(true)
        .with_audit_trail_integrity(true);

    // Validate compliance before processing
    let validation_result = compliance_engine
        .validate_operation(&audit_context)
        .await?;

    if !validation_result.is_compliant() {
        for violation in validation_result.violations() {
            eprintln!("Compliance violation: {} - {}",
                     violation.violation_id, violation.title);
        }
        return Err("Compliance validation failed".into());
    }

    // Process data with comprehensive auditing
    let schema = parse_copybook_with_audit(&copybook_text, audit_context.clone())?;

    // Audit trail is automatically maintained throughout processing
    let processing_result = process_financial_data_with_audit(
        &schema,
        "financial-data.bin",
        &audit_context
    ).await?;

    // Generate compliance reports
    let compliance_report = compliance_engine
        .generate_compliance_report(&processing_result)
        .await?;

    println!("Compliance status: {:?}", compliance_report.overall_status);

    Ok(())
}
```

### Enterprise Configuration

```yaml
# copybook-audit.yaml - Enterprise audit configuration
audit:
  enabled: true
  output_format: json
  retention_days: 2555  # 7 years for SOX compliance

  compliance:
    sox:
      enabled: true
      validation_level: strict
      retention_years: 7
    hipaa:
      enabled: true
      phi_protection: strict
      breach_notification: automatic
    gdpr:
      enabled: true
      data_subject_rights: enabled
      cross_border_monitoring: true

  security:
    access_monitoring: true
    anomaly_detection: true
    encryption_validation: required

  performance:
    baseline_tracking: true
    regression_threshold: 0.05  # 5% degradation threshold

  data_lineage:
    field_level: true
    transformation_tracking: comprehensive
```

### Audit Trail Features

- **Cryptographic Integrity**: SHA-256 chaining for tamper-evident audit trails
- **Comprehensive Logging**: All operations tracked with detailed context
- **Compliance Validation**: Automated validation against regulatory requirements
- **Performance Monitoring**: Baseline tracking and regression detection
- **Data Lineage**: Field-level transformation tracking and impact analysis
- **Security Monitoring**: Access pattern analysis and anomaly detection

### Documentation

For comprehensive enterprise audit system documentation:

- **Specification**: [docs/specs/enterprise-audit-system-spec.md](docs/specs/enterprise-audit-system-spec.md)
- **Compliance Guide**: [docs/enterprise-compliance-guide.md](docs/enterprise-compliance-guide.md)
- **API Reference**: [docs/audit-api-reference.md](docs/audit-api-reference.md)
- **Architecture**: [docs/explanation/enterprise-audit-architecture.md](docs/explanation/enterprise-audit-architecture.md)

## Numeric Data Type Examples

copybook-rs provides comprehensive numeric data type handling with proper precision preservation and improved formatting support. The SmallDecimal type now implements the Display trait for improved debugging and string representation.

### Zoned Decimal Processing
```bash
# Decode EBCDIC zoned decimals with sign zones (C=+, D=-)
copybook decode schema.cpy mainframe-data.bin \
  --codepage cp037 \
  --json-number lossless \
  --output financial-data.jsonl
```

### Packed Decimal (COMP-3) Processing
```bash
# Decode packed decimal fields with nibble signs
copybook decode schema.cpy comp3-data.bin \
  --codepage cp037 \
  --json-number lossless \
  --output packed-data.jsonl
```

### Binary Integer Processing
```bash
# Process binary integers with explicit widths
copybook decode schema.cpy binary-data.bin \
  --json-number native \  # integers can use JSON numbers
  --output binary-data.jsonl

# Handle SYNCHRONIZED binary fields with automatic alignment
copybook decode schema.cpy aligned-binary-data.bin \
  --codepage cp037 \
  --json-number native \
  --output aligned-data.jsonl
```

### Binary Field Alignment (SYNCHRONIZED)

copybook-rs implements IBM mainframe SYNCHRONIZED alignment standards for binary fields:

- **16-bit binary fields**: Aligned to 2-byte boundaries
- **32-bit binary fields**: Aligned to 4-byte boundaries  
- **64-bit binary fields**: Aligned to 8-byte boundaries
- **Padding insertion**: Automatic insertion of alignment padding bytes (0x00) when SYNCHRONIZED is specified
- **Cross-platform consistency**: Alignment behavior matches IBM COBOL compilers across platforms

## Supported COBOL Features

### Data Types
- **Alphanumeric**: `PIC X(n)` - Character data with EBCDIC/ASCII conversion
- **Zoned Decimal**: `PIC 9(n)V9(m)`, `PIC S9(n)V9(m)` - Display numeric with EBCDIC/ASCII sign zones
  - **EBCDIC Overpunch**: Zone nibbles (C/F = positive, D = negative) in sign position
  - **ASCII Overpunch**: Special characters in sign position (A-I = +1 to +9, } = +0, J-R = -1 to -9)
  - **Negative Zero Normalization**: `-0` (negative zero) automatically normalizes to `"0"` in JSON output
  - **Sign Preservation**: Non-zero negative values preserve their negative sign (`-1234` remains `"-1234"`)
- **Packed Decimal**: `PIC 9(n)V9(m) COMP-3`, `PIC S9(n)V9(m) COMP-3` - Binary-coded decimal with nibble signs
  - Enhanced sign nibble handling (0xC/0xF = positive, 0xD/0xB = negative)
- **Binary Integer**: `PIC 9(n) COMP/BINARY`, `PIC S9(n) COMP/BINARY` - Big-endian integers (1-8 bytes)
- **Explicit Binary Width**: `PIC 9(n) BINARY(w)` - Binary integers with explicit byte width (1, 2, 4, 8)
- **Signed Fields**: Full support for signed zoned, packed, and binary types with proper sign handling

### Structure Features
- **Level Numbers**: 01-49 hierarchical grouping
- **REDEFINES**: Multiple views over same storage area
- **OCCURS**: Fixed arrays and variable arrays (OCCURS DEPENDING ON)
- **SYNCHRONIZED**: Field alignment on natural boundaries following IBM mainframe standards (2/4/8-byte boundaries for binary fields)
- **BLANK WHEN ZERO**: Special handling for zero values

### Record Formats
- **Fixed-Length**: Constant LRECL records
- **Variable-Length**: RDW (Record Descriptor Word) format
- **ODO Support**: Variable arrays at tail position only

### Limitations
- **Fully Supported**: Level-88 condition values (VALUE clauses) with complete parse, codec, and structural validation (see docs/reference/COBOL_SUPPORT_MATRIX.md for test evidence)
- **Unsupported**: COMP-1 (single-precision float) and COMP-2 (double-precision float)
- **Unsupported**: Edited PIC clauses (Z, slash, comma, $, CR, DB)
- **Unsupported**: SIGN LEADING/TRAILING SEPARATE directives
- **Unsupported**: Nested OCCURS DEPENDING ON arrays (ODO within ODO)
- **Unsupported**: RENAMES (66-level) items

## Error Handling

copybook-rs uses a comprehensive error taxonomy with stable codes:

### Parse Errors (CBKP*)
- `CBKP001_SYNTAX`: Copybook syntax errors
- `CBKP011_UNSUPPORTED_CLAUSE`: Unsupported COBOL clause or feature
- `CBKP021_ODO_NOT_TAIL`: ODO array not at tail position
- `CBKP051_UNSUPPORTED_EDITED_PIC`: Edited PIC clauses not supported

### Schema Validation Errors (CBKS*)
- `CBKS121_COUNTER_NOT_FOUND`: ODO counter field not found
- `CBKS141_RECORD_TOO_LARGE`: Record size exceeds maximum limit
- `CBKS301_ODO_CLIPPED`: ODO counter exceeds maximum (strict: fatal, lenient: warning with clamping)
- `CBKS302_ODO_RAISED`: ODO counter below minimum (strict: fatal, lenient: warning with clamping)

### Data Errors (CBKD*)
- `CBKD101_INVALID_FIELD_TYPE`: Invalid field type for operation
- `CBKD301_RECORD_TOO_SHORT`: Record truncated or too short for field data
- `CBKD401_COMP3_INVALID_NIBBLE`: Invalid packed decimal data
- `CBKD411_ZONED_BAD_SIGN`: Invalid zoned decimal sign or ASCII overpunch
- `CBKD412_ZONED_BLANK_IS_ZERO`: BLANK WHEN ZERO field decoded as zero
- `CBKD413_ZONED_INVALID_ENCODING`: Invalid zoned decimal encoding format
- `CBKD414_ZONED_MIXED_ENCODING`: Mixed ASCII/EBCDIC encoding in single field
- `CBKD415_ZONED_ENCODING_DETECTION_FAILED`: Unable to detect zoned decimal encoding format

### Encoding Errors (CBKE*)
- `CBKE501_JSON_TYPE_MISMATCH`: JSON type doesn't match field type or REDEFINES ambiguity
- `CBKE521_ARRAY_LEN_OOB`: Array length out of bounds

### Record Format Errors (CBKR*)
- `CBKR101_FIXED_RECORD_ERROR`: Error processing fixed-length record
- `CBKR201_RDW_READ_ERROR`: Error reading Record Descriptor Word header
- `CBKR211_RDW_RESERVED_NONZERO`: RDW reserved bytes contain non-zero values (warning)

See [ERROR_CODES.md](docs/reference/ERROR_CODES.md) for complete error reference and [REPORT.md](docs/REPORT.md) for detailed project status and performance analysis.

## Operational Notes

### Toolchain
- **Rust**: 1.90+ MSRV (workspace-enforced) | **Edition**: 2024
- **Platforms**: Developed and tested primarily on Linux; community validation exists for macOS and Windows
- **Memory**: Streaming decode/encode runs typically remain below 256 MiB on reference datasets
- **Dependencies**: Zero unsafe code in public APIs; clippy pedantic enforced in CI

### Current Reliability Snapshot
<!-- TEST_STATUS:BEGIN -->
- **Tests**: `cargo nextest` reports 615/615 passing (54 skipped) with comprehensive coverage across COBOL parsing, data encoding, and CLI integration (see `integrative_gate_summary.md`)
<!-- TEST_STATUS:END -->
- **Benchmarks**: Performance validated with CI receipts and baseline tracking. See [copybook-bench/BASELINE_METHODOLOGY.md](copybook-bench/BASELINE_METHODOLOGY.md) for measurement procedures, [copybook-bench/HARDWARE_SPECS.md](copybook-bench/HARDWARE_SPECS.md) for reference hardware specifications, and `scripts/bench/perf.json` artifact for current measurements (policy: accuracy-first).
- **Automation gaps**: The Python utilities promised in Issue #52 (`bench_runner.py`, `baseline_manager.py`, `slo_validator.py`, etc.) have not shipped yet; see `docs/backlog/benchmark_tooling.md`
- **Documentation**: Public messaging intentionally highlights correctness and open issues; raw performance tables live in `PERFORMANCE_VALIDATION_FINAL.md`

### Benchmarking & Regression Tracking
- Run ad-hoc benchmarks with `cargo bench --package copybook-bench`; `just bench-json` mirrors receipts for CI
- Receipts land in `scripts/bench/perf.json` via `just bench-json`, `bash scripts/bench.sh`, or `scripts\bench.bat`
- Perf receipts are machine-readable and attached to pull requests as `perf-json` artifacts with 90-day retention
- Until the Issue #52 utilities land, baseline promotion, regression detection, and SLO validation require manual analysis of the generated JSON plus the backlog tracker

### Performance receipts (deterministic)
```bash
# Linux/macOS
bash scripts/bench.sh

# Windows
scripts\bench.bat
```

```bash
# Capture host metadata + percentiles for receipts
bash scripts/perf-annotate-host.sh
bash scripts/soak-aggregate.sh
jq '.summary' scripts/bench/perf.json
```

Set `BENCH_FILTER=all` (or any Criterion filter) to widen coverage while keeping receipts in `scripts/bench/perf.json`.

Artifacts: `scripts/bench/perf.json` (90-day retention in CI). Performance gates use realistic floors (DISPLAY ≥ 80 MiB/s; COMP-3 ≥ 40 MiB/s) with neutral status. See [copybook-bench/BASELINE_METHODOLOGY.md](copybook-bench/BASELINE_METHODOLOGY.md) for baseline methodology and measurement procedures (policy: accuracy-first).

## Development

### Building

```bash
# Clone the repository
git clone https://github.com/EffortlessMetrics/copybook-rs.git
cd copybook-rs

# Build all crates
cargo build --workspace

# Build with optimizations
cargo build --workspace --release
```

### Testing

```bash
# Run all tests
cargo test --workspace

# Run with coverage
cargo test --workspace -- --nocapture

# Run performance benchmarks (JSON receipts)
just bench-json
# or: bash scripts/bench.sh / scripts\bench.bat

# Run clippy
cargo clippy --workspace -- -D warnings -W clippy::pedantic

# Format code
cargo fmt --all
```

### CI at a Glance

- **Blocking**: Quick (lint/build/test) and Test Suite (excludes `copybook-bench`)
- **Non-blocking**: Coverage upload (best-effort), Perf/Bench receipts, Nightly "Comprehensive"
- **Guards**:
  - `nextest` pinned via `taiki-e/install-action@v2` (checksum enabled)
  - All blocking test commands must include `--exclude copybook-bench`
- **Perf tests**: live in dedicated workflows. To run locally:
  ```bash
  cargo test -p copybook-bench --features perf
  ```

### Project Status & Roadmap

### **Current Status: Engineering Preview (v0.3.1)** ⚠️

**Canonical Status**: See [ROADMAP.md](docs/ROADMAP.md) for official project status and adoption guidance.

copybook-rs is suitable for teams that validate their copybooks against the supported feature set, but known limitations mean cautious adoption is recommended:

- ⚠️ **Feature Coverage**: COMP-1/COMP-2, edited PIC clauses, SIGN SEPARATE, nested ODOs, and RENAMES (66-level) remain unsupported; Level-88 condition values fully supported
- ⚠️ **Performance Variance**: Measurements show environmental variance in WSL2; see perf workflow artifacts and `scripts/bench/perf.json` for current receipts (advisory-only policy per Issues #74, #75)
- ✅ **Benchmark Automation**: `bench-report` CLI tool available (Issue #52) with baseline management, comparison, and validation commands
- ✅ **Quality Signals**: 615 tests passing (54 skipped), zero unsafe code, comprehensive error taxonomy including CBKR* family
- ✅ **Interface Stability**: CLI and library APIs are production-ready; feature completeness remains in preview

**Adoption Recommendations**:
1. Validate representative copybooks against supported features before deployment
2. Run pilot workloads to verify throughput meets requirements
3. Review [REPORT.md](docs/REPORT.md) for detailed readiness assessment
4. Consult [ROADMAP.md](docs/ROADMAP.md) for v0.5.0 dialect features and v1.0.0 stability timeline

See `integrative_gate_summary.md` and `PERFORMANCE_VALIDATION_FINAL.md` for detailed validation evidence.

### **Development Roadmap**
See [ROADMAP.md](docs/ROADMAP.md) for planned features and development phases. Current focus: v0.5.0 dialect features and benchmark automation (Issue #52).

## Repository Structure

### Core Workspace
- **copybook-core** - COBOL parsing engine (lexer, parser, AST)
- **copybook-codec** - Data encoding/decoding with character conversion
- **copybook-cli** - Command-line interface with subcommands
- **copybook-gen** - Test fixture generation and golden corpus
- **copybook-bench** - Performance benchmarks and validation

### Documentation (`docs/`)
- **reference/** - API docs, CLI reference, error codes
- **specs/** - Technical specifications and feature definitions
- **reports/** - Validation reports and PR analysis
- **adr/** - Architecture decision records
- **api/** - API documentation and contracts
- **explanation/** - In-depth technical explanations

### Development Resources
- **examples/** - Usage examples (basic/integration/enterprise)
- **fixtures/** - Test data and golden corpus for validation
- **test-data/** - Simple test copybooks and sample data
- **tools/** - Debug utilities and maintenance scripts
- **scripts/** - Automation and CI/CD scripts
- **schemas/** - JSON schemas for data validation

### Configuration
- **.config/** - Tool configurations (rustfmt, clippy, nextest)
- **.github/** - GitHub workflows and issue templates

## Contributing

We welcome contributions! Please see [REPORT.md](docs/REPORT.md) for current project status and [ROADMAP.md](docs/ROADMAP.md) for development priorities.

#### Development Workflow
1. Fork the repository
2. Create a feature branch
3. Make your changes with tests
4. Run `just ci-quick` or `cargo xtask ci --quick`
5. Submit a pull request using the provided template

#### Code Standards
- Follow Rust conventions and idioms with clippy pedantic compliance
- Add comprehensive tests for new features and help retire the remaining flaky/leaky cases highlighted by `cargo nextest`
- Update documentation for API changes
- Maintain MSRV compatibility (Rust 1.89)
- Use idiomatic Rust patterns (div_ceil, is_empty, range contains)
- Implement Display trait for user-facing types where appropriate
- Use safe type conversions (try_from() instead of unsafe casts)
- Optimize memory usage with scratch buffer patterns

## License

This project is licensed under the **GNU Affero General Public License v3.0 or later** (AGPL-3.0-or-later).

See [LICENSE](LICENSE) for the full license text.

### Why AGPL-3.0?

copybook-rs is designed for enterprise mainframe data processing with strong copyleft protections to ensure improvements benefit the community. The AGPL ensures that modifications made in cloud/SaaS deployments are shared back.

### Contributor License Agreement (CLA)

Contributors must sign a CLA before their contributions can be accepted. See [CLA.md](CLA.md) for details.

**For commercial licensing or alternative arrangements**, please contact the maintainers.
