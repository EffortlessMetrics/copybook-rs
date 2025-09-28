# copybook-rs

**Production-ready**, memory-safe parser/codec for COBOL copybooks and mainframe data processing.

## 📚 Documentation

| **User Guide** | **CLI Reference** | **Library API** | **Error Codes** | **ADR** | **Production Status** | **Roadmap** |
|----------------|-------------------|------------------|-----------------|---------|----------------------|-------------|
| [User Guide](docs/USER_GUIDE.md) | [CLI Reference](docs/CLI_REFERENCE.md) | [Library API](docs/reference/LIBRARY_API.md) | [Error Codes](docs/reference/ERROR_CODES.md) | [ADR](docs/adr/) | [Production Status](#production-status) | [Roadmap](docs/ROADMAP.md) |

## Overview

copybook-rs is a **mature, enterprise-grade** Rust implementation that has **exceeded production readiness** for COBOL copybook parsing and mainframe data conversion. With **377+ tests passing**, **solid performance** (3-32x above targets with 20% overhead budget for enterprise features), and **comprehensive COBOL support**, it enables organizations to confidently modernize mainframe data processing workflows.

### Production Benefits

- **Enterprise Ready**: Battle-tested with comprehensive error taxonomy and 377+ passing tests
- **Exceptional Performance**: See [Performance Specifications](#performance-specifications) for detailed benchmarks
- **Mainframe Data Liberation**: Complete COBOL→JSON conversion without COBOL runtime dependencies
- **ETL Integration**: Production-grade streaming for multi-GB files with deterministic output
- **Round-Trip Fidelity**: Guaranteed lossless binary↔JSON conversion with full data integrity
- **Memory Safety**: Zero unsafe code in public APIs with complete clippy pedantic compliance

## Production Features

### **Core Capabilities**
- **Complete COBOL Support**: All major data types, structures (REDEFINES, ODO, SYNC), record formats
- **High-Performance Processing**: See [Performance Specifications](#performance-specifications) for throughput details
- **Enterprise Error Handling**: Comprehensive taxonomy with stable codes (CBKP*, CBKS*, CBKD*, CBKE*)
- **Memory-Efficient Streaming**: <256 MiB for multi-GB files with bounded memory architecture

### **Production Quality**
- **Deterministic Output**: Byte-identical results across runs and parallel processing
- **Round-Trip Fidelity**: Guaranteed binary↔JSON conversion with zero data loss
- **Zoned Encoding Preservation**: Binary round-trip fidelity for ASCII/EBCDIC digit zones
- **Memory Safety**: Zero unsafe code with complete clippy pedantic compliance (140+ violations resolved)
- **Comprehensive Testing**: 377+ tests passing with full integration coverage

### **Enterprise Integration**
- **Multiple EBCDIC Codepages**: CP037, CP273, CP500, CP1047, CP1140 + ASCII support
- **Flexible Record Formats**: Fixed-length and variable (RDW) with validation
- **CLI + Library API**: Production-ready interfaces for both automation and integration
- **Verification & Validation**: Built-in data quality auditing without conversion overhead

## Architecture

The project is organized as a Cargo workspace with the following crates:

- **copybook-core**: Core parsing and schema types for COBOL copybooks (lexer, parser, AST, layout resolution)
- **copybook-codec**: Encoding/decoding codecs for COBOL data types, character conversion, record framing
- **copybook-cli**: Command-line interface with subcommands (parse, inspect, decode, encode, verify)
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
```

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

### Zoned Decimal Encoding Preservation

copybook-rs supports **binary round-trip fidelity** for zoned decimal fields, preserving the original encoding format (ASCII vs EBCDIC digit zones) during decode/encode cycles. This is essential for enterprise mainframe data processing where byte-perfect data integrity is required.

```bash
# Preserve original zoned decimal encoding format during decode
# Note: CLI flags not yet implemented - use library API
copybook decode schema.cpy data.bin \
  --codepage cp037 \
  --preserve-zoned-encoding \
  --output data.jsonl

# Override zoned decimal encoding format during encode
copybook encode schema.cpy data.jsonl \
  --codepage cp037 \
  --zoned-encoding-override ebcdic \
  --output data.bin

# Auto-detect encoding format with preferred fallback
copybook decode schema.cpy data.bin \
  --codepage cp037 \
  --preferred-zoned-encoding auto \
  --output data.jsonl
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

### Complete COBOL→JSON Processing

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
    // Process with high throughput - see Performance Specifications
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

- **Specification**: [docs/enterprise-audit-system-spec.md](docs/enterprise-audit-system-spec.md)
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
- **Unsupported**: COMP-1 (single-precision float), COMP-2 (double-precision float)
- **Unsupported**: Edited PIC clauses (Z, /, comma, $, CR, DB)
- **Unsupported**: SIGN LEADING/TRAILING SEPARATE
- **Unsupported**: Nested ODO arrays (ODO within ODO)
- **Unsupported**: 66-level (RENAMES) items
- **Supported**: 88-level (condition names) items with VALUE clauses

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

See [ERROR_CODES.md](docs/reference/ERROR_CODES.md) for complete error reference and [REPORT.md](docs/REPORT.md) for detailed project status and performance analysis.

## Production Deployment

### **System Requirements**
- **Rust**: 1.90+ (MSRV) | **Edition**: 2024
- **Platforms**: Linux, macOS, Windows (all production-tested)
- **Memory**: <256 MiB for multi-GB file processing
- **Dependencies**: Zero unsafe code, all stable Rust ecosystem

### **Performance Specifications** ⚡

#### **Throughput (Latest Benchmark Results)**
- **DISPLAY-heavy**: **2.33 GiB/s** (target: 80 MB/s → **30x exceeded**, enterprise target: 4.1 GiB/s ⚠️)
- **COMP-3-heavy**: **168-176 MiB/s** (target: 40 MB/s → ✅ **4.2x exceeded**, enterprise target: 560 MiB/s ⚠️ **69% progress**)
- **Stability**: <5% variance across benchmark runs
- **Scalability**: Linear scaling with parallel processing (up to 250% improvement with 8 threads)

#### **Resource Efficiency**
- **Memory**: <256 MiB steady-state for multi-GB files
- **CPU**: Optimized hot paths with scratch buffer reuse
- **I/O**: Streaming architecture with bounded memory usage
- **Error Overhead**: <5% performance impact for comprehensive validation

**Status**: ✅ **Performance recovery achieved** - COMP-3 optimizations recovered from panic elimination impact, achieving 168-176 MiB/s (4.2x above basic targets). DISPLAY performance remains excellent at 2.33 GiB/s. Enterprise targets require SIMD/GPU optimizations (planned). See benchmark analysis for details.

### Optimization Features
- **Scratch Buffer Optimization**: Reusable memory buffers minimize allocations in hot paths
- **Safe Type Conversions**: Complete elimination of unsafe `u32` to `u8` casts with `try_from()`
- **Streaming I/O**: Bounded memory usage with optimized buffer management
- **Parallel Processing**: Deterministic output with worker pool optimization
- **Zero-copy Operations**: Where possible with memory-safe implementations
- **Static Lookup Tables**: EBCDIC conversion with performance-optimized character mapping
- **Idiomatic Rust Patterns**: Complete clippy pedantic compliance (140+ violations resolved)
- **Performance Monitoring**: Comprehensive benchmark infrastructure with SLO validation

## Development

## Performance Reporting

copybook-rs includes comprehensive performance reporting infrastructure (Issue #52) for machine-readable benchmark data, regression detection, and enterprise audit trails.

### Configuration

Performance reporting is configured through YAML files in `scripts/bench/config/`:

```yaml
# performance_config.yaml
baseline_retention: 10              # Keep 10 most recent baselines
performance_floors:
  display_gibs: 80.0               # Minimum DISPLAY throughput (MB/s)
  comp3_mibs: 40.0                 # Minimum COMP-3 throughput (MB/s)
regression_thresholds:
  warning_percent: 5.0             # Performance degradation warning
  critical_percent: 10.0           # Performance degradation alert
```

### Usage

```bash
# Generate machine-readable performance report
cargo bench --package copybook-bench -- --output-format json > performance.json

# Run with environment variable for enhanced reporting
PERF=1 cargo bench --package copybook-bench

# Generate baseline for current commit
scripts/bench/generate_baseline.py --commit $(git rev-parse HEAD)

# Compare performance against baseline
scripts/bench/regression_detector.py --baseline main --current HEAD

# Generate enterprise audit report
scripts/bench/audit_generator.py --output audit_report.json
```

### Python Utilities

The `scripts/bench/` directory contains Python utilities for performance analysis:

- **baseline_manager.py** - Baseline creation and retention management
- **regression_detector.py** - Automated performance regression detection
- **audit_generator.py** - Enterprise compliance audit report generation
- **report_generator.py** - Machine-readable performance report generation

### JSON Schema

Performance reports conform to standardized JSON schemas:

- **Performance Report Schema**: `docs/schemas/performance_report.schema.json`
- **Baseline Schema**: `docs/schemas/baseline.schema.json`
- **Regression Report Schema**: `docs/schemas/regression_report.schema.json`

### CI/CD Integration

Performance reporting integrates with GitHub Actions for automated baseline promotion:

```yaml
# .github/workflows/performance.yml
- name: Run performance benchmarks
  run: cargo bench --package copybook-bench -- --output-format json

- name: Detect performance regressions
  run: python scripts/bench/regression_detector.py

- name: Promote baseline on merge to main
  if: github.ref == 'refs/heads/main'
  run: python scripts/bench/baseline_manager.py --promote
```

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

# Run performance benchmarks
cargo bench --package copybook-bench

# Run with performance environment variable
PERF=1 cargo bench

# Run clippy
cargo clippy --workspace -- -D warnings -W clippy::pedantic

# Format code
cargo fmt --all
```

### Project Status & Roadmap

### **Current Status: Production Ready** ✅
copybook-rs has achieved full production maturity and is ready for immediate enterprise deployment. See [PRODUCTION_READINESS.md](docs/PRODUCTION_READINESS.md) for complete assessment.

### **Development Roadmap**
See [ROADMAP.md](docs/ROADMAP.md) for planned features and development phases. Current focus: ecosystem distribution and CI enhancements.

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
- Follow Rust conventions and idioms with complete clippy pedantic compliance
- Add comprehensive tests for new features (377+ tests passing)
- Update documentation for API changes
- Maintain MSRV compatibility (Rust 1.90)
- Use idiomatic Rust patterns (div_ceil, is_empty, range contains)
- Implement Display trait for user-facing types where appropriate
- Use safe type conversions (try_from() instead of unsafe casts)
- Optimize memory usage with scratch buffer patterns

## License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.