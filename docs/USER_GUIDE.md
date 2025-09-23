# Production User Guide

## Overview

copybook-rs is a **production-ready** system for enterprise mainframe data processing. This guide covers deployment, configuration, and best practices for production environments.

## Production Deployment

### **System Ready for Enterprise Use** ✅

copybook-rs has **achieved full production maturity** with:
- **127 tests passing** (comprehensive validation)
- **15-52x performance above enterprise targets**
- **Zero unsafe code** with complete memory safety
- **Multi-GB file processing** with <256 MiB memory usage
- **Comprehensive error taxonomy** for production monitoring

### Quick Production Setup

```bash
# Install from source (crates.io publishing pending)
git clone https://github.com/EffortlessMetrics/copybook-rs.git
cd copybook-rs
cargo build --workspace --release

# Add to PATH for CLI access
export PATH="$PATH:$(pwd)/target/release"
```

## Production Use Cases

### 1. Mainframe Data Migration
```bash
# High-performance data conversion for migration projects
copybook decode legacy-schema.cpy mainframe-data.bin \
  --output modern-data.jsonl \
  --format fixed \
  --codepage cp037 \
  --threads 8 \
  --emit-meta
```

### 2. ETL Pipeline Integration
```bash
# Streaming conversion for data warehouse pipelines
copybook decode schema.cpy daily-extract.bin \
  --output warehouse-feed.jsonl \
  --format rdw \
  --codepage cp1047 \
  --json-number lossless \
  --max-errors 100
```

### 3. Data Quality Auditing
```bash
# Validate data integrity without conversion overhead
copybook verify schema.cpy production-data.bin \
  --format fixed \
  --codepage cp037 \
  --report audit-results.json
```

## Performance Characteristics

### **Enterprise-Grade Throughput**
For detailed performance benchmarks and specifications, see [Performance Specifications](../README.md#performance-specifications).
- **Memory efficiency**: <256 MiB for multi-GB files
- **Parallel scaling**: Linear performance with thread count

### Production Benchmarking
```bash
# Validate performance for your workload
cargo bench --package copybook-bench
PERF=1 cargo bench  # Performance mode for production validation
```

## Error Handling in Production

### Error Code Categories
- **CBKP\***: Parse errors (copybook syntax issues)
- **CBKS\***: Schema validation (ODO bounds, record size limits)
- **CBKD\***: Data errors (invalid numeric formats, truncated records)
- **CBKE\***: Encoding errors (JSON type mismatches)

### Production Error Strategies
```bash
# Fail-fast for critical pipelines
copybook decode schema.cpy data.bin --fail-fast --output results.jsonl

# Lenient mode for data exploration
copybook decode schema.cpy data.bin --max-errors 1000 --output results.jsonl

# Detailed error reporting
copybook decode schema.cpy data.bin --verbose --output results.jsonl 2> errors.log
```

## COBOL Feature Support

### **Production-Ready Data Types**
- ✅ **Alphanumeric** (PIC X) with EBCDIC/ASCII conversion
- ✅ **Zoned Decimals** with proper sign handling
- ✅ **Packed Decimals** (COMP-3) with enhanced nibble processing
- ✅ **Binary Integers** with explicit width support
- ✅ **SYNCHRONIZED** fields with IBM mainframe alignment

### **Enterprise Structure Features**
- ✅ **REDEFINES** for multiple storage views
- ✅ **OCCURS DEPENDING ON** for variable arrays
- ✅ **Fixed/Variable Records** (RDW format support)
- ✅ **Multiple Codepages** (CP037, CP273, CP500, CP1047, CP1140)

### **Known Limitations** (By Design)
- COMP-1/COMP-2 floating-point (rare in production)
- Edited PIC clauses (display formatting - use JSON for presentation)
- Nested ODO arrays (complex edge case)

## Production Best Practices

### Memory Management
```bash
# For large files, use streaming with bounded memory
copybook decode schema.cpy huge-file.bin \
  --output streaming-results.jsonl \
  --threads 4  # Memory stays <256 MiB regardless of file size
```

### Data Validation
```bash
# Always validate before processing
copybook verify schema.cpy production-data.bin \
  --format fixed \
  --report validation.json

# Check validation results
echo "Exit code: $?"  # 0=valid, 3=data errors, 2=fatal errors
```

### Round-Trip Testing
```bash
# Validate data integrity for critical workflows
copybook decode schema.cpy original.bin --output temp.jsonl
copybook encode schema.cpy temp.jsonl --output roundtrip.bin
diff original.bin roundtrip.bin  # Should be identical
```

## Integration Patterns

### Library API for Custom Applications
```rust
use copybook_core::parse_copybook;
use copybook_codec::{decode_file_to_jsonl, DecodeOptions, Codepage};

// Production-ready processing
let schema = parse_copybook(&copybook_text)?;
let opts = DecodeOptions::new()
    .with_codepage(Codepage::Cp037)
    .with_json_number_mode(JsonNumberMode::Lossless);

let summary = decode_file_to_jsonl(&schema, input, output, &opts)?;
println!("Processed {} records at {:.2} MB/s",
         summary.records_processed, summary.throughput_mbps);
```

### CLI Automation
```bash
#!/bin/bash
# Production pipeline example
set -euo pipefail

SCHEMA="schemas/customer.cpy"
INPUT="data/daily-extract.bin"
OUTPUT="processed/$(date +%Y%m%d)-customers.jsonl"

# Validate first
copybook verify "$SCHEMA" "$INPUT" --format fixed --codepage cp037

# Process with error handling
if copybook decode "$SCHEMA" "$INPUT" \
    --output "$OUTPUT" \
    --format fixed \
    --codepage cp037 \
    --max-errors 10; then
    echo "Processing completed successfully"
    # Trigger downstream processing
else
    echo "Processing failed with exit code $?"
    # Alert operations team
fi
```

## Monitoring and Observability

### Performance Monitoring
```bash
# Enable verbose logging for throughput metrics
copybook decode schema.cpy data.bin --verbose --output results.jsonl 2>&1 | \
    grep -E "(MB/s|records|errors)"
```

### Error Analysis
```bash
# Extract error patterns for monitoring
copybook decode schema.cpy data.bin --verbose --output results.jsonl 2>&1 | \
    grep -E "CBK[DPSE][0-9]+" | sort | uniq -c
```

## Support and Documentation

### Complete Documentation
- **[README.md](../README.md)**: Comprehensive feature overview
- **[REPORT.md](../REPORT.md)**: Production readiness assessment
- **[ERROR_CODES.md](ERROR_CODES.md)**: Complete error reference
- **[CLI_REFERENCE.md](CLI_REFERENCE.md)**: Detailed command documentation
- **[LIBRARY_API.md](LIBRARY_API.md)**: Programming interface guide

### Production Support
For production deployments:
1. Review comprehensive test coverage (127 tests)
2. Validate performance for your specific workload
3. Implement error monitoring using stable error codes
4. Use verification mode for data quality auditing

**Status**: copybook-rs is production-ready and suitable for immediate enterprise deployment with substantial performance safety margins.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
