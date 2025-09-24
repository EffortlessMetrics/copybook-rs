# CLI Interface Enhancements for Zoned Encoding Preservation

## Overview

This document specifies the command-line interface enhancements required to support zoned decimal encoding format detection and preservation in copybook-rs. The enhancements maintain backward compatibility while providing fine-grained control over encoding behavior.

## Decode Command Enhancements

### New Flags

```bash
# Enable encoding format preservation
copybook decode --preserve-encoding copybook.cpy data.bin output.jsonl

# Set preferred encoding for auto-detection fallback
copybook decode --preferred-zoned-encoding ascii copybook.cpy data.bin output.jsonl
copybook decode --preferred-zoned-encoding ebcdic copybook.cpy data.bin output.jsonl
copybook decode --preferred-zoned-encoding auto copybook.cpy data.bin output.jsonl

# Combined usage
copybook decode \
  --format fixed \
  --codepage cp037 \
  --preserve-encoding \
  --preferred-zoned-encoding ascii \
  copybook.cpy data.bin output.jsonl
```

### Flag Specifications

#### `--preserve-encoding`

- **Type**: Boolean flag (no argument)
- **Default**: `false`
- **Purpose**: Enable detection and preservation of zoned decimal encoding formats
- **Behavior**:
  - When enabled, adds encoding metadata to JSON output
  - Stores detected format information for use during encode operations
  - Generates warnings for mixed encoding scenarios in non-strict mode
  - Generates errors for mixed encoding scenarios in strict mode

#### `--preferred-zoned-encoding <FORMAT>`

- **Type**: Enum argument
- **Values**: `ascii` | `ebcdic` | `auto`
- **Default**: `ebcdic` (maintains backward compatibility)
- **Purpose**: Set fallback encoding when detection is uncertain
- **Behavior**:
  - Used when encoding detection confidence is low
  - Used for fields with no identifiable zone patterns
  - Applied when `--preserve-encoding` is enabled
  - Ignored when `--preserve-encoding` is disabled

### Argument Validation

```rust
// In copybook-cli/src/commands/decode.rs

#[derive(Parser)]
pub struct DecodeCommand {
    // ... existing fields ...

    /// Enable zoned decimal encoding preservation
    #[arg(long, help = "Preserve zoned decimal encoding format (ASCII vs EBCDIC) for round-trip fidelity")]
    pub preserve_encoding: bool,

    /// Preferred zoned encoding format for auto-detection fallback
    #[arg(
        long,
        value_enum,
        help = "Preferred encoding when detection is uncertain (requires --preserve-encoding)"
    )]
    pub preferred_zoned_encoding: Option<ZonedEncodingFormat>,
}

impl DecodeCommand {
    /// Validate argument combinations
    pub fn validate(&self) -> Result<(), String> {
        if self.preferred_zoned_encoding.is_some() && !self.preserve_encoding {
            return Err(
                "--preferred-zoned-encoding requires --preserve-encoding to be enabled".to_string()
            );
        }

        Ok(())
    }

    /// Convert to DecodeOptions
    pub fn to_decode_options(&self) -> DecodeOptions {
        DecodeOptions::new()
            .with_format(self.format)
            .with_codepage(self.codepage)
            .with_preserve_zoned_encoding(self.preserve_encoding)
            .with_preferred_zoned_encoding(self.preferred_zoned_encoding)
            // ... other options ...
    }
}
```

## Encode Command Enhancements

### New Flags

```bash
# Override zoned decimal encoding format
copybook encode --zoned-encoding ascii copybook.cpy input.jsonl output.bin
copybook encode --zoned-encoding ebcdic copybook.cpy input.jsonl output.bin
copybook encode --zoned-encoding auto copybook.cpy input.jsonl output.bin

# Use preserved encoding from decode metadata (default behavior when metadata present)
copybook encode copybook.cpy input.jsonl output.bin

# Force specific encoding, ignoring preserved metadata
copybook encode --zoned-encoding ebcdic copybook.cpy input.jsonl output.bin

# Combined usage
copybook encode \
  --format fixed \
  --codepage cp037 \
  --zoned-encoding ascii \
  --fail-fast \
  copybook.cpy input.jsonl output.bin
```

### Flag Specifications

#### `--zoned-encoding <FORMAT>`

- **Type**: Enum argument
- **Values**: `ascii` | `ebcdic` | `auto`
- **Default**: Uses preserved metadata, falls back to `ebcdic`
- **Purpose**: Override zoned decimal encoding format during encode
- **Behavior**:
  - Takes precedence over preserved encoding metadata
  - `auto` value uses preserved metadata when available
  - Falls back to EBCDIC for backward compatibility when no metadata
  - Validates compatibility with input data in strict mode

### Argument Validation

```rust
// In copybook-cli/src/commands/encode.rs

#[derive(Parser)]
pub struct EncodeCommand {
    // ... existing fields ...

    /// Override zoned decimal encoding format
    #[arg(
        long,
        value_enum,
        help = "Override zoned decimal encoding format (ascii/ebcdic/auto)"
    )]
    pub zoned_encoding: Option<ZonedEncodingFormat>,
}

impl EncodeCommand {
    /// Convert to EncodeOptions
    pub fn to_encode_options(&self) -> EncodeOptions {
        EncodeOptions::new()
            .with_format(self.format)
            .with_codepage(self.codepage)
            .with_zoned_encoding_override(self.zoned_encoding)
            // ... other options ...
    }
}
```

## Help Text and Documentation

### Decode Command Help

```bash
$ copybook decode --help

Decode binary COBOL data to JSONL format

Usage: copybook decode [OPTIONS] <COPYBOOK> <INPUT> <OUTPUT>

Arguments:
  <COPYBOOK>  Path to COBOL copybook file
  <INPUT>     Path to binary data file
  <OUTPUT>    Path to output JSONL file

Options:
      --format <FORMAT>
          Record format [default: fixed] [possible values: fixed, rdw]
      --codepage <CODEPAGE>
          Character encoding [default: cp037] [possible values: ascii, cp037, cp273, cp500, cp1047, cp1140]
      --preserve-encoding
          Preserve zoned decimal encoding format (ASCII vs EBCDIC) for round-trip fidelity
      --preferred-zoned-encoding <FORMAT>
          Preferred encoding when detection is uncertain (requires --preserve-encoding)
          [possible values: ascii, ebcdic, auto]
      --strict
          Enable strict error handling mode
      --fail-fast
          Stop processing on first error
      --max-errors <COUNT>
          Maximum number of errors before stopping
      --threads <COUNT>
          Number of processing threads [default: 1]
  -h, --help
          Print help information
```

### Encode Command Help

```bash
$ copybook encode --help

Encode JSONL data to binary COBOL format

Usage: copybook encode [OPTIONS] <COPYBOOK> <INPUT> <OUTPUT>

Arguments:
  <COPYBOOK>  Path to COBOL copybook file
  <INPUT>     Path to input JSONL file
  <OUTPUT>    Path to output binary file

Options:
      --format <FORMAT>
          Record format [default: fixed] [possible values: fixed, rdw]
      --codepage <CODEPAGE>
          Character encoding [default: cp037] [possible values: ascii, cp037, cp273, cp500, cp1047, cp1140]
      --zoned-encoding <FORMAT>
          Override zoned decimal encoding format (ascii/ebcdic/auto)
          [possible values: ascii, ebcdic, auto]
      --use-raw
          Use raw data when available in JSON metadata
      --bwz-encode
          Enable BLANK WHEN ZERO encoding policy
      --strict
          Enable strict error handling mode
      --fail-fast
          Stop processing on first error
      --max-errors <COUNT>
          Maximum number of errors before stopping
      --threads <COUNT>
          Number of processing threads [default: 1]
  -h, --help
          Print help information
```

## Usage Examples

### Basic Round-Trip Workflow

```bash
# Step 1: Decode with encoding preservation
copybook decode \
  --format fixed \
  --codepage cp037 \
  --preserve-encoding \
  customer.cpy mainframe_data.bin customer.jsonl

# Step 2: Encode using preserved formats (byte-identical output)
copybook encode \
  --format fixed \
  --codepage cp037 \
  customer.cpy customer.jsonl reconstructed_data.bin

# Step 3: Verify byte-identical round-trip
cmp mainframe_data.bin reconstructed_data.bin
# No output = files are identical
```

### Mixed Encoding Handling

```bash
# Decode with ASCII preference for uncertain fields
copybook decode \
  --format fixed \
  --codepage ascii \
  --preserve-encoding \
  --preferred-zoned-encoding ascii \
  mixed_data.cpy input.bin output.jsonl

# Force EBCDIC encoding, ignoring preserved metadata
copybook encode \
  --format fixed \
  --codepage cp037 \
  --zoned-encoding ebcdic \
  mixed_data.cpy output.jsonl ebcdic_output.bin
```

### Enterprise Validation Workflow

```bash
# Strict mode with encoding validation
copybook decode \
  --format fixed \
  --codepage cp037 \
  --preserve-encoding \
  --strict \
  --fail-fast \
  enterprise.cpy production_data.bin validated.jsonl

# Performance benchmarking with encoding detection
time copybook decode \
  --format fixed \
  --codepage cp037 \
  --preserve-encoding \
  --threads 8 \
  large_data.cpy multi_gb_file.bin output.jsonl
```

## Error Handling and Messages

### Validation Errors

```bash
# Missing --preserve-encoding
$ copybook decode --preferred-zoned-encoding ascii data.cpy input.bin output.jsonl
Error: --preferred-zoned-encoding requires --preserve-encoding to be enabled

# Invalid encoding format
$ copybook decode --preferred-zoned-encoding invalid data.cpy input.bin output.jsonl
Error: Invalid value 'invalid' for '--preferred-zoned-encoding <FORMAT>'
  Possible values: ascii, ebcdic, auto
```

### Runtime Warnings

```bash
# Mixed encoding detected (non-strict mode)
$ copybook decode --preserve-encoding data.cpy input.bin output.jsonl
Warning: CBKD414_ZONED_MIXED_ENCODING: Mixed encoding in field 'customer_id', using ebcdic (confidence: 75.0%)
Records processed: 1000
Warnings: 1
```

### Runtime Errors

```bash
# Mixed encoding detected (strict mode)
$ copybook decode --preserve-encoding --strict data.cpy input.bin output.jsonl
Error: CBKD414_ZONED_MIXED_ENCODING: Mixed encoding detected in field 'customer_id': 2 inconsistent zones
Exit code: 2
```

## Integration with Existing Flags

### Compatibility Matrix

| Flag Combination | Behavior | Notes |
|------------------|----------|-------|
| `--preserve-encoding` | Enable encoding detection and metadata emission | New functionality |
| `--preserve-encoding --strict` | Error on mixed/uncertain encoding | Enhanced validation |
| `--preferred-zoned-encoding` without `--preserve-encoding` | Error | Invalid combination |
| `--zoned-encoding auto` | Use preserved metadata or EBCDIC default | Backward compatible |
| `--zoned-encoding ascii/ebcdic` | Override preserved metadata | Explicit control |

### Performance Considerations

```bash
# Encoding detection adds ~2-5% overhead
$ time copybook decode data.cpy input.bin output.jsonl
real    0m10.500s

$ time copybook decode --preserve-encoding data.cpy input.bin output.jsonl
real    0m10.815s  # ~3% overhead for encoding detection

# Parallel processing maintains efficiency
$ time copybook decode --preserve-encoding --threads 8 data.cpy large_file.bin output.jsonl
# Scales linearly with thread count
```

## Testing Strategy

### CLI Integration Tests

```bash
# Test script: test_cli_encoding_flags.sh
#!/bin/bash

# Test 1: Basic preserve-encoding functionality
copybook decode --preserve-encoding test.cpy ascii_data.bin output1.jsonl
grep "_encoding.*ascii" output1.jsonl || exit 1

# Test 2: Round-trip fidelity
copybook decode --preserve-encoding test.cpy input.bin temp.jsonl
copybook encode test.cpy temp.jsonl output.bin
cmp input.bin output.bin || exit 1

# Test 3: Error handling
copybook decode --preferred-zoned-encoding ascii test.cpy input.bin output.jsonl 2>&1 | \
    grep "requires --preserve-encoding" || exit 1

# Test 4: Performance regression
time copybook decode --preserve-encoding large.cpy multi_gb.bin output.jsonl
# Verify throughput maintains >4.1 GiB/s for DISPLAY fields
```

### Argument Parsing Tests

```rust
#[cfg(test)]
mod cli_tests {
    use super::*;
    use clap::Parser;

    #[test]
    fn test_decode_preserve_encoding_flag() {
        let args = DecodeCommand::parse_from([
            "decode",
            "--preserve-encoding",
            "test.cpy",
            "input.bin",
            "output.jsonl"
        ]);
        assert!(args.preserve_encoding);
        assert!(args.preferred_zoned_encoding.is_none());
    }

    #[test]
    fn test_decode_preferred_encoding_requires_preserve() {
        let args = DecodeCommand::parse_from([
            "decode",
            "--preferred-zoned-encoding", "ascii",
            "test.cpy",
            "input.bin",
            "output.jsonl"
        ]);
        assert!(args.validate().is_err());
    }

    #[test]
    fn test_encode_zoned_encoding_override() {
        let args = EncodeCommand::parse_from([
            "encode",
            "--zoned-encoding", "ascii",
            "test.cpy",
            "input.jsonl",
            "output.bin"
        ]);
        assert_eq!(args.zoned_encoding, Some(ZonedEncodingFormat::Ascii));
    }
}
```

This CLI interface specification ensures enterprise-grade usability while maintaining copybook-rs's commitment to backward compatibility and performance excellence.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
