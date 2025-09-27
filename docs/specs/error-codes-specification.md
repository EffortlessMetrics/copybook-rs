# Error Code Specification for Zoned Encoding Enhancement

## Overview

This document defines the new CBKD* error codes required for zoned decimal encoding format detection and validation, ensuring consistency with copybook-rs's enterprise error taxonomy and structured error handling patterns.

## New Error Codes

### CBKD413_ZONED_INVALID_ENCODING

**Purpose**: Invalid zoned decimal encoding format specification

**Severity**: Error

**Category**: Data validation error

**Usage Context**:
- CLI argument validation for invalid encoding format values
- API validation for ZonedEncodingFormat enum conversion
- Configuration validation in DecodeOptions/EncodeOptions

**Example Scenarios**:
```rust
// CLI: Invalid encoding format argument
copybook decode --preferred-zoned-encoding invalid_format data.cpy input.bin output.jsonl
// Error: CBKD413_ZONED_INVALID_ENCODING: Invalid encoding format 'invalid_format'. Valid formats: ascii, ebcdic, auto

// API: Invalid enum conversion
let format = ZonedEncodingFormat::from_str("xyz")?;
// Error: CBKD413_ZONED_INVALID_ENCODING: Unknown zoned encoding format 'xyz'

// Configuration: Incompatible format with codepage
let options = DecodeOptions::new()
    .with_codepage(Codepage::CP037)
    .with_preferred_zoned_encoding(Some(ZonedEncodingFormat::Ascii));
// Warning: ASCII encoding specified with EBCDIC codepage CP037
```

**Error Context Fields**:
- `invalid_format`: The invalid format string provided
- `valid_formats`: List of valid format options
- `suggested_format`: Closest valid format suggestion (if applicable)

### CBKD414_ZONED_MIXED_ENCODING

**Purpose**: Mixed ASCII/EBCDIC encoding detected within a single zoned decimal field

**Severity**: Warning (non-strict mode), Error (strict mode)

**Category**: Data integrity warning/error

**Usage Context**:
- Decode operations with encoding detection enabled
- Fields containing inconsistent zone nibbles (mix of 0x3 and 0xF)
- Enterprise data validation workflows requiring encoding consistency

**Example Scenarios**:
```rust
// Non-strict mode: Warning with graceful degradation
let data = vec![0x31, 0x32, 0xF3]; // ASCII '1', '2', then EBCDIC '3'
let result = decode_zoned_decimal(&data, 3, 0, false, Codepage::CP037, false);
// Warning: CBKD414_ZONED_MIXED_ENCODING: Mixed encoding in field 'customer_id': 1 EBCDIC zone, 2 ASCII zones. Using EBCDIC (confidence: 66.7%)

// Strict mode: Error with processing halt
let options = DecodeOptions::new().with_strict_mode(true);
// Error: CBKD414_ZONED_MIXED_ENCODING: Mixed encoding detected in field 'account_number': 2 inconsistent zones

// Enterprise audit trail
// Warning: CBKD414_ZONED_MIXED_ENCODING: Record 1247, Field 'transaction_amount', Bytes 156-163: Mixed ASCII/EBCDIC zones detected
```

**Error Context Fields**:
- `field_path`: Full path to the field with mixed encoding
- `record_index`: Record number where the issue occurred
- `byte_offset`: Byte offset within the record
- `ascii_zones`: Count of ASCII zone nibbles (0x3)
- `ebcdic_zones`: Count of EBCDIC zone nibbles (0xF)
- `detection_confidence`: Confidence level of format selection (0.0-1.0)
- `selected_format`: The encoding format chosen for processing

### CBKD415_ZONED_ENCODING_DETECTION_FAILED

**Purpose**: Unable to reliably detect zoned decimal encoding format

**Severity**: Warning (non-strict mode), Error (strict mode)

**Category**: Detection failure

**Usage Context**:
- Fields with no identifiable zone patterns
- Fields with all invalid zone nibbles
- Low confidence detection results in strict mode

**Example Scenarios**:
```rust
// Fields with invalid zones
let data = vec![0x41, 0x42, 0x43]; // Zone nibbles 0x4 (invalid for zoned decimal)
// Warning: CBKD415_ZONED_ENCODING_DETECTION_FAILED: No valid zone patterns in field 'legacy_field'. Using preferred format: EBCDIC

// Low confidence detection in strict mode
let data = vec![0x30]; // Single ASCII digit - low confidence
let options = DecodeOptions::new()
    .with_strict_mode(true)
    .with_preserve_zoned_encoding(true);
// Error: CBKD415_ZONED_ENCODING_DETECTION_FAILED: Low confidence encoding detection in field 'amount' (25.0% confidence). Minimum required: 80.0%

// Blank or zero-length fields
let data = vec![];
// Info: CBKD415_ZONED_ENCODING_DETECTION_FAILED: Empty field 'optional_field'. Using default format: EBCDIC
```

**Error Context Fields**:
- `field_path`: Full path to the field with detection failure
- `detection_confidence`: Confidence level achieved (0.0-1.0)
- `required_confidence`: Minimum confidence required (default: 0.8)
- `zone_pattern`: Description of zone nibbles found
- `fallback_format`: The encoding format used as fallback
- `field_length`: Length of the field data analyzed

## Error Code Integration

### Error Taxonomy Updates

```rust
// In copybook-core/src/error.rs

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorCode {
    // ... existing CBKD codes ...

    /// CBKD413: Invalid zoned decimal encoding format
    CBKD413_ZONED_INVALID_ENCODING,

    /// CBKD414: Mixed ASCII/EBCDIC encoding in single field
    CBKD414_ZONED_MIXED_ENCODING,

    /// CBKD415: Unable to detect zoned decimal encoding format
    CBKD415_ZONED_ENCODING_DETECTION_FAILED,
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // ... existing cases ...
            Self::CBKD413_ZONED_INVALID_ENCODING => write!(f, "CBKD413_ZONED_INVALID_ENCODING"),
            Self::CBKD414_ZONED_MIXED_ENCODING => write!(f, "CBKD414_ZONED_MIXED_ENCODING"),
            Self::CBKD415_ZONED_ENCODING_DETECTION_FAILED => write!(f, "CBKD415_ZONED_ENCODING_DETECTION_FAILED"),
        }
    }
}
```

### Error Severity Classification

```rust
// In copybook-core/src/error_reporter.rs

impl ErrorSeverity {
    pub fn from_error_code(code: ErrorCode, strict_mode: bool) -> Self {
        match code {
            // ... existing cases ...

            ErrorCode::CBKD413_ZONED_INVALID_ENCODING => ErrorSeverity::Error,

            ErrorCode::CBKD414_ZONED_MIXED_ENCODING => {
                if strict_mode {
                    ErrorSeverity::Error
                } else {
                    ErrorSeverity::Warning
                }
            },

            ErrorCode::CBKD415_ZONED_ENCODING_DETECTION_FAILED => {
                if strict_mode {
                    ErrorSeverity::Error
                } else {
                    ErrorSeverity::Warning
                }
            },
        }
    }
}
```

## Error Message Templates

### Structured Error Messages

```rust
/// Generate structured error message for encoding validation
pub fn format_encoding_error(
    code: ErrorCode,
    field_path: &str,
    details: &EncodingErrorDetails,
) -> String {
    match code {
        ErrorCode::CBKD413_ZONED_INVALID_ENCODING => {
            format!(
                "Invalid zoned encoding format '{}'. Valid formats: {}. Suggested: {}",
                details.invalid_format,
                details.valid_formats.join(", "),
                details.suggested_format.unwrap_or("ebcdic")
            )
        },

        ErrorCode::CBKD414_ZONED_MIXED_ENCODING => {
            if details.ascii_zones > 0 && details.ebcdic_zones > 0 {
                format!(
                    "Mixed encoding in field '{}': {} ASCII zones (0x3), {} EBCDIC zones (0xF). Using {} (confidence: {:.1}%)",
                    field_path,
                    details.ascii_zones,
                    details.ebcdic_zones,
                    details.selected_format,
                    details.detection_confidence * 100.0
                )
            } else {
                format!(
                    "Invalid zone patterns in field '{}': {} inconsistent zones detected",
                    field_path,
                    details.inconsistent_zones
                )
            }
        },

        ErrorCode::CBKD415_ZONED_ENCODING_DETECTION_FAILED => {
            if details.detection_confidence < details.required_confidence {
                format!(
                    "Low confidence encoding detection in field '{}': {:.1}% confidence (minimum: {:.1}%). Zone pattern: {}",
                    field_path,
                    details.detection_confidence * 100.0,
                    details.required_confidence * 100.0,
                    details.zone_pattern
                )
            } else {
                format!(
                    "No valid zone patterns in field '{}' (length: {}). Using fallback: {}",
                    field_path,
                    details.field_length,
                    details.fallback_format
                )
            }
        },

        _ => format!("Unexpected error code: {}", code),
    }
}

#[derive(Debug, Clone)]
pub struct EncodingErrorDetails {
    pub invalid_format: String,
    pub valid_formats: Vec<String>,
    pub suggested_format: Option<String>,
    pub ascii_zones: usize,
    pub ebcdic_zones: usize,
    pub inconsistent_zones: usize,
    pub detection_confidence: f32,
    pub required_confidence: f32,
    pub selected_format: String,
    pub zone_pattern: String,
    pub field_length: usize,
    pub fallback_format: String,
}
```

## Error Handling Patterns

### Graceful Degradation Strategy

```rust
/// Handle encoding detection with appropriate error/warning generation
pub fn handle_encoding_detection_result(
    detection: &ZonedEncodingDetection,
    field_path: &str,
    options: &DecodeOptions,
) -> Result<ZonedEncodingFormat> {
    // Check for mixed encoding
    if detection.has_mixed_encoding() {
        let details = EncodingErrorDetails {
            ascii_zones: detection.ascii_zones,
            ebcdic_zones: detection.ebcdic_zones,
            inconsistent_zones: detection.inconsistent_zones,
            detection_confidence: detection.confidence,
            selected_format: detection.format.to_string(),
            ..Default::default()
        };

        if options.strict_mode {
            return Err(Error::new(
                ErrorCode::CBKD414_ZONED_MIXED_ENCODING,
                format_encoding_error(
                    ErrorCode::CBKD414_ZONED_MIXED_ENCODING,
                    field_path,
                    &details
                )
            ).with_field_path(field_path));
        } else {
            warn!(
                "{}",
                format_encoding_error(
                    ErrorCode::CBKD414_ZONED_MIXED_ENCODING,
                    field_path,
                    &details
                )
            );
            crate::lib_api::increment_warning_counter();
        }
    }

    // Check for low confidence detection
    if !detection.is_reliable() {
        let details = EncodingErrorDetails {
            detection_confidence: detection.confidence,
            required_confidence: 0.8,
            zone_pattern: format!("{} valid zones", detection.consistent_zones),
            field_length: detection.field_length,
            fallback_format: detection.format.to_string(),
            ..Default::default()
        };

        if options.strict_mode && detection.confidence < 0.8 {
            return Err(Error::new(
                ErrorCode::CBKD415_ZONED_ENCODING_DETECTION_FAILED,
                format_encoding_error(
                    ErrorCode::CBKD415_ZONED_ENCODING_DETECTION_FAILED,
                    field_path,
                    &details
                )
            ).with_field_path(field_path));
        } else if detection.confidence < 0.5 {
            warn!(
                "{}",
                format_encoding_error(
                    ErrorCode::CBKD415_ZONED_ENCODING_DETECTION_FAILED,
                    field_path,
                    &details
                )
            );
            crate::lib_api::increment_warning_counter();
        }
    }

    Ok(detection.format)
}
```

### CLI Error Reporting

```rust
/// Enhanced CLI error reporting for encoding issues
pub fn report_encoding_cli_error(error: &copybook_core::Error) -> i32 {
    match error.code() {
        ErrorCode::CBKD413_ZONED_INVALID_ENCODING => {
            eprintln!("Error: Invalid encoding format specified");
            eprintln!("  {}", error.message());
            eprintln!("  Use --help to see valid encoding format options");
            4 // Configuration error exit code
        },

        ErrorCode::CBKD414_ZONED_MIXED_ENCODING => {
            eprintln!("Error: Data integrity issue - mixed encoding detected");
            eprintln!("  {}", error.message());
            if let Some(field) = error.field_path() {
                eprintln!("  Field: {}", field);
            }
            eprintln!("  Suggestion: Use --preferred-zoned-encoding to specify expected format");
            eprintln!("  Suggestion: Disable --strict mode to process with warnings");
            2 // Data error exit code
        },

        ErrorCode::CBKD415_ZONED_ENCODING_DETECTION_FAILED => {
            eprintln!("Error: Unable to detect encoding format reliably");
            eprintln!("  {}", error.message());
            if let Some(field) = error.field_path() {
                eprintln!("  Field: {}", field);
            }
            eprintln!("  Suggestion: Use --preferred-zoned-encoding to specify fallback format");
            eprintln!("  Suggestion: Disable --strict mode to use best-guess detection");
            2 // Data error exit code
        },

        _ => {
            // Fall back to standard error reporting
            emit_fatal(error)
        }
    }
}
```

## Testing Strategy

### Error Code Validation Tests

```rust
#[cfg(test)]
mod error_code_tests {
    use super::*;

    #[test]
    fn test_cbkd413_invalid_encoding_format() {
        let error = Error::new(
            ErrorCode::CBKD413_ZONED_INVALID_ENCODING,
            "Invalid encoding format 'xyz'"
        );

        assert_eq!(error.code(), ErrorCode::CBKD413_ZONED_INVALID_ENCODING);
        assert_eq!(
            error.code().to_string(),
            "CBKD413_ZONED_INVALID_ENCODING"
        );
    }

    #[test]
    fn test_cbkd414_mixed_encoding_severity() {
        let code = ErrorCode::CBKD414_ZONED_MIXED_ENCODING;

        // Non-strict mode should be warning
        assert_eq!(
            ErrorSeverity::from_error_code(code, false),
            ErrorSeverity::Warning
        );

        // Strict mode should be error
        assert_eq!(
            ErrorSeverity::from_error_code(code, true),
            ErrorSeverity::Error
        );
    }

    #[test]
    fn test_encoding_error_message_formatting() {
        let details = EncodingErrorDetails {
            invalid_format: "xyz".to_string(),
            valid_formats: vec!["ascii".to_string(), "ebcdic".to_string(), "auto".to_string()],
            suggested_format: Some("ebcdic".to_string()),
            ..Default::default()
        };

        let message = format_encoding_error(
            ErrorCode::CBKD413_ZONED_INVALID_ENCODING,
            "test_field",
            &details
        );

        assert!(message.contains("Invalid zoned encoding format 'xyz'"));
        assert!(message.contains("Valid formats: ascii, ebcdic, auto"));
        assert!(message.contains("Suggested: ebcdic"));
    }
}
```

### Integration Tests

```rust
#[cfg(test)]
mod integration_tests {
    use super::*;

    #[test]
    fn test_mixed_encoding_detection_and_error_generation() {
        // Create test data with mixed ASCII/EBCDIC zones
        let mixed_data = vec![
            0x31, // ASCII '1' (zone 0x3)
            0x32, // ASCII '2' (zone 0x3)
            0xF3, // EBCDIC '3' (zone 0xF)
        ];

        let options = DecodeOptions::new()
            .with_preserve_zoned_encoding(true)
            .with_strict_mode(false);

        let result = decode_zoned_decimal_with_encoding_detection(
            &mixed_data,
            3,
            0,
            false,
            &options
        );

        // Should succeed with warning in non-strict mode
        assert!(result.is_ok());

        // Should increment warning counter
        let warnings_before = get_warning_counter();
        let _ = result.unwrap();
        let warnings_after = get_warning_counter();
        assert_eq!(warnings_after, warnings_before + 1);
    }

    #[test]
    fn test_mixed_encoding_strict_mode_error() {
        let mixed_data = vec![0x31, 0xF2]; // ASCII '1', EBCDIC '2'

        let options = DecodeOptions::new()
            .with_preserve_zoned_encoding(true)
            .with_strict_mode(true);

        let result = decode_zoned_decimal_with_encoding_detection(
            &mixed_data,
            2,
            0,
            false,
            &options
        );

        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code(), ErrorCode::CBKD414_ZONED_MIXED_ENCODING);
    }
}
```

This error code specification ensures robust error handling while maintaining copybook-rs's enterprise-grade reliability and structured error taxonomy.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
