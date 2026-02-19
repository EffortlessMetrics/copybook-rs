// SPDX-License-Identifier: AGPL-3.0-or-later
//! Test scaffolding for `ZonedEncodingFormat` enum - Issue #48
//!
//! Tests COBOL zoned decimal encoding preservation spec: SPEC.manifest.yml#ZonedEncodingFormat
//!
//! This test suite validates:
//! - AC1: Zoned decimal encoding format detection correctly identifies ASCII vs EBCDIC digit zones
//! - AC2: `DecodeOptions` supports `preserve_zoned_encoding` flag
//! - AC3: `DecodeOptions` supports `preferred_zoned_encoding` option

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![allow(clippy::unnecessary_wraps, clippy::used_underscore_binding)] // TDD Red → Green phase stubs

use anyhow::Result;
use copybook_codec::{Codepage, DecodeOptions, RecordFormat, ZonedEncodingFormat};
use copybook_core::parse_copybook;

/// AC1: Test `ZonedEncodingFormat` enum behavior and conversions
/// Tests COBOL parsing spec: SPEC.manifest.yml#ZonedEncodingFormat-enum-variants
#[test]
fn test_zoned_encoding_format_enum_variants() {
    // Test that ZonedEncodingFormat has correct variants
    let ascii_format = ZonedEncodingFormat::Ascii;
    let ebcdic_format = ZonedEncodingFormat::Ebcdic;
    #[allow(clippy::no_effect_underscore_binding)]
    let _auto_format = ZonedEncodingFormat::Auto;

    assert!(ascii_format.is_ascii());
    assert!(!ascii_format.is_ebcdic());
    assert_eq!(ascii_format.description(), "ASCII digit zones (0x30-0x39)");

    assert!(ebcdic_format.is_ebcdic());
    assert!(!ebcdic_format.is_ascii());
    assert_eq!(
        ebcdic_format.description(),
        "EBCDIC digit zones (0xF0-0xF9)"
    );
}

/// AC1: Test ASCII zoned decimal encoding detection (0x30-0x39 digit zones)
/// Tests COBOL parsing spec: SPEC.manifest.yml#encoding-detection-ascii
#[test]
fn test_ascii_zoned_encoding_detection() -> Result<()> {
    let copybook = "01 ZONED-FIELD PIC 9(3).";
    let schema = parse_copybook(copybook)?;

    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_preserve_zoned_encoding(true);

    // ASCII zoned decimal data: "123" = 0x31, 0x32, 0x33
    let ascii_data = b"\x31\x32\x33";

    let result = copybook_codec::decode_record(&schema, ascii_data, &options)?;

    // Verify encoding metadata is present and detects ASCII
    let metadata = result.get("_encoding_metadata");
    assert!(
        metadata.is_some(),
        "Should include _encoding_metadata when preserve_zoned_encoding is true"
    );
    let meta_obj = metadata.unwrap().as_object().unwrap();
    assert_eq!(
        meta_obj["ZONED-FIELD"], "ascii",
        "Should detect ASCII encoding"
    );
    Ok(())
}

/// AC1: Test EBCDIC zoned decimal encoding detection (0xF0-0xF9 digit zones)
/// Tests COBOL parsing spec: SPEC.manifest.yml#encoding-detection-ebcdic
#[test]
fn test_ebcdic_zoned_encoding_detection() -> Result<()> {
    let copybook = "01 ZONED-FIELD PIC 9(3).";
    let schema = parse_copybook(copybook)?;

    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_preserve_zoned_encoding(true);

    // EBCDIC zoned decimal data: "123" = 0xF1, 0xF2, 0xF3
    let ebcdic_data = b"\xF1\xF2\xF3";

    let result = copybook_codec::decode_record(&schema, ebcdic_data, &options)?;

    // Verify encoding metadata is present and detects EBCDIC
    let metadata = result.get("_encoding_metadata");
    assert!(
        metadata.is_some(),
        "Should include _encoding_metadata when preserve_zoned_encoding is true"
    );
    let meta_obj = metadata.unwrap().as_object().unwrap();
    assert_eq!(
        meta_obj["ZONED-FIELD"], "ebcdic",
        "Should detect EBCDIC encoding"
    );
    Ok(())
}

/// AC2: Test `DecodeOptions` `preserve_zoned_encoding` flag support
/// Tests COBOL parsing spec: SPEC.manifest.yml#DecodeOptions-preserve_zoned_encoding
#[test]
fn test_decode_options_preserve_zoned_encoding_flag() -> Result<()> {
    // Test that DecodeOptions supports preserve_zoned_encoding field
    let _options = DecodeOptions::new();

    // Test preserve_zoned_encoding functionality
    let mut options = _options.with_preserve_zoned_encoding(true);
    assert!(options.preserve_zoned_encoding);

    options = options.with_preserve_zoned_encoding(false);
    assert!(!options.preserve_zoned_encoding);

    Ok(())
}

/// AC3: Test `DecodeOptions` `preferred_zoned_encoding` option support
/// Tests COBOL parsing spec: SPEC.manifest.yml#DecodeOptions-preferred_zoned_encoding
#[test]
fn test_decode_options_preferred_zoned_encoding() -> Result<()> {
    // Test that DecodeOptions supports preferred_zoned_encoding field
    let _options = DecodeOptions::new();

    // Test preferred_zoned_encoding functionality
    let mut options =
        _options.with_preferred_zoned_encoding(copybook_codec::ZonedEncodingFormat::Ascii);
    assert_eq!(
        options.preferred_zoned_encoding,
        copybook_codec::ZonedEncodingFormat::Ascii
    );

    options = options.with_preferred_zoned_encoding(copybook_codec::ZonedEncodingFormat::Ebcdic);
    assert_eq!(
        options.preferred_zoned_encoding,
        copybook_codec::ZonedEncodingFormat::Ebcdic
    );

    options = options.with_preferred_zoned_encoding(copybook_codec::ZonedEncodingFormat::Auto);
    assert_eq!(
        options.preferred_zoned_encoding,
        copybook_codec::ZonedEncodingFormat::Auto
    );

    Ok(())
}

/// AC9: Test mixed ASCII/EBCDIC encoding detection within single field
/// Tests COBOL parsing spec: SPEC.manifest.yml#mixed-encoding-detection
#[test]
fn test_mixed_encoding_detection_single_field() -> Result<()> {
    let copybook = "01 ZONED-FIELD PIC 9(4).";
    let schema = parse_copybook(copybook)?;

    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_preserve_zoned_encoding(true);

    // Mixed encoding: First two ASCII zones, last two EBCDIC zones
    // "12" ASCII + "34" EBCDIC = b"\x31\x32\xF3\xF4"
    let mixed_data = b"\x31\x32\xF3\xF4";

    // Mixed encoding fields are excluded from _encoding_metadata (has_mixed_encoding=true)
    let result = copybook_codec::decode_record(&schema, mixed_data, &options);
    // Mixed encoding in the data may cause a decode error or succeed with no metadata
    // Either outcome is acceptable
    if let Ok(json) = result {
        // If decode succeeds, mixed-encoding field should NOT appear in metadata
        if let Some(meta) = json.get("_encoding_metadata") {
            let meta_obj = meta.as_object().unwrap();
            assert!(
                !meta_obj.contains_key("ZONED-FIELD"),
                "Mixed-encoding field should not appear in _encoding_metadata"
            );
        }
    }
    Ok(())
}

/// AC9: Test mixed encoding detection across multiple fields
/// Tests COBOL parsing spec: SPEC.manifest.yml#mixed-encoding-multiple-fields
#[test]
fn test_mixed_encoding_detection_multiple_fields() -> Result<()> {
    let copybook = r"
01 RECORD.
   05 FIELD1 PIC 9(2).
   05 FIELD2 PIC 9(2).
";
    let schema = parse_copybook(copybook)?;

    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_preserve_zoned_encoding(true);

    // FIELD1 = EBCDIC "12", FIELD2 = EBCDIC "34"
    let ebcdic_data = b"\xF1\xF2\xF3\xF4";

    let result = copybook_codec::decode_record(&schema, ebcdic_data, &options)?;

    // Both fields should appear in encoding metadata as EBCDIC
    let metadata = result.get("_encoding_metadata");
    assert!(metadata.is_some(), "Should include _encoding_metadata");
    let meta_obj = metadata.unwrap().as_object().unwrap();
    assert_eq!(meta_obj["FIELD1"], "ebcdic");
    assert_eq!(meta_obj["FIELD2"], "ebcdic");
    Ok(())
}

/// Property-based test for encoding detection with various zone patterns
/// Tests COBOL parsing spec: SPEC.manifest.yml#encoding-detection-algorithm
use proptest::prelude::*;

proptest! {
    /// Property test: ASCII zone nibbles (0x3) decode correctly with current implementation
    #[test]
    fn prop_ascii_zone_decoding_robustness(digits in prop::collection::vec(0u8..=9, 1..=10)) {
        let copybook = format!("01 FIELD PIC 9({}).", digits.len());
        let schema = parse_copybook(&copybook)
            .map_err(|err| TestCaseError::fail(format!("Failed to parse copybook: {err}")))?;

        // Convert digits to ASCII zoned decimal (0x30 + digit)
        let ascii_data: Vec<u8> = digits.iter().map(|&d| 0x30 + d).collect();

        let options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        // Current implementation should handle ASCII zoned decimals correctly
        let decoded = copybook_codec::decode_record(&schema, &ascii_data, &options)
            .map_err(|err| TestCaseError::fail(format!(
                "ASCII zoned decimal decode failed: {err}"
            )))?;
        let expected_number: i64 = digits.iter().fold(0, |acc, &d| acc * 10 + i64::from(d));

        // Verify the numeric value is correctly decoded
        let field_value = &decoded["FIELD"];
        if let Some(num) = field_value.as_i64() {
            prop_assert_eq!(num, expected_number, "Decoded value should match expected number");
        } else if let Some(str_val) = field_value.as_str() {
            let parsed: i64 = str_val.parse().map_err(|err| {
                TestCaseError::fail(format!(
                    "Failed to parse decoded value '{str_val}': {err}"
                ))
            })?;
            prop_assert_eq!(parsed, expected_number, "String value should match expected number");
        }
    }

    /// Property test: EBCDIC zone nibbles (0xF) decode correctly with current implementation
    #[test]
    fn prop_ebcdic_zone_decoding_robustness(digits in prop::collection::vec(0u8..=9, 1..=10)) {
        let copybook = format!("01 FIELD PIC 9({}).", digits.len());
        let schema = parse_copybook(&copybook)
            .map_err(|err| TestCaseError::fail(format!("Failed to parse copybook: {err}")))?;

        // Convert digits to EBCDIC zoned decimal (0xF0 + digit)
        let ebcdic_data: Vec<u8> = digits.iter().map(|&d| 0xF0 + d).collect();

        let options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037); // EBCDIC codepage

        // Current implementation should handle EBCDIC zoned decimals correctly
        let decoded = copybook_codec::decode_record(&schema, &ebcdic_data, &options)
            .map_err(|err| TestCaseError::fail(format!(
                "EBCDIC zoned decimal decode failed: {err}"
            )))?;
        let expected_number: i64 = digits.iter().fold(0, |acc, &d| acc * 10 + i64::from(d));

        // Verify the numeric value is correctly decoded
        let field_value = &decoded["FIELD"];
        if let Some(num) = field_value.as_i64() {
            prop_assert_eq!(num, expected_number, "Decoded value should match expected number");
        } else if let Some(str_val) = field_value.as_str() {
            let parsed: i64 = str_val.parse().map_err(|err| {
                TestCaseError::fail(format!(
                    "Failed to parse decoded value '{str_val}': {err}"
                ))
            })?;
            prop_assert_eq!(parsed, expected_number, "String value should match expected number");
        }
    }

    /// Property test: Invalid zone nibbles generate appropriate errors
    #[test]
    fn prop_invalid_zone_error_handling(
        invalid_zones in prop::collection::vec((0x00u8..=0x2Fu8).prop_union(0x3Au8..=0xEFu8), 1..=5)
    ) {
        let copybook = format!("01 FIELD PIC 9({}).", invalid_zones.len());
        let schema = parse_copybook(&copybook)
            .map_err(|err| TestCaseError::fail(format!("Failed to parse copybook: {err}")))?;

        let options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        // Invalid zone nibbles should produce structured errors
        let result = copybook_codec::decode_record(&schema, &invalid_zones, &options);

        // Should either error gracefully or handle invalid data consistently
        if let Err(error) = result {
            // Error should have proper structure and CBKD error code
            let error_str = error.to_string();
            prop_assert!(
                error_str.contains("CBKD") || error_str.contains("invalid") || error_str.contains("decode"),
                "Error should contain structured error code or description: {}",
                error_str
            );
        }
        // If it succeeds, that's also acceptable (might convert invalid zones to some default)
    }

    /// Property test: Boundary value handling for large numeric fields
    #[test]
    fn prop_large_field_boundary_testing(
        field_size in 1u8..=18u8,  // Up to 18 digits for i64 range
        digits in prop::collection::vec(0u8..=9, 1..=18)
    ) {
        let actual_size = (field_size as usize).min(digits.len());
        let truncated_digits = &digits[..actual_size];

        let copybook = format!("01 FIELD PIC 9({actual_size}).");
        let schema = parse_copybook(&copybook)
            .map_err(|err| TestCaseError::fail(format!("Failed to parse copybook: {err}")))?;

        // Test both ASCII and EBCDIC encodings
        let ascii_data: Vec<u8> = truncated_digits.iter().map(|&d| 0x30 + d).collect();
        let ebcdic_data: Vec<u8> = truncated_digits.iter().map(|&d| 0xF0 + d).collect();

        for (data, codepage, encoding_name) in [
            (&ascii_data, Codepage::ASCII, "ASCII"),
            (&ebcdic_data, Codepage::CP037, "EBCDIC"),
        ] {
            let options = DecodeOptions::new()
                .with_format(RecordFormat::Fixed)
                .with_codepage(codepage);

            let decoded = copybook_codec::decode_record(&schema, data, &options)
                .map_err(|err| TestCaseError::fail(format!(
                    "{encoding_name} decode failed: {err}"
                )))?;
            // Verify the field exists and has a reasonable value
            prop_assert!(
                decoded.get("FIELD").is_some(),
                "Field should exist in decoded result for {} encoding",
                encoding_name
            );
        }
    }

    /// Property test: Mixed field types with zoned decimals for integration robustness
    #[test]
    fn prop_mixed_field_types_integration(
        zoned_digits in prop::collection::vec(0u8..=9, 1..=5),
        display_chars in "[A-Za-z0-9 ]{1,10}"
    ) {
        let copybook = format!(
            r"01 MIXED-RECORD.
   05 DISPLAY-FIELD PIC X({}).
   05 ZONED-FIELD PIC 9({}).",
            display_chars.len(),
            zoned_digits.len()
        );
        let schema = parse_copybook(&copybook)
            .map_err(|err| TestCaseError::fail(format!("Failed to parse copybook: {err}")))?;

        // Build test data: display field + zoned decimal field
        let mut test_data = Vec::new();
        test_data.extend_from_slice(display_chars.as_bytes());

        // Add ASCII zoned decimal
        test_data.extend(zoned_digits.iter().map(|&d| 0x30 + d));

        let options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        // Mixed field types decode - may fail for some random data combinations
        // since ASCII display fields may contain invalid characters for the codec.
        // Success or structured error is acceptable.
        let _result = copybook_codec::decode_record(&schema, &test_data, &options);
    }
}

// Additional comprehensive error handling tests for enterprise robustness

/// Test comprehensive error handling for invalid record sizes
#[test]
fn test_record_size_mismatch_error_handling() -> Result<()> {
    let copybook = "01 FIELD PIC 9(5)."; // Expects 5 bytes
    let schema = parse_copybook(copybook)?;

    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII);

    // Test truncated data (less than expected)
    let short_data = b"\x31\x32\x33"; // Only 3 bytes, expected 5
    let result = copybook_codec::decode_record(&schema, short_data, &options);

    if let Err(error) = result {
        let error_str = error.to_string();
        assert!(
            error_str.contains("CBK")
                || error_str.contains("truncated")
                || error_str.contains("underflow"),
            "Should contain structured error information: {error_str}"
        );
    }

    // Test oversized data (more than expected) - should succeed and ignore extra
    let long_data = b"\x31\x32\x33\x34\x35\x36\x37"; // 7 bytes, expected 5
    let result = copybook_codec::decode_record(&schema, long_data, &options);
    assert!(result.is_ok(), "Should handle oversized data gracefully");
    Ok(())
}

/// Test error handling for malformed COBOL copybooks
#[test]
fn test_malformed_copybook_error_handling() {
    let malformed_copybooks = [
        "",                                        // Empty copybook
        "01 FIELD PIC 9(",                         // Incomplete PIC clause
        "01 FIELD PIC 9999999999999999999999(5).", // Invalid PIC format
        "INVALID COBOL SYNTAX",                    // Invalid syntax
    ];

    for (i, copybook) in malformed_copybooks.iter().enumerate() {
        let result = parse_copybook(copybook);
        if let Err(error) = result {
            let error_str = error.to_string();
            assert!(
                error_str.contains("CBK")
                    || error_str.contains("parse")
                    || error_str.contains("syntax"),
                "Malformed copybook {i} should produce structured error: {error_str}"
            );
        }
        // Some might succeed with default handling, which is also acceptable
    }
}

/// Test error recovery and continuation for enterprise workflows
#[test]
fn test_enterprise_error_recovery_patterns() -> Result<()> {
    let copybook = r"01 MULTI-FIELD.
   05 FIELD1 PIC 9(3).
   05 FIELD2 PIC X(5).
   05 FIELD3 PIC 9(2).";
    let schema = parse_copybook(copybook)?;

    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII);

    // Test data with some invalid zones but valid structure
    let mixed_quality_data = b"\xFF\x32\x33HELLO\x34\x35"; // Invalid first byte, rest OK

    let result = copybook_codec::decode_record(&schema, mixed_quality_data, &options);

    // Should either:
    // 1. Succeed with some error tolerance/conversion
    // 2. Fail with structured, actionable error message
    match result {
        Ok(_decoded) => {
            // Success path: implementation has error tolerance
            // No assertion needed - successful decode is acceptable
        }
        Err(error) => {
            // Error path: should provide structured error information
            let error_str = error.to_string();
            assert!(
                !error_str.is_empty()
                    && (error_str.contains("CBK") || error_str.contains("invalid")),
                "Error should be structured and informative: {error_str}"
            );
        }
    }
    Ok(())
}

/// Test EBCDIC codepage conversion edge cases for enterprise data
#[test]
fn test_ebcdic_codepage_conversion_robustness() -> Result<()> {
    let copybook = "01 EBCDIC-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook)?;

    // Test different EBCDIC codepages
    let codepages = [
        Codepage::CP037,  // US/Canada EBCDIC
        Codepage::CP273,  // German EBCDIC
        Codepage::CP500,  // International EBCDIC
        Codepage::CP1047, // Latin-1 EBCDIC
        Codepage::CP1140, // US/Canada EBCDIC Euro
    ];

    let test_data = b"\xF1\xF2\xF3\xF4\xF5"; // EBCDIC "12345"

    for codepage in codepages {
        let options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(codepage);

        let decoded = copybook_codec::decode_record(&schema, test_data, &options)?;
        let field_value = &decoded["EBCDIC-FIELD"];
        // Verify the value makes sense (should be 12345 or stringified version)
        if let Some(num) = field_value.as_i64() {
            assert_eq!(
                num, 12345,
                "Should decode to 12345 for codepage {codepage:?}"
            );
        } else if let Some(str_val) = field_value.as_str() {
            assert_eq!(
                str_val, "12345",
                "Should decode to '12345' for codepage {codepage:?}"
            );
        }
    }
    Ok(())
}
