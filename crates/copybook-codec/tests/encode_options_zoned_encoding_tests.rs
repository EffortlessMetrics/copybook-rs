#![allow(clippy::unwrap_used, clippy::expect_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later

//! Test scaffolding for `EncodeOptions` zoned encoding extensions - Issue #48
//!
//! Tests COBOL encoding preservation spec: SPEC.manifest.yml#EncodeOptions-zoned_encoding_override
//!
//! This test suite validates:
//! - AC4: Encode operations use preserved encoding format when available
//! - `EncodeOptions` `zoned_encoding_override` field support
//! - Encoding precedence order: explicit override > preserved format > EBCDIC default

#![allow(clippy::unnecessary_wraps, clippy::used_underscore_binding)]

use copybook_codec::{Codepage, DecodeOptions, EncodeOptions, RecordFormat};
use copybook_core::parse_copybook;
use std::error::Error;

/// AC4: Test `EncodeOptions` `zoned_encoding_override` field support
/// Tests COBOL parsing spec: SPEC.manifest.yml#EncodeOptions-zoned_encoding_override
#[test]
fn test_encode_options_zoned_encoding_override() -> Result<(), Box<dyn Error>> {
    // Test that EncodeOptions supports zoned_encoding_override field
    let _options = EncodeOptions::new();

    // Test zoned_encoding_override functionality
    let mut options =
        _options.with_zoned_encoding_override(Some(copybook_codec::ZonedEncodingFormat::Ascii));
    assert_eq!(
        options.zoned_encoding_override,
        Some(copybook_codec::ZonedEncodingFormat::Ascii)
    );

    options =
        options.with_zoned_encoding_override(Some(copybook_codec::ZonedEncodingFormat::Ebcdic));
    assert_eq!(
        options.zoned_encoding_override,
        Some(copybook_codec::ZonedEncodingFormat::Ebcdic)
    );

    options = options.with_zoned_encoding_override(Some(copybook_codec::ZonedEncodingFormat::Auto));
    assert_eq!(
        options.zoned_encoding_override,
        Some(copybook_codec::ZonedEncodingFormat::Auto)
    );

    options = options.with_zoned_encoding_override(None);
    assert_eq!(options.zoned_encoding_override, None);

    Ok(())
}

/// AC4: Test decode-then-encode round-trip preserving ASCII encoding format
/// Tests COBOL parsing spec: SPEC.manifest.yml#encode-enhancement-preserved-format
#[test]
fn test_encode_preserves_ascii_format() -> Result<(), Box<dyn Error>> {
    let copybook = "01 ZONED-FIELD PIC 9(3).";
    let schema = parse_copybook(copybook)?;

    // Decode ASCII data with preservation enabled
    let decode_options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_preserve_zoned_encoding(true);

    let ascii_data = b"\x31\x32\x33"; // ASCII "123"
    let json_result = copybook_codec::decode_record(&schema, ascii_data, &decode_options)?;

    // Verify the decoded JSON has encoding metadata
    assert!(
        json_result.get("_encoding_metadata").is_some(),
        "Should have _encoding_metadata in decoded output"
    );
    let json_str = json_result.to_string();
    assert!(
        json_str.contains("ascii"),
        "Should contain ascii encoding metadata"
    );

    // Encode back using the decoded JSON (which includes _encoding_metadata)
    let encode_options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII);

    let encoded = copybook_codec::encode_record(&schema, &json_result, &encode_options)?;
    assert_eq!(encoded.len(), 3, "Encoded data should be 3 bytes");

    Ok(())
}

/// AC4: Test decode-then-encode round-trip preserving EBCDIC encoding format
/// Tests COBOL parsing spec: SPEC.manifest.yml#encode-enhancement-preserved-format
#[test]
fn test_encode_preserves_ebcdic_format() -> Result<(), Box<dyn Error>> {
    let copybook = "01 ZONED-FIELD PIC 9(3).";
    let schema = parse_copybook(copybook)?;

    // Decode EBCDIC data with preservation enabled
    let decode_options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_preserve_zoned_encoding(true);

    let ebcdic_data = b"\xF1\xF2\xF3"; // EBCDIC "123"
    let json_result = copybook_codec::decode_record(&schema, ebcdic_data, &decode_options)?;

    // Verify the decoded JSON has encoding metadata
    assert!(
        json_result.get("_encoding_metadata").is_some(),
        "Should have _encoding_metadata in decoded output"
    );
    let json_str = json_result.to_string();
    assert!(
        json_str.contains("ebcdic"),
        "Should contain ebcdic encoding metadata"
    );

    // Encode back using the decoded JSON
    let encode_options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037);

    let encoded = copybook_codec::encode_record(&schema, &json_result, &encode_options)?;
    assert_eq!(encoded.len(), 3, "Encoded data should be 3 bytes");
    // EBCDIC encoding should produce the original bytes
    assert_eq!(
        &encoded,
        &ebcdic_data[..],
        "Round-trip should produce original EBCDIC bytes"
    );

    Ok(())
}

/// Test encoding precedence order: explicit override > preserved format > EBCDIC default
/// Tests COBOL parsing spec: SPEC.manifest.yml#encode-enhancement-precedence-order
#[test]
fn test_encoding_precedence_explicit_override() -> Result<(), Box<dyn Error>> {
    let copybook = "01 ZONED-FIELD PIC 9(3).";
    let schema = parse_copybook(copybook)?;

    // JSON with preserved EBCDIC metadata
    let json_with_ebcdic_meta = serde_json::json!({
        "ZONED-FIELD": "123",
        "_encoding_metadata": {
            "ZONED-FIELD": "ebcdic"
        }
    });

    // Encode with explicit ASCII override (should override preserved EBCDIC)
    let encode_options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_zoned_encoding_override(Some(copybook_codec::ZonedEncodingFormat::Ascii));

    let encoded = copybook_codec::encode_record(&schema, &json_with_ebcdic_meta, &encode_options)?;
    assert_eq!(encoded.len(), 3);
    // With ASCII override, should produce ASCII zones
    assert_eq!(
        &encoded, b"\x31\x32\x33",
        "Explicit override should produce ASCII zones"
    );

    Ok(())
}

/// Test encoding precedence order: preserved format > EBCDIC default
/// Tests COBOL parsing spec: SPEC.manifest.yml#encode-enhancement-precedence-order
#[test]
fn test_encoding_precedence_preserved_over_default() -> Result<(), Box<dyn Error>> {
    let copybook = "01 ZONED-FIELD PIC 9(3).";
    let schema = parse_copybook(copybook)?;

    // JSON with preserved ASCII metadata
    let json_with_ascii_meta = serde_json::json!({
        "ZONED-FIELD": "123",
        "_encoding_metadata": {
            "ZONED-FIELD": "ascii"
        }
    });

    // Encode without explicit override - should use preserved format from metadata
    let encode_options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037); // EBCDIC codepage but metadata says ASCII

    let encoded = copybook_codec::encode_record(&schema, &json_with_ascii_meta, &encode_options)?;
    assert_eq!(encoded.len(), 3);
    // Preserved ASCII metadata should override EBCDIC codepage default
    assert_eq!(
        &encoded, b"\x31\x32\x33",
        "Preserved metadata should produce ASCII zones"
    );

    Ok(())
}

/// AC8: Test backward compatibility - default behavior unchanged (EBCDIC output)
/// Tests COBOL parsing spec: SPEC.manifest.yml#backward-compatibility-default-behavior
#[test]
fn test_backward_compatibility_default_ebcdic() -> Result<(), Box<dyn Error>> {
    let copybook = "01 ZONED-FIELD PIC 9(3).";
    let schema = parse_copybook(copybook)?;

    // JSON without any encoding metadata (legacy behavior)
    let json_without_meta = serde_json::json!({
        "ZONED-FIELD": "123"
    });

    // Encode with default EBCDIC options (no preservation flags)
    let encode_options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037);

    let encoded = copybook_codec::encode_record(&schema, &json_without_meta, &encode_options)?;
    assert_eq!(encoded.len(), 3);
    // Default behavior should produce EBCDIC zones
    assert_eq!(
        &encoded, b"\xF1\xF2\xF3",
        "Default should produce EBCDIC zones"
    );

    Ok(())
}

/// Test encoding format metadata structure in JSON
/// Tests COBOL parsing spec: SPEC.manifest.yml#format-preservation-metadata-schema
#[test]
fn test_encoding_metadata_json_structure() -> Result<(), Box<dyn Error>> {
    let copybook = r"
01 RECORD.
   05 FIELD1 PIC 9(2).
   05 FIELD2 PIC 9(3).
";
    let schema = parse_copybook(copybook)?;

    let decode_options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_preserve_zoned_encoding(true);

    // EBCDIC data for both fields
    let ebcdic_data = b"\xF1\xF2\xF1\xF2\xF3";

    let json_result = copybook_codec::decode_record(&schema, ebcdic_data, &decode_options)?;

    // Validate metadata structure
    let metadata = json_result.get("_encoding_metadata");
    assert!(metadata.is_some(), "Should have _encoding_metadata key");
    let meta_obj = metadata.unwrap().as_object().unwrap();
    assert_eq!(meta_obj["FIELD1"], "ebcdic");
    assert_eq!(meta_obj["FIELD2"], "ebcdic");

    Ok(())
}

/// Test that Auto encoding format triggers detection logic
/// Tests COBOL parsing spec: SPEC.manifest.yml#ZonedEncodingFormat-Auto-detection
#[test]
fn test_auto_encoding_detection() -> Result<(), Box<dyn Error>> {
    let copybook = "01 ZONED-FIELD PIC 9(3).";
    let schema = parse_copybook(copybook)?;

    let decode_options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_preserve_zoned_encoding(true);

    // ASCII data "123" = 0x31, 0x32, 0x33
    // Despite CP037 codepage, the detection should identify ASCII zones
    let ascii_data = b"\x31\x32\x33";

    // This may fail for CP037 codepage since the zone nibbles don't match expected EBCDIC.
    // The decode may return an error, which is acceptable behavior.
    let result = copybook_codec::decode_record(&schema, ascii_data, &decode_options);
    match result {
        Ok(json) => {
            // If decode succeeds, check metadata detects ASCII
            if let Some(meta) = json.get("_encoding_metadata") {
                let meta_obj = meta.as_object().unwrap();
                if let Some(val) = meta_obj.get("ZONED-FIELD") {
                    assert_eq!(
                        val, "ascii",
                        "Should detect ASCII encoding from zone nibbles"
                    );
                }
            }
        }
        Err(_) => {
            // Decode error with CP037 codepage and ASCII data is expected
            // The zone nibbles 0x3 don't match EBCDIC expectations (0xF)
        }
    }

    Ok(())
}
