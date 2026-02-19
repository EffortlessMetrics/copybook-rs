// SPDX-License-Identifier: AGPL-3.0-or-later
//! Tests for specific error codes in the codec layer
//!
//! This module tests error conditions that are difficult to trigger through
//! normal parsing but are important for robustness.

use copybook_codec::record::FixedRecordReader;
use copybook_codec::{Codepage, EncodeOptions, encode_record};
use copybook_core::ErrorCode;
use copybook_core::schema::{Field, FieldKind, Schema};
use serde_json::json;
use std::io::Cursor;

/// Test CBKI001_INVALID_STATE: FixedRecordReader requires LRECL
#[test]
fn test_cbki001_fixed_reader_requires_lrecl() {
    let data = Cursor::new(b"test data");

    // Attempt to create reader without LRECL
    let result = FixedRecordReader::new(data, None);

    assert!(result.is_err(), "Expected CBKI001 error for missing LRECL");
    if let Err(err) = result {
        assert_eq!(
            err.code,
            ErrorCode::CBKI001_INVALID_STATE,
            "Expected CBKI001_INVALID_STATE"
        );
        assert!(
            err.message.contains("LRECL"),
            "Error should mention LRECL requirement"
        );
    }
}

/// Test CBKI001_INVALID_STATE: FixedRecordReader requires non-zero LRECL
#[test]
fn test_cbki001_fixed_reader_requires_nonzero_lrecl() {
    let data = Cursor::new(b"test data");

    // Attempt to create reader with zero LRECL
    let result = FixedRecordReader::new(data, Some(0));

    assert!(result.is_err(), "Expected CBKI001 error for zero LRECL");
    if let Err(err) = result {
        assert_eq!(
            err.code,
            ErrorCode::CBKI001_INVALID_STATE,
            "Expected CBKI001_INVALID_STATE"
        );
        assert!(
            err.message.contains("greater than zero"),
            "Error should mention LRECL must be greater than zero"
        );
    }
}

/// Test CBKD101_INVALID_FIELD_TYPE: RENAMES field without resolved metadata
///
/// This tests the codec path that handles RENAMES fields missing their
/// resolved metadata (offset, length, members). This is a synthetic error
/// condition since the parser normally resolves all RENAMES.
#[test]
fn test_cbkd101_renames_without_resolved_metadata() {
    use copybook_codec::Codepage;
    use copybook_codec::{DecodeOptions, decode_record};
    use copybook_core::schema::{Field, FieldKind, Schema};

    // Create a synthetic schema with RENAMES but no resolved_renames
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut field1 = Field::new(5, "FIELD1".to_string());
    field1.path = "ROOT.FIELD1".to_string();
    field1.kind = FieldKind::Alphanum { len: 10 };
    field1.len = 10;
    field1.offset = 0;

    // Create a RENAMES field without resolved_renames metadata
    let mut renames = Field::new(66, "ALIAS".to_string());
    renames.path = "ROOT.ALIAS".to_string();
    renames.level = 66;
    renames.kind = FieldKind::Renames {
        from_field: "FIELD1".to_string(),
        thru_field: "FIELD1".to_string(),
    };
    // Intentionally NOT setting resolved_renames
    renames.resolved_renames = None;
    renames.offset = 0;
    renames.len = 10;

    root.children = vec![field1, renames];

    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(10);

    // Create test data
    let data = b"TEST DATA!";

    let options = DecodeOptions::new().with_codepage(Codepage::CP037);

    // Attempt to decode - should fail with CBKD101
    let result = decode_record(&schema, data, &options);

    assert!(
        result.is_err(),
        "Expected CBKD101 error for unresolved RENAMES"
    );
    if let Err(err) = result {
        assert_eq!(
            err.code,
            ErrorCode::CBKD101_INVALID_FIELD_TYPE,
            "Expected CBKD101_INVALID_FIELD_TYPE"
        );
        assert!(
            err.message.contains("ALIAS") || err.message.contains("RENAMES"),
            "Error should mention the RENAMES field"
        );
    }
}

/// Test CBKE515_STRING_LENGTH_VIOLATION: String exceeds field capacity
#[test]
fn test_cbke515_string_length_exceeds_capacity() {
    // Create a simple schema with a 5-byte alphanumeric field
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut field = Field::new(5, "SHORT-FIELD".to_string());
    field.path = "ROOT.SHORT-FIELD".to_string();
    field.kind = FieldKind::Alphanum { len: 5 };
    field.len = 5;
    field.offset = 0;

    root.children = vec![field];

    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(5);

    // Try to encode a string that's too long
    let json_value = json!({
        "ROOT": {
            "SHORT-FIELD": "THIS STRING IS WAY TOO LONG"
        }
    });

    let options = EncodeOptions::new().with_codepage(Codepage::CP037);

    let result = encode_record(&schema, &json_value, &options);

    assert!(
        result.is_err(),
        "Expected CBKE515 error for string too long"
    );
    if let Err(err) = result {
        assert_eq!(
            err.code,
            ErrorCode::CBKE515_STRING_LENGTH_VIOLATION,
            "Expected CBKE515_STRING_LENGTH_VIOLATION, got {:?}",
            err.code
        );
        assert!(
            err.message.contains("String length") && err.message.contains("exceeds"),
            "Error should mention string length exceeds capacity: {}",
            err.message
        );
    }
}

/// Test CBKE510_NUMERIC_OVERFLOW: Zoned decimal value too large
#[test]
fn test_cbke510_zoned_decimal_overflow() {
    // Create a schema with a 3-digit zoned decimal field
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut field = Field::new(5, "SMALL-NUM".to_string());
    field.path = "ROOT.SMALL-NUM".to_string();
    field.kind = FieldKind::ZonedDecimal {
        digits: 3,
        scale: 0,
        signed: false,
        sign_separate: None,
    };
    field.len = 3;
    field.offset = 0;

    root.children = vec![field];

    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(3);

    // Try to encode a value that's too large for 3 digits
    let json_value = json!({
        "ROOT": {
            "SMALL-NUM": "99999"  // 5 digits, but only 3 allowed
        }
    });

    let options = EncodeOptions::new().with_codepage(Codepage::CP037);

    let result = encode_record(&schema, &json_value, &options);

    assert!(
        result.is_err(),
        "Expected CBKE510 error for numeric overflow"
    );
    if let Err(err) = result {
        assert_eq!(
            err.code,
            ErrorCode::CBKE510_NUMERIC_OVERFLOW,
            "Expected CBKE510_NUMERIC_OVERFLOW, got {:?}",
            err.code
        );
        assert!(
            err.message.contains("too large") || err.message.contains("overflow"),
            "Error should mention value too large: {}",
            err.message
        );
    }
}

/// Test CBKE510_NUMERIC_OVERFLOW: Packed decimal value too large
#[test]
fn test_cbke510_packed_decimal_overflow() {
    // Create a schema with a 4-digit packed decimal field (3 bytes)
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut field = Field::new(5, "SMALL-COMP3".to_string());
    field.path = "ROOT.SMALL-COMP3".to_string();
    field.kind = FieldKind::PackedDecimal {
        digits: 4,
        scale: 0,
        signed: true,
    };
    field.len = 3; // (4+1)/2 = 2.5 -> 3 bytes
    field.offset = 0;

    root.children = vec![field];

    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(3);

    // Try to encode a value that's too large for 4 digits
    let json_value = json!({
        "ROOT": {
            "SMALL-COMP3": "999999"  // 6 digits, but only 4 allowed
        }
    });

    let options = EncodeOptions::new().with_codepage(Codepage::CP037);

    let result = encode_record(&schema, &json_value, &options);

    assert!(
        result.is_err(),
        "Expected CBKE510 error for packed decimal overflow"
    );
    if let Err(err) = result {
        assert_eq!(
            err.code,
            ErrorCode::CBKE510_NUMERIC_OVERFLOW,
            "Expected CBKE510_NUMERIC_OVERFLOW, got {:?}",
            err.code
        );
        assert!(
            err.message.contains("too large") || err.message.contains("overflow"),
            "Error should mention value too large: {}",
            err.message
        );
    }
}
