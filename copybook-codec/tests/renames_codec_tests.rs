//! RENAMES codec projection tests (Slice-2 for Issue #110)
//!
//! Tests decode/encode functionality for RENAMES (level-66) fields
//! using resolved alias metadata from the parser.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_codec::{Codepage, DecodeOptions, EncodeOptions, decode_record, encode_record};
use copybook_core::parse_copybook;
use serde_json::Value;

/// Test R1: Simple same-scope RENAMES decode
#[test]
fn test_renames_r1_simple_decode() {
    let copybook = r"
01 RECORD-A.
   05 FIELD-1 PIC X(10).
   05 FIELD-2 PIC 9(5).
   05 FIELD-3 PIC X(2).
   66 ALIAS-A RENAMES FIELD-1 THRU FIELD-3.
";

    let schema = parse_copybook(copybook).expect("parse ok");

    // Create test data: "HELLO     12345XY"
    let mut data = vec![0x40u8; 17]; // EBCDIC spaces
    data[0..5].copy_from_slice(b"HELLO"); // ASCII for simplicity in test
    data[10..15].copy_from_slice(b"12345");
    data[15..17].copy_from_slice(b"XY");

    let options = DecodeOptions::new().with_codepage(Codepage::ASCII);
    let result = decode_record(&schema, &data, &options).expect("decode ok");

    // Verify the RENAMES field is decoded
    let obj = result.as_object().expect("result is object");

    // Check that ALIAS-A field exists and contains the aliased data
    let alias_value = obj.get("ALIAS-A").expect("ALIAS-A field exists");
    let alias_str = alias_value.as_str().expect("ALIAS-A is string");

    // Should contain the concatenated data from FIELD-1, FIELD-2, FIELD-3
    assert_eq!(alias_str.len(), 17); // 10 + 5 + 2
    assert!(alias_str.starts_with("HELLO"));
}

/// Test R1: Single field RENAMES decode
#[test]
fn test_renames_r1_single_field_decode() {
    let copybook = r"
01 RECORD-A.
   05 ITEM PIC 9(4).
   66 ONLY RENAMES ITEM THRU ITEM.
";

    let schema = parse_copybook(copybook).expect("parse ok");

    // Create test data: "1234"
    let data = b"1234";

    let options = DecodeOptions::new().with_codepage(Codepage::ASCII);
    let result = decode_record(&schema, data, &options).expect("decode ok");

    let obj = result.as_object().expect("result is object");
    let alias_value = obj.get("ONLY").expect("ONLY field exists");
    let alias_str = alias_value.as_str().expect("ONLY is string");

    assert_eq!(alias_str, "1234");
}

/// Test R1: RENAMES with missing resolved metadata (should error)
#[test]
fn test_renames_missing_metadata_error() {
    let copybook = r"
01 RECORD-A.
   05 FIELD-1 PIC X(10).
   66 ALIAS-A RENAMES FIELD-1 THRU FIELD-1.
";

    let mut schema = parse_copybook(copybook).expect("parse ok");

    // Manually clear resolved_renames to simulate missing metadata
    for field in &mut schema.fields[0].children {
        if field.level == 66 {
            field.resolved_renames = None;
        }
    }

    let data = b"HELLO     ";
    let options = DecodeOptions::new().with_codepage(Codepage::ASCII);

    let result = decode_record(&schema, data, &options);

    // Should get error about missing resolved metadata
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("no resolved metadata"));
}

/// Test R1: RENAMES with data truncation (should error)
#[test]
fn test_renames_data_truncation_error() {
    let copybook = r"
01 RECORD-A.
   05 FIELD-1 PIC X(10).
   05 FIELD-2 PIC X(10).
   66 ALIAS-A RENAMES FIELD-1 THRU FIELD-2.
";

    let schema = parse_copybook(copybook).expect("parse ok");

    // Create truncated data (only 10 bytes instead of 20)
    let data = b"HELLO     ";

    let options = DecodeOptions::new().with_codepage(Codepage::ASCII);
    let result = decode_record(&schema, &data[..], &options);

    // Should get error about record too short
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("exceeds data length"));
}

/// Test encode with RENAMES fields (should skip encoding)
#[test]
fn test_renames_encode_skips_alias() {
    let copybook = r"
01 RECORD-A.
   05 FIELD-1 PIC X(5).
   05 FIELD-2 PIC X(5).
   66 ALIAS-A RENAMES FIELD-1 THRU FIELD-2.
";

    let schema = parse_copybook(copybook).expect("parse ok");

    // Create JSON with only storage fields (not the alias)
    let mut fields = serde_json::Map::new();
    fields.insert("FIELD-1".to_string(), Value::String("HELLO".to_string()));
    fields.insert("FIELD-2".to_string(), Value::String("WORLD".to_string()));

    let json = Value::Object(fields);

    let options = EncodeOptions::default().with_codepage(Codepage::ASCII);
    let result = encode_record(&schema, &json, &options).expect("encode ok");

    // Should produce 10 bytes (5 + 5)
    assert_eq!(result.len(), 10);

    // Verify the content
    let decoded_text = String::from_utf8_lossy(&result);
    assert!(decoded_text.starts_with("HELLO"));
    assert!(decoded_text.contains("WORLD"));
}

/// Test R2: Group RENAMES decode (multi-member, same scope)
#[test]
fn test_renames_r2_group_decode() {
    let copybook = r"
01 RECORD-A.
   05 CUST-ID PIC 9(5).
   05 CUST-NAME PIC X(20).
   66 CUSTOMER-HEADER RENAMES CUST-ID THRU CUST-NAME.
";

    let schema = parse_copybook(copybook).expect("parse ok");

    // Create test data: "12345" + "JOHN DOE            "
    let mut data = vec![b'0'; 25];
    data[0..5].copy_from_slice(b"12345");
    data[5..13].copy_from_slice(b"JOHN DOE");
    data[13..25].fill(b' ');

    let options = DecodeOptions::new().with_codepage(Codepage::ASCII);
    let result = decode_record(&schema, &data, &options).expect("decode ok");

    let obj = result.as_object().expect("result is object");

    // Check that CUSTOMER-HEADER field exists
    let header_value = obj
        .get("CUSTOMER-HEADER")
        .expect("CUSTOMER-HEADER field exists");
    let header_str = header_value.as_str().expect("CUSTOMER-HEADER is string");

    // Should contain the full 25 bytes
    assert_eq!(header_str.len(), 25);
}

/// Test EBCDIC decode with RENAMES
#[test]
fn test_renames_ebcdic_decode() {
    let copybook = r"
01 RECORD-A.
   05 FIELD-1 PIC X(5).
   05 FIELD-2 PIC X(5).
   66 ALIAS-A RENAMES FIELD-1 THRU FIELD-2.
";

    let schema = parse_copybook(copybook).expect("parse ok");

    // EBCDIC for "HELLO" = C8 C5 D3 D3 D6
    // EBCDIC for "WORLD" = E6 D6 D9 D3 C4
    let data = vec![
        0xC8, 0xC5, 0xD3, 0xD3, 0xD6, // HELLO
        0xE6, 0xD6, 0xD9, 0xD3, 0xC4, // WORLD
    ];

    let options = DecodeOptions::new().with_codepage(Codepage::CP037);
    let result = decode_record(&schema, &data, &options).expect("decode ok");

    let obj = result.as_object().expect("result is object");
    let alias_value = obj.get("ALIAS-A").expect("ALIAS-A field exists");
    let alias_str = alias_value.as_str().expect("ALIAS-A is string");

    // Should decode to "HELLOWORLD"
    assert_eq!(alias_str, "HELLOWORLD");
}
