// SPDX-License-Identifier: AGPL-3.0-or-later
//! Scale testing with large schemas.
//!
//! Validates that the parser and codec handle copybooks with 100+ fields
//! correctly, and that all fields appear in both the schema and decoded JSON.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
    encode_record,
};
use copybook_core::parse_copybook;

fn decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

fn encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

/// Generate a copybook with N PIC X(5) fields.
fn gen_alphanum_copybook(n: usize) -> String {
    let mut cpy = String::from("       01 LARGE-REC.\n");
    for i in 1..=n {
        cpy.push_str(&format!("          05 FIELD-{i:04} PIC X(5).\n"));
    }
    cpy
}

/// Generate a copybook with N numeric PIC 9(4) fields.
fn gen_numeric_copybook(n: usize) -> String {
    let mut cpy = String::from("       01 NUMERIC-REC.\n");
    for i in 1..=n {
        cpy.push_str(&format!("          05 NUM-{i:04} PIC 9(4).\n"));
    }
    cpy
}

/// Generate a mixed-type copybook with groups.
fn gen_mixed_copybook(n_groups: usize, fields_per_group: usize) -> String {
    let mut cpy = String::from("       01 MIXED-REC.\n");
    for g in 1..=n_groups {
        cpy.push_str(&format!("          05 GRP-{g:03}.\n"));
        for f in 1..=fields_per_group {
            if f % 2 == 0 {
                cpy.push_str(&format!("             10 GF-{g:03}-{f:03} PIC 9(4).\n"));
            } else {
                cpy.push_str(&format!("             10 GF-{g:03}-{f:03} PIC X(6).\n"));
            }
        }
    }
    cpy
}

// ---------------------------------------------------------------------------
// 1. Parse 100-field copybook
// ---------------------------------------------------------------------------

#[test]
fn large_schema_parse_100_fields() {
    let cpy = gen_alphanum_copybook(100);
    let schema = parse_copybook(&cpy).expect("parse 100-field copybook");

    let all_fields = schema.all_fields();
    // 100 leaf fields + 1 root group = at least 101
    assert!(
        all_fields.len() >= 101,
        "Expected at least 101 fields (100 leaves + root), got {}",
        all_fields.len()
    );
}

// ---------------------------------------------------------------------------
// 2. Parse 200-field copybook
// ---------------------------------------------------------------------------

#[test]
fn large_schema_parse_200_fields() {
    let cpy = gen_alphanum_copybook(200);
    let schema = parse_copybook(&cpy).expect("parse 200-field copybook");

    let all_fields = schema.all_fields();
    assert!(
        all_fields.len() >= 201,
        "Expected at least 201 fields, got {}",
        all_fields.len()
    );
}

// ---------------------------------------------------------------------------
// 3. Decode 100-field record – all keys present
// ---------------------------------------------------------------------------

#[test]
fn large_schema_decode_100_fields_all_keys_present() {
    let n = 100;
    let cpy = gen_alphanum_copybook(n);
    let schema = parse_copybook(&cpy).expect("parse");

    // Each field is PIC X(5) = 5 bytes, total = 500 bytes
    // Fill with EBCDIC "ABCDE" (CP037: A=C1 B=C2 C=C3 D=C4 E=C5)
    let data: Vec<u8> = vec![0xC1, 0xC2, 0xC3, 0xC4, 0xC5]
        .into_iter()
        .cycle()
        .take(n * 5)
        .collect();

    let json = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert!(json.is_object(), "Decoded value must be a JSON object");

    for i in 1..=n {
        let field_name = format!("FIELD-{i:04}");
        assert!(
            json.get(&field_name).is_some(),
            "Field '{field_name}' must be present in decoded JSON"
        );
        assert_eq!(
            json[&field_name], "ABCDE",
            "Field '{field_name}' must decode to 'ABCDE'"
        );
    }
}

// ---------------------------------------------------------------------------
// 4. Decode numeric 100-field record
// ---------------------------------------------------------------------------

#[test]
fn large_schema_decode_100_numeric_fields() {
    let n = 100;
    let cpy = gen_numeric_copybook(n);
    let schema = parse_copybook(&cpy).expect("parse");

    // Each PIC 9(4) = 4 bytes EBCDIC, "1234" = F1 F2 F3 F4
    let data: Vec<u8> = vec![0xF1, 0xF2, 0xF3, 0xF4]
        .into_iter()
        .cycle()
        .take(n * 4)
        .collect();

    let json = decode_record(&schema, &data, &decode_opts()).expect("decode");

    for i in 1..=n {
        let field_name = format!("NUM-{i:04}");
        assert!(
            json.get(&field_name).is_some(),
            "Numeric field '{field_name}' must be present"
        );
    }
}

// ---------------------------------------------------------------------------
// 5. Round-trip 100-field record
// ---------------------------------------------------------------------------

#[test]
fn large_schema_roundtrip_100_fields() {
    let n = 100;
    let cpy = gen_alphanum_copybook(n);
    let schema = parse_copybook(&cpy).expect("parse");

    let data: Vec<u8> = vec![0xC1, 0xC2, 0xC3, 0xC4, 0xC5]
        .into_iter()
        .cycle()
        .take(n * 5)
        .collect();

    let json = decode_record(&schema, &data, &decode_opts()).expect("decode");
    let re_encoded = encode_record(&schema, &json, &encode_opts()).expect("encode");

    assert_eq!(
        data, re_encoded,
        "100-field round-trip must be binary-exact"
    );
}

// ---------------------------------------------------------------------------
// 6. Mixed-type large schema (20 groups × 5 fields = 100 leaf fields)
// ---------------------------------------------------------------------------

#[test]
fn large_schema_mixed_groups_100_leaves() {
    let cpy = gen_mixed_copybook(20, 5);
    let schema = parse_copybook(&cpy).expect("parse");

    let all_fields = schema.all_fields();
    // 20 groups + 100 leaves + 1 root = 121 total
    assert!(
        all_fields.len() >= 121,
        "Expected at least 121 fields in mixed schema, got {}",
        all_fields.len()
    );
}

// ---------------------------------------------------------------------------
// 7. Decode mixed-type large schema
// ---------------------------------------------------------------------------

#[test]
fn large_schema_decode_mixed_groups() {
    let n_groups = 10;
    let fields_per_group = 4;
    let cpy = gen_mixed_copybook(n_groups, fields_per_group);
    let schema = parse_copybook(&cpy).expect("parse");

    // Compute record length: each group has alternating PIC X(6) and PIC 9(4)
    // odd fields: 6 bytes, even fields: 4 bytes
    // per group: 2 odd (6 bytes each) + 2 even (4 bytes each) = 20 bytes
    let bytes_per_group: usize = (1..=fields_per_group)
        .map(|f| if f % 2 == 0 { 4 } else { 6 })
        .sum();
    let total_len = n_groups * bytes_per_group;

    // Fill with EBCDIC spaces and digits
    let mut data: Vec<u8> = Vec::with_capacity(total_len);
    for _ in 0..n_groups {
        for f in 1..=fields_per_group {
            if f % 2 == 0 {
                // PIC 9(4) → "0001"
                data.extend_from_slice(&[0xF0, 0xF0, 0xF0, 0xF1]);
            } else {
                // PIC X(6) → "TESTAB"
                data.extend_from_slice(&[0xE3, 0xC5, 0xE2, 0xE3, 0xC1, 0xC2]);
            }
        }
    }

    let json = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert!(
        json.is_object(),
        "Decoded mixed schema must be a JSON object"
    );
}

// ---------------------------------------------------------------------------
// 8. Schema LRECL correct for 100 PIC X(5) fields
// ---------------------------------------------------------------------------

#[test]
fn large_schema_lrecl_correct() {
    let n = 100;
    let cpy = gen_alphanum_copybook(n);
    let schema = parse_copybook(&cpy).expect("parse");

    assert_eq!(
        schema.lrecl_fixed,
        Some(500),
        "100 × PIC X(5) = 500 bytes LRECL"
    );
}

// ---------------------------------------------------------------------------
// 9. Parse 500-field copybook
// ---------------------------------------------------------------------------

#[test]
fn large_schema_parse_500_fields() {
    let cpy = gen_alphanum_copybook(500);
    let schema = parse_copybook(&cpy).expect("parse 500-field copybook");

    let all_fields = schema.all_fields();
    assert!(
        all_fields.len() >= 501,
        "Expected at least 501 fields for 500-leaf schema, got {}",
        all_fields.len()
    );
    assert_eq!(
        schema.lrecl_fixed,
        Some(2500),
        "500 × PIC X(5) = 2500 bytes LRECL"
    );
}

// ---------------------------------------------------------------------------
// 10. Encode large JSON → 100 fields
// ---------------------------------------------------------------------------

#[test]
fn large_schema_encode_100_fields() {
    let n = 100;
    let cpy = gen_alphanum_copybook(n);
    let schema = parse_copybook(&cpy).expect("parse");

    let mut json_obj = serde_json::Map::new();
    for i in 1..=n {
        let field_name = format!("FIELD-{i:04}");
        json_obj.insert(field_name, serde_json::Value::String("XXXXX".to_string()));
    }
    let json = serde_json::Value::Object(json_obj);

    let encoded = encode_record(&schema, &json, &encode_opts()).expect("encode");
    assert_eq!(
        encoded.len(),
        500,
        "Encoded output must be 500 bytes for 100 × PIC X(5)"
    );
}

// ---------------------------------------------------------------------------
// 11. Decode and re-encode large schema are deterministic
// ---------------------------------------------------------------------------

#[test]
fn large_schema_deterministic_decode() {
    let n = 100;
    let cpy = gen_alphanum_copybook(n);
    let schema = parse_copybook(&cpy).expect("parse");

    let data: Vec<u8> = (0..n)
        .flat_map(|i| {
            let byte = 0xC1 + (i as u8 % 9); // A-I in EBCDIC
            vec![byte; 5]
        })
        .collect();

    let json1 = decode_record(&schema, &data, &decode_opts()).expect("decode 1");
    let json2 = decode_record(&schema, &data, &decode_opts()).expect("decode 2");
    assert_eq!(json1, json2, "Large schema decode must be deterministic");
}

// ---------------------------------------------------------------------------
// 12. Large schema with COMP-3 fields
// ---------------------------------------------------------------------------

#[test]
fn large_schema_with_comp3_fields() {
    let mut cpy = String::from("       01 COMP3-LARGE-REC.\n");
    let n = 50;
    for i in 1..=n {
        cpy.push_str(&format!("          05 PKD-{i:04} PIC S9(5) COMP-3.\n"));
    }

    let schema = parse_copybook(&cpy).expect("parse");

    // PIC S9(5) COMP-3 = 3 bytes each
    // Fill with +12345 COMP-3 = 0x01 0x23 0x45 0x0C → wait, S9(5) = 3 bytes
    // Actually: 5 digits + sign = 6 nibbles = 3 bytes: 0x12 0x34 0x5C
    let mut data: Vec<u8> = Vec::new();
    for _ in 0..n {
        data.extend_from_slice(&[0x12, 0x34, 0x5C]); // +12345
    }

    let json = decode_record(&schema, &data, &decode_opts()).expect("decode");
    for i in 1..=n {
        let field_name = format!("PKD-{i:04}");
        assert!(
            json.get(&field_name).is_some(),
            "COMP-3 field '{field_name}' must be present"
        );
    }
}

// ---------------------------------------------------------------------------
// 13. Unique field values preserved in large schema
// ---------------------------------------------------------------------------

#[test]
fn large_schema_unique_values_preserved() {
    let n = 50;
    let cpy = gen_numeric_copybook(n);
    let schema = parse_copybook(&cpy).expect("parse");

    // Each field gets a unique 4-digit value
    let mut data: Vec<u8> = Vec::new();
    for i in 0..n {
        let val = format!("{:04}", i + 1);
        for ch in val.chars() {
            data.push(0xF0 + ch.to_digit(10).unwrap() as u8);
        }
    }

    let json = decode_record(&schema, &data, &decode_opts()).expect("decode");

    for i in 1..=n {
        let field_name = format!("NUM-{i:04}");
        let expected = format!("{i:04}");
        assert_eq!(
            json[&field_name], expected,
            "Field '{field_name}' should have unique value '{expected}'"
        );
    }
}

// ---------------------------------------------------------------------------
// 14. Large schema JSON output is well-formed
// ---------------------------------------------------------------------------

#[test]
fn large_schema_json_well_formed() {
    let n = 100;
    let cpy = gen_alphanum_copybook(n);
    let schema = parse_copybook(&cpy).expect("parse");
    let data: Vec<u8> = vec![0x40; n * 5]; // all spaces

    let json = decode_record(&schema, &data, &decode_opts()).expect("decode");

    // Serialize to string and re-parse to verify well-formedness
    let json_str = serde_json::to_string(&json).expect("serialize");
    let reparsed: serde_json::Value = serde_json::from_str(&json_str).expect("reparse");
    assert_eq!(json, reparsed, "JSON must survive serialization round-trip");
}

// ---------------------------------------------------------------------------
// 15. Large schema with groups and nested structure
// ---------------------------------------------------------------------------

#[test]
fn large_schema_deeply_nested_groups() {
    let mut cpy = String::from("       01 DEEP-REC.\n");
    // 10 groups, each with 10 leaf fields = 100 leaves
    for g in 1..=10 {
        cpy.push_str(&format!("          05 SEC-{g:02}.\n"));
        for f in 1..=10 {
            cpy.push_str(&format!("             10 LEAF-{g:02}-{f:02} PIC X(3).\n"));
        }
    }

    let schema = parse_copybook(&cpy).expect("parse");
    let all_fields = schema.all_fields();

    // 1 root + 10 groups + 100 leaves = 111
    assert!(
        all_fields.len() >= 111,
        "Expected at least 111 fields, got {}",
        all_fields.len()
    );

    // Decode: 100 leaves × 3 bytes = 300 bytes
    let data: Vec<u8> = vec![0xC1, 0xC2, 0xC3]
        .into_iter()
        .cycle()
        .take(300)
        .collect();

    let json = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert!(json.is_object());
}
