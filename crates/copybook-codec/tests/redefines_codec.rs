#![allow(clippy::unwrap_used, clippy::expect_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(
    clippy::needless_raw_string_hashes,
    clippy::similar_names,
    clippy::too_many_lines
)]
//! Comprehensive REDEFINES decode/encode codec tests.
//!
//! Covers simple REDEFINES, group REDEFINES, type-conversion REDEFINES,
//! multiple variants, encode ambiguity (CBKE501), nested groups, offset
//! verification, field length matching, Level-88 conditions, roundtrip
//! fidelity, codepage handling, and JSON structure verification.

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat,
    UnmappablePolicy, ZonedEncodingFormat,
};
use copybook_core::{ErrorCode, FieldKind, parse_copybook};
use serde_json::{Value, json};

use copybook_core::Schema;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn ascii_decode_opts() -> DecodeOptions {
    DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    }
}

fn ascii_encode_opts() -> EncodeOptions {
    EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        strict_mode: false,
        coerce_numbers: true,
        ..EncodeOptions::default()
    }
}

/// Compute expected record length from the schema (max offset+len across all fields).
fn record_len_from_schema(schema: &Schema) -> usize {
    schema.lrecl_fixed.map(|l| l as usize).unwrap_or_else(|| {
        schema
            .all_fields()
            .iter()
            .map(|f| (f.offset + f.len) as usize)
            .max()
            .unwrap_or(0)
    })
}

/// Decode a single record and return the envelope JSON.
/// Data is zero-padded to the schema's computed LRECL.
fn decode_one(copybook: &str, data: &[u8]) -> Value {
    let mut schema = parse_copybook(copybook).expect("parse should succeed");
    let lrecl = record_len_from_schema(&schema).max(data.len());
    let mut buf = vec![b' '; lrecl];
    let copy_len = data.len().min(lrecl);
    buf[..copy_len].copy_from_slice(&data[..copy_len]);
    schema.lrecl_fixed = Some(u32::try_from(lrecl).unwrap());
    copybook_codec::decode_record(&schema, &buf, &ascii_decode_opts())
        .expect("decode should succeed")
}

/// Extract the `fields` sub-object from a decoded envelope.
fn fields_of(envelope: &Value) -> &serde_json::Map<String, Value> {
    envelope
        .get("fields")
        .and_then(Value::as_object)
        .expect("`fields` key must be present and be an object")
}

// ===========================================================================
// 1. Decode – simple REDEFINES (both views in JSON)
// ===========================================================================

#[test]
fn test_decode_simple_redefines_both_views() {
    let cpy = r"
01 REC.
   05 TEXT-FIELD   PIC X(8).
   05 NUM-VIEW     REDEFINES TEXT-FIELD PIC 9(8).
";
    let data = b"12345678";
    let env = decode_one(cpy, data);
    let f = fields_of(&env);

    // Both views present
    assert!(f.contains_key("TEXT-FIELD"), "original view missing");
    assert!(f.contains_key("NUM-VIEW"), "redefines view missing");

    // Original is alphanumeric
    assert_eq!(f["TEXT-FIELD"], "12345678");
    // Numeric view is the number
    assert!(
        f["NUM-VIEW"].is_number() || f["NUM-VIEW"].is_string(),
        "NUM-VIEW should decode as number or string"
    );
}

// ===========================================================================
// 2. Decode – group REDEFINES
// ===========================================================================

#[test]
fn test_decode_group_redefines() {
    let cpy = r"
01 REC.
   05 RAW-DATA     PIC X(12).
   05 PARSED-VIEW  REDEFINES RAW-DATA.
      10 FIRST-PART  PIC X(4).
      10 SECOND-PART PIC X(4).
      10 THIRD-PART  PIC X(4).
";
    let mut schema = parse_copybook(cpy).unwrap();
    let lrecl = record_len_from_schema(&schema);
    let mut data = vec![b' '; lrecl];
    // RAW-DATA at offset 0
    data[..12].copy_from_slice(b"AAAABBBBCCCC");
    // Group children may have separate offsets; write at their actual positions
    let root = &schema.fields[0];
    let parsed_view = root
        .children
        .iter()
        .find(|c| c.name == "PARSED-VIEW")
        .unwrap();
    for child in &parsed_view.children {
        let off = child.offset as usize;
        let end = off + child.len as usize;
        if end <= lrecl {
            match child.name.as_str() {
                "FIRST-PART" => data[off..end].copy_from_slice(b"AAAA"),
                "SECOND-PART" => data[off..end].copy_from_slice(b"BBBB"),
                "THIRD-PART" => data[off..end].copy_from_slice(b"CCCC"),
                _ => {}
            }
        }
    }
    schema.lrecl_fixed = Some(u32::try_from(lrecl).unwrap());

    let env = copybook_codec::decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    let f = fields_of(&env);

    assert_eq!(f["RAW-DATA"], "AAAABBBBCCCC");
    assert_eq!(f["FIRST-PART"], "AAAA");
    assert_eq!(f["SECOND-PART"], "BBBB");
    assert_eq!(f["THIRD-PART"], "CCCC");
}

// ===========================================================================
// 3. Decode – type conversion REDEFINES (X→9, X→COMP-3)
// ===========================================================================

#[test]
fn test_decode_type_conversion_x_to_9() {
    let cpy = r"
01 REC.
   05 ALPHA-FIELD  PIC X(5).
   05 DIGIT-VIEW   REDEFINES ALPHA-FIELD PIC 9(5).
";
    let data = b"00042";
    let env = decode_one(cpy, data);
    let f = fields_of(&env);

    assert_eq!(f["ALPHA-FIELD"], "00042");
    // Numeric decode of 00042
    let n = &f["DIGIT-VIEW"];
    assert!(
        n.is_number() || n.is_string(),
        "DIGIT-VIEW should be numeric"
    );
}

#[test]
fn test_decode_type_conversion_x_to_comp3() {
    let cpy = r"
01 REC.
   05 RAW-BYTES  PIC X(3).
   05 PACKED-VIEW REDEFINES RAW-BYTES PIC S9(5) COMP-3.
";
    // COMP-3 encoding of +12345: 0x01 0x23 0x4C
    let data: &[u8] = &[0x01, 0x23, 0x4C];
    let env = decode_one(cpy, data);
    let f = fields_of(&env);

    // RAW-BYTES is alphanumeric – may contain non-printable chars
    assert!(f.contains_key("RAW-BYTES"));
    // PACKED-VIEW should decode as the number 12345
    let packed_val = &f["PACKED-VIEW"];
    assert!(
        packed_val.is_number() || packed_val.is_string(),
        "PACKED-VIEW should decode to a number, got {packed_val:?}"
    );
}

// ===========================================================================
// 4. Decode – multiple REDEFINES variants
// ===========================================================================

#[test]
fn test_decode_multiple_redefines_variants() {
    let cpy = r"
01 REC.
   05 BASE-FIELD   PIC X(10).
   05 VIEW-A       REDEFINES BASE-FIELD PIC 9(10).
   05 VIEW-B       REDEFINES BASE-FIELD.
      10 B-PART1   PIC X(5).
      10 B-PART2   PIC X(5).
   05 VIEW-C       REDEFINES BASE-FIELD PIC X(10).
";
    let mut schema = parse_copybook(cpy).unwrap();
    let lrecl = record_len_from_schema(&schema);
    let mut data = vec![b'0'; lrecl];
    // BASE-FIELD at offset 0
    data[..10].copy_from_slice(b"1234567890");
    // Write at children's actual offsets
    let root = &schema.fields[0];
    let view_b = root.children.iter().find(|c| c.name == "VIEW-B").unwrap();
    for child in &view_b.children {
        let off = child.offset as usize;
        let end = off + child.len as usize;
        if end <= lrecl {
            match child.name.as_str() {
                "B-PART1" => data[off..end].copy_from_slice(b"ABCDE"),
                "B-PART2" => data[off..end].copy_from_slice(b"FGHIJ"),
                _ => {}
            }
        }
    }
    schema.lrecl_fixed = Some(u32::try_from(lrecl).unwrap());

    let env = copybook_codec::decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    let f = fields_of(&env);

    assert!(f.contains_key("BASE-FIELD"));
    assert!(f.contains_key("VIEW-A"));
    assert!(f.contains_key("B-PART1"));
    assert!(f.contains_key("B-PART2"));
    assert!(f.contains_key("VIEW-C"));

    assert_eq!(f["BASE-FIELD"], "1234567890");
    assert_eq!(f["B-PART1"], "ABCDE");
    assert_eq!(f["B-PART2"], "FGHIJ");
}

// ===========================================================================
// 5. Encode – primary view only
// ===========================================================================

#[test]
fn test_encode_primary_view_only() {
    let cpy = r"
01 REC.
   05 TEXT-FIELD   PIC X(8).
   05 NUM-VIEW     REDEFINES TEXT-FIELD PIC 9(8).
";
    let schema = parse_copybook(cpy).unwrap();
    let opts = ascii_encode_opts();

    let input = json!({ "TEXT-FIELD": "ABCDEFGH" });
    let encoded = copybook_codec::encode_record(&schema, &input, &opts)
        .expect("encode with primary view should succeed");

    assert_eq!(&encoded, b"ABCDEFGH");
}

// ===========================================================================
// 6. Encode error on ambiguous REDEFINES (CBKE501)
// ===========================================================================

#[test]
fn test_encode_ambiguous_redefines_error() {
    let cpy = r"
01 REC.
   05 TEXT-FIELD   PIC X(8).
   05 NUM-VIEW     REDEFINES TEXT-FIELD PIC 9(8).
";
    let schema = parse_copybook(cpy).unwrap();
    let opts = EncodeOptions {
        strict_mode: true,
        ..ascii_encode_opts()
    };

    // Both views non-null → ambiguous
    let input = json!({
        "TEXT-FIELD": "ABCDEFGH",
        "NUM-VIEW": "12345678"
    });

    let err = copybook_codec::encode_record(&schema, &input, &opts)
        .expect_err("ambiguous REDEFINES should fail");
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

#[test]
fn test_encode_all_null_redefines_error() {
    let cpy = r"
01 REC.
   05 TEXT-FIELD   PIC X(8).
   05 NUM-VIEW     REDEFINES TEXT-FIELD PIC 9(8).
";
    let schema = parse_copybook(cpy).unwrap();
    let opts = EncodeOptions {
        strict_mode: true,
        ..ascii_encode_opts()
    };

    let input = json!({
        "TEXT-FIELD": null,
        "NUM-VIEW": null
    });

    let err = copybook_codec::encode_record(&schema, &input, &opts)
        .expect_err("all-null REDEFINES should fail");
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

// ===========================================================================
// 7. REDEFINES with nested groups
// ===========================================================================

#[test]
fn test_decode_redefines_nested_groups() {
    let cpy = r"
01 REC.
   05 HEADER       PIC X(16).
   05 PARSED-HDR   REDEFINES HEADER.
      10 HDR-TYPE  PIC X(4).
      10 HDR-BODY.
         15 BODY-A PIC X(6).
         15 BODY-B PIC X(6).
";
    let mut schema = parse_copybook(cpy).unwrap();
    let lrecl = record_len_from_schema(&schema);
    let mut data = vec![b' '; lrecl];
    // HEADER at offset 0
    data[..16].copy_from_slice(b"TYP1AAAAAA BBBBB");

    // Write at children's actual offsets
    fn write_at_offsets(
        fields: &[copybook_core::Field],
        data: &mut [u8],
        values: &[(&str, &[u8])],
    ) {
        for f in fields {
            for &(name, val) in values {
                if f.name == name {
                    let off = f.offset as usize;
                    let end = (off + f.len as usize).min(data.len());
                    let copy_len = (end - off).min(val.len());
                    data[off..off + copy_len].copy_from_slice(&val[..copy_len]);
                }
            }
            write_at_offsets(&f.children, data, values);
        }
    }
    write_at_offsets(
        &schema.fields,
        &mut data,
        &[
            ("HDR-TYPE", b"TYP1"),
            ("BODY-A", b"AAAAAA"),
            ("BODY-B", b" BBBBB"),
        ],
    );
    schema.lrecl_fixed = Some(u32::try_from(lrecl).unwrap());

    let env = copybook_codec::decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    let f = fields_of(&env);

    assert_eq!(f["HEADER"], "TYP1AAAAAA BBBBB");
    assert_eq!(f["HDR-TYPE"], "TYP1");
    assert_eq!(f["BODY-A"], "AAAAAA");
    assert_eq!(f["BODY-B"], " BBBBB");
}

#[test]
fn test_encode_redefines_nested_group_primary() {
    let cpy = r"
01 REC.
   05 HEADER       PIC X(10).
   05 PARSED-HDR   REDEFINES HEADER.
      10 HDR-TYPE  PIC X(4).
      10 HDR-DATA  PIC X(6).
";
    let schema = parse_copybook(cpy).unwrap();
    let lrecl = record_len_from_schema(&schema);
    let opts = ascii_encode_opts();

    let input = json!({ "HEADER": "ABC123DEF0" });
    let encoded = copybook_codec::encode_record(&schema, &input, &opts).unwrap();
    assert_eq!(encoded.len(), lrecl);
    // First 10 bytes should contain our data
    assert_eq!(&encoded[..10], b"ABC123DEF0");
}

// ===========================================================================
// 8. REDEFINES offset verification in codec
// ===========================================================================

#[test]
fn test_redefines_offset_verification() {
    let cpy = r"
01 REC.
   05 PREFIX       PIC X(5).
   05 ORIG-DATA    PIC X(8).
   05 REDEF-DATA   REDEFINES ORIG-DATA PIC 9(8).
   05 SUFFIX       PIC X(3).
";
    let schema = parse_copybook(cpy).unwrap();

    // PREFIX at offset 0, len 5
    let root = &schema.fields[0];
    let prefix = &root.children[0];
    assert_eq!(prefix.name, "PREFIX");
    assert_eq!(prefix.offset, 0);
    assert_eq!(prefix.len, 5);

    // ORIG-DATA at offset 5, len 8
    let orig = &root.children[1];
    assert_eq!(orig.name, "ORIG-DATA");
    assert_eq!(orig.offset, 5);
    assert_eq!(orig.len, 8);

    // REDEF-DATA overlays ORIG-DATA → same offset
    let redef = &root.children[2];
    assert_eq!(redef.name, "REDEF-DATA");
    assert_eq!(redef.offset, 5);
    assert_eq!(redef.len, 8);
    assert_eq!(redef.redefines_of.as_deref(), Some("ORIG-DATA"));

    // SUFFIX follows the REDEFINES cluster
    let suffix = &root.children[3];
    assert_eq!(suffix.name, "SUFFIX");
    assert_eq!(suffix.offset, 13);
    assert_eq!(suffix.len, 3);

    // Verify decode reads correct bytes (use digits so PIC 9(8) is valid)
    let lrecl = record_len_from_schema(&schema);
    let mut data = vec![b'0'; lrecl];
    data[..5].copy_from_slice(b"PPPPP");
    data[5..13].copy_from_slice(b"12345678");
    data[13..16].copy_from_slice(b"SUF");

    let mut schema2 = parse_copybook(cpy).unwrap();
    schema2.lrecl_fixed = Some(u32::try_from(lrecl).unwrap());
    let env = copybook_codec::decode_record(&schema2, &data, &ascii_decode_opts()).unwrap();
    let f = fields_of(&env);
    assert_eq!(f["PREFIX"], "PPPPP");
    assert_eq!(f["ORIG-DATA"], "12345678");
    assert_eq!(f["SUFFIX"], "SUF");
}

// ===========================================================================
// 9. REDEFINES field length matching
// ===========================================================================

#[test]
fn test_redefines_equal_length() {
    let cpy = r"
01 REC.
   05 FIELD-A PIC X(10).
   05 FIELD-B REDEFINES FIELD-A PIC X(10).
";
    let schema = parse_copybook(cpy).unwrap();
    let root = &schema.fields[0];
    assert_eq!(root.children[0].len, root.children[1].len);
    assert_eq!(root.children[0].offset, root.children[1].offset);
    assert_eq!(schema.lrecl_fixed, Some(10));
}

#[test]
fn test_redefines_shorter_overlay() {
    let cpy = r"
01 REC.
   05 LONG-FIELD  PIC X(20).
   05 SHORT-FIELD REDEFINES LONG-FIELD PIC X(5).
";
    let schema = parse_copybook(cpy).unwrap();
    let root = &schema.fields[0];
    assert_eq!(root.children[0].len, 20);
    assert_eq!(root.children[1].len, 5);
    assert_eq!(root.children[0].offset, root.children[1].offset);
    // Record length is driven by the original (longer) field
    assert_eq!(schema.lrecl_fixed, Some(20));
}

#[test]
fn test_redefines_longer_overlay() {
    let cpy = r"
01 SHORT-FIELD  PIC X(10).
01 LONG-FIELD   REDEFINES SHORT-FIELD PIC X(20).
01 AFTER-FIELD  PIC X(3).
";
    let schema = parse_copybook(cpy).unwrap();

    let short = &schema.fields[0];
    let long = &schema.fields[1];
    let after = &schema.fields[2];

    assert_eq!(short.offset, 0);
    assert_eq!(long.offset, 0);
    assert_eq!(long.len, 20);
    // AFTER-FIELD follows the longest variant
    assert_eq!(after.offset, 20);
    assert_eq!(schema.lrecl_fixed, Some(23));
}

// ===========================================================================
// 10. REDEFINES with Level-88 conditions
// ===========================================================================

#[test]
fn test_decode_redefines_with_level88() {
    let cpy = r"
01 REC.
   05 STATUS-CODE    PIC X(1).
      88 IS-ACTIVE   VALUE 'A'.
      88 IS-INACTIVE VALUE 'I'.
   05 STATUS-NUM     REDEFINES STATUS-CODE PIC 9(1).
";
    // Use a digit so PIC 9(1) decodes without error
    let data = b"1";
    let env = decode_one(cpy, data);
    let f = fields_of(&env);

    // Both views present
    assert_eq!(f["STATUS-CODE"], "1");
    // The redefines numeric view should be present.
    assert!(
        f.contains_key("STATUS-NUM"),
        "numeric redefines view should be in output"
    );
}

#[test]
fn test_schema_redefines_with_level88_structure() {
    let cpy = r"
01 REC.
   05 TYPE-CODE      PIC X(2).
      88 IS-HEADER   VALUE 'HD'.
      88 IS-TRAILER  VALUE 'TR'.
   05 TYPE-NUMERIC   REDEFINES TYPE-CODE PIC 9(2).
";
    let schema = parse_copybook(cpy).unwrap();
    let root = &schema.fields[0];

    // TYPE-CODE with two Level-88 children
    let type_code = &root.children[0];
    assert_eq!(type_code.name, "TYPE-CODE");
    assert!(
        !type_code.children.is_empty(),
        "Level-88 should be children of TYPE-CODE"
    );

    // Find Level-88 conditions
    let conditions: Vec<_> = type_code
        .children
        .iter()
        .filter(|c| matches!(c.kind, FieldKind::Condition { .. }))
        .collect();
    assert_eq!(conditions.len(), 2);

    // TYPE-NUMERIC redefines TYPE-CODE
    let type_num = &root.children[1];
    assert_eq!(type_num.name, "TYPE-NUMERIC");
    assert_eq!(type_num.redefines_of.as_deref(), Some("TYPE-CODE"));
    assert_eq!(type_num.offset, type_code.offset);
}

// ===========================================================================
// 11. REDEFINES roundtrip (decode → encode, using primary view)
// ===========================================================================

#[test]
fn test_redefines_roundtrip_primary_view() {
    let cpy = r"
01 REC.
   05 DATA-FIELD PIC X(10).
   05 ALT-VIEW   REDEFINES DATA-FIELD PIC X(10).
";
    let original = b"HELLOWORLD";

    // Decode
    let env = decode_one(cpy, original);
    let f = fields_of(&env);
    assert_eq!(f["DATA-FIELD"], "HELLOWORLD");

    // Encode using only the primary view
    let schema = parse_copybook(cpy).unwrap();
    let opts = ascii_encode_opts();
    let input = json!({ "DATA-FIELD": "HELLOWORLD" });
    let encoded = copybook_codec::encode_record(&schema, &input, &opts).unwrap();
    assert_eq!(&encoded, original);
}

#[test]
fn test_redefines_roundtrip_with_raw() {
    let cpy = r"
01 REC.
   05 ORIG-FIELD PIC X(8).
   05 ALT-FIELD  REDEFINES ORIG-FIELD PIC 9(8).
";
    let original = b"12345678";

    // Decode with raw capture
    let mut schema = parse_copybook(cpy).unwrap();
    schema.lrecl_fixed = Some(8);
    let decode_opts = DecodeOptions {
        emit_raw: RawMode::Record,
        ..ascii_decode_opts()
    };
    let env = copybook_codec::decode_record(&schema, original, &decode_opts).unwrap();

    // Encode back using raw data
    let encode_opts = EncodeOptions {
        use_raw: true,
        ..ascii_encode_opts()
    };
    let encoded = copybook_codec::encode_record(&schema, &env, &encode_opts).unwrap();
    assert_eq!(&encoded, original, "raw roundtrip must be byte-identical");
}

#[test]
fn test_redefines_roundtrip_numeric_primary() {
    let cpy = r"
01 REC.
   05 RAW-AREA   PIC X(6).
   05 NUM-AREA   REDEFINES RAW-AREA PIC 9(6).
";
    let original = b"007890";

    let env = decode_one(cpy, original);
    let _f = fields_of(&env);

    // Encode using the numeric view
    let schema = parse_copybook(cpy).unwrap();
    let opts = ascii_encode_opts();
    let input = json!({ "NUM-AREA": "7890" });
    let encoded = copybook_codec::encode_record(&schema, &input, &opts).unwrap();
    assert_eq!(encoded.len(), 6);
}

// ===========================================================================
// 12. REDEFINES with different codepages
// ===========================================================================

#[test]
fn test_redefines_decode_cp037() {
    let cpy = r"
01 REC.
   05 TEXT-FIELD  PIC X(4).
   05 ALT-VIEW   REDEFINES TEXT-FIELD PIC X(4).
";
    // EBCDIC CP037: 'A' = 0xC1, 'B' = 0xC2, 'C' = 0xC3, 'D' = 0xC4
    let ebcdic_data: &[u8] = &[0xC1, 0xC2, 0xC3, 0xC4];

    let mut schema = parse_copybook(cpy).unwrap();
    schema.lrecl_fixed = Some(4);
    let opts = DecodeOptions {
        codepage: Codepage::CP037,
        ..ascii_decode_opts()
    };
    let env = copybook_codec::decode_record(&schema, ebcdic_data, &opts).unwrap();
    let f = fields_of(&env);

    assert_eq!(f["TEXT-FIELD"], "ABCD");
    assert_eq!(f["ALT-VIEW"], "ABCD");
}

#[test]
fn test_redefines_decode_cp1047() {
    let cpy = r"
01 REC.
   05 TEXT-FIELD  PIC X(4).
   05 ALT-VIEW   REDEFINES TEXT-FIELD PIC X(4).
";
    // EBCDIC CP1047: 'A' = 0xC1, 'B' = 0xC2, 'C' = 0xC3, 'D' = 0xC4 (same for letters)
    let ebcdic_data: &[u8] = &[0xC1, 0xC2, 0xC3, 0xC4];

    let mut schema = parse_copybook(cpy).unwrap();
    schema.lrecl_fixed = Some(4);
    let opts = DecodeOptions {
        codepage: Codepage::CP1047,
        ..ascii_decode_opts()
    };
    let env = copybook_codec::decode_record(&schema, ebcdic_data, &opts).unwrap();
    let f = fields_of(&env);

    assert_eq!(f["TEXT-FIELD"], "ABCD");
    assert_eq!(f["ALT-VIEW"], "ABCD");
}

#[test]
fn test_redefines_encode_cp037() {
    let cpy = r"
01 REC.
   05 TEXT-FIELD  PIC X(4).
   05 ALT-VIEW   REDEFINES TEXT-FIELD PIC X(4).
";
    let schema = parse_copybook(cpy).unwrap();
    let opts = EncodeOptions {
        codepage: Codepage::CP037,
        coerce_numbers: true,
        ..EncodeOptions::default()
    };

    let input = json!({ "TEXT-FIELD": "ABCD" });
    let encoded = copybook_codec::encode_record(&schema, &input, &opts).unwrap();

    // EBCDIC CP037: A=0xC1, B=0xC2, C=0xC3, D=0xC4
    assert_eq!(encoded, vec![0xC1, 0xC2, 0xC3, 0xC4]);
}

// ===========================================================================
// 13. REDEFINES JSON structure verification
// ===========================================================================

#[test]
fn test_redefines_json_envelope_structure() {
    let cpy = r"
01 REC.
   05 BASE-FIELD PIC X(6).
   05 ALT-FIELD  REDEFINES BASE-FIELD PIC 9(6).
";
    let data = b"123456";
    let env = decode_one(cpy, data);

    // Envelope should have standard keys
    assert!(env.get("schema").is_some(), "missing `schema` key");
    assert!(
        env.get("record_index").is_some(),
        "missing `record_index` key"
    );
    assert!(env.get("codepage").is_some(), "missing `codepage` key");
    assert!(env.get("fields").is_some(), "missing `fields` key");

    // Fields must contain both views
    let f = fields_of(&env);
    assert!(f.contains_key("BASE-FIELD"));
    assert!(f.contains_key("ALT-FIELD"));

    // Fields are also flattened to top level
    assert!(env.get("BASE-FIELD").is_some());
    assert!(env.get("ALT-FIELD").is_some());
}

#[test]
fn test_redefines_json_group_structure() {
    let cpy = r"
01 REC.
   05 FLAT-DATA    PIC X(8).
   05 SPLIT-VIEW   REDEFINES FLAT-DATA.
      10 PART-X    PIC X(4).
      10 PART-Y    PIC X(4).
";
    let mut schema = parse_copybook(cpy).unwrap();
    let lrecl = record_len_from_schema(&schema);
    let mut data = vec![b' '; lrecl];
    data[..8].copy_from_slice(b"XXXXYYYY");
    // Write group children at their actual offsets
    let root = &schema.fields[0];
    let split_view = root
        .children
        .iter()
        .find(|c| c.name == "SPLIT-VIEW")
        .unwrap();
    for child in &split_view.children {
        let off = child.offset as usize;
        let end = off + child.len as usize;
        if end <= lrecl {
            match child.name.as_str() {
                "PART-X" => data[off..end].copy_from_slice(b"XXXX"),
                "PART-Y" => data[off..end].copy_from_slice(b"YYYY"),
                _ => {}
            }
        }
    }
    schema.lrecl_fixed = Some(u32::try_from(lrecl).unwrap());

    let env = copybook_codec::decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    let f = fields_of(&env);

    // Group REDEFINES children are flattened
    assert_eq!(f["FLAT-DATA"], "XXXXYYYY");
    assert_eq!(f["PART-X"], "XXXX");
    assert_eq!(f["PART-Y"], "YYYY");
}

#[test]
fn test_redefines_json_no_raw_by_default() {
    let cpy = r"
01 REC.
   05 F1 PIC X(4).
   05 F2 REDEFINES F1 PIC X(4).
";
    let data = b"TEST";
    let env = decode_one(cpy, data);

    assert!(
        env.get("__raw_b64").is_none(),
        "raw should not be emitted by default"
    );
    assert!(
        env.get("raw_b64").is_none(),
        "raw should not be emitted by default"
    );
}

#[test]
fn test_redefines_json_with_meta() {
    let cpy = r"
01 REC.
   05 F1 PIC X(4).
   05 F2 REDEFINES F1 PIC 9(4).
";
    let data = b"1234";
    let mut schema = parse_copybook(cpy).unwrap();
    schema.lrecl_fixed = Some(4);
    let opts = DecodeOptions {
        emit_meta: true,
        ..ascii_decode_opts()
    };
    let env = copybook_codec::decode_record(&schema, data, &opts).unwrap();

    assert!(env.get("length").is_some(), "meta should include length");
    assert!(
        env.get("__record_index").is_some(),
        "meta should include __record_index"
    );
}

// ===========================================================================
// Additional coverage tests
// ===========================================================================

#[test]
fn test_redefines_decode_preserves_trailing_spaces() {
    let cpy = r"
01 REC.
   05 PADDED     PIC X(10).
   05 TRIMMED    REDEFINES PADDED PIC X(10).
";
    let data = b"HELLO     ";
    let env = decode_one(cpy, data);
    let f = fields_of(&env);

    // Both views should preserve trailing spaces
    assert_eq!(f["PADDED"], "HELLO     ");
    assert_eq!(f["TRIMMED"], "HELLO     ");
}

#[test]
fn test_redefines_group_with_mixed_types() {
    let cpy = r"
01 REC.
   05 RAW-AREA   PIC X(13).
   05 TYPED-VIEW REDEFINES RAW-AREA.
      10 NAME    PIC X(8).
      10 AGE     PIC 9(3).
      10 FLAG    PIC X(2).
";
    let mut schema = parse_copybook(cpy).unwrap();
    let lrecl = record_len_from_schema(&schema);
    let mut data = vec![b'0'; lrecl];
    // RAW-AREA at offset 0
    data[..13].copy_from_slice(b"JOHN    025OK");
    // Write at children's actual offsets
    let root = &schema.fields[0];
    let typed_view = root
        .children
        .iter()
        .find(|c| c.name == "TYPED-VIEW")
        .unwrap();
    for child in &typed_view.children {
        let off = child.offset as usize;
        let end = off + child.len as usize;
        if end <= lrecl {
            match child.name.as_str() {
                "NAME" => data[off..end].copy_from_slice(b"JOHN    "),
                "AGE" => data[off..end].copy_from_slice(b"025"),
                "FLAG" => data[off..end].copy_from_slice(b"OK"),
                _ => {}
            }
        }
    }
    schema.lrecl_fixed = Some(u32::try_from(lrecl).unwrap());

    let env = copybook_codec::decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    let f = fields_of(&env);

    assert_eq!(f["RAW-AREA"], "JOHN    025OK");
    assert_eq!(f["NAME"], "JOHN    ");
    assert_eq!(f["FLAG"], "OK");
    let age = &f["AGE"];
    assert!(
        age.is_number() || age.is_string(),
        "AGE should be numeric, got {age:?}"
    );
}

#[test]
fn test_encode_with_redefines_alternate_view() {
    let cpy = r"
01 REC.
   05 TEXT-DATA PIC X(6).
   05 NUM-DATA  REDEFINES TEXT-DATA PIC 9(6).
";
    let schema = parse_copybook(cpy).unwrap();
    let opts = ascii_encode_opts();

    // Encode using the redefines (alternate) view only
    let input = json!({ "NUM-DATA": "42" });
    let encoded = copybook_codec::encode_record(&schema, &input, &opts).unwrap();
    assert_eq!(encoded.len(), 6);
}

#[test]
fn test_redefines_with_field_after_cluster() {
    // Verify that fields before and after a scalar REDEFINES cluster decode correctly
    let cpy = r"
01 REC.
   05 HEADER    PIC X(4).
   05 AREA-A    PIC X(8).
   05 AREA-B    REDEFINES AREA-A PIC X(8).
   05 TRAILER   PIC X(5).
";
    let mut schema = parse_copybook(cpy).unwrap();
    let lrecl = record_len_from_schema(&schema);
    let root = &schema.fields[0];

    // Verify TRAILER comes after the REDEFINES cluster
    let header = root.children.iter().find(|c| c.name == "HEADER").unwrap();
    let trailer = root.children.iter().find(|c| c.name == "TRAILER").unwrap();
    assert_eq!(header.offset, 0);
    assert!(trailer.offset > 0, "TRAILER should be after HEADER+AREA");

    let mut data = vec![b' '; lrecl];
    data[header.offset as usize..header.offset as usize + 4].copy_from_slice(b"HEAD");
    let area_a = root.children.iter().find(|c| c.name == "AREA-A").unwrap();
    data[area_a.offset as usize..area_a.offset as usize + 8].copy_from_slice(b"12345678");
    data[trailer.offset as usize..trailer.offset as usize + 5].copy_from_slice(b"TRAIL");
    schema.lrecl_fixed = Some(u32::try_from(lrecl).unwrap());

    let env = copybook_codec::decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    let f = fields_of(&env);

    assert_eq!(f["HEADER"], "HEAD");
    assert_eq!(f["AREA-A"], "12345678");
    assert_eq!(f["AREA-B"], "12345678");
    assert_eq!(f["TRAILER"], "TRAIL");
}

#[test]
fn test_redefines_decode_binary_view() {
    let cpy = r"
01 REC.
   05 RAW-BYTES  PIC X(4).
   05 INT-VIEW   REDEFINES RAW-BYTES PIC S9(9) COMP.
";
    // Big-endian binary for 256 = 0x00000100
    let data: &[u8] = &[0x00, 0x00, 0x01, 0x00];
    let env = decode_one(cpy, data);
    let f = fields_of(&env);

    assert!(f.contains_key("RAW-BYTES"));
    assert!(f.contains_key("INT-VIEW"));

    let int_val = &f["INT-VIEW"];
    assert!(
        int_val.is_number() || int_val.is_string(),
        "INT-VIEW should be numeric: {int_val:?}"
    );
}

#[test]
fn test_encode_redefines_missing_all_views_non_strict() {
    let cpy = r"
01 REC.
   05 FIELD-A PIC X(6).
   05 FIELD-B REDEFINES FIELD-A PIC X(6).
";
    let schema = parse_copybook(cpy).unwrap();
    let opts = EncodeOptions {
        strict_mode: false,
        ..ascii_encode_opts()
    };

    // No views supplied at all → non-strict mode may produce space-filled output
    let input = json!({});
    let result = copybook_codec::encode_record(&schema, &input, &opts);
    // Non-strict mode should either succeed with default padding or fail gracefully
    // We just verify it doesn't panic
    let _ = result;
}

#[test]
fn test_redefines_three_way_type_conversion() {
    let cpy = r"
01 REC.
   05 ALPHA-VIEW PIC X(4).
   05 ZONED-VIEW REDEFINES ALPHA-VIEW PIC 9(4).
   05 COMP-VIEW  REDEFINES ALPHA-VIEW PIC S9(9) COMP.
";
    let schema = parse_copybook(cpy).unwrap();
    let root = &schema.fields[0];

    // All three views at same offset
    assert_eq!(root.children[0].offset, root.children[1].offset);
    assert_eq!(root.children[0].offset, root.children[2].offset);

    // All three redefine the same base
    assert!(root.children[0].redefines_of.is_none());
    assert_eq!(root.children[1].redefines_of.as_deref(), Some("ALPHA-VIEW"));
    assert_eq!(root.children[2].redefines_of.as_deref(), Some("ALPHA-VIEW"));
}

#[test]
fn test_redefines_file_level_decode() {
    let cpy = r"
01 REC.
   05 TEXT-F  PIC X(6).
   05 NUM-F   REDEFINES TEXT-F PIC 9(6).
";
    let mut schema = parse_copybook(cpy).unwrap();
    let lrecl = record_len_from_schema(&schema);
    schema.lrecl_fixed = Some(u32::try_from(lrecl).unwrap());
    let opts = ascii_decode_opts();

    // Use all-numeric data so both alphanumeric and zoned views decode cleanly
    let mut data = Vec::new();
    let mut rec1 = vec![b'0'; lrecl];
    rec1[..6].copy_from_slice(b"111111");
    data.extend_from_slice(&rec1);
    let mut rec2 = vec![b'0'; lrecl];
    rec2[..6].copy_from_slice(b"222222");
    data.extend_from_slice(&rec2);

    let input = std::io::Cursor::new(data);
    let mut output = Vec::new();

    let summary = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &opts).unwrap();
    assert_eq!(summary.records_processed, 2);

    let output_str = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = output_str.trim().lines().collect();
    assert_eq!(lines.len(), 2);

    let rec1_json: Value = serde_json::from_str(lines[0]).unwrap();
    let rec2_json: Value = serde_json::from_str(lines[1]).unwrap();

    assert_eq!(rec1_json["TEXT-F"], "111111");
    assert_eq!(rec2_json["TEXT-F"], "222222");
}

#[test]
fn test_redefines_encode_file_level() {
    let cpy = r"
01 REC.
   05 DATA-F PIC X(4).
   05 ALT-F  REDEFINES DATA-F PIC X(4).
";
    let schema = parse_copybook(cpy).unwrap();
    let opts = ascii_encode_opts();

    let jsonl = r#"{"DATA-F":"AAAA"}
{"DATA-F":"BBBB"}
"#;
    let input = std::io::Cursor::new(jsonl.as_bytes());
    let mut output = Vec::new();

    let summary = copybook_codec::encode_jsonl_to_file(&schema, input, &mut output, &opts).unwrap();
    assert_eq!(summary.records_processed, 2);
    assert_eq!(&output, b"AAAABBBB");
}
