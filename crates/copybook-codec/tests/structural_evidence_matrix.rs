// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]
//! Consolidated structural-feature evidence matrix (issue #574).
//!
//! The exhaustive ODO/REDEFINES/RENAMES/Level-88 suites in this crate and in
//! `copybook-core` are gated behind the `comprehensive-tests` feature and do
//! **not** run under a plain `cargo test`. This suite consolidates a compact,
//! **default-feature** scenario plane — one supported row and one rejection row
//! per feature where applicable — so the core structural contracts are exercised
//! by the normal PR-gating test run.
//!
//! It also pins two behaviors corrected alongside this evidence work:
//!   * variable-length (tail-ODO) records decode through RDW framing when the
//!     payload is shorter than the schema's maximum allocation, and
//!   * a group that REDEFINES another field overlays it — its children take
//!     offsets relative to the redefined region, not appended after it.

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_file_to_jsonl,
    decode_record, encode_record,
};
use copybook_core::{ErrorCode, FieldKind, parse_copybook};
use serde_json::{Value, json};
use std::io::Cursor;

fn decode_opts(format: RecordFormat) -> DecodeOptions {
    DecodeOptions::new()
        .with_format(format)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(false)
}

fn encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
}

/// RDW frame: 2-byte big-endian length, 2 reserved bytes, then payload.
fn rdw_frame(payload: &[u8]) -> Vec<u8> {
    let len = u16::try_from(payload.len()).expect("payload fits in u16");
    let mut framed = len.to_be_bytes().to_vec();
    framed.extend_from_slice(&[0x00, 0x00]);
    framed.extend_from_slice(payload);
    framed
}

// ===========================================================================
// OCCURS DEPENDING ON (ODO)
// ===========================================================================

/// Supported: a tail ODO array's length is driven by its counter field.
#[test]
fn odo_counter_drives_array_length() {
    let schema = parse_copybook(
        "01 REC.\n   05 CNT PIC 9(2).\n   05 ARR OCCURS 1 TO 5 TIMES DEPENDING ON CNT PIC X(3).",
    )
    .expect("ODO copybook parses");

    // CNT = 03 -> 3 elements: ABC DEF GHI (2 + 9 = 11 bytes).
    let json = decode_record(&schema, b"03ABCDEFGHI", &decode_opts(RecordFormat::Fixed))
        .expect("ODO record decodes");
    assert_eq!(json.get("CNT").and_then(Value::as_str), Some("03"));
    let arr = json
        .get("ARR")
        .and_then(Value::as_array)
        .expect("ARR is array");
    assert_eq!(arr.len(), 3);
    assert_eq!(arr[0], "ABC");
    assert_eq!(arr[2], "GHI");
}

/// Supported (regression guard): a variable-length tail-ODO record decodes
/// through RDW framing even though its payload (11 bytes) is shorter than the
/// schema's maximum allocation (2 + 5*3 = 17 bytes).
#[test]
fn odo_variable_length_decodes_through_rdw() {
    let schema = parse_copybook(
        "01 REC.\n   05 CNT PIC 9(2).\n   05 ARR OCCURS 1 TO 5 TIMES DEPENDING ON CNT PIC X(3).",
    )
    .expect("ODO copybook parses");

    let framed = rdw_frame(b"03ABCDEFGHI"); // 11-byte payload < 17-byte max
    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(
        &schema,
        Cursor::new(framed),
        &mut output,
        &decode_opts(RecordFormat::RDW),
    )
    .expect("RDW+ODO decode succeeds");
    assert_eq!(summary.records_processed, 1, "record must not underflow");

    let line = String::from_utf8(output).expect("utf-8");
    let json: Value = serde_json::from_str(line.trim()).expect("valid JSON line");
    let arr = json
        .get("ARR")
        .and_then(Value::as_array)
        .expect("ARR is array");
    assert_eq!(arr.len(), 3);
}

/// Rejection: a storage field after a tail ODO array is not at the tail and is
/// rejected at parse time.
#[test]
fn odo_non_tail_is_rejected() {
    let err = parse_copybook(
        "01 REC.\n   05 CNT PIC 9(2).\n   05 ARR OCCURS 1 TO 5 TIMES DEPENDING ON CNT PIC X(3).\n   05 TRAILER PIC X(4).",
    )
    .expect_err("storage sibling after ODO must be rejected");
    assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
}

// ===========================================================================
// REDEFINES
// ===========================================================================

/// Supported (regression guard): a group that REDEFINES a field overlays it —
/// the group's children read the redefined region, not bytes appended after it.
#[test]
fn redefines_group_overlays_original() {
    let schema = parse_copybook(
        "01 REC.\n   05 ORIG PIC X(8).\n   05 NUM REDEFINES ORIG PIC 9(8).\n   05 PARTS REDEFINES ORIG.\n      10 LEFT PIC X(4).\n      10 RIGHT PIC X(4).",
    )
    .expect("REDEFINES copybook parses");
    assert_eq!(
        schema.lrecl_fixed,
        Some(8),
        "all views overlay the same 8 bytes"
    );

    let json = decode_record(&schema, b"12345678", &decode_opts(RecordFormat::Fixed))
        .expect("REDEFINES record decodes");
    // The nested `fields` envelope preserves group structure: the group view
    // PARTS nests LEFT/RIGHT, both overlaying ORIG's 8 bytes.
    let fields = json
        .get("fields")
        .and_then(Value::as_object)
        .expect("fields envelope");
    assert_eq!(fields.get("ORIG").and_then(Value::as_str), Some("12345678"));
    assert_eq!(fields.get("NUM").and_then(Value::as_str), Some("12345678"));
    let parts = fields
        .get("PARTS")
        .and_then(Value::as_object)
        .expect("PARTS group view");
    assert_eq!(parts.get("LEFT").and_then(Value::as_str), Some("1234"));
    assert_eq!(parts.get("RIGHT").and_then(Value::as_str), Some("5678"));
}

/// Rejection: encoding with two non-null REDEFINES views is ambiguous.
#[test]
fn redefines_encode_ambiguity_is_rejected() {
    let schema =
        parse_copybook("01 REC.\n   05 ORIG PIC X(8).\n   05 NUM REDEFINES ORIG PIC 9(8).")
            .expect("REDEFINES copybook parses");

    let ambiguous = json!({ "ORIG": "ABCDEFGH", "NUM": "12345678" });
    let err = encode_record(&schema, &ambiguous, &encode_opts())
        .expect_err("two non-null views must be rejected");
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

// ===========================================================================
// RENAMES (level 66)
// ===========================================================================

/// Supported: a level-66 RENAMES alias resolves to its storage span.
#[test]
fn renames_alias_resolves_to_members() {
    // The renamed fields must be in the same scope as the level-66 entry.
    let schema = parse_copybook(
        "01 REC.\n   05 FIELD-1 PIC X(3).\n   05 FIELD-2 PIC X(3).\n   66 SPAN RENAMES FIELD-1 THRU FIELD-2.",
    )
    .expect("RENAMES copybook parses");

    let span = schema
        .find_field_or_alias("SPAN")
        .expect("alias SPAN resolves");
    let FieldKind::Renames { .. } = &span.kind else {
        panic!("SPAN should be a RENAMES field, got {:?}", span.kind);
    };
    let resolved = span
        .resolved_renames
        .as_ref()
        .expect("RENAMES resolved metadata present");
    assert_eq!(
        resolved.length, 6,
        "FIELD-1 THRU FIELD-2 spans both 3-byte members"
    );
}

/// Rejection: a reversed RENAMES range (from after thru) is rejected.
#[test]
fn renames_reversed_range_is_rejected() {
    let err = parse_copybook(
        "01 REC.\n   05 FIELD-1 PIC X(3).\n   05 FIELD-2 PIC X(3).\n   66 SPAN RENAMES FIELD-2 THRU FIELD-1.",
    )
    .expect_err("reversed RENAMES range must be rejected");
    assert_eq!(err.code, ErrorCode::CBKS604_RENAME_REVERSED_RANGE);
}

// ===========================================================================
// Level-88 condition names
// ===========================================================================

/// Supported: Level-88 condition names parse and consume zero storage.
#[test]
fn level88_is_non_storage() {
    let schema = parse_copybook(
        "01 REC.\n   05 STATUS-CODE PIC X.\n      88 OK VALUE 'Y'.\n      88 NOT-OK VALUE 'N'.\n   05 TRAILER PIC X(3).",
    )
    .expect("Level-88 copybook parses");

    // The two 88 conditions add no bytes: 1 (STATUS-CODE) + 3 (TRAILER) = 4.
    assert_eq!(
        schema.lrecl_fixed,
        Some(4),
        "Level-88 conditions consume no storage"
    );

    let json =
        decode_record(&schema, b"YXYZ", &decode_opts(RecordFormat::Fixed)).expect("record decodes");
    assert_eq!(json.get("STATUS-CODE").and_then(Value::as_str), Some("Y"));
    assert_eq!(json.get("TRAILER").and_then(Value::as_str), Some("XYZ"));
}
