// SPDX-License-Identifier: AGPL-3.0-or-later
//! Tests that `EncodeOptions::on_encode_unmappable` is wired through the
//! `lib_api` encode path for alphanumeric (PIC X) fields.
//!
//! The Replace and Skip policies are advertised on `EncodeOptions` and round-
//! tripped through CLI/option tests, but historically they had no effect on
//! actual encoding — `utf8_to_ebcdic` always errored on unmappable characters.
//! These tests pin that wiring.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::{Codepage, EncodeOptions, RecordFormat, UnmappablePolicy, encode_record};
use copybook_core::parse_copybook;
use serde_json::json;

fn schema(size: usize) -> copybook_core::Schema {
    let cpy = format!("       01 REC.\n           05 FLD PIC X({size}).");
    parse_copybook(&cpy).expect("schema should parse")
}

fn ebcdic_opts(policy: UnmappablePolicy) -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_unmappable_policy(policy)
}

// PIC X size is set above the UTF-8 byte length of the input so the byte-based
// length pre-check in encode_alphanum_field is satisfied. Only the codepage
// mapping should reject (or rewrite) the CJK character.
const FIELD_SIZE: usize = 7;

#[test]
fn error_policy_rejects_unmappable() {
    // CJK character has no CP037 mapping; default Error policy must reject.
    let s = schema(FIELD_SIZE);
    let value = json!({ "FLD": "A日B" });
    let err = encode_record(&s, &value, &ebcdic_opts(UnmappablePolicy::Error)).unwrap_err();
    assert_eq!(
        err.code,
        copybook_core::ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
        "expected CBKC301 for unmappable encode, got {err:?}",
    );
}

#[test]
fn replace_policy_substitutes_codepage_space() {
    // Replace must substitute the codepage's space byte (0x40 for EBCDIC) and
    // preserve the input-char count; remaining bytes pad to the field width.
    let s = schema(FIELD_SIZE);
    let value = json!({ "FLD": "A日B" });
    let bytes = encode_record(&s, &value, &ebcdic_opts(UnmappablePolicy::Replace)).unwrap();
    // 'A' -> 0xC1, '日' -> replaced with 0x40, 'B' -> 0xC2, then 4 bytes of pad.
    assert_eq!(bytes, vec![0xC1, 0x40, 0xC2, 0x40, 0x40, 0x40, 0x40]);
}

#[test]
fn skip_policy_drops_unmappable_and_pads() {
    // Skip must drop the unmappable char; the field then space-pads to width.
    let s = schema(FIELD_SIZE);
    let value = json!({ "FLD": "A日B" });
    let bytes = encode_record(&s, &value, &ebcdic_opts(UnmappablePolicy::Skip)).unwrap();
    // 'A' -> 0xC1, 日 dropped, 'B' -> 0xC2, then 5 bytes of pad.
    assert_eq!(bytes, vec![0xC1, 0xC2, 0x40, 0x40, 0x40, 0x40, 0x40]);
}

#[test]
fn ascii_codepage_passthrough_ignores_policy() {
    // ASCII codepage is a transparent pass-through; the policy is irrelevant
    // because no character is "unmappable" under that mode.
    let s = schema(5);
    let value = json!({ "FLD": "HELLO" });
    let opts = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_unmappable_policy(UnmappablePolicy::Error);
    let bytes = encode_record(&s, &value, &opts).unwrap();
    assert_eq!(bytes, b"HELLO");
}
