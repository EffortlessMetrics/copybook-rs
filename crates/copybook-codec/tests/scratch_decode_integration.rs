// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used)]

//! Integration tests verifying that `decode_record_with_scratch` produces
//! identical output to `decode_record` and that scratch buffers are properly
//! reused across calls.

use copybook_codec::memory::ScratchBuffers;
use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
    ZonedEncodingFormat, decode_record, decode_record_with_scratch,
};
use copybook_core::{Field, FieldKind, Schema};

/// Build a minimal schema with a single DISPLAY field for testing.
fn simple_display_schema() -> Schema {
    Schema {
        fields: vec![Field {
            path: "ROOT.NAME".to_string(),
            name: "NAME".to_string(),
            level: 5,
            kind: FieldKind::Alphanum { len: 10 },
            offset: 0,
            len: 10,
            redefines_of: None,
            occurs: None,
            sync_padding: None,
            synchronized: false,
            blank_when_zero: false,
            resolved_renames: None,
            children: vec![],
        }],
        lrecl_fixed: Some(10),
        tail_odo: None,
        fingerprint: String::new(),
    }
}

/// Build a schema with a zoned decimal field.
fn numeric_schema() -> Schema {
    Schema {
        fields: vec![Field {
            path: "ROOT.AMOUNT".to_string(),
            name: "AMOUNT".to_string(),
            level: 5,
            kind: FieldKind::ZonedDecimal {
                digits: 7,
                scale: 2,
                signed: false,
                sign_separate: None,
            },
            offset: 0,
            len: 7,
            redefines_of: None,
            occurs: None,
            sync_padding: None,
            synchronized: false,
            blank_when_zero: false,
            resolved_renames: None,
            children: vec![],
        }],
        lrecl_fixed: Some(7),
        tail_odo: None,
        fingerprint: String::new(),
    }
}

fn default_options() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_filler(false)
        .with_emit_meta(false)
        .with_emit_raw(RawMode::Off)
        .with_strict_mode(false)
        .with_max_errors(None)
        .with_unmappable_policy(UnmappablePolicy::Error)
        .with_threads(1)
        .with_preserve_zoned_encoding(false)
        .with_preferred_zoned_encoding(ZonedEncodingFormat::Auto)
}

// ---------------------------------------------------------------------------
// 10. decode_record_with_scratch produces identical output to decode_record
// ---------------------------------------------------------------------------

#[test]
fn scratch_decode_matches_plain_decode_alphanumeric() {
    let schema = simple_display_schema();
    let data = b"HELLO     ";
    let options = default_options();

    let plain = decode_record(&schema, data, &options).unwrap();
    let mut scratch = ScratchBuffers::new();
    let with_scratch = decode_record_with_scratch(&schema, data, &options, &mut scratch).unwrap();

    assert_eq!(
        plain, with_scratch,
        "scratch decode must produce identical JSON to plain decode"
    );
}

#[test]
fn scratch_decode_matches_plain_decode_numeric() {
    let schema = numeric_schema();
    let data = b"0012345"; // 123.45
    let options = default_options();

    let plain = decode_record(&schema, data, &options).unwrap();
    let mut scratch = ScratchBuffers::new();
    let with_scratch = decode_record_with_scratch(&schema, data, &options, &mut scratch).unwrap();

    assert_eq!(
        plain, with_scratch,
        "scratch decode must produce identical JSON to plain decode for numerics"
    );
}

#[test]
fn scratch_decode_matches_across_multiple_records() {
    let schema = simple_display_schema();
    let options = default_options();
    let mut scratch = ScratchBuffers::new();

    let records: Vec<&[u8; 10]> = vec![
        b"AAAAAAAAAA",
        b"BBBBBBBBBB",
        b"CCCCCCCCCC",
        b"0123456789",
        b"          ",
    ];

    for data in records {
        let plain = decode_record(&schema, data.as_slice(), &options).unwrap();
        let with_scratch =
            decode_record_with_scratch(&schema, data.as_slice(), &options, &mut scratch).unwrap();
        assert_eq!(plain, with_scratch);
        scratch.clear();
    }
}

// ---------------------------------------------------------------------------
// 11. decode_record_with_scratch reuses scratch across calls
// ---------------------------------------------------------------------------

#[test]
fn scratch_reused_across_decode_calls_no_growth() {
    let schema = simple_display_schema();
    let options = default_options();
    let mut scratch = ScratchBuffers::new();

    // Warmup: a few calls to let buffers stabilize
    for _ in 0..5 {
        let _ = decode_record_with_scratch(&schema, b"WARMUP    ", &options, &mut scratch);
        scratch.clear();
    }

    let byte_cap = scratch.byte_buffer.capacity();
    let string_cap = scratch.string_buffer.capacity();

    // Steady-state: 100 more calls with same-sized records
    for i in 0..100_u32 {
        let data = format!("REC{i:07}");
        let _ = decode_record_with_scratch(&schema, data.as_bytes(), &options, &mut scratch);
        scratch.clear();
    }

    assert_eq!(
        scratch.byte_buffer.capacity(),
        byte_cap,
        "byte_buffer should not grow after warmup"
    );
    assert_eq!(
        scratch.string_buffer.capacity(),
        string_cap,
        "string_buffer should not grow after warmup"
    );
}
