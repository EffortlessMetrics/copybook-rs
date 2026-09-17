#![allow(clippy::expect_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used)]
#![allow(
    clippy::items_after_statements,
    clippy::too_many_lines,
    clippy::similar_names,
    clippy::cast_precision_loss,
    clippy::cast_sign_loss,
    clippy::print_literal,
    clippy::uninlined_format_args,
    clippy::bool_assert_comparison
)]

use anyhow::{Context, Result};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, ZonedEncodingFormat, decode_record,
    encode_record,
};
use copybook_core::parse_copybook;
use serde_json::Value;

type TestResult = Result<()>;

const ZONED_HEAVY_COPYBOOK: &str = r"
       01  ZONED-RECORD.
           05  FIELD-01            PIC S9(9).
           05  FIELD-02            PIC S9(9).
           05  FIELD-03            PIC S9(9).
           05  FIELD-04            PIC S9(9).
           05  FIELD-05            PIC S9(9).
";

/// Deterministic zoned-preservation behavior (#990).
///
/// Timing comparisons live in the criterion benchmark
/// (`bench_decode_zoned_preserve` in `tools/copybook-bench`), where repeated
/// measurements and the governed noise/budget policy apply — never in a
/// single-sample `Instant` ratio inside the ordinary suite.
#[test]
fn test_zoned_preservation_metadata_and_values() -> TestResult {
    let schema = parse_copybook(ZONED_HEAVY_COPYBOOK)
        .context("failed to parse performance regression copybook fixture")?;

    // Test data: 5 fields × 9 bytes each = 45 bytes total. Each field holds
    // EBCDIC digits F0-F7 followed by F0, i.e. the value 012345670.
    let mut zoned_data = Vec::new();
    for _field in 0..5 {
        for i in 0..8 {
            zoned_data.push(0xF0 + (i % 10) as u8);
        }
        zoned_data.push(0xF0);
    }

    let options_default = DecodeOptions::default();
    let options_preserve = DecodeOptions::default().with_preserve_zoned_encoding(true);
    assert!(
        !options_default.preserve_zoned_encoding,
        "default policy must not preserve zoned encoding"
    );
    assert!(
        options_preserve.preserve_zoned_encoding,
        "opt-in policy must preserve zoned encoding"
    );

    let default_json = decode_record(&schema, &zoned_data, &options_default)?;
    let preserve_json = decode_record(&schema, &zoned_data, &options_preserve)?;

    // Preservation must not alter decoded values: every field reads 012345670.
    for (label, json) in [("default", &default_json), ("preserve", &preserve_json)] {
        let fields = json
            .get("fields")
            .and_then(Value::as_object)
            .context("decoded JSON missing fields object")?;
        assert_eq!(fields.len(), 5, "{label}: expected 5 fields");
        for n in 1..=5 {
            let key = format!("FIELD-{n:02}");
            assert_eq!(
                fields.get(&key).and_then(Value::as_str),
                Some("012345670"),
                "{label}: {key} must decode to 012345670"
            );
        }
    }

    // Preservation metadata is present only under the opt-in policy.
    assert!(
        default_json.get("_encoding_metadata").is_none(),
        "default policy must not emit zoned-encoding metadata"
    );
    let metadata = preserve_json
        .get("_encoding_metadata")
        .and_then(Value::as_object)
        .context("preserve policy must emit zoned-encoding metadata")?;
    assert_eq!(metadata.len(), 5, "metadata must cover all 5 fields");
    for n in 1..=5 {
        let key = format!("FIELD-{n:02}");
        assert_eq!(
            metadata.get(&key).and_then(Value::as_str),
            Some("ebcdic"),
            "{key} must record ebcdic zoned encoding"
        );
    }

    // Exact round-trip bytes under the explicit EBCDIC zoned format.
    let encode_options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_zoned_encoding_format(ZonedEncodingFormat::Ebcdic);
    let roundtrip = encode_record(&schema, &preserve_json, &encode_options)?;
    assert_eq!(
        roundtrip, zoned_data,
        "preserve-mode decode must round-trip to byte-identical input"
    );
    Ok(())
}

#[test]
fn test_default_behavior_unchanged() -> TestResult {
    let schema = parse_copybook(ZONED_HEAVY_COPYBOOK)
        .context("failed to parse performance regression copybook fixture")?;

    let mut zoned_data = Vec::new();
    for _field in 0..5 {
        for i in 0..8 {
            zoned_data.push(0xF0 + (i % 10) as u8);
        }
        zoned_data.push(0xF0);
    }

    // Verify that default options have preserve_zoned_encoding = false
    let options_default = DecodeOptions::default();
    assert_eq!(
        options_default.preserve_zoned_encoding, false,
        "Default DecodeOptions should have preserve_zoned_encoding = false"
    );

    // Verify that it works correctly
    let result = decode_record(&schema, &zoned_data, &options_default)?;
    println!("Default decode result: {}", result);

    // Envelope should expose fields object with expected length
    let obj = result
        .as_object()
        .context("decoded JSON value was not an object")?;
    let fields = obj
        .get("fields")
        .and_then(Value::as_object)
        .context("decoded JSON missing fields object")?;
    assert_eq!(fields.len(), 5);
    Ok(())
}
