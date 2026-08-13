// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::{Context, Result};
use base64::Engine;
use copybook_codec::{
    DecodeOptions, EncodeOptions, decode_record, decode_record_with_scratch, encode_jsonl_to_file,
    encode_record,
};
use copybook_core::{ErrorCode, parse_copybook};
use copybook_options::{Codepage, RawMode, RecordFormat};
use serde_json::{Value, json};

const RESERVED: [u8; 2] = [0xA5, 0x5A];

fn rdw_options(use_raw: bool, threads: usize, strict_mode: bool) -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(Codepage::ASCII)
        .with_format(RecordFormat::RDW)
        .with_use_raw(use_raw)
        .with_strict_mode(strict_mode)
        .with_threads(threads)
}

fn raw_b64(payload: &[u8], reserved: [u8; 2]) -> Result<String> {
    let payload_len = u16::try_from(payload.len()).context("test RDW payload exceeds u16")?;
    let mut record = Vec::with_capacity(4 + payload.len());
    record.extend_from_slice(&payload_len.to_be_bytes());
    record.extend_from_slice(&reserved);
    record.extend_from_slice(payload);
    Ok(base64::engine::general_purpose::STANDARD.encode(record))
}

fn payload_b64(payload: &[u8]) -> String {
    base64::engine::general_purpose::STANDARD.encode(payload)
}

#[test]
fn raw_rdw_unchanged_replay_is_byte_identical_and_canonical_key_wins() -> Result<()> {
    let schema = parse_copybook("01 REC PIC X(3).")?;
    let canonical = raw_b64(b"ABC", RESERVED)?;
    let legacy = raw_b64(b"XYZ", [0, 0])?;
    let json = json!({
        "fields": {"REC": "ABC"},
        "raw_b64": canonical,
        "__raw_b64": legacy,
        "raw_capture": "record+rdw",
    });

    let encoded = encode_record(&schema, &json, &rdw_options(true, 1, true))?;

    anyhow::ensure!(encoded == [b"\0\x03\xA5\x5A".as_slice(), b"ABC"].concat());
    Ok(())
}

#[test]
fn raw_record_provenance_wraps_payload_without_length_guessing() -> Result<()> {
    let schema = parse_copybook("01 REC PIC X.")?;
    for payload in [
        Vec::new(),
        vec![b'A'],
        vec![b'A', b'B'],
        vec![b'A', b'B', b'C'],
        vec![0, 0, 0xA5, 0x5A],
        b"ABCDE".to_vec(),
    ] {
        let json = json!({
            "raw_b64": payload_b64(&payload),
            "raw_capture": "record",
        });
        let encoded = encode_record(&schema, &json, &rdw_options(true, 1, true))?;
        let length = u16::try_from(payload.len()).context("fixture payload exceeds u16")?;
        let expected = [length.to_be_bytes().as_slice(), &[0, 0], payload.as_slice()].concat();
        anyhow::ensure!(encoded == expected);
    }
    Ok(())
}

#[test]
fn invalid_or_conflicting_raw_capture_is_cbke501() -> Result<()> {
    let schema = parse_copybook("01 REC PIC X.")?;
    for marker in [json!("unknown"), json!(3)] {
        let json = json!({
            "raw_b64": payload_b64(b"A"),
            "raw_capture": marker,
        });
        let error = encode_record(&schema, &json, &rdw_options(true, 1, true))
            .err()
            .context("invalid raw_capture unexpectedly succeeded")?;
        anyhow::ensure!(error.code == ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
    }

    let fixed_options = EncodeOptions::new()
        .with_codepage(Codepage::ASCII)
        .with_format(RecordFormat::Fixed)
        .with_use_raw(true);
    let conflicting = json!({
        "raw_b64": payload_b64(b"A"),
        "raw_capture": "record+rdw",
    });
    let error = encode_record(&schema, &conflicting, &fixed_options)
        .err()
        .context("RDW provenance unexpectedly accepted for fixed replay")?;
    anyhow::ensure!(error.code == ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
    Ok(())
}

#[test]
fn fixed_record_provenance_keeps_raw_replay_unchanged() -> Result<()> {
    let schema = parse_copybook("01 REC PIC X(5).")?;
    let fixed_options = EncodeOptions::new()
        .with_codepage(Codepage::ASCII)
        .with_format(RecordFormat::Fixed)
        .with_use_raw(true);
    let json = json!({
        "fields": {"REC": "XXXXX"},
        "raw_b64": payload_b64(b"HELLO"),
        "raw_capture": "record",
    });
    anyhow::ensure!(encode_record(&schema, &json, &fixed_options)? == b"HELLO");
    Ok(())
}

#[test]
fn explicit_record_rdw_provenance_rejects_malformed_frame() -> Result<()> {
    let schema = parse_copybook("01 REC PIC X.")?;
    let json = json!({
        "raw_b64": payload_b64(&[0x7E; 3]),
        "raw_capture": "record+rdw",
    });
    let error = encode_record(&schema, &json, &rdw_options(true, 1, true))
        .err()
        .context("short explicit raw RDW unexpectedly succeeded")?;
    anyhow::ensure!(error.code == ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
    Ok(())
}

#[test]
fn direct_record_rdw_capture_without_header_is_cbkf102() -> Result<()> {
    let schema = parse_copybook("01 REC PIC X.")?;
    let options = DecodeOptions::new()
        .with_codepage(Codepage::ASCII)
        .with_format(RecordFormat::RDW)
        .with_emit_raw(RawMode::RecordRDW);
    let error = decode_record(&schema, b"A", &options)
        .err()
        .context("RecordRDW capture without a physical header unexpectedly succeeded")?;
    anyhow::ensure!(error.code == ErrorCode::CBKF102_RECORD_LENGTH_INVALID);

    let mut scratch = copybook_codec::runtime::ScratchBuffers::new();
    let scratch_error = decode_record_with_scratch(&schema, b"A", &options, &mut scratch)
        .err()
        .context("scratch RecordRDW capture without a physical header unexpectedly succeeded")?;
    anyhow::ensure!(scratch_error.code == ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
    Ok(())
}

#[test]
fn raw_rdw_mutation_preserves_reserved_bytes_at_max_payload() -> Result<()> {
    let schema = parse_copybook("01 REC PIC X(65535).")?;
    let changed = "B".repeat(u16::MAX as usize);
    let json = json!({
        "fields": {"REC": changed},
        "raw_b64": raw_b64(b"", RESERVED)?,
    });

    let encoded = encode_record(&schema, &json, &rdw_options(true, 1, true))?;

    anyhow::ensure!(encoded.len() == u16::MAX as usize + 4);
    anyhow::ensure!(encoded.get(..4) == Some([0xFF, 0xFF, 0xA5, 0x5A].as_slice()));
    anyhow::ensure!(encoded.get(4..) == Some("B".repeat(u16::MAX as usize).as_bytes()));
    Ok(())
}

#[test]
fn raw_rdw_mutation_above_max_payload_is_cbkf102() -> Result<()> {
    let schema = parse_copybook("01 REC. 05 A PIC X(65535). 05 B PIC X.")?;
    let json = json!({
        "fields": {"A": "A".repeat(u16::MAX as usize), "B": "B"},
        "raw_b64": raw_b64(b"", RESERVED)?,
    });

    let error = encode_record(&schema, &json, &rdw_options(true, 1, true))
        .err()
        .context("oversized raw RDW mutation unexpectedly succeeded")?;
    anyhow::ensure!(error.code == ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
    Ok(())
}

#[test]
fn raw_rdw_short_headers_fail_before_field_fallback() -> Result<()> {
    let schema = parse_copybook("01 REC PIC X.")?;

    for raw_len in 0..4 {
        let json = json!({
            "fields": {"REC": "A"},
            "raw_b64": base64::engine::general_purpose::STANDARD.encode(vec![0x7E; raw_len]),
        });
        let error = encode_record(&schema, &json, &rdw_options(true, 1, true))
            .err()
            .with_context(|| format!("{raw_len}-byte raw RDW unexpectedly succeeded"))?;
        anyhow::ensure!(error.code == ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
    }

    Ok(())
}

#[test]
fn raw_rdw_declared_length_mismatch_is_cbkf102_before_replay_or_rebuild() -> Result<()> {
    let schema = parse_copybook("01 REC PIC X(3).")?;

    for declared_len in [1_u16, 5] {
        let mismatched = [
            declared_len.to_be_bytes().as_slice(),
            RESERVED.as_slice(),
            b"ABC",
        ]
        .concat();
        let encoded_raw = base64::engine::general_purpose::STANDARD.encode(mismatched);

        for raw_key in ["raw_b64", "__raw_b64"] {
            for field_value in [json!("ABC"), json!("XYZ"), json!(123)] {
                let mut json = json!({"fields": {"REC": field_value}});
                json[raw_key] = Value::String(encoded_raw.clone());

                let error = encode_record(&schema, &json, &rdw_options(true, 1, true))
                    .err()
                    .with_context(|| {
                        format!(
                            "RDW declaring {declared_len} bytes unexpectedly encoded from {raw_key} with field value {field_value}"
                        )
                    })?;
                anyhow::ensure!(error.code == ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
            }
        }
    }
    Ok(())
}

#[test]
fn use_raw_false_ignores_short_raw_rdw() -> Result<()> {
    let schema = parse_copybook("01 REC PIC X.")?;
    let json = json!({
        "fields": {"REC": "A"},
        "raw_b64": base64::engine::general_purpose::STANDARD.encode([0x7E; 3]),
    });

    let encoded = encode_record(&schema, &json, &rdw_options(false, 1, true))?;

    anyhow::ensure!(encoded == b"\0\x01\0\0A");
    Ok(())
}

#[test]
fn jsonl_routes_retain_cbkf102_failure_detail() -> Result<()> {
    let schema = parse_copybook("01 REC PIC X.")?;
    let malformed = json!({
        "fields": {"REC": "A"},
        "__raw_b64": base64::engine::general_purpose::STANDARD.encode([0x7E; 3]),
    });
    let input = format!("{malformed}\n");

    for threads in [1, 2] {
        let mut output = Vec::new();
        let summary = encode_jsonl_to_file(
            &schema,
            input.as_bytes(),
            &mut output,
            &rdw_options(true, threads, true),
        )?;
        anyhow::ensure!(summary.records_processed == 0);
        anyhow::ensure!(summary.records_with_errors == 1);
        anyhow::ensure!(summary.total_records() == 1);
        anyhow::ensure!(summary.failures.len() == 1);
        anyhow::ensure!(
            summary.failures.first().map(|failure| failure.error.code)
                == Some(ErrorCode::CBKF102_RECORD_LENGTH_INVALID)
        );
        anyhow::ensure!(output.is_empty());
    }

    Ok(())
}

#[test]
fn jsonl_success_and_failure_accounting_matches_across_routes() -> Result<()> {
    let schema = parse_copybook("01 REC PIC X.")?;
    let malformed_raw = base64::engine::general_purpose::STANDARD.encode([0x7E; 3]);
    let records = [
        json!({"fields": {"REC": "A"}}),
        json!({"fields": {"REC": "B"}, "raw_b64": malformed_raw}),
        json!({"fields": {"REC": "C"}}),
    ];
    let input = records
        .iter()
        .map(Value::to_string)
        .collect::<Vec<_>>()
        .join("\n")
        + "\n";

    for threads in [1, 2] {
        for strict_mode in [true, false] {
            let mut output = Vec::new();
            let summary = encode_jsonl_to_file(
                &schema,
                input.as_bytes(),
                &mut output,
                &rdw_options(true, threads, strict_mode),
            )?;
            let expected_processed = if strict_mode { 1 } else { 2 };
            let expected_output = if strict_mode {
                b"\0\x01\0\0A".as_slice()
            } else {
                b"\0\x01\0\0A\0\x01\0\0C".as_slice()
            };

            anyhow::ensure!(summary.records_processed == expected_processed);
            anyhow::ensure!(summary.records_with_errors == 1);
            anyhow::ensure!(summary.total_records() == expected_processed + 1);
            anyhow::ensure!(summary.failures.len() == 1);
            let failure = summary
                .failures
                .first()
                .context("expected retained JSONL encode failure")?;
            anyhow::ensure!(failure.record_index == 2);
            anyhow::ensure!(failure.error.code == ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
            anyhow::ensure!(output == expected_output);
        }
    }

    Ok(())
}
