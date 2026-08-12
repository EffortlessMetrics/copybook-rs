// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::{Context, Result};
use base64::Engine;
use copybook_codec::{EncodeOptions, encode_jsonl_to_file, encode_record};
use copybook_core::{ErrorCode, parse_copybook};
use copybook_options::{Codepage, RecordFormat};
use serde_json::json;

const RESERVED: [u8; 2] = [0xA5, 0x5A];

fn rdw_options(use_raw: bool, threads: usize) -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(Codepage::ASCII)
        .with_format(RecordFormat::RDW)
        .with_use_raw(use_raw)
        .with_strict_mode(true)
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

#[test]
fn raw_rdw_unchanged_replay_is_byte_identical_and_canonical_key_wins() -> Result<()> {
    let schema = parse_copybook("01 REC PIC X(3).")?;
    let canonical = raw_b64(b"ABC", RESERVED)?;
    let legacy = raw_b64(b"XYZ", [0, 0])?;
    let json = json!({
        "fields": {"REC": "ABC"},
        "raw_b64": canonical,
        "__raw_b64": legacy,
    });

    let encoded = encode_record(&schema, &json, &rdw_options(true, 1))?;

    anyhow::ensure!(encoded == [b"\0\x03\xA5\x5A".as_slice(), b"ABC"].concat());
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

    let encoded = encode_record(&schema, &json, &rdw_options(true, 1))?;

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

    let error = encode_record(&schema, &json, &rdw_options(true, 1))
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
        let error = encode_record(&schema, &json, &rdw_options(true, 1))
            .err()
            .with_context(|| format!("{raw_len}-byte raw RDW unexpectedly succeeded"))?;
        anyhow::ensure!(error.code == ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
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

    let encoded = encode_record(&schema, &json, &rdw_options(false, 1))?;

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
            &rdw_options(true, threads),
        )?;
        anyhow::ensure!(summary.records_with_errors == 1);
        anyhow::ensure!(summary.failures.len() == 1);
        anyhow::ensure!(
            summary.failures.first().map(|failure| failure.error.code)
                == Some(ErrorCode::CBKF102_RECORD_LENGTH_INVALID)
        );
        anyhow::ensure!(output.is_empty());
    }

    Ok(())
}
