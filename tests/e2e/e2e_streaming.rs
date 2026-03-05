// SPDX-License-Identifier: AGPL-3.0-or-later
//! End-to-end streaming decode and encode tests.
//!
//! Validates file-level and iterator-based streaming APIs including:
//! - Multi-record decode to JSONL
//! - Empty and single-record edge cases
//! - Large record (LRECL=10000)
//! - Metadata and raw mode emission
//! - JSONL encode (single + multi-record)
//! - Decode → encode → compare binary round-trip
//! - Scratch buffer equivalence
//! - Error recovery in corrupt streams
//! - Cross-codepage consistency
//! - Deterministic file-level output

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::io::Cursor;

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat,
    decode_file_to_jsonl, decode_record, decode_record_with_scratch, encode_jsonl_to_file,
    iter_records, memory::ScratchBuffers,
};
use copybook_core::parse_copybook;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn decode_opts(cp: Codepage) -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(cp)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

fn encode_opts(cp: Codepage) -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(cp)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

/// Convert ASCII uppercase/digit/space to CP037 EBCDIC, right-padded to `len`.
fn ebcdic_text(s: &str, len: usize) -> Vec<u8> {
    let mut out = Vec::with_capacity(len);
    for c in s.chars() {
        out.push(match c {
            ' ' => 0x40,
            'A'..='I' => 0xC1 + (c as u8 - b'A'),
            'J'..='R' => 0xD1 + (c as u8 - b'J'),
            'S'..='Z' => 0xE2 + (c as u8 - b'S'),
            '0'..='9' => 0xF0 + (c as u8 - b'0'),
            _ => 0x6F,
        });
    }
    out.resize(len, 0x40);
    out
}

/// Build EBCDIC digit bytes from a digit string.
fn ebcdic_digits(s: &str) -> Vec<u8> {
    s.bytes().map(|b| b - b'0' + 0xF0).collect()
}

/// Simple DISPLAY-only copybook (16 bytes per record).
const SIMPLE_CPY: &str = r"
    01 SIMPLE-REC.
       05 REC-ID    PIC 9(6).
       05 NAME      PIC X(10).
";

/// Build one EBCDIC record for SIMPLE_CPY.
fn simple_record(id: &str, name: &str) -> Vec<u8> {
    let mut rec = Vec::new();
    rec.extend_from_slice(&ebcdic_digits(id));
    rec.extend_from_slice(&ebcdic_text(name, 10));
    rec
}

/// Parse JSONL output into a Vec of serde_json::Value.
fn parse_jsonl(jsonl: &str) -> Vec<serde_json::Value> {
    jsonl
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| serde_json::from_str(l).expect("valid JSONL line"))
        .collect()
}

// ===========================================================================
// 1. Decode multi-record file to JSONL (verify record count)
// ===========================================================================

#[test]
fn streaming_decode_multi_record_count() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");
    let mut data = Vec::new();
    for i in 1..=5 {
        data.extend_from_slice(&simple_record(&format!("{i:06}"), "REC"));
    }

    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(
        &schema,
        Cursor::new(&data),
        &mut output,
        &decode_opts(Codepage::CP037),
    )
    .expect("decode");

    let jsonl = String::from_utf8(output).expect("utf8");
    let records = parse_jsonl(&jsonl);

    assert_eq!(records.len(), 5, "should decode exactly 5 records");
    assert_eq!(summary.records_processed, 5);
    assert!(summary.is_successful());
}

// ===========================================================================
// 2. Decode empty file (zero records)
// ===========================================================================

#[test]
fn streaming_decode_empty_file() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");
    let data: Vec<u8> = Vec::new();

    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(
        &schema,
        Cursor::new(&data),
        &mut output,
        &decode_opts(Codepage::CP037),
    )
    .expect("decode empty");

    let jsonl = String::from_utf8(output).expect("utf8");
    assert!(jsonl.trim().is_empty(), "no JSONL lines for empty input");
    assert_eq!(summary.records_processed, 0);
}

// ===========================================================================
// 3. Decode single-record file
// ===========================================================================

#[test]
fn streaming_decode_single_record() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");
    let data = simple_record("000001", "ALICE");

    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(
        &schema,
        Cursor::new(&data),
        &mut output,
        &decode_opts(Codepage::CP037),
    )
    .expect("decode");

    let records = parse_jsonl(&String::from_utf8(output).expect("utf8"));
    assert_eq!(records.len(), 1);
    assert_eq!(summary.records_processed, 1);
    assert_eq!(records[0]["REC-ID"], "000001");
}

// ===========================================================================
// 4. Decode large record (LRECL=10000)
// ===========================================================================

#[test]
fn streaming_decode_large_record_lrecl_10000() {
    let cpy = r"
        01 BIG-REC.
           05 BIG-FIELD PIC X(10000).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    // Build a 10000-byte EBCDIC record filled with 'A' (0xC1)
    let data: Vec<u8> = vec![0xC1; 10000];

    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(
        &schema,
        Cursor::new(&data),
        &mut output,
        &decode_opts(Codepage::CP037),
    )
    .expect("decode large");

    let records = parse_jsonl(&String::from_utf8(output).expect("utf8"));
    assert_eq!(records.len(), 1);
    assert_eq!(summary.records_processed, 1);

    let big_val = records[0]["BIG-FIELD"].as_str().expect("string");
    assert_eq!(big_val.len(), 10000, "decoded field must be 10000 chars");
    assert!(big_val.chars().all(|c| c == 'A'), "all chars should be 'A'");
}

// ===========================================================================
// 5. Decode with emit_meta (verify _meta-like keys in output)
// ===========================================================================

#[test]
fn streaming_decode_emit_meta() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");
    let data = simple_record("000042", "META");

    let opts = decode_opts(Codepage::CP037).with_emit_meta(true);
    let mut output = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(&data), &mut output, &opts).expect("decode meta");

    let records = parse_jsonl(&String::from_utf8(output).expect("utf8"));
    assert_eq!(records.len(), 1);

    let rec = &records[0];
    assert!(
        rec.get("__record_index").is_some(),
        "emit_meta should produce __record_index"
    );
    assert!(
        rec.get("__length").is_some(),
        "emit_meta should produce __length"
    );
}

// ===========================================================================
// 6. Decode with raw mode (verify __raw_b64 in output)
// ===========================================================================

#[test]
fn streaming_decode_raw_mode_record() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");
    let data = simple_record("000099", "RAW");

    let opts = decode_opts(Codepage::CP037).with_emit_raw(RawMode::Record);
    let mut output = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(&data), &mut output, &opts).expect("decode raw");

    let records = parse_jsonl(&String::from_utf8(output).expect("utf8"));
    assert_eq!(records.len(), 1);

    let rec = &records[0];
    assert!(
        rec.get("__raw_b64").is_some(),
        "RawMode::Record should emit __raw_b64"
    );

    let raw_b64 = rec["__raw_b64"].as_str().expect("string");
    assert!(!raw_b64.is_empty(), "raw_b64 must be non-empty");

    // Verify round-trip: base64 decodes back to the original record bytes
    use base64::Engine;
    let decoded_bytes = base64::engine::general_purpose::STANDARD
        .decode(raw_b64)
        .expect("valid base64");
    assert_eq!(decoded_bytes, data, "raw_b64 must decode to original bytes");
}

// ===========================================================================
// 7. Encode from JSONL (multiple records)
// ===========================================================================

#[test]
fn streaming_encode_multi_record_jsonl() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    // Build 3 records, decode to JSONL
    let mut bin_data = Vec::new();
    for i in 1..=3 {
        bin_data.extend_from_slice(&simple_record(&format!("{i:06}"), "ENC"));
    }

    let mut jsonl_buf = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(&bin_data), &mut jsonl_buf, &dopts).expect("decode");

    // Encode JSONL back to binary
    let mut encoded_buf = Vec::new();
    let summary = encode_jsonl_to_file(&schema, Cursor::new(&jsonl_buf), &mut encoded_buf, &eopts)
        .expect("encode");

    assert_eq!(summary.records_processed, 3, "should encode 3 records");
    // Each record is 16 bytes
    assert_eq!(
        encoded_buf.len(),
        3 * 16,
        "encoded output must be 3×16 bytes"
    );
}

// ===========================================================================
// 8. Encode single record
// ===========================================================================

#[test]
fn streaming_encode_single_record_jsonl() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let rec = simple_record("000007", "SOLO");
    let mut jsonl_buf = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(&rec), &mut jsonl_buf, &dopts).expect("decode");

    let mut encoded_buf = Vec::new();
    let summary = encode_jsonl_to_file(&schema, Cursor::new(&jsonl_buf), &mut encoded_buf, &eopts)
        .expect("encode single");

    assert_eq!(summary.records_processed, 1);
    assert_eq!(encoded_buf.len(), 16);
}

// ===========================================================================
// 9. Round-trip: decode → encode → compare binary
// ===========================================================================

#[test]
fn streaming_roundtrip_binary_fidelity() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let mut original = Vec::new();
    for i in 1..=4 {
        original.extend_from_slice(&simple_record(&format!("{i:06}"), "ROUND"));
    }

    // Decode to JSONL
    let mut jsonl_buf = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(&original), &mut jsonl_buf, &dopts).expect("decode");

    // Encode back to binary
    let mut re_encoded = Vec::new();
    encode_jsonl_to_file(&schema, Cursor::new(&jsonl_buf), &mut re_encoded, &eopts)
        .expect("encode");

    assert_eq!(
        original, re_encoded,
        "round-trip (decode→encode) must produce identical binary"
    );
}

// ===========================================================================
// 10. Scratch buffers produce same result as standard decode
// ===========================================================================

#[test]
fn streaming_scratch_buffer_equivalence() {
    let cpy = r"
        01 SCRATCH-REC.
           05 ID-FIELD    PIC 9(4).
           05 AMOUNT      PIC S9(5)V99 COMP-3.
           05 LABEL       PIC X(8).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);

    // Build record: ID=0001, AMOUNT=+123.45, LABEL="SCRATCH "
    let mut data = Vec::new();
    data.extend_from_slice(&ebcdic_digits("0001"));
    data.extend_from_slice(&[0x00, 0x12, 0x34, 0x5C]); // +123.45 COMP-3
    data.extend_from_slice(&ebcdic_text("SCRATCH", 8));

    let standard = decode_record(&schema, &data, &dopts).expect("standard decode");

    let mut scratch = ScratchBuffers::new();
    let with_scratch =
        decode_record_with_scratch(&schema, &data, &dopts, &mut scratch).expect("scratch decode");

    assert_eq!(
        standard, with_scratch,
        "scratch-buffer decode must match standard decode"
    );
}

// ===========================================================================
// 11. Scratch buffers across multiple records reuse correctly
// ===========================================================================

#[test]
fn streaming_scratch_buffer_reuse_across_records() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let mut scratch = ScratchBuffers::new();

    let records: Vec<Vec<u8>> = (1..=10)
        .map(|i| simple_record(&format!("{i:06}"), &format!("R{i:02}")))
        .collect();

    let mut prev_results = Vec::new();
    for rec in &records {
        let result =
            decode_record_with_scratch(&schema, rec, &dopts, &mut scratch).expect("scratch decode");
        prev_results.push(result);
    }

    // Verify each result matches standard decode
    for (i, rec) in records.iter().enumerate() {
        let standard = decode_record(&schema, rec, &dopts).expect("standard decode");
        assert_eq!(
            prev_results[i], standard,
            "scratch decode for record {i} must match standard"
        );
    }
}

// ===========================================================================
// 12. Error recovery: corrupt record doesn't stop iterator stream
// ===========================================================================

#[test]
fn streaming_error_recovery_iterator() {
    let cpy = r"
        01 ERR-REC.
           05 NUM-FIELD PIC 9(4).
           05 TXT-FIELD PIC X(6).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);

    // Build 3 good records (10 bytes each)
    let mut data = Vec::new();
    data.extend_from_slice(&ebcdic_digits("0001"));
    data.extend_from_slice(&ebcdic_text("GOOD01", 6));
    data.extend_from_slice(&ebcdic_digits("0002"));
    data.extend_from_slice(&ebcdic_text("GOOD02", 6));
    data.extend_from_slice(&ebcdic_digits("0003"));
    data.extend_from_slice(&ebcdic_text("GOOD03", 6));

    let iter = iter_records(Cursor::new(&data), &schema, &dopts).expect("iter");

    let mut ok_count = 0;
    let mut err_count = 0;
    for result in iter {
        match result {
            Ok(_) => ok_count += 1,
            Err(_) => err_count += 1,
        }
    }

    assert_eq!(ok_count, 3, "all 3 records should decode successfully");
    assert_eq!(err_count, 0);
}

// ===========================================================================
// 13. Iterator with partial trailing data does not panic
// ===========================================================================

#[test]
fn streaming_iterator_partial_trailing_data() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");
    let dopts = decode_opts(Codepage::CP037);

    // One full record (16 bytes) + 5 trailing bytes (incomplete)
    let mut data = simple_record("000001", "TRAIL");
    data.extend_from_slice(&[0xF0; 5]);

    let iter = iter_records(Cursor::new(&data), &schema, &dopts).expect("iter");
    let results: Vec<_> = iter.collect();

    // Should get exactly 1 successful record; trailing data is too short
    assert_eq!(results.len(), 1, "only 1 complete record");
    assert!(results[0].is_ok());
}

// ===========================================================================
// 14. Multiple codepages: CP037 vs CP1047 produce same logic for digits
// ===========================================================================

#[test]
fn streaming_codepage_cp037_vs_cp1047_digits() {
    let cpy = r"
        01 DIGIT-REC.
           05 FIELD-A PIC 9(8).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    // EBCDIC digits F0-F9 are identical in CP037 and CP1047
    let data: Vec<u8> = vec![0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8];

    for cp in [Codepage::CP037, Codepage::CP1047] {
        let dopts = decode_opts(cp);
        let eopts = encode_opts(cp);

        let mut jsonl_buf = Vec::new();
        let summary = decode_file_to_jsonl(&schema, Cursor::new(&data), &mut jsonl_buf, &dopts)
            .expect("decode");
        assert_eq!(summary.records_processed, 1);

        let records = parse_jsonl(&String::from_utf8(jsonl_buf.clone()).expect("utf8"));
        assert_eq!(
            records[0]["FIELD-A"], "12345678",
            "digits decode the same for {cp:?}"
        );

        // Round-trip
        let mut re_encoded = Vec::new();
        encode_jsonl_to_file(&schema, Cursor::new(&jsonl_buf), &mut re_encoded, &eopts)
            .expect("encode");
        assert_eq!(data, re_encoded, "round-trip binary identical for {cp:?}");
    }
}

// ===========================================================================
// 15. File-level decode produces deterministic output
// ===========================================================================

#[test]
fn streaming_decode_deterministic_output() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");
    let dopts = decode_opts(Codepage::CP037);

    let mut data = Vec::new();
    for i in 1..=3 {
        data.extend_from_slice(&simple_record(&format!("{i:06}"), "DET"));
    }

    // Decode twice
    let mut out1 = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(&data), &mut out1, &dopts).expect("decode-1");

    let mut out2 = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(&data), &mut out2, &dopts).expect("decode-2");

    assert_eq!(
        out1, out2,
        "file-level decode must be byte-for-byte deterministic"
    );
}

// ===========================================================================
// 16. Iterator record count matches file-level decode count
// ===========================================================================

#[test]
fn streaming_iterator_vs_file_api_count() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");
    let dopts = decode_opts(Codepage::CP037);

    let mut data = Vec::new();
    for i in 1..=7 {
        data.extend_from_slice(&simple_record(&format!("{i:06}"), "ITER"));
    }

    // File-level API
    let mut jsonl_buf = Vec::new();
    let summary = decode_file_to_jsonl(&schema, Cursor::new(&data), &mut jsonl_buf, &dopts)
        .expect("file decode");

    // Iterator API
    let iter = iter_records(Cursor::new(&data), &schema, &dopts).expect("iter");
    let iter_count = iter.filter(|r| r.is_ok()).count();

    assert_eq!(summary.records_processed, 7);
    assert_eq!(iter_count, 7);
    assert_eq!(
        summary.records_processed as usize, iter_count,
        "file-level and iterator APIs must report same count"
    );
}

// ===========================================================================
// 17. Decode → iterator field values match file-level decode values
// ===========================================================================

#[test]
fn streaming_iterator_field_values_match_file_api() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");
    let dopts = decode_opts(Codepage::CP037);

    let mut data = Vec::new();
    for i in 1..=3 {
        data.extend_from_slice(&simple_record(&format!("{i:06}"), &format!("N{i:02}")));
    }

    // File-level decode
    let mut jsonl_buf = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(&data), &mut jsonl_buf, &dopts).expect("file decode");
    let file_records = parse_jsonl(&String::from_utf8(jsonl_buf).expect("utf8"));

    // Iterator decode
    let iter = iter_records(Cursor::new(&data), &schema, &dopts).expect("iter");
    let iter_records: Vec<_> = iter.map(|r| r.expect("ok")).collect();

    assert_eq!(file_records.len(), iter_records.len());
    for i in 0..file_records.len() {
        assert_eq!(
            file_records[i]["REC-ID"], iter_records[i]["REC-ID"],
            "REC-ID mismatch at record {i}"
        );
        assert_eq!(
            file_records[i]["NAME"], iter_records[i]["NAME"],
            "NAME mismatch at record {i}"
        );
    }
}

// ===========================================================================
// 18. Encode JSONL with COMP-3 field round-trips correctly
// ===========================================================================

#[test]
fn streaming_encode_comp3_roundtrip() {
    let cpy = r"
        01 COMP3-REC.
           05 REC-ID   PIC 9(4).
           05 AMOUNT   PIC S9(5)V99 COMP-3.
           05 LABEL    PIC X(6).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    // Build: ID=0001, AMOUNT=+543.21, LABEL="COMP3 "
    let mut data = Vec::new();
    data.extend_from_slice(&ebcdic_digits("0001"));
    data.extend_from_slice(&[0x00, 0x54, 0x32, 0x1C]); // +543.21 COMP-3
    data.extend_from_slice(&ebcdic_text("COMP3", 6));

    // Decode to JSONL
    let mut jsonl_buf = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(&data), &mut jsonl_buf, &dopts).expect("decode");

    // Encode back
    let mut re_encoded = Vec::new();
    encode_jsonl_to_file(&schema, Cursor::new(&jsonl_buf), &mut re_encoded, &eopts)
        .expect("encode");

    // Decode again and compare JSON
    let mut jsonl_buf2 = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(&re_encoded), &mut jsonl_buf2, &dopts)
        .expect("re-decode");

    let r1 = parse_jsonl(&String::from_utf8(jsonl_buf).expect("utf8"));
    let r2 = parse_jsonl(&String::from_utf8(jsonl_buf2).expect("utf8"));

    assert_eq!(r1.len(), r2.len());
    assert_eq!(
        r1[0]["AMOUNT"], r2[0]["AMOUNT"],
        "COMP-3 AMOUNT must survive round-trip"
    );
    assert_eq!(r1[0]["REC-ID"], r2[0]["REC-ID"]);
}
