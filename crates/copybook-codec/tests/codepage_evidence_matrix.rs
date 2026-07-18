// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]
//! Consolidated per-codepage evidence matrix (issue #573).
//!
//! Prior coverage already exercises the charset byte tables
//! (`crates/copybook-charset/tests/*`), fixed-format single-record decode/encode
//! (`codepage_comprehensive.rs`), and zoned-overpunch behavior by codepage
//! (`decimal_edge_cases.rs`). The remaining evidence plane called out in #573 is
//! a single per-codepage scenario table that links
//! **parse → layout → decode → encode → rejection** across *both* `Fixed` and
//! `RDW` record framing, driven through the streaming `decode_file_to_jsonl` /
//! `encode_jsonl_to_file` entry points as well as the single-record API.
//!
//! Two orthogonal discriminators pin down "the requested codepage really
//! governed the conversion":
//!
//! * **Decode discriminator** — one raw EBCDIC byte (`probe_byte`) that decodes
//!   to a *different* character (`probe_ch`) under each codepage (e.g. byte
//!   `0x4A` is `¢` in CP037 but `Ä` in CP273 and `[` in CP500).
//! * **Round-trip discriminator** — the ASCII character `[` (`RT_CH`), which
//!   maps to a *different* EBCDIC byte (`rt_byte`) under each codepage. `[` is a
//!   single UTF-8 byte, so it also round-trips cleanly through the encode
//!   capacity check for a one-byte `PIC X(1)` field (unlike a national
//!   character such as `¢`, which is two UTF-8 bytes — see
//!   `encode_capacity_is_measured_in_utf8_bytes`).

use copybook_codec::charset::{ebcdic_to_utf8, utf8_to_ebcdic};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, UnmappablePolicy,
    decode_file_to_jsonl, decode_record, encode_jsonl_to_file, encode_record,
};
use copybook_core::{ErrorCode, parse_copybook};
use std::io::Cursor;

/// One-field copybook used across the matrix: a single alphanumeric byte, so the
/// record LRECL is exactly one byte regardless of codepage.
const SIG_COPYBOOK: &str = "01 REC.\n   05 SIG PIC X(1).";

/// The ASCII round-trip character shared by every row (see module docs).
const RT_CH: &str = "[";

/// Per-codepage signature row.
struct Signature {
    cp: Codepage,
    /// Raw byte whose decoded character discriminates this codepage.
    probe_byte: u8,
    /// Expected decode of `probe_byte` under `cp`.
    probe_ch: &'static str,
    /// EBCDIC byte that `RT_CH` (`[`) maps to under `cp`.
    rt_byte: u8,
}

const SIGNATURES: &[Signature] = &[
    // CP037: 0x4A = ¢, and `[` lives at 0xBA.
    Signature {
        cp: Codepage::CP037,
        probe_byte: 0x4A,
        probe_ch: "¢",
        rt_byte: 0xBA,
    },
    // CP273: 0x4A = Ä (German A-umlaut), and `[` lives at 0x63.
    Signature {
        cp: Codepage::CP273,
        probe_byte: 0x4A,
        probe_ch: "Ä",
        rt_byte: 0x63,
    },
    // CP500: 0x4A = [ itself (so `[` maps back to 0x4A).
    Signature {
        cp: Codepage::CP500,
        probe_byte: 0x4A,
        probe_ch: "[",
        rt_byte: 0x4A,
    },
    // CP1047: 0xBA = Ý (where CP037 has [), and `[` lives at 0xAD.
    Signature {
        cp: Codepage::CP1047,
        probe_byte: 0xBA,
        probe_ch: "Ý",
        rt_byte: 0xAD,
    },
    // CP1140: 0xFF = € (the single byte differing from CP037); `[` at 0xBA.
    Signature {
        cp: Codepage::CP1140,
        probe_byte: 0xFF,
        probe_ch: "€",
        rt_byte: 0xBA,
    },
];

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn decode_opts(cp: Codepage, format: RecordFormat) -> DecodeOptions {
    DecodeOptions::new()
        .with_format(format)
        .with_codepage(cp)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(false)
}

fn encode_opts(cp: Codepage, format: RecordFormat) -> EncodeOptions {
    EncodeOptions::new().with_format(format).with_codepage(cp)
}

/// Wrap a payload in a canonical RDW frame: 2-byte big-endian length, 2 reserved
/// bytes (zero), then the payload.
fn rdw_frame(payload: &[u8]) -> Vec<u8> {
    let len = u16::try_from(payload.len()).expect("payload fits in u16");
    let mut framed = Vec::with_capacity(payload.len() + 4);
    framed.extend_from_slice(&len.to_be_bytes());
    framed.extend_from_slice(&[0x00, 0x00]);
    framed.extend_from_slice(payload);
    framed
}

/// Extract the `SIG` field value from the first JSONL record line. Handles both
/// the flat envelope (`emit_meta(false)`) and a nested `fields` envelope.
fn sig_from_jsonl(jsonl: &[u8]) -> String {
    let text = String::from_utf8(jsonl.to_vec()).expect("jsonl output is valid UTF-8");
    let line = text.lines().next().expect("at least one JSONL record");
    let value: serde_json::Value = serde_json::from_str(line).expect("valid JSON line");
    let field = value
        .get("SIG")
        .or_else(|| value.get("fields").and_then(|fields| fields.get("SIG")))
        .and_then(serde_json::Value::as_str);
    match field {
        Some(text) => text.to_owned(),
        None => panic!("SIG field missing in JSONL line: {line}"),
    }
}

// ===========================================================================
// Charset plane: the signature invariants the rest of the matrix relies on.
// ===========================================================================

#[test]
fn charset_signatures_hold() {
    for sig in SIGNATURES {
        // Decode discriminator.
        let decoded = ebcdic_to_utf8(&[sig.probe_byte], sig.cp, UnmappablePolicy::Error)
            .unwrap_or_else(|e| {
                panic!(
                    "decode 0x{:02X} under {} failed: {e}",
                    sig.probe_byte, sig.cp
                )
            });
        assert_eq!(
            decoded, sig.probe_ch,
            "byte 0x{:02X} must decode to {:?} under {}",
            sig.probe_byte, sig.probe_ch, sig.cp
        );

        // Round-trip discriminator: `[` maps to rt_byte, and back.
        let encoded = utf8_to_ebcdic(RT_CH, sig.cp)
            .unwrap_or_else(|e| panic!("encode {RT_CH:?} under {} failed: {e}", sig.cp));
        assert_eq!(
            encoded,
            vec![sig.rt_byte],
            "{RT_CH:?} must encode to 0x{:02X} under {}",
            sig.rt_byte,
            sig.cp
        );
        let back =
            ebcdic_to_utf8(&[sig.rt_byte], sig.cp, UnmappablePolicy::Error).unwrap_or_else(|e| {
                panic!("decode 0x{:02X} under {} failed: {e}", sig.rt_byte, sig.cp)
            });
        assert_eq!(
            back, RT_CH,
            "0x{:02X} must decode to {RT_CH:?} under {}",
            sig.rt_byte, sig.cp
        );
    }
}

// ===========================================================================
// Parse / layout plane: the schema (and thus LRECL/layout) is codepage-
// independent, so a fixed stream of N probe bytes yields exactly N records
// under every codepage, each decoding to that codepage's probe character.
// ===========================================================================

#[test]
fn parse_layout_stable_across_codepages() {
    let schema = parse_copybook(SIG_COPYBOOK).expect("copybook parses");

    for sig in SIGNATURES {
        let payload = vec![sig.probe_byte; 3];
        let mut output = Vec::new();
        let summary = decode_file_to_jsonl(
            &schema,
            Cursor::new(payload),
            &mut output,
            &decode_opts(sig.cp, RecordFormat::Fixed),
        )
        .unwrap_or_else(|e| panic!("fixed decode under {} failed: {e}", sig.cp));

        assert_eq!(
            summary.records_processed, 3,
            "1-byte LRECL layout must yield 3 records under {}",
            sig.cp
        );

        let text = String::from_utf8(output).expect("jsonl utf-8");
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines.len(), 3, "expected 3 JSONL lines under {}", sig.cp);
        for line in lines {
            assert_eq!(
                sig_from_jsonl(line.as_bytes()),
                sig.probe_ch,
                "each record must decode to {:?} under {}",
                sig.probe_ch,
                sig.cp
            );
        }
    }
}

// ===========================================================================
// Single-record decode/encode plane (Fixed), using the ASCII round-trip char.
// ===========================================================================

#[test]
fn single_record_decode_encode_fixed() {
    let schema = parse_copybook(SIG_COPYBOOK).expect("copybook parses");

    for sig in SIGNATURES {
        let json = decode_record(
            &schema,
            &[sig.rt_byte],
            &decode_opts(sig.cp, RecordFormat::Fixed),
        )
        .unwrap_or_else(|e| panic!("decode_record under {} failed: {e}", sig.cp));
        assert_eq!(
            json.get("SIG").and_then(serde_json::Value::as_str),
            Some(RT_CH),
            "single-record decode of 0x{:02X} must yield {RT_CH:?} under {}",
            sig.rt_byte,
            sig.cp
        );

        let encoded = encode_record(&schema, &json, &encode_opts(sig.cp, RecordFormat::Fixed))
            .unwrap_or_else(|e| panic!("encode_record under {} failed: {e}", sig.cp));
        assert_eq!(
            encoded,
            vec![sig.rt_byte],
            "single-record encode of {RT_CH:?} must reproduce 0x{:02X} under {}",
            sig.rt_byte,
            sig.cp
        );
    }
}

// ===========================================================================
// Streaming decode → encode round-trip plane (Fixed).
// ===========================================================================

#[test]
fn streaming_roundtrip_fixed() {
    let schema = parse_copybook(SIG_COPYBOOK).expect("copybook parses");

    for sig in SIGNATURES {
        let original = vec![sig.rt_byte];

        let mut jsonl = Vec::new();
        decode_file_to_jsonl(
            &schema,
            Cursor::new(original.clone()),
            &mut jsonl,
            &decode_opts(sig.cp, RecordFormat::Fixed),
        )
        .unwrap_or_else(|e| panic!("fixed decode under {} failed: {e}", sig.cp));
        assert_eq!(
            sig_from_jsonl(&jsonl),
            RT_CH,
            "fixed decode under {}",
            sig.cp
        );

        let mut reencoded = Vec::new();
        encode_jsonl_to_file(
            &schema,
            Cursor::new(jsonl),
            &mut reencoded,
            &encode_opts(sig.cp, RecordFormat::Fixed),
        )
        .unwrap_or_else(|e| panic!("fixed encode under {} failed: {e}", sig.cp));
        assert_eq!(
            reencoded, original,
            "fixed streaming round-trip must be byte-identical under {}",
            sig.cp
        );
    }
}

// ===========================================================================
// Streaming decode → encode round-trip plane (RDW) — the primary #573 gap.
// ===========================================================================

#[test]
fn streaming_roundtrip_rdw() {
    let schema = parse_copybook(SIG_COPYBOOK).expect("copybook parses");

    for sig in SIGNATURES {
        let framed = rdw_frame(&[sig.rt_byte]);

        let mut jsonl = Vec::new();
        decode_file_to_jsonl(
            &schema,
            Cursor::new(framed.clone()),
            &mut jsonl,
            &decode_opts(sig.cp, RecordFormat::RDW),
        )
        .unwrap_or_else(|e| panic!("RDW decode under {} failed: {e}", sig.cp));
        assert_eq!(sig_from_jsonl(&jsonl), RT_CH, "RDW decode under {}", sig.cp);

        let mut reencoded = Vec::new();
        encode_jsonl_to_file(
            &schema,
            Cursor::new(jsonl),
            &mut reencoded,
            &encode_opts(sig.cp, RecordFormat::RDW),
        )
        .unwrap_or_else(|e| panic!("RDW encode under {} failed: {e}", sig.cp));
        assert_eq!(
            reencoded, framed,
            "RDW streaming round-trip must reproduce header+payload under {}",
            sig.cp
        );
    }
}

// ===========================================================================
// Fixed-vs-RDW linkage: the same payload byte decodes to the same character
// regardless of framing, for every codepage.
// ===========================================================================

#[test]
fn fixed_and_rdw_decode_agree() {
    let schema = parse_copybook(SIG_COPYBOOK).expect("copybook parses");

    for sig in SIGNATURES {
        let mut fixed_out = Vec::new();
        decode_file_to_jsonl(
            &schema,
            Cursor::new(vec![sig.probe_byte]),
            &mut fixed_out,
            &decode_opts(sig.cp, RecordFormat::Fixed),
        )
        .unwrap_or_else(|e| panic!("fixed decode under {} failed: {e}", sig.cp));

        let mut rdw_out = Vec::new();
        decode_file_to_jsonl(
            &schema,
            Cursor::new(rdw_frame(&[sig.probe_byte])),
            &mut rdw_out,
            &decode_opts(sig.cp, RecordFormat::RDW),
        )
        .unwrap_or_else(|e| panic!("RDW decode under {} failed: {e}", sig.cp));

        assert_eq!(
            sig_from_jsonl(&fixed_out),
            sig_from_jsonl(&rdw_out),
            "Fixed and RDW framing must decode identically under {}",
            sig.cp
        );
        assert_eq!(
            sig_from_jsonl(&fixed_out),
            sig.probe_ch,
            "framing-agnostic decode must yield {:?} under {}",
            sig.probe_ch,
            sig.cp
        );
    }
}

// ===========================================================================
// Rejection plane: deliberate, stable rejections.
// ===========================================================================

#[test]
fn decode_rejects_unmappable_byte_all_codepages() {
    let schema = parse_copybook(SIG_COPYBOOK).expect("copybook parses");

    for sig in SIGNATURES {
        // 0x00 (EBCDIC NUL / U+0000) is an unmappable control byte in every
        // supported codepage; with the Error policy the decode must reject it.
        let opts = decode_opts(sig.cp, RecordFormat::Fixed)
            .with_unmappable_policy(UnmappablePolicy::Error);
        let err = decode_record(&schema, &[0x00], &opts)
            .expect_err("unmappable NUL byte must be rejected under Error policy");
        assert_eq!(
            err.code,
            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
            "unmappable decode must surface CBKC301 under {}",
            sig.cp
        );
    }
}

#[test]
fn encode_euro_is_specific_to_cp1140() {
    // The Euro sign is representable only in CP1140 among the supported EBCDIC
    // codepages. Encoding it into a field wide enough to pass the (UTF-8-byte)
    // length check therefore succeeds under CP1140 and is rejected as an
    // unmappable character (CBKC301) under every other codepage.
    let schema = parse_copybook("01 REC.\n   05 EU PIC X(4).").expect("copybook parses");
    let json = serde_json::json!({ "EU": "€" });

    for sig in SIGNATURES {
        let result = encode_record(&schema, &json, &encode_opts(sig.cp, RecordFormat::Fixed));
        if sig.cp == Codepage::CP1140 {
            let encoded = result.unwrap_or_else(|e| panic!("CP1140 must encode €: {e}"));
            assert_eq!(encoded[0], 0xFF, "€ must encode to 0xFF under CP1140");
        } else {
            let err = result.expect_err("€ must be rejected outside CP1140");
            assert_eq!(
                err.code,
                ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
                "unmappable € must surface CBKC301 under {}",
                sig.cp
            );
        }
    }
}

// ===========================================================================
// Documented asymmetry: the encode capacity check counts UTF-8 bytes, not
// characters, so a single-byte alphanumeric field cannot hold a national
// character that occupies more than one UTF-8 byte even though it maps to a
// single EBCDIC byte. This pins the current, observed contract.
// ===========================================================================

#[test]
fn encode_capacity_is_measured_in_utf8_bytes() {
    let schema = parse_copybook(SIG_COPYBOOK).expect("copybook parses");
    // "¢" is one character but two UTF-8 bytes; PIC X(1) has capacity one byte.
    let json = serde_json::json!({ "SIG": "¢" });
    let err = encode_record(
        &schema,
        &json,
        &encode_opts(Codepage::CP037, RecordFormat::Fixed),
    )
    .expect_err("2-byte UTF-8 char must not fit a 1-byte field");
    assert_eq!(err.code, ErrorCode::CBKE515_STRING_LENGTH_VIOLATION);
}
