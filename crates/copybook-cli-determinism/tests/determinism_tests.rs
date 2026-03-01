// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for the `copybook-cli-determinism` crate.
//!
//! Covers: verdict/exit-code logic, hash truncation, human/JSON rendering,
//! option builders, schema loading, decode/encode/round-trip execution,
//! and error paths (missing files, empty JSON, invalid schemas).

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_cli_determinism::{
    BLAKE3_HEX_LEN, CommonDeterminismArgs, DEFAULT_MAX_DIFFS, DecodeDeterminismArgs,
    DeterminismCommand, DeterminismModeCommand, DeterminismRun, DeterminismVerdict,
    EncodeDeterminismArgs, OutputFormat, RoundTripDeterminismArgs, build_decode_options,
    build_encode_options, load_schema, render_human_result, render_json_result, run, truncate_hash,
};
use copybook_codec::{Codepage, JsonNumberMode, RecordFormat};
use copybook_determinism::{ByteDiff, DeterminismMode, DeterminismResult};
use std::fs;
use std::path::PathBuf;
use tempfile::tempdir;

// ── Fixtures ──────────────────────────────────────────────────────────

const SIMPLE_COPYBOOK: &str = r"
       01 RECORD.
          05 FIELD PIC X(5).
";

/// CP037-encoded "ABCDE"
const SIMPLE_DATA_CP037: [u8; 5] = [0xC1, 0xC2, 0xC3, 0xC4, 0xC5];

const NUMERIC_COPYBOOK: &str = r"
       01 RECORD.
          05 AMOUNT PIC 9(5)V99.
";

/// CP037-encoded "1234567" (unsigned display numeric, 7 bytes)
const NUMERIC_DATA_CP037: [u8; 7] = [0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7];

fn write_fixture(
    dir: &tempfile::TempDir,
    copybook: &str,
    data: &[u8],
    json_line: &str,
) -> (PathBuf, PathBuf, PathBuf) {
    let cpy = dir.path().join("schema.cpy");
    let bin = dir.path().join("input.bin");
    let jsonl = dir.path().join("input.jsonl");
    fs::write(&cpy, copybook).expect("write copybook");
    fs::write(&bin, data).expect("write data");
    fs::write(&jsonl, format!("{json_line}\n")).expect("write json");
    (cpy, bin, jsonl)
}

fn make_common(copybook: PathBuf) -> CommonDeterminismArgs {
    CommonDeterminismArgs {
        copybook,
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037,
        json_number: JsonNumberMode::Lossless,
        emit_meta: false,
        output: OutputFormat::Human,
        max_diffs: DEFAULT_MAX_DIFFS,
    }
}

// ═══════════════════════════════════════════════════════════════════════
// 1. Verdict & exit-code
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn verdict_deterministic_exit_code_is_zero() {
    assert_eq!(DeterminismVerdict::Deterministic.exit_code(), 0);
}

#[test]
fn verdict_non_deterministic_exit_code_is_two() {
    assert_eq!(DeterminismVerdict::NonDeterministic.exit_code(), 2);
}

#[test]
fn determinism_run_exit_code_delegates() {
    let pass = DeterminismRun {
        verdict: DeterminismVerdict::Deterministic,
        output: String::new(),
    };
    let fail = DeterminismRun {
        verdict: DeterminismVerdict::NonDeterministic,
        output: "diff".into(),
    };
    assert_eq!(pass.exit_code(), 0);
    assert_eq!(fail.exit_code(), 2);
}

#[test]
fn verdict_from_result_deterministic() {
    let result = DeterminismResult {
        mode: DeterminismMode::DecodeOnly,
        round1_hash: "a".repeat(64),
        round2_hash: "a".repeat(64),
        is_deterministic: true,
        byte_differences: None,
    };
    let run_result = render_human_result(&result, 10);
    assert!(run_result.contains("DETERMINISTIC"));
}

#[test]
fn verdict_from_result_non_deterministic() {
    let result = DeterminismResult {
        mode: DeterminismMode::EncodeOnly,
        round1_hash: "a".repeat(64),
        round2_hash: "b".repeat(64),
        is_deterministic: false,
        byte_differences: Some(vec![ByteDiff {
            offset: 0,
            round1_byte: 0x00,
            round2_byte: 0xFF,
        }]),
    };
    let run_result = render_human_result(&result, 10);
    assert!(run_result.contains("NON-DETERMINISTIC"));
}

// ═══════════════════════════════════════════════════════════════════════
// 2. Hash truncation
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn truncate_hash_long_adds_ellipsis() {
    let hash = "0123456789abcdef0123456789abcdef";
    assert_eq!(truncate_hash(hash), "0123456789abcdef...");
}

#[test]
fn truncate_hash_short_unchanged() {
    assert_eq!(truncate_hash("abc"), "abc");
}

#[test]
fn truncate_hash_empty() {
    assert_eq!(truncate_hash(""), "");
}

#[test]
fn truncate_hash_exactly_16_no_ellipsis() {
    let hash = "0123456789abcdef";
    assert_eq!(truncate_hash(hash), hash);
}

#[test]
fn truncate_hash_exactly_17_adds_ellipsis() {
    let hash = "0123456789abcdef0";
    assert_eq!(truncate_hash(hash), "0123456789abcdef...");
}

#[test]
fn truncate_hash_preserves_prefix() {
    let hash = "deadbeefcafebabe1234567890abcdef";
    let truncated = truncate_hash(hash);
    assert!(truncated.starts_with("deadbeefcafebabe"));
    assert!(truncated.ends_with("..."));
    assert_eq!(truncated.len(), 19);
}

// ═══════════════════════════════════════════════════════════════════════
// 3. Human rendering
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn human_render_deterministic_shows_checkmark() {
    let result = DeterminismResult {
        mode: DeterminismMode::DecodeOnly,
        round1_hash: "f".repeat(64),
        round2_hash: "f".repeat(64),
        is_deterministic: true,
        byte_differences: None,
    };
    let output = render_human_result(&result, 100);
    assert!(output.contains("✅ DETERMINISTIC"));
    assert!(output.contains("Byte differences: none"));
    assert!(!output.contains("NON-DETERMINISTIC"));
}

#[test]
fn human_render_non_deterministic_shows_cross() {
    let result = DeterminismResult {
        mode: DeterminismMode::EncodeOnly,
        round1_hash: "a".repeat(64),
        round2_hash: "b".repeat(64),
        is_deterministic: false,
        byte_differences: Some(vec![ByteDiff {
            offset: 42,
            round1_byte: 0xAA,
            round2_byte: 0xBB,
        }]),
    };
    let output = render_human_result(&result, 100);
    assert!(output.contains("❌ NON-DETERMINISTIC"));
    assert!(output.contains("Byte differences: 1 total"));
    assert!(output.contains("0x002A")); // offset 42
    assert!(output.contains("0xAA"));
    assert!(output.contains("0xBB"));
}

#[test]
fn human_render_shows_mode_name_for_all_modes() {
    for mode in [
        DeterminismMode::DecodeOnly,
        DeterminismMode::EncodeOnly,
        DeterminismMode::RoundTrip,
    ] {
        let result = DeterminismResult {
            mode,
            round1_hash: "c".repeat(64),
            round2_hash: "c".repeat(64),
            is_deterministic: true,
            byte_differences: None,
        };
        let output = render_human_result(&result, 10);
        assert!(output.contains("Determinism mode:"), "missing mode header");
        assert!(output.contains("Round 1 hash:"));
        assert!(output.contains("Round 2 hash:"));
    }
}

#[test]
fn human_render_diff_table_has_headers() {
    let result = DeterminismResult {
        mode: DeterminismMode::DecodeOnly,
        round1_hash: "0".repeat(64),
        round2_hash: "1".repeat(64),
        is_deterministic: false,
        byte_differences: Some(vec![ByteDiff {
            offset: 0,
            round1_byte: 0x00,
            round2_byte: 0x01,
        }]),
    };
    let output = render_human_result(&result, 10);
    assert!(output.contains("Offset"));
    assert!(output.contains("Round1"));
    assert!(output.contains("Round2"));
    assert!(output.contains("------"));
}

#[test]
fn human_render_truncates_diffs_with_max_diffs() {
    let diffs: Vec<ByteDiff> = (0..10)
        .map(|i| ByteDiff {
            offset: i,
            round1_byte: i as u8,
            round2_byte: (i as u8).wrapping_add(1),
        })
        .collect();
    let result = DeterminismResult {
        mode: DeterminismMode::DecodeOnly,
        round1_hash: "a".repeat(64),
        round2_hash: "b".repeat(64),
        is_deterministic: false,
        byte_differences: Some(diffs),
    };
    let output = render_human_result(&result, 3);
    assert!(output.contains("Byte differences: 10 total"));
    assert!(output.contains("... 7 more differences not shown"));
}

#[test]
fn human_render_max_diffs_zero_shows_no_rows() {
    let result = DeterminismResult {
        mode: DeterminismMode::DecodeOnly,
        round1_hash: "a".repeat(64),
        round2_hash: "b".repeat(64),
        is_deterministic: false,
        byte_differences: Some(vec![ByteDiff {
            offset: 0,
            round1_byte: 0x00,
            round2_byte: 0xFF,
        }]),
    };
    let output = render_human_result(&result, 0);
    assert!(output.contains("Byte differences: 1 total"));
    assert!(output.contains("... 1 more differences not shown"));
    // No individual diff rows shown when max_diffs is 0
    assert!(!output.contains("0x0000"));
}

#[test]
fn human_render_empty_byte_differences_vec() {
    let result = DeterminismResult {
        mode: DeterminismMode::DecodeOnly,
        round1_hash: "a".repeat(64),
        round2_hash: "b".repeat(64),
        is_deterministic: false,
        byte_differences: Some(vec![]),
    };
    let output = render_human_result(&result, 10);
    assert!(output.contains("Byte differences: 0 total"));
}

// ═══════════════════════════════════════════════════════════════════════
// 4. JSON rendering
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn json_render_deterministic_result() {
    let result = DeterminismResult {
        mode: DeterminismMode::RoundTrip,
        round1_hash: "c".repeat(64),
        round2_hash: "c".repeat(64),
        is_deterministic: true,
        byte_differences: None,
    };
    let json = render_json_result(&result).unwrap();
    let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
    assert_eq!(parsed["is_deterministic"], true);
    assert_eq!(parsed["mode"], "round_trip");
    // byte_differences should be absent (skip_serializing_if = None)
    assert!(parsed.get("byte_differences").is_none());
}

#[test]
fn json_render_non_deterministic_includes_diffs() {
    let result = DeterminismResult {
        mode: DeterminismMode::DecodeOnly,
        round1_hash: "d".repeat(64),
        round2_hash: "e".repeat(64),
        is_deterministic: false,
        byte_differences: Some(vec![
            ByteDiff {
                offset: 5,
                round1_byte: 0x10,
                round2_byte: 0x20,
            },
            ByteDiff {
                offset: 12,
                round1_byte: 0xFF,
                round2_byte: 0x00,
            },
        ]),
    };
    let json = render_json_result(&result).unwrap();
    let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
    assert_eq!(parsed["is_deterministic"], false);
    let diffs = parsed["byte_differences"].as_array().unwrap();
    assert_eq!(diffs.len(), 2);
    assert_eq!(diffs[0]["offset"], 5);
    assert_eq!(diffs[1]["offset"], 12);
}

#[test]
fn json_render_round_trips_through_serde() {
    let result = DeterminismResult {
        mode: DeterminismMode::EncodeOnly,
        round1_hash: "ab".repeat(32),
        round2_hash: "ab".repeat(32),
        is_deterministic: true,
        byte_differences: None,
    };
    let json = render_json_result(&result).unwrap();
    let deserialized: DeterminismResult = serde_json::from_str(&json).unwrap();
    assert_eq!(deserialized.mode, result.mode);
    assert_eq!(deserialized.is_deterministic, result.is_deterministic);
    assert_eq!(deserialized.round1_hash, result.round1_hash);
}

#[test]
fn json_render_mode_names_are_snake_case() {
    for (mode, expected) in [
        (DeterminismMode::DecodeOnly, "decode_only"),
        (DeterminismMode::EncodeOnly, "encode_only"),
        (DeterminismMode::RoundTrip, "round_trip"),
    ] {
        let result = DeterminismResult {
            mode,
            round1_hash: "0".repeat(64),
            round2_hash: "0".repeat(64),
            is_deterministic: true,
            byte_differences: None,
        };
        let json = render_json_result(&result).unwrap();
        assert!(
            json.contains(expected),
            "expected mode {expected} in JSON: {json}"
        );
    }
}

// ═══════════════════════════════════════════════════════════════════════
// 5. Option builders
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn build_decode_options_uses_common_args() {
    let common = CommonDeterminismArgs {
        copybook: PathBuf::from("dummy.cpy"),
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037,
        json_number: JsonNumberMode::Lossless,
        emit_meta: true,
        output: OutputFormat::Human,
        max_diffs: 50,
    };
    let opts = build_decode_options(&common);
    // Verify the options were built without panicking; exact field inspection
    // requires public accessors which may not exist, so the test validates construction.
    let _ = opts;
}

#[test]
fn build_encode_options_uses_common_args() {
    let common = CommonDeterminismArgs {
        copybook: PathBuf::from("dummy.cpy"),
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037,
        json_number: JsonNumberMode::Native,
        emit_meta: false,
        output: OutputFormat::Json,
        max_diffs: 10,
    };
    let opts = build_encode_options(&common);
    let _ = opts;
}

#[test]
fn build_decode_options_with_native_json_number() {
    let common = CommonDeterminismArgs {
        copybook: PathBuf::from("dummy.cpy"),
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037,
        json_number: JsonNumberMode::Native,
        emit_meta: false,
        output: OutputFormat::Human,
        max_diffs: DEFAULT_MAX_DIFFS,
    };
    let opts = build_decode_options(&common);
    let _ = opts;
}

// ═══════════════════════════════════════════════════════════════════════
// 6. Schema loading
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn load_schema_success() {
    let tmp = tempdir().expect("tempdir");
    let path = tmp.path().join("test.cpy");
    fs::write(&path, SIMPLE_COPYBOOK).expect("write");
    let schema = load_schema(&path).expect("load schema");
    assert!(!schema.fields.is_empty());
}

#[test]
fn load_schema_nonexistent_file_returns_error() {
    let result = load_schema(&PathBuf::from("nonexistent_schema_xyz.cpy"));
    assert!(result.is_err());
}

#[test]
fn load_schema_invalid_copybook_returns_error() {
    let tmp = tempdir().expect("tempdir");
    let path = tmp.path().join("bad.cpy");
    fs::write(&path, "THIS IS NOT A VALID COPYBOOK").expect("write");
    let result = load_schema(&path);
    assert!(result.is_err());
}

#[test]
fn load_schema_empty_file_returns_error() {
    let tmp = tempdir().expect("tempdir");
    let path = tmp.path().join("empty.cpy");
    fs::write(&path, "").expect("write");
    let result = load_schema(&path);
    assert!(result.is_err());
}

// ═══════════════════════════════════════════════════════════════════════
// 7. Decode determinism (end-to-end via `run`)
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn decode_determinism_human_output_is_deterministic() {
    let tmp = tempdir().expect("tempdir");
    let (cpy, bin, _) = write_fixture(
        &tmp,
        SIMPLE_COPYBOOK,
        &SIMPLE_DATA_CP037,
        r#"{"FIELD":"ABCDE"}"#,
    );
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::Decode(DecodeDeterminismArgs {
            common: make_common(cpy),
            data: bin,
        }),
    };
    let result = run(&cmd).expect("run decode");
    assert_eq!(result.verdict, DeterminismVerdict::Deterministic);
    assert_eq!(result.exit_code(), 0);
    assert!(result.output.contains("DETERMINISTIC"));
}

#[test]
fn decode_determinism_json_output_is_valid_json() {
    let tmp = tempdir().expect("tempdir");
    let (cpy, bin, _) = write_fixture(
        &tmp,
        SIMPLE_COPYBOOK,
        &SIMPLE_DATA_CP037,
        r#"{"FIELD":"ABCDE"}"#,
    );
    let mut common = make_common(cpy);
    common.output = OutputFormat::Json;
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::Decode(DecodeDeterminismArgs { common, data: bin }),
    };
    let result = run(&cmd).expect("run decode");
    let parsed: serde_json::Value = serde_json::from_str(&result.output).expect("valid json");
    assert_eq!(parsed["is_deterministic"], true);
    assert_eq!(parsed["round1_hash"], parsed["round2_hash"]);
}

#[test]
fn decode_determinism_hashes_are_64_hex_chars() {
    let tmp = tempdir().expect("tempdir");
    let (cpy, bin, _) = write_fixture(
        &tmp,
        SIMPLE_COPYBOOK,
        &SIMPLE_DATA_CP037,
        r#"{"FIELD":"ABCDE"}"#,
    );
    let mut common = make_common(cpy);
    common.output = OutputFormat::Json;
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::Decode(DecodeDeterminismArgs { common, data: bin }),
    };
    let result = run(&cmd).expect("run decode");
    let parsed: serde_json::Value = serde_json::from_str(&result.output).unwrap();
    let hash = parsed["round1_hash"].as_str().unwrap();
    assert_eq!(hash.len(), BLAKE3_HEX_LEN);
    assert!(hash.chars().all(|c| c.is_ascii_hexdigit()));
}

#[test]
fn decode_determinism_with_numeric_copybook() {
    let tmp = tempdir().expect("tempdir");
    let (cpy, bin, _) = write_fixture(
        &tmp,
        NUMERIC_COPYBOOK,
        &NUMERIC_DATA_CP037,
        r#"{"AMOUNT":"12345.67"}"#,
    );
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::Decode(DecodeDeterminismArgs {
            common: make_common(cpy),
            data: bin,
        }),
    };
    let result = run(&cmd).expect("run decode numeric");
    assert_eq!(result.verdict, DeterminismVerdict::Deterministic);
}

#[test]
fn decode_determinism_missing_data_file_returns_error() {
    let tmp = tempdir().expect("tempdir");
    let cpy = tmp.path().join("schema.cpy");
    fs::write(&cpy, SIMPLE_COPYBOOK).expect("write");
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::Decode(DecodeDeterminismArgs {
            common: make_common(cpy),
            data: PathBuf::from("nonexistent_data.bin"),
        }),
    };
    assert!(run(&cmd).is_err());
}

#[test]
fn decode_determinism_missing_schema_returns_error() {
    let tmp = tempdir().expect("tempdir");
    let bin = tmp.path().join("input.bin");
    fs::write(&bin, &SIMPLE_DATA_CP037).expect("write");
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::Decode(DecodeDeterminismArgs {
            common: make_common(PathBuf::from("no_such_schema.cpy")),
            data: bin,
        }),
    };
    assert!(run(&cmd).is_err());
}

// ═══════════════════════════════════════════════════════════════════════
// 8. Encode determinism (end-to-end via `run`)
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn encode_determinism_is_deterministic() {
    let tmp = tempdir().expect("tempdir");
    let (cpy, _, jsonl) = write_fixture(
        &tmp,
        SIMPLE_COPYBOOK,
        &SIMPLE_DATA_CP037,
        r#"{"FIELD":"ABCDE"}"#,
    );
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::Encode(EncodeDeterminismArgs {
            common: make_common(cpy),
            json: jsonl,
        }),
    };
    let result = run(&cmd).expect("run encode");
    assert_eq!(result.verdict, DeterminismVerdict::Deterministic);
    assert!(result.output.contains("Round 1 hash"));
}

#[test]
fn encode_determinism_json_output_format() {
    let tmp = tempdir().expect("tempdir");
    let (cpy, _, jsonl) = write_fixture(
        &tmp,
        SIMPLE_COPYBOOK,
        &SIMPLE_DATA_CP037,
        r#"{"FIELD":"ABCDE"}"#,
    );
    let mut common = make_common(cpy);
    common.output = OutputFormat::Json;
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::Encode(EncodeDeterminismArgs {
            common,
            json: jsonl,
        }),
    };
    let result = run(&cmd).expect("run encode json");
    let parsed: serde_json::Value = serde_json::from_str(&result.output).expect("valid json");
    assert_eq!(parsed["is_deterministic"], true);
    assert_eq!(parsed["mode"], "encode_only");
}

#[test]
fn encode_determinism_empty_json_file_returns_error() {
    let tmp = tempdir().expect("tempdir");
    let cpy = tmp.path().join("schema.cpy");
    let jsonl = tmp.path().join("empty.jsonl");
    fs::write(&cpy, SIMPLE_COPYBOOK).expect("write cpy");
    fs::write(&jsonl, "").expect("write empty json");
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::Encode(EncodeDeterminismArgs {
            common: make_common(cpy),
            json: jsonl,
        }),
    };
    let err = run(&cmd).unwrap_err();
    let msg = format!("{err:#}");
    assert!(msg.contains("empty"), "expected 'empty' in error: {msg}");
}

#[test]
fn encode_determinism_invalid_json_returns_error() {
    let tmp = tempdir().expect("tempdir");
    let cpy = tmp.path().join("schema.cpy");
    let jsonl = tmp.path().join("bad.jsonl");
    fs::write(&cpy, SIMPLE_COPYBOOK).expect("write cpy");
    fs::write(&jsonl, "NOT VALID JSON\n").expect("write bad json");
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::Encode(EncodeDeterminismArgs {
            common: make_common(cpy),
            json: jsonl,
        }),
    };
    assert!(run(&cmd).is_err());
}

#[test]
fn encode_determinism_missing_json_file_returns_error() {
    let tmp = tempdir().expect("tempdir");
    let cpy = tmp.path().join("schema.cpy");
    fs::write(&cpy, SIMPLE_COPYBOOK).expect("write cpy");
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::Encode(EncodeDeterminismArgs {
            common: make_common(cpy),
            json: PathBuf::from("nonexistent.jsonl"),
        }),
    };
    assert!(run(&cmd).is_err());
}

#[test]
fn encode_determinism_with_numeric_copybook() {
    let tmp = tempdir().expect("tempdir");
    let (cpy, _, jsonl) = write_fixture(
        &tmp,
        NUMERIC_COPYBOOK,
        &NUMERIC_DATA_CP037,
        r#"{"AMOUNT":"12345.67"}"#,
    );
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::Encode(EncodeDeterminismArgs {
            common: make_common(cpy),
            json: jsonl,
        }),
    };
    let result = run(&cmd).expect("run encode numeric");
    assert_eq!(result.verdict, DeterminismVerdict::Deterministic);
}

// ═══════════════════════════════════════════════════════════════════════
// 9. Round-trip determinism (end-to-end via `run`)
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn round_trip_determinism_is_deterministic() {
    let tmp = tempdir().expect("tempdir");
    let (cpy, bin, _) = write_fixture(
        &tmp,
        SIMPLE_COPYBOOK,
        &SIMPLE_DATA_CP037,
        r#"{"FIELD":"ABCDE"}"#,
    );
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::RoundTrip(RoundTripDeterminismArgs {
            common: make_common(cpy),
            data: bin,
        }),
    };
    let result = run(&cmd).expect("run round-trip");
    assert_eq!(result.verdict, DeterminismVerdict::Deterministic);
    assert_eq!(result.exit_code(), 0);
    assert!(result.output.contains("Byte differences"));
}

#[test]
fn round_trip_determinism_json_output() {
    let tmp = tempdir().expect("tempdir");
    let (cpy, bin, _) = write_fixture(
        &tmp,
        SIMPLE_COPYBOOK,
        &SIMPLE_DATA_CP037,
        r#"{"FIELD":"ABCDE"}"#,
    );
    let mut common = make_common(cpy);
    common.output = OutputFormat::Json;
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::RoundTrip(RoundTripDeterminismArgs { common, data: bin }),
    };
    let result = run(&cmd).expect("run round-trip json");
    let parsed: serde_json::Value = serde_json::from_str(&result.output).expect("valid json");
    assert_eq!(parsed["is_deterministic"], true);
    assert_eq!(parsed["mode"], "round_trip");
}

#[test]
fn round_trip_determinism_with_numeric_copybook() {
    let tmp = tempdir().expect("tempdir");
    let (cpy, bin, _) = write_fixture(
        &tmp,
        NUMERIC_COPYBOOK,
        &NUMERIC_DATA_CP037,
        r#"{"AMOUNT":"12345.67"}"#,
    );
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::RoundTrip(RoundTripDeterminismArgs {
            common: make_common(cpy),
            data: bin,
        }),
    };
    let result = run(&cmd).expect("run round-trip numeric");
    assert_eq!(result.verdict, DeterminismVerdict::Deterministic);
}

#[test]
fn round_trip_determinism_missing_data_returns_error() {
    let tmp = tempdir().expect("tempdir");
    let cpy = tmp.path().join("schema.cpy");
    fs::write(&cpy, SIMPLE_COPYBOOK).expect("write");
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::RoundTrip(RoundTripDeterminismArgs {
            common: make_common(cpy),
            data: PathBuf::from("no_such_file.bin"),
        }),
    };
    assert!(run(&cmd).is_err());
}

// ═══════════════════════════════════════════════════════════════════════
// 10. Configuration & option coverage
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn output_format_equality() {
    assert_eq!(OutputFormat::Human, OutputFormat::Human);
    assert_eq!(OutputFormat::Json, OutputFormat::Json);
    assert_ne!(OutputFormat::Human, OutputFormat::Json);
}

#[test]
fn common_args_emit_meta_flag_accepted() {
    let tmp = tempdir().expect("tempdir");
    let (cpy, bin, _) = write_fixture(
        &tmp,
        SIMPLE_COPYBOOK,
        &SIMPLE_DATA_CP037,
        r#"{"FIELD":"ABCDE"}"#,
    );
    let mut common = make_common(cpy);
    common.emit_meta = true;
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::Decode(DecodeDeterminismArgs { common, data: bin }),
    };
    let result = run(&cmd).expect("run with emit_meta");
    assert_eq!(result.verdict, DeterminismVerdict::Deterministic);
}

#[test]
fn common_args_custom_max_diffs() {
    let tmp = tempdir().expect("tempdir");
    let (cpy, bin, _) = write_fixture(
        &tmp,
        SIMPLE_COPYBOOK,
        &SIMPLE_DATA_CP037,
        r#"{"FIELD":"ABCDE"}"#,
    );
    let mut common = make_common(cpy);
    common.max_diffs = 5;
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::Decode(DecodeDeterminismArgs { common, data: bin }),
    };
    let result = run(&cmd).expect("run with custom max_diffs");
    assert_eq!(result.verdict, DeterminismVerdict::Deterministic);
}

#[test]
fn decode_twice_produces_same_hashes() {
    let tmp = tempdir().expect("tempdir");
    let (cpy, bin, _) = write_fixture(
        &tmp,
        SIMPLE_COPYBOOK,
        &SIMPLE_DATA_CP037,
        r#"{"FIELD":"ABCDE"}"#,
    );

    let mut common1 = make_common(cpy.clone());
    common1.output = OutputFormat::Json;
    let cmd1 = DeterminismCommand {
        mode: DeterminismModeCommand::Decode(DecodeDeterminismArgs {
            common: common1,
            data: bin.clone(),
        }),
    };

    let mut common2 = make_common(cpy);
    common2.output = OutputFormat::Json;
    let cmd2 = DeterminismCommand {
        mode: DeterminismModeCommand::Decode(DecodeDeterminismArgs {
            common: common2,
            data: bin,
        }),
    };

    let r1 = run(&cmd1).expect("first run");
    let r2 = run(&cmd2).expect("second run");

    let v1: serde_json::Value = serde_json::from_str(&r1.output).unwrap();
    let v2: serde_json::Value = serde_json::from_str(&r2.output).unwrap();

    assert_eq!(v1["round1_hash"], v2["round1_hash"]);
    assert_eq!(v1["round2_hash"], v2["round2_hash"]);
}

// ═══════════════════════════════════════════════════════════════════════
// 11. Constants re-exported correctly
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn blake3_hex_len_is_64() {
    assert_eq!(BLAKE3_HEX_LEN, 64);
}

#[test]
fn default_max_diffs_is_100() {
    assert_eq!(DEFAULT_MAX_DIFFS, 100);
}

// ═══════════════════════════════════════════════════════════════════════
// 12. Multi-field copybook
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn multi_field_decode_determinism() {
    let copybook = r"
       01 RECORD.
          05 NAME   PIC X(10).
          05 CODE   PIC X(3).
    ";
    // CP037 for "JOHN      ABC"  (10 + 3 = 13 bytes)
    let data: Vec<u8> = vec![
        0xD1, 0xD6, 0xC8, 0xD5, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, // JOHN______
        0xC1, 0xC2, 0xC3, // ABC
    ];
    let tmp = tempdir().expect("tempdir");
    let (cpy, bin, _) = write_fixture(
        &tmp,
        copybook,
        &data,
        r#"{"NAME":"JOHN      ","CODE":"ABC"}"#,
    );
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::Decode(DecodeDeterminismArgs {
            common: make_common(cpy),
            data: bin,
        }),
    };
    let result = run(&cmd).expect("multi-field decode");
    assert_eq!(result.verdict, DeterminismVerdict::Deterministic);
}

#[test]
fn multi_field_round_trip_determinism() {
    let copybook = r"
       01 RECORD.
          05 NAME   PIC X(10).
          05 CODE   PIC X(3).
    ";
    let data: Vec<u8> = vec![
        0xD1, 0xD6, 0xC8, 0xD5, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0xC1, 0xC2, 0xC3,
    ];
    let tmp = tempdir().expect("tempdir");
    let (cpy, bin, _) = write_fixture(
        &tmp,
        copybook,
        &data,
        r#"{"NAME":"JOHN      ","CODE":"ABC"}"#,
    );
    let cmd = DeterminismCommand {
        mode: DeterminismModeCommand::RoundTrip(RoundTripDeterminismArgs {
            common: make_common(cpy),
            data: bin,
        }),
    };
    let result = run(&cmd).expect("multi-field round-trip");
    assert_eq!(result.verdict, DeterminismVerdict::Deterministic);
}
