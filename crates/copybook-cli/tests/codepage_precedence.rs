// SPDX-License-Identifier: AGPL-3.0-or-later
//! CLI-level per-codepage decode evidence and codepage precedence (issue #573).
//!
//! Before this suite only CP037 was exercised through the compiled `copybook`
//! binary (`cli_args.rs`). Here we drive `copybook decode` for every supported
//! EBCDIC codepage and prove the `--codepage` flag — not a silent default —
//! governs the conversion. Each codepage decodes a single "signature" byte to a
//! character that is unique to (or distinctly placed in) that codepage.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

mod common;

use assert_fs::prelude::*;
use common::bin;

/// One-field copybook: a single alphanumeric byte (LRECL = 1).
const SIG_CPY: &str = "01 REC.\n   05 SIG PIC X(1).\n";

fn path_str(p: &std::path::Path) -> String {
    p.to_string_lossy().into_owned()
}

/// Decode a single `byte` through `copybook decode`, optionally passing
/// `--codepage`, then parse the JSONL output and return the decoded `SIG` field.
/// Parsing (rather than a substring match) keeps the assertion exact and robust
/// to any future change in the JSONL envelope.
fn decode_sig(byte: u8, codepage: Option<&str>) -> String {
    let dir = assert_fs::TempDir::new().unwrap();
    let cpy = dir.child("sig.cpy");
    cpy.write_str(SIG_CPY).unwrap();
    let data = dir.child("sig.bin");
    data.write_binary(&[byte]).unwrap();
    let out = dir.child("out.jsonl");

    let mut cmd = bin();
    cmd.args([
        "decode",
        &path_str(cpy.path()),
        &path_str(data.path()),
        "--output",
        &path_str(out.path()),
        "--format",
        "fixed",
    ]);
    if let Some(cp) = codepage {
        cmd.args(["--codepage", cp]);
    }
    cmd.assert().success();

    let contents = std::fs::read_to_string(out.path()).unwrap();
    let line = contents.lines().next().expect("at least one JSONL record");
    let value: serde_json::Value = serde_json::from_str(line).expect("valid JSON line");
    value
        .get("SIG")
        .and_then(serde_json::Value::as_str)
        .expect("SIG field present")
        .to_owned()
}

// ====================================================================
// Per-codepage decode evidence (CP273/CP500/CP1047/CP1140 previously
// untested through the compiled binary).
// ====================================================================

#[test]
fn cli_decode_cp273_signature() {
    // CP273 0x4A = Ä (German A-umlaut).
    assert_eq!(decode_sig(0x4A, Some("cp273")), "Ä");
}

#[test]
fn cli_decode_cp500_signature() {
    // CP500 0x4F = ! (where CP037 has |).
    assert_eq!(decode_sig(0x4F, Some("cp500")), "!");
}

#[test]
fn cli_decode_cp1047_signature() {
    // CP1047 0xBA = Ý (where CP037 has [) — the z/OS-Unix bracket swap.
    assert_eq!(decode_sig(0xBA, Some("cp1047")), "Ý");
}

#[test]
fn cli_decode_cp1140_signature() {
    // CP1140 0xFF = € (Euro), the single byte that differs from CP037.
    assert_eq!(decode_sig(0xFF, Some("cp1140")), "€");
}

// ====================================================================
// Codepage precedence: the same input byte decodes differently
// depending on --codepage, and an explicit flag overrides the CP037
// default.
// ====================================================================

#[test]
fn cli_codepage_flag_changes_decoded_character() {
    // Byte 0x4A is ¢ in CP037 but Ä in CP273 — same bytes, different flag.
    assert_eq!(decode_sig(0x4A, Some("cp037")), "¢");
    assert_eq!(decode_sig(0x4A, Some("cp273")), "Ä");
}

#[test]
fn cli_explicit_codepage_overrides_default() {
    // With no --codepage the CP037 default applies (0x4A → ¢); an explicit
    // --codepage cp273 overrides that default (0x4A → Ä).
    assert_eq!(decode_sig(0x4A, None), "¢");
    assert_eq!(decode_sig(0x4A, Some("cp273")), "Ä");
}
