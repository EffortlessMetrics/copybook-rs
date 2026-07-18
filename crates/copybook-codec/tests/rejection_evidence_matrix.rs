// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]
//! Consolidated stable-error / deliberate-rejection evidence matrix (issue #576).
//!
//! Existing negative coverage is thorough but scattered across many files and
//! partly gated behind the `comprehensive-tests` feature. This suite pins, in a
//! single **default-feature** place, the mapping from each documented
//! unsupported/rejected scenario to its **stable error code**, exercised through
//! the public library entry points across all three phases:
//!
//! * **parse** (`parse_copybook`) — structural `CBKP*` / `CBKS*` rejections,
//! * **decode** (`decode_record`) — data `CBKD*` / `CBKC*` rejections,
//! * **encode** (`encode_record`) — `CBKE*` rejections.
//!
//! Each row carries a scenario id: the structural rows match a scenario in
//! `docs/reference/COBOL_SUPPORT_MATRIX.md` (`O4`/`O5`/`O6`, `renames-occurs`,
//! `renames-redefines`); the remaining rows are keyed by their stable error code
//! as documented in `docs/reference/ERROR_CODES.md` (e.g. `renames-unknown-from`
//! → `CBKS601`). The copybooks are independent reproductions, not copies of the
//! scattered single-purpose negative tests they overlap with. The CLI
//! command-context mapping (family → process exit code) lives in
//! `copybook-cli/tests/rejection_exit_codes.rs`.

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, UnmappablePolicy, decode_record,
    encode_record,
};
use copybook_core::{ErrorCode, parse_copybook};

// ===========================================================================
// Parse-phase structural rejections (CBKP* / CBKS*)
// ===========================================================================

/// `(scenario_id, copybook, expected_code)`. Copybooks are minimal, verified
/// reproductions of the documented rejection scenarios.
const PARSE_REJECTIONS: &[(&str, &str, ErrorCode)] = &[
    // O4 — ODO array followed by a storage sibling (not at tail).
    (
        "O4:odo-not-tail",
        "01 INV-REC.\n   05 ITEM-COUNT PIC 9(3).\n   05 ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON ITEM-COUNT.\n      10 ITEM-CODE PIC X(4).\n   05 TRAILER PIC X(5).\n",
        ErrorCode::CBKP021_ODO_NOT_TAIL,
    ),
    // O5 — ODO nested inside another ODO. (Independent reproduction, distinct
    // from `nested_odo_negative_tests.rs`, to keep this row genuinely additive.)
    (
        "O5:nested-odo",
        "01 REPORT-REC.\n   05 SECTION-COUNT PIC 9(2).\n   05 SECTION OCCURS 1 TO 20 TIMES DEPENDING ON SECTION-COUNT.\n      10 LINE-COUNT PIC 9(2).\n      10 LINE-ITEM OCCURS 1 TO 40 TIMES DEPENDING ON LINE-COUNT.\n         15 LINE-TEXT PIC X(8).\n",
        ErrorCode::CBKP022_NESTED_ODO,
    ),
    // O6 — ODO declared inside a REDEFINES region. (Independent reproduction.)
    (
        "O6:odo-over-redefines",
        "01 MSG-REC.\n   05 MSG-KIND PIC X(2).\n   05 ELEM-COUNT PIC 9(2).\n   05 MSG-BODY PIC X(80).\n   05 MSG-ELEMS REDEFINES MSG-BODY.\n      10 ELEM OCCURS 1 TO 40 TIMES DEPENDING ON ELEM-COUNT.\n         15 ELEM-VAL PIC X(2).\n",
        ErrorCode::CBKP023_ODO_REDEFINES,
    ),
    // renames-occurs — RENAMES range crosses an OCCURS boundary. (Independent
    // reproduction, distinct from `renames_resolver_negative_tests.rs`.)
    (
        "renames-occurs",
        "01 ACCT-REC.\n   05 ACCT-HEAD PIC X(4).\n   05 BUCKET PIC 9(2) OCCURS 6 TIMES.\n   05 ACCT-TAIL PIC X(3).\n   66 ACCT-SPAN RENAMES ACCT-HEAD THRU ACCT-TAIL.\n",
        ErrorCode::CBKS607_RENAME_CROSSES_OCCURS,
    ),
    // renames-redefines — RENAMES span includes a REDEFINES field. (Independent
    // reproduction.)
    (
        "renames-redefines",
        "01 POLICY-REC.\n   05 PRIMARY PIC X(6).\n   05 SECONDARY REDEFINES PRIMARY PIC X(6).\n   05 SUFFIX PIC 9(4).\n   66 POLICY-SPAN RENAMES PRIMARY THRU SUFFIX.\n",
        ErrorCode::CBKS609_RENAME_OVER_REDEFINES,
    ),
    // RENAMES with an unknown FROM field.
    (
        "renames-unknown-from",
        "01 ROOT-REC.\n   05 FIELD-A PIC X(5).\n   05 FIELD-B PIC X(2).\n   66 ALIAS RENAMES NOPE THRU FIELD-B.\n",
        ErrorCode::CBKS601_RENAME_UNKNOWN_FROM,
    ),
];

#[test]
fn parse_rejections_map_to_stable_codes() {
    for (scenario, copybook, expected) in PARSE_REJECTIONS {
        let err = parse_copybook(copybook)
            .expect_err(&format!("{scenario} must be rejected at parse time"));
        assert_eq!(
            err.code, *expected,
            "scenario {scenario}: expected {expected:?}, got {:?} ({})",
            err.code, err.message
        );
    }
}

// ===========================================================================
// Decode-phase data rejections (CBKD* / CBKC*)
// ===========================================================================

#[test]
fn decode_record_too_short_is_cbkd301() {
    // ODO array declares up to 5 elements; record supplies only the count byte.
    let copybook = "01 RECORD.\n   05 ARRAY-COUNT PIC 9(1).\n   05 DYNAMIC-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON ARRAY-COUNT.\n      10 ELEMENT PIC X(10).\n";
    let schema = parse_copybook(copybook).expect("copybook parses");
    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037);

    // EBCDIC '3' = 0xF3 => count 3 => needs 30 more bytes, none present.
    let err = decode_record(&schema, &[0xF3], &options)
        .expect_err("record shorter than ODO-driven length must be rejected");
    assert_eq!(err.code, ErrorCode::CBKD301_RECORD_TOO_SHORT);
}

#[test]
fn decode_unmappable_byte_is_cbkc301() {
    let copybook = "01 REC.\n   05 SIG PIC X(1).\n";
    let schema = parse_copybook(copybook).expect("copybook parses");
    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_unmappable_policy(UnmappablePolicy::Error);

    // 0x00 (EBCDIC NUL) is an unmappable control byte under the Error policy.
    let err = decode_record(&schema, &[0x00], &options)
        .expect_err("unmappable control byte must be rejected under Error policy");
    assert_eq!(err.code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE);
}

// ===========================================================================
// Encode-phase rejections (CBKE*)
// ===========================================================================

#[test]
fn encode_numeric_overflow_is_cbke510() {
    let copybook = "01 REC.\n   05 N PIC 9(3).\n";
    let schema = parse_copybook(copybook).expect("copybook parses");
    let options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037);

    // 5 digits into a 3-digit field.
    let json = serde_json::json!({ "N": "99999" });
    let err = encode_record(&schema, &json, &options)
        .expect_err("value exceeding field digits must be rejected");
    assert_eq!(err.code, ErrorCode::CBKE510_NUMERIC_OVERFLOW);
}

#[test]
fn encode_string_too_long_is_cbke515() {
    let copybook = "01 REC.\n   05 S PIC X(3).\n";
    let schema = parse_copybook(copybook).expect("copybook parses");
    let options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037);

    let json = serde_json::json!({ "S": "TOOLONG" });
    let err = encode_record(&schema, &json, &options)
        .expect_err("string longer than field capacity must be rejected");
    assert_eq!(err.code, ErrorCode::CBKE515_STRING_LENGTH_VIOLATION);
}

// ===========================================================================
// Error-code stability: every asserted code reports its documented family
// prefix, so the family → exit-code contract (see CLI suite) stays anchored.
// ===========================================================================

#[test]
fn asserted_codes_report_documented_family_prefixes() {
    let expectations = [
        (ErrorCode::CBKP021_ODO_NOT_TAIL, "CBKP"),
        (ErrorCode::CBKP022_NESTED_ODO, "CBKP"),
        (ErrorCode::CBKP023_ODO_REDEFINES, "CBKP"),
        (ErrorCode::CBKS601_RENAME_UNKNOWN_FROM, "CBKS"),
        (ErrorCode::CBKS607_RENAME_CROSSES_OCCURS, "CBKS"),
        (ErrorCode::CBKS609_RENAME_OVER_REDEFINES, "CBKS"),
        (ErrorCode::CBKD301_RECORD_TOO_SHORT, "CBKD"),
        (ErrorCode::CBKC301_INVALID_EBCDIC_BYTE, "CBKC"),
        (ErrorCode::CBKE510_NUMERIC_OVERFLOW, "CBKE"),
        (ErrorCode::CBKE515_STRING_LENGTH_VIOLATION, "CBKE"),
    ];
    for (code, family) in expectations {
        assert_eq!(
            code.family_prefix(),
            family,
            "{code:?} must report family {family}"
        );
    }
}
