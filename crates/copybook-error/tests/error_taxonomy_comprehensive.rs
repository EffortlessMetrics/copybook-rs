// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for the copybook-error crate: all CBK error code
//! categories, Display formatting, family_prefix mapping, Error construction,
//! context builders, serde roundtrip, and error macro.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_error::{Error, ErrorCode, ErrorContext, error};
use std::collections::HashSet;

// ============================================================================
// Helper: exhaustive list of every ErrorCode variant
// ============================================================================

fn all_error_codes() -> Vec<ErrorCode> {
    vec![
        // CBKP
        ErrorCode::CBKP001_SYNTAX,
        ErrorCode::CBKP011_UNSUPPORTED_CLAUSE,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        ErrorCode::CBKP022_NESTED_ODO,
        ErrorCode::CBKP023_ODO_REDEFINES,
        ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC,
        ErrorCode::CBKP101_INVALID_PIC,
        // CBKS
        ErrorCode::CBKS121_COUNTER_NOT_FOUND,
        ErrorCode::CBKS141_RECORD_TOO_LARGE,
        ErrorCode::CBKS301_ODO_CLIPPED,
        ErrorCode::CBKS302_ODO_RAISED,
        ErrorCode::CBKS601_RENAME_UNKNOWN_FROM,
        ErrorCode::CBKS602_RENAME_UNKNOWN_THRU,
        ErrorCode::CBKS603_RENAME_NOT_CONTIGUOUS,
        ErrorCode::CBKS604_RENAME_REVERSED_RANGE,
        ErrorCode::CBKS605_RENAME_FROM_CROSSES_GROUP,
        ErrorCode::CBKS606_RENAME_THRU_CROSSES_GROUP,
        ErrorCode::CBKS607_RENAME_CROSSES_OCCURS,
        ErrorCode::CBKS608_RENAME_QUALIFIED_NAME_NOT_FOUND,
        ErrorCode::CBKS609_RENAME_OVER_REDEFINES,
        ErrorCode::CBKS610_RENAME_MULTIPLE_REDEFINES,
        ErrorCode::CBKS611_RENAME_PARTIAL_OCCURS,
        ErrorCode::CBKS612_RENAME_ODO_NOT_SUPPORTED,
        ErrorCode::CBKS701_PROJECTION_INVALID_ODO,
        ErrorCode::CBKS702_PROJECTION_UNRESOLVED_ALIAS,
        ErrorCode::CBKS703_PROJECTION_FIELD_NOT_FOUND,
        // CBKR
        ErrorCode::CBKR211_RDW_RESERVED_NONZERO,
        // CBKC
        ErrorCode::CBKC201_JSON_WRITE_ERROR,
        ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
        // CBKD
        ErrorCode::CBKD101_INVALID_FIELD_TYPE,
        ErrorCode::CBKD301_RECORD_TOO_SHORT,
        ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED,
        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
        ErrorCode::CBKD410_ZONED_OVERFLOW,
        ErrorCode::CBKD411_ZONED_BAD_SIGN,
        ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO,
        ErrorCode::CBKD413_ZONED_INVALID_ENCODING,
        ErrorCode::CBKD414_ZONED_MIXED_ENCODING,
        ErrorCode::CBKD415_ZONED_ENCODING_AMBIGUOUS,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
        ErrorCode::CBKD423_EDITED_PIC_BLANK_WHEN_ZERO,
        ErrorCode::CBKD431_FLOAT_NAN,
        ErrorCode::CBKD432_FLOAT_INFINITY,
        // CBKI
        ErrorCode::CBKI001_INVALID_STATE,
        // CBKE
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
        ErrorCode::CBKE505_SCALE_MISMATCH,
        ErrorCode::CBKE510_NUMERIC_OVERFLOW,
        ErrorCode::CBKE515_STRING_LENGTH_VIOLATION,
        ErrorCode::CBKE521_ARRAY_LEN_OOB,
        ErrorCode::CBKE530_SIGN_SEPARATE_ENCODE_ERROR,
        ErrorCode::CBKE531_FLOAT_ENCODE_OVERFLOW,
        // CBKF
        ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
        ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
        ErrorCode::CBKF221_RDW_UNDERFLOW,
        // CBKA
        ErrorCode::CBKA001_BASELINE_ERROR,
        // CBKW
        ErrorCode::CBKW001_SCHEMA_CONVERSION,
        ErrorCode::CBKW002_TYPE_MAPPING,
        ErrorCode::CBKW003_DECIMAL_OVERFLOW,
        ErrorCode::CBKW004_BATCH_BUILD,
        ErrorCode::CBKW005_PARQUET_WRITE,
    ]
}

// ============================================================================
// 1. Every error code has a unique Display string
// ============================================================================

#[test]
fn all_error_codes_have_unique_display_strings() {
    let codes = all_error_codes();
    let mut seen = HashSet::new();
    for code in &codes {
        let display = format!("{code}");
        assert!(
            seen.insert(display.clone()),
            "duplicate Display for {code:?}: {display}"
        );
    }
}

// ============================================================================
// 2. Every Display string starts with "CBK"
// ============================================================================

#[test]
fn all_display_strings_start_with_cbk() {
    for code in all_error_codes() {
        let display = format!("{code}");
        assert!(
            display.starts_with("CBK"),
            "{code:?} Display doesn't start with CBK: {display}"
        );
    }
}

// ============================================================================
// 3. CBKP family — all parse error codes
// ============================================================================

#[test]
fn cbkp_family_prefix_correct() {
    let cbkp_codes = [
        ErrorCode::CBKP001_SYNTAX,
        ErrorCode::CBKP011_UNSUPPORTED_CLAUSE,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        ErrorCode::CBKP022_NESTED_ODO,
        ErrorCode::CBKP023_ODO_REDEFINES,
        ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC,
        ErrorCode::CBKP101_INVALID_PIC,
    ];
    for code in cbkp_codes {
        assert_eq!(code.family_prefix(), "CBKP", "{code:?}");
        assert!(format!("{code}").starts_with("CBKP"), "{code:?}");
    }
}

// ============================================================================
// 4. CBKS family — all schema error codes
// ============================================================================

#[test]
fn cbks_family_prefix_correct() {
    let cbks_codes = [
        ErrorCode::CBKS121_COUNTER_NOT_FOUND,
        ErrorCode::CBKS141_RECORD_TOO_LARGE,
        ErrorCode::CBKS301_ODO_CLIPPED,
        ErrorCode::CBKS302_ODO_RAISED,
        ErrorCode::CBKS601_RENAME_UNKNOWN_FROM,
        ErrorCode::CBKS703_PROJECTION_FIELD_NOT_FOUND,
    ];
    for code in cbks_codes {
        assert_eq!(code.family_prefix(), "CBKS", "{code:?}");
    }
}

// ============================================================================
// 5. CBKD family — all data decode error codes
// ============================================================================

#[test]
fn cbkd_family_prefix_correct() {
    let cbkd_codes = [
        ErrorCode::CBKD101_INVALID_FIELD_TYPE,
        ErrorCode::CBKD301_RECORD_TOO_SHORT,
        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
        ErrorCode::CBKD411_ZONED_BAD_SIGN,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
        ErrorCode::CBKD431_FLOAT_NAN,
        ErrorCode::CBKD432_FLOAT_INFINITY,
    ];
    for code in cbkd_codes {
        assert_eq!(code.family_prefix(), "CBKD", "{code:?}");
    }
}

// ============================================================================
// 6. CBKE family — all encode error codes
// ============================================================================

#[test]
fn cbke_family_prefix_correct() {
    let cbke_codes = [
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
        ErrorCode::CBKE505_SCALE_MISMATCH,
        ErrorCode::CBKE510_NUMERIC_OVERFLOW,
        ErrorCode::CBKE515_STRING_LENGTH_VIOLATION,
        ErrorCode::CBKE521_ARRAY_LEN_OOB,
        ErrorCode::CBKE530_SIGN_SEPARATE_ENCODE_ERROR,
        ErrorCode::CBKE531_FLOAT_ENCODE_OVERFLOW,
    ];
    for code in cbke_codes {
        assert_eq!(code.family_prefix(), "CBKE", "{code:?}");
    }
}

// ============================================================================
// 7. CBKR family — record error codes
// ============================================================================

#[test]
fn cbkr_family_prefix_correct() {
    assert_eq!(
        ErrorCode::CBKR211_RDW_RESERVED_NONZERO.family_prefix(),
        "CBKR"
    );
}

// ============================================================================
// 8. CBKC, CBKI, CBKF, CBKA, CBKW families
// ============================================================================

#[test]
fn cbkc_family_prefix_correct() {
    assert_eq!(ErrorCode::CBKC201_JSON_WRITE_ERROR.family_prefix(), "CBKC");
    assert_eq!(
        ErrorCode::CBKC301_INVALID_EBCDIC_BYTE.family_prefix(),
        "CBKC"
    );
}

#[test]
fn cbki_family_prefix_correct() {
    assert_eq!(ErrorCode::CBKI001_INVALID_STATE.family_prefix(), "CBKI");
}

#[test]
fn cbkf_family_prefix_correct() {
    assert_eq!(
        ErrorCode::CBKF102_RECORD_LENGTH_INVALID.family_prefix(),
        "CBKF"
    );
    assert_eq!(ErrorCode::CBKF104_RDW_SUSPECT_ASCII.family_prefix(), "CBKF");
    assert_eq!(ErrorCode::CBKF221_RDW_UNDERFLOW.family_prefix(), "CBKF");
}

#[test]
fn cbka_family_prefix_correct() {
    assert_eq!(ErrorCode::CBKA001_BASELINE_ERROR.family_prefix(), "CBKA");
}

#[test]
fn cbkw_family_prefix_correct() {
    assert_eq!(ErrorCode::CBKW001_SCHEMA_CONVERSION.family_prefix(), "CBKW");
    assert_eq!(ErrorCode::CBKW005_PARQUET_WRITE.family_prefix(), "CBKW");
}

// ============================================================================
// 9. family_prefix matches Display prefix for all codes
// ============================================================================

#[test]
fn family_prefix_matches_display_prefix_for_all_codes() {
    for code in all_error_codes() {
        let display = format!("{code}");
        let prefix = code.family_prefix();
        assert!(
            display.starts_with(prefix),
            "{code:?}: Display '{display}' doesn't start with prefix '{prefix}'"
        );
    }
}

// ============================================================================
// 10. Error construction — new
// ============================================================================

#[test]
fn error_new_sets_code_and_message() {
    let err = Error::new(ErrorCode::CBKP001_SYNTAX, "bad syntax");
    assert_eq!(err.code, ErrorCode::CBKP001_SYNTAX);
    assert_eq!(err.message, "bad syntax");
    assert!(err.context.is_none());
}

#[test]
fn error_new_accepts_string_and_str() {
    let err1 = Error::new(ErrorCode::CBKP001_SYNTAX, "static");
    let err2 = Error::new(ErrorCode::CBKP001_SYNTAX, String::from("dynamic"));
    assert_eq!(err1.message, "static");
    assert_eq!(err2.message, "dynamic");
}

// ============================================================================
// 11. Error Display format: "CODE: message"
// ============================================================================

#[test]
fn error_display_format_without_context() {
    let err = Error::new(ErrorCode::CBKD411_ZONED_BAD_SIGN, "invalid sign");
    let display = format!("{err}");
    assert_eq!(display, "CBKD411_ZONED_BAD_SIGN: invalid sign");
}

#[test]
fn error_display_format_with_context() {
    let err = Error::new(ErrorCode::CBKD411_ZONED_BAD_SIGN, "invalid sign")
        .with_field("AMOUNT")
        .with_record(5);
    let display = format!("{err}");
    assert!(display.contains("CBKD411_ZONED_BAD_SIGN"));
    assert!(display.contains("invalid sign"));
    assert!(display.contains("field AMOUNT"));
    assert!(display.contains("record 5"));
}

// ============================================================================
// 12. Error context builders
// ============================================================================

#[test]
fn with_record_sets_record_index() {
    let err = Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, "short").with_record(42);
    let ctx = err.context.as_ref().unwrap();
    assert_eq!(ctx.record_index, Some(42));
    assert!(ctx.field_path.is_none());
}

#[test]
fn with_field_sets_field_path() {
    let err = Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, "short").with_field("CUSTOMER.NAME");
    let ctx = err.context.as_ref().unwrap();
    assert_eq!(ctx.field_path.as_deref(), Some("CUSTOMER.NAME"));
    assert!(ctx.record_index.is_none());
}

#[test]
fn with_offset_sets_byte_offset() {
    let err = Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, "short").with_offset(256);
    let ctx = err.context.as_ref().unwrap();
    assert_eq!(ctx.byte_offset, Some(256));
}

#[test]
fn chained_builders_accumulate_context() {
    let err = Error::new(ErrorCode::CBKD401_COMP3_INVALID_NIBBLE, "bad nibble")
        .with_record(10)
        .with_field("BALANCE")
        .with_offset(64);
    let ctx = err.context.as_ref().unwrap();
    assert_eq!(ctx.record_index, Some(10));
    assert_eq!(ctx.field_path.as_deref(), Some("BALANCE"));
    assert_eq!(ctx.byte_offset, Some(64));
}

#[test]
fn with_context_sets_full_context() {
    let ctx = ErrorContext {
        record_index: Some(99),
        field_path: Some("ROOT.CHILD".into()),
        byte_offset: Some(512),
        line_number: Some(42),
        details: Some("extra info".into()),
    };
    let err = Error::new(ErrorCode::CBKP001_SYNTAX, "syntax").with_context(ctx);
    let c = err.context.as_ref().unwrap();
    assert_eq!(c.record_index, Some(99));
    assert_eq!(c.field_path.as_deref(), Some("ROOT.CHILD"));
    assert_eq!(c.byte_offset, Some(512));
    assert_eq!(c.line_number, Some(42));
    assert_eq!(c.details.as_deref(), Some("extra info"));
}

// ============================================================================
// 13. Error code() and family_prefix() accessors
// ============================================================================

#[test]
fn error_code_accessor() {
    let err = Error::new(ErrorCode::CBKE510_NUMERIC_OVERFLOW, "overflow");
    assert_eq!(err.code(), ErrorCode::CBKE510_NUMERIC_OVERFLOW);
}

#[test]
fn error_family_prefix_accessor() {
    let err = Error::new(ErrorCode::CBKR211_RDW_RESERVED_NONZERO, "reserved");
    assert_eq!(err.family_prefix(), "CBKR");
}

// ============================================================================
// 14. Error macro — static and formatted
// ============================================================================

#[test]
fn error_macro_static_message() {
    let err = error!(ErrorCode::CBKP001_SYNTAX, "empty copybook");
    assert_eq!(err.code, ErrorCode::CBKP001_SYNTAX);
    assert_eq!(err.message, "empty copybook");
}

#[test]
fn error_macro_formatted_message() {
    let field = "CUSTOMER_ID";
    let err = error!(
        ErrorCode::CBKD301_RECORD_TOO_SHORT,
        "Field {} missing", field
    );
    assert_eq!(err.code, ErrorCode::CBKD301_RECORD_TOO_SHORT);
    assert!(err.message.contains("CUSTOMER_ID"));
}

// ============================================================================
// 15. Clone and PartialEq
// ============================================================================

#[test]
fn error_clone_preserves_all_fields() {
    let err = Error::new(ErrorCode::CBKE501_JSON_TYPE_MISMATCH, "mismatch")
        .with_field("AMOUNT")
        .with_record(3);
    let cloned = err.clone();
    assert_eq!(err, cloned);
}

#[test]
fn error_codes_are_eq_when_same() {
    assert_eq!(ErrorCode::CBKP001_SYNTAX, ErrorCode::CBKP001_SYNTAX);
}

#[test]
fn error_codes_are_ne_when_different() {
    assert_ne!(
        ErrorCode::CBKP001_SYNTAX,
        ErrorCode::CBKS121_COUNTER_NOT_FOUND
    );
}

// ============================================================================
// 16. Serde roundtrip for ErrorCode
// ============================================================================

#[test]
fn error_code_serde_roundtrip_all() {
    for code in all_error_codes() {
        let json = serde_json::to_string(&code).unwrap();
        let back: ErrorCode = serde_json::from_str(&json).unwrap();
        assert_eq!(code, back, "serde roundtrip for {code:?}");
    }
}

#[test]
fn error_code_serde_json_representation() {
    let json = serde_json::to_string(&ErrorCode::CBKD411_ZONED_BAD_SIGN).unwrap();
    assert_eq!(json, "\"CBKD411_ZONED_BAD_SIGN\"");
}

// ============================================================================
// 17. ErrorContext Display
// ============================================================================

#[test]
fn error_context_display_all_fields() {
    let ctx = ErrorContext {
        record_index: Some(5),
        field_path: Some("ROOT.CHILD".into()),
        byte_offset: Some(128),
        line_number: Some(10),
        details: Some("extra".into()),
    };
    let display = format!("{ctx}");
    assert!(display.contains("record 5"));
    assert!(display.contains("field ROOT.CHILD"));
    assert!(display.contains("offset 128"));
    assert!(display.contains("line 10"));
    assert!(display.contains("extra"));
}

#[test]
fn error_context_display_partial_fields() {
    let ctx = ErrorContext {
        record_index: Some(1),
        field_path: None,
        byte_offset: None,
        line_number: None,
        details: None,
    };
    let display = format!("{ctx}");
    assert_eq!(display, "record 1");
}

#[test]
fn error_context_display_empty() {
    let ctx = ErrorContext {
        record_index: None,
        field_path: None,
        byte_offset: None,
        line_number: None,
        details: None,
    };
    let display = format!("{ctx}");
    assert_eq!(display, "");
}

// ============================================================================
// 18. Error implements std::error::Error (thiserror)
// ============================================================================

#[test]
fn error_is_std_error() {
    let err = Error::new(ErrorCode::CBKP001_SYNTAX, "test");
    let _: &dyn std::error::Error = &err;
}

// ============================================================================
// 19. ErrorCode is Copy + Hash
// ============================================================================

#[test]
fn error_code_is_copy() {
    let code = ErrorCode::CBKP001_SYNTAX;
    let copy = code; // Copy
    assert_eq!(code, copy);
}

#[test]
fn error_code_hash_unique() {
    let codes = all_error_codes();
    let set: HashSet<ErrorCode> = codes.iter().copied().collect();
    assert_eq!(
        set.len(),
        codes.len(),
        "all codes should be unique in HashSet"
    );
}
