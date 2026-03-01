// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep integration tests for the copybook-error crate covering error codes,
//! Display/Debug traits, serde roundtrip, context attachment, equality,
//! thread safety, category grouping, and error construction patterns.

#![allow(clippy::unwrap_used)]

use copybook_error::{Error, ErrorCode, ErrorContext, error};
use std::collections::HashSet;

// ====================================================================
// Helper: exhaustive list of every ErrorCode variant
// ====================================================================

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

// ====================================================================
// 1. All error code variants exist and are distinct
// ====================================================================

#[test]
fn all_error_codes_are_distinct() {
    let codes = all_error_codes();
    let mut set = HashSet::new();
    for code in &codes {
        assert!(set.insert(*code), "duplicate error code: {code}");
    }
    // Verify we have a reasonable count of variants
    assert!(
        set.len() >= 55,
        "expected at least 55 error code variants, got {}",
        set.len()
    );
}

#[test]
fn all_error_codes_have_unique_display_strings() {
    let codes = all_error_codes();
    let mut display_set = HashSet::new();
    for code in &codes {
        let s = format!("{code}");
        assert!(
            display_set.insert(s.clone()),
            "duplicate display string: {s}"
        );
    }
}

// ====================================================================
// 2. Error code formatting (CBK prefix)
// ====================================================================

#[test]
fn every_error_code_display_starts_with_cbk() {
    for code in all_error_codes() {
        let display = format!("{code}");
        assert!(
            display.starts_with("CBK"),
            "error code {display} does not start with CBK"
        );
    }
}

#[test]
fn every_error_code_display_matches_family_prefix() {
    for code in all_error_codes() {
        let display = format!("{code}");
        let prefix = code.family_prefix();
        assert!(
            display.starts_with(prefix),
            "{display} should start with {prefix}"
        );
    }
}

#[test]
fn error_code_display_contains_underscore_separator() {
    for code in all_error_codes() {
        let display = format!("{code}");
        assert!(
            display.contains('_'),
            "error code {display} should contain underscore separator"
        );
    }
}

// ====================================================================
// 3. Error category grouping (CBKP, CBKS, CBKD, CBKE, CBKR, etc.)
// ====================================================================

#[test]
fn cbkp_family_codes_grouped_correctly() {
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
        assert_eq!(code.family_prefix(), "CBKP", "failed for {code}");
    }
}

#[test]
fn cbks_family_codes_grouped_correctly() {
    let cbks_codes = [
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
    ];
    for code in cbks_codes {
        assert_eq!(code.family_prefix(), "CBKS", "failed for {code}");
    }
}

#[test]
fn cbkd_family_codes_grouped_correctly() {
    let cbkd_codes = [
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
    ];
    for code in cbkd_codes {
        assert_eq!(code.family_prefix(), "CBKD", "failed for {code}");
    }
}

#[test]
fn cbke_family_codes_grouped_correctly() {
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
        assert_eq!(code.family_prefix(), "CBKE", "failed for {code}");
    }
}

#[test]
fn cbkr_family_code_grouped_correctly() {
    assert_eq!(
        ErrorCode::CBKR211_RDW_RESERVED_NONZERO.family_prefix(),
        "CBKR"
    );
}

#[test]
fn cbkc_family_codes_grouped_correctly() {
    assert_eq!(ErrorCode::CBKC201_JSON_WRITE_ERROR.family_prefix(), "CBKC");
    assert_eq!(
        ErrorCode::CBKC301_INVALID_EBCDIC_BYTE.family_prefix(),
        "CBKC"
    );
}

#[test]
fn cbkf_family_codes_grouped_correctly() {
    let cbkf_codes = [
        ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
        ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
        ErrorCode::CBKF221_RDW_UNDERFLOW,
    ];
    for code in cbkf_codes {
        assert_eq!(code.family_prefix(), "CBKF", "failed for {code}");
    }
}

#[test]
fn cbki_family_code_grouped_correctly() {
    assert_eq!(ErrorCode::CBKI001_INVALID_STATE.family_prefix(), "CBKI");
}

#[test]
fn cbka_family_code_grouped_correctly() {
    assert_eq!(ErrorCode::CBKA001_BASELINE_ERROR.family_prefix(), "CBKA");
}

#[test]
fn cbkw_family_codes_grouped_correctly() {
    let cbkw_codes = [
        ErrorCode::CBKW001_SCHEMA_CONVERSION,
        ErrorCode::CBKW002_TYPE_MAPPING,
        ErrorCode::CBKW003_DECIMAL_OVERFLOW,
        ErrorCode::CBKW004_BATCH_BUILD,
        ErrorCode::CBKW005_PARQUET_WRITE,
    ];
    for code in cbkw_codes {
        assert_eq!(code.family_prefix(), "CBKW", "failed for {code}");
    }
}

// ====================================================================
// 4. Error context attachment
// ====================================================================

#[test]
fn error_new_has_no_context() {
    let err = Error::new(ErrorCode::CBKP001_SYNTAX, "test");
    assert!(err.context.is_none());
}

#[test]
fn error_with_record_attaches_record_index() {
    let err = Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, "short").with_record(42);
    let ctx = err.context.as_ref().unwrap();
    assert_eq!(ctx.record_index, Some(42));
    assert!(ctx.field_path.is_none());
    assert!(ctx.byte_offset.is_none());
    assert!(ctx.line_number.is_none());
    assert!(ctx.details.is_none());
}

#[test]
fn error_with_field_attaches_field_path() {
    let err =
        Error::new(ErrorCode::CBKD411_ZONED_BAD_SIGN, "bad sign").with_field("CUSTOMER.BALANCE");
    let ctx = err.context.as_ref().unwrap();
    assert_eq!(ctx.field_path.as_deref(), Some("CUSTOMER.BALANCE"));
    assert!(ctx.record_index.is_none());
}

#[test]
fn error_with_offset_attaches_byte_offset() {
    let err = Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, "short").with_offset(256);
    let ctx = err.context.as_ref().unwrap();
    assert_eq!(ctx.byte_offset, Some(256));
}

#[test]
fn error_chained_context_builders_accumulate() {
    let err = Error::new(ErrorCode::CBKD401_COMP3_INVALID_NIBBLE, "bad nibble")
        .with_record(10)
        .with_field("CUSTOMER.BALANCE")
        .with_offset(64);
    let ctx = err.context.as_ref().unwrap();
    assert_eq!(ctx.record_index, Some(10));
    assert_eq!(ctx.field_path.as_deref(), Some("CUSTOMER.BALANCE"));
    assert_eq!(ctx.byte_offset, Some(64));
}

#[test]
fn error_with_context_sets_full_context() {
    let ctx = ErrorContext {
        record_index: Some(99),
        field_path: Some("ROOT.CHILD".into()),
        byte_offset: Some(512),
        line_number: Some(42),
        details: Some("extra detail".into()),
    };
    let err = Error::new(ErrorCode::CBKP001_SYNTAX, "bad syntax").with_context(ctx);
    let c = err.context.as_ref().unwrap();
    assert_eq!(c.record_index, Some(99));
    assert_eq!(c.field_path.as_deref(), Some("ROOT.CHILD"));
    assert_eq!(c.byte_offset, Some(512));
    assert_eq!(c.line_number, Some(42));
    assert_eq!(c.details.as_deref(), Some("extra detail"));
}

#[test]
fn error_with_context_replaces_previous_context() {
    let err = Error::new(ErrorCode::CBKP001_SYNTAX, "test")
        .with_record(1)
        .with_context(ErrorContext {
            record_index: Some(99),
            field_path: None,
            byte_offset: None,
            line_number: None,
            details: None,
        });
    let c = err.context.as_ref().unwrap();
    assert_eq!(c.record_index, Some(99));
}

// ====================================================================
// 5. Display trait
// ====================================================================

#[test]
fn error_display_without_context() {
    let err = Error::new(ErrorCode::CBKP001_SYNTAX, "Unexpected token at line 5");
    assert_eq!(
        format!("{err}"),
        "CBKP001_SYNTAX: Unexpected token at line 5"
    );
}

#[test]
fn error_display_with_field_context() {
    let err = Error::new(ErrorCode::CBKD411_ZONED_BAD_SIGN, "bad sign").with_field("AMOUNT");
    let display = format!("{err}");
    assert!(display.contains("CBKD411_ZONED_BAD_SIGN: bad sign"));
    assert!(display.contains("field AMOUNT"));
}

#[test]
fn error_display_with_record_and_offset_context() {
    let err = Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, "truncated")
        .with_record(7)
        .with_offset(128);
    let display = format!("{err}");
    assert!(display.contains("record 7"));
    assert!(display.contains("offset 128"));
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
    assert_eq!(format!("{ctx}"), "");
}

#[test]
fn error_context_display_all_parts() {
    let ctx = ErrorContext {
        record_index: Some(7),
        field_path: Some("ROOT.CHILD".into()),
        byte_offset: Some(32),
        line_number: Some(15),
        details: Some("extra info".into()),
    };
    let display = format!("{ctx}");
    assert_eq!(
        display,
        "record 7, field ROOT.CHILD, offset 32, line 15, extra info"
    );
}

#[test]
fn error_context_display_single_field() {
    let ctx = ErrorContext {
        record_index: None,
        field_path: Some("AMOUNT".into()),
        byte_offset: None,
        line_number: None,
        details: None,
    };
    assert_eq!(format!("{ctx}"), "field AMOUNT");
}

// ====================================================================
// 6. Debug trait
// ====================================================================

#[test]
fn error_debug_contains_code_and_message() {
    let err = Error::new(ErrorCode::CBKP001_SYNTAX, "test debug");
    let debug = format!("{err:?}");
    assert!(debug.contains("CBKP001_SYNTAX"));
    assert!(debug.contains("test debug"));
}

#[test]
fn error_code_debug_output() {
    let code = ErrorCode::CBKD411_ZONED_BAD_SIGN;
    let debug = format!("{code:?}");
    assert!(debug.contains("CBKD411_ZONED_BAD_SIGN"));
}

// ====================================================================
// 7. Serde roundtrip for error types
// ====================================================================

#[test]
fn serde_roundtrip_all_error_codes() {
    for code in all_error_codes() {
        let json = serde_json::to_string(&code).unwrap();
        let roundtripped: ErrorCode = serde_json::from_str(&json).unwrap();
        assert_eq!(roundtripped, code, "serde roundtrip failed for {code}");
    }
}

#[test]
fn serde_error_code_serializes_as_string() {
    let code = ErrorCode::CBKD411_ZONED_BAD_SIGN;
    let json = serde_json::to_string(&code).unwrap();
    assert_eq!(json, r#""CBKD411_ZONED_BAD_SIGN""#);
}

#[test]
fn serde_deserialization_from_string_literal() {
    let json = r#""CBKP101_INVALID_PIC""#;
    let code: ErrorCode = serde_json::from_str(json).unwrap();
    assert_eq!(code, ErrorCode::CBKP101_INVALID_PIC);
}

#[test]
fn serde_deserialization_rejects_invalid_code() {
    let json = r#""NOT_A_REAL_CODE""#;
    let result: std::result::Result<ErrorCode, _> = serde_json::from_str(json);
    assert!(result.is_err());
}

#[test]
fn serde_deserialization_rejects_numeric_value() {
    let json = "42";
    let result: std::result::Result<ErrorCode, _> = serde_json::from_str(json);
    assert!(result.is_err());
}

#[test]
fn serde_roundtrip_in_json_array() {
    let codes = vec![
        ErrorCode::CBKP001_SYNTAX,
        ErrorCode::CBKS121_COUNTER_NOT_FOUND,
        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
    ];
    let json = serde_json::to_string(&codes).unwrap();
    let roundtripped: Vec<ErrorCode> = serde_json::from_str(&json).unwrap();
    assert_eq!(roundtripped, codes);
}

// ====================================================================
// 8. Error code string parsing (via Display consistency)
// ====================================================================

#[test]
fn error_code_display_string_contains_numeric_portion() {
    let display = format!("{}", ErrorCode::CBKP001_SYNTAX);
    assert!(
        display.contains("001"),
        "expected numeric in display: {display}"
    );
}

#[test]
fn error_code_display_equals_serde_name() {
    for code in all_error_codes() {
        let display = format!("{code}");
        let serde_json_str = serde_json::to_string(&code).unwrap();
        // serde wraps in quotes
        let serde_name = serde_json_str.trim_matches('"');
        assert_eq!(
            display, serde_name,
            "Display and serde name differ for {code}"
        );
    }
}

// ====================================================================
// 9. Error chain construction (builder pattern)
// ====================================================================

#[test]
fn error_chain_record_then_field_then_offset() {
    let err = Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, "chain test")
        .with_record(1)
        .with_field("FIELD-A")
        .with_offset(16);
    let ctx = err.context.as_ref().unwrap();
    assert_eq!(ctx.record_index, Some(1));
    assert_eq!(ctx.field_path.as_deref(), Some("FIELD-A"));
    assert_eq!(ctx.byte_offset, Some(16));
}

#[test]
fn error_chain_offset_then_record() {
    let err = Error::new(ErrorCode::CBKE510_NUMERIC_OVERFLOW, "overflow")
        .with_offset(64)
        .with_record(5);
    let ctx = err.context.as_ref().unwrap();
    assert_eq!(ctx.byte_offset, Some(64));
    assert_eq!(ctx.record_index, Some(5));
}

// ====================================================================
// 10. Error comparison/equality
// ====================================================================

#[test]
fn error_equality_same_code_and_message() {
    let err1 = Error::new(ErrorCode::CBKP001_SYNTAX, "same message");
    let err2 = Error::new(ErrorCode::CBKP001_SYNTAX, "same message");
    assert_eq!(err1, err2);
}

#[test]
fn error_inequality_different_code() {
    let err1 = Error::new(ErrorCode::CBKP001_SYNTAX, "same message");
    let err2 = Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, "same message");
    assert_ne!(err1, err2);
}

#[test]
fn error_inequality_different_message() {
    let err1 = Error::new(ErrorCode::CBKP001_SYNTAX, "message a");
    let err2 = Error::new(ErrorCode::CBKP001_SYNTAX, "message b");
    assert_ne!(err1, err2);
}

#[test]
fn error_inequality_with_vs_without_context() {
    let err1 = Error::new(ErrorCode::CBKP001_SYNTAX, "msg");
    let err2 = Error::new(ErrorCode::CBKP001_SYNTAX, "msg").with_record(1);
    assert_ne!(err1, err2);
}

#[test]
fn error_clone_equals_original() {
    let err = Error::new(ErrorCode::CBKE501_JSON_TYPE_MISMATCH, "mismatch")
        .with_field("AMOUNT")
        .with_record(3);
    let cloned = err.clone();
    assert_eq!(err, cloned);
}

#[test]
fn error_code_copy_semantics() {
    let code = ErrorCode::CBKP001_SYNTAX;
    let copy = code;
    assert_eq!(code, copy);
}

#[test]
fn error_code_hash_in_set() {
    let mut set = HashSet::new();
    set.insert(ErrorCode::CBKP001_SYNTAX);
    set.insert(ErrorCode::CBKD301_RECORD_TOO_SHORT);
    set.insert(ErrorCode::CBKP001_SYNTAX); // duplicate
    assert_eq!(set.len(), 2);
}

// ====================================================================
// 11. Thread safety (Send + Sync)
// ====================================================================

#[test]
fn error_is_send() {
    fn assert_send<T: Send>() {}
    assert_send::<Error>();
}

#[test]
fn error_is_sync() {
    fn assert_sync<T: Sync>() {}
    assert_sync::<Error>();
}

#[test]
fn error_code_is_send_sync() {
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<ErrorCode>();
}

#[test]
fn error_can_cross_thread_boundary() {
    let err = Error::new(ErrorCode::CBKD411_ZONED_BAD_SIGN, "thread test").with_record(1);
    let handle = std::thread::spawn(move || {
        assert_eq!(err.code, ErrorCode::CBKD411_ZONED_BAD_SIGN);
        err.message.clone()
    });
    let msg = handle.join().unwrap();
    assert_eq!(msg, "thread test");
}

// ====================================================================
// 12. Error implements std::error::Error
// ====================================================================

#[test]
fn error_implements_std_error() {
    let err = Error::new(ErrorCode::CBKD411_ZONED_BAD_SIGN, "bad sign");
    let std_err: &dyn std::error::Error = &err;
    assert!(std_err.source().is_none());
    assert!(!std_err.to_string().is_empty());
}

#[test]
fn error_to_string_matches_display() {
    let err = Error::new(ErrorCode::CBKP001_SYNTAX, "test msg");
    assert_eq!(err.to_string(), format!("{err}"));
}

// ====================================================================
// 13. Error code accessor methods
// ====================================================================

#[test]
fn error_code_accessor_returns_code() {
    let err = Error::new(ErrorCode::CBKE510_NUMERIC_OVERFLOW, "overflow");
    assert_eq!(err.code(), ErrorCode::CBKE510_NUMERIC_OVERFLOW);
}

#[test]
fn error_family_prefix_via_error() {
    let err = Error::new(ErrorCode::CBKR211_RDW_RESERVED_NONZERO, "reserved");
    assert_eq!(err.family_prefix(), "CBKR");
}

// ====================================================================
// 14. Error macro
// ====================================================================

#[test]
fn error_macro_static_message() {
    let err = error!(ErrorCode::CBKP001_SYNTAX, "Empty copybook");
    assert_eq!(err.code, ErrorCode::CBKP001_SYNTAX);
    assert_eq!(err.message, "Empty copybook");
    assert!(err.context.is_none());
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

#[test]
fn error_macro_multiple_format_args() {
    let field = "AMOUNT";
    let expected = 8;
    let actual = 4;
    let err = error!(
        ErrorCode::CBKD301_RECORD_TOO_SHORT,
        "Field {} expected {} bytes, got {}", field, expected, actual
    );
    assert_eq!(err.message, "Field AMOUNT expected 8 bytes, got 4");
}

// ====================================================================
// 15. Family prefix exhaustive coverage
// ====================================================================

#[test]
fn all_family_prefixes_are_four_chars() {
    for code in all_error_codes() {
        let prefix = code.family_prefix();
        assert_eq!(
            prefix.len(),
            4,
            "family prefix for {code} has length {} (expected 4)",
            prefix.len()
        );
    }
}

#[test]
fn all_family_prefixes_start_with_cbk() {
    for code in all_error_codes() {
        let prefix = code.family_prefix();
        assert!(
            prefix.starts_with("CBK"),
            "family prefix {prefix} for {code} does not start with CBK"
        );
    }
}

#[test]
fn known_family_prefixes_are_exhaustive() {
    let mut prefixes = HashSet::new();
    for code in all_error_codes() {
        prefixes.insert(code.family_prefix());
    }
    let expected = [
        "CBKP", "CBKS", "CBKR", "CBKC", "CBKD", "CBKI", "CBKE", "CBKF", "CBKA", "CBKW",
    ];
    for exp in &expected {
        assert!(
            prefixes.contains(exp),
            "expected family prefix {exp} not found"
        );
    }
}

// ====================================================================
// 16. ErrorContext edge cases
// ====================================================================

#[test]
fn error_context_partial_display_field_and_line() {
    let ctx = ErrorContext {
        record_index: None,
        field_path: Some("AMOUNT".into()),
        byte_offset: None,
        line_number: Some(3),
        details: None,
    };
    assert_eq!(format!("{ctx}"), "field AMOUNT, line 3");
}

#[test]
fn error_context_display_details_only() {
    let ctx = ErrorContext {
        record_index: None,
        field_path: None,
        byte_offset: None,
        line_number: None,
        details: Some("expected 8 bytes, got 4".into()),
    };
    assert_eq!(format!("{ctx}"), "expected 8 bytes, got 4");
}

#[test]
fn error_context_display_record_and_details() {
    let ctx = ErrorContext {
        record_index: Some(100),
        field_path: None,
        byte_offset: None,
        line_number: None,
        details: Some("truncated".into()),
    };
    assert_eq!(format!("{ctx}"), "record 100, truncated");
}
