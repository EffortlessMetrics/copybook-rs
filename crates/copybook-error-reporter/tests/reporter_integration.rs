// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive integration tests for copybook-error-reporter.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_error::{Error, ErrorCode};
use copybook_error_reporter::{ErrorMode, ErrorReporter, ErrorSeverity, ErrorSummary};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn make_error(code: ErrorCode, msg: &str) -> Error {
    Error::new(code, msg)
}

fn make_data_error(msg: &str) -> Error {
    Error::new(ErrorCode::CBKD401_COMP3_INVALID_NIBBLE, msg)
}

// =========================================================================
// 1. Error reporting policies — Strict mode
// =========================================================================

#[test]
fn strict_halts_on_first_data_error() {
    let mut r = ErrorReporter::new(ErrorMode::Strict, None);
    assert!(r.report_error(make_data_error("first")).is_err());
    assert_eq!(r.error_count(), 1);
}

#[test]
fn strict_halts_on_fatal_error() {
    let mut r = ErrorReporter::new(ErrorMode::Strict, None);
    assert!(
        r.report_error(make_error(ErrorCode::CBKP001_SYNTAX, "bad"))
            .is_err()
    );
    assert!(r.has_errors());
}

#[test]
fn strict_halts_on_all_error_severity_codes() {
    let codes = [
        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
        ErrorCode::CBKD411_ZONED_BAD_SIGN,
        ErrorCode::CBKD410_ZONED_OVERFLOW,
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
        ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
        ErrorCode::CBKD431_FLOAT_NAN,
    ];
    for code in codes {
        let mut r = ErrorReporter::new(ErrorMode::Strict, None);
        assert!(
            r.report_error(make_error(code, "strict")).is_err(),
            "{code} must halt"
        );
    }
}

#[test]
fn strict_odo_clipped_raised_are_fatal() {
    for code in [
        ErrorCode::CBKS301_ODO_CLIPPED,
        ErrorCode::CBKS302_ODO_RAISED,
    ] {
        let mut r = ErrorReporter::new(ErrorMode::Strict, None);
        assert!(r.report_error(make_error(code, "odo")).is_err());
    }
}

// =========================================================================
// 2. Error reporting policies — Lenient mode
// =========================================================================

#[test]
fn lenient_continues_on_data_errors() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    for i in 0..10 {
        assert!(r.report_error(make_data_error(&format!("err {i}"))).is_ok());
    }
    assert_eq!(r.error_count(), 10);
}

#[test]
fn lenient_halts_on_fatal_errors() {
    let fatal_codes = [
        ErrorCode::CBKP001_SYNTAX,
        ErrorCode::CBKP011_UNSUPPORTED_CLAUSE,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        ErrorCode::CBKS121_COUNTER_NOT_FOUND,
        ErrorCode::CBKS141_RECORD_TOO_LARGE,
        ErrorCode::CBKS601_RENAME_UNKNOWN_FROM,
        ErrorCode::CBKS701_PROJECTION_INVALID_ODO,
        ErrorCode::CBKE505_SCALE_MISMATCH,
        ErrorCode::CBKE510_NUMERIC_OVERFLOW,
        ErrorCode::CBKI001_INVALID_STATE,
    ];
    for code in fatal_codes {
        let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
        assert!(
            r.report_error(make_error(code, "fatal")).is_err(),
            "{code} must halt"
        );
    }
}

#[test]
fn lenient_odo_clipped_raised_are_warnings() {
    for code in [
        ErrorCode::CBKS301_ODO_CLIPPED,
        ErrorCode::CBKS302_ODO_RAISED,
    ] {
        let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
        assert!(r.report_error(make_error(code, "odo")).is_ok());
        assert!(r.has_warnings());
        assert!(!r.has_errors());
    }
}

#[test]
fn lenient_unlimited_errors_accepted() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    for i in 0..200 {
        let _ = r.report_error(make_data_error(&format!("err {i}")));
    }
    assert_eq!(r.error_count(), 200);
}

// =========================================================================
// 3. Max-errors threshold enforcement
// =========================================================================

#[test]
fn max_errors_zero_rejects_immediately() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, Some(0));
    assert!(r.report_error(make_data_error("first")).is_err());
}

#[test]
fn max_errors_one_allows_exactly_one() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, Some(1));
    assert!(r.report_error(make_data_error("first")).is_ok());
    assert!(r.report_error(make_data_error("second")).is_err());
    assert_eq!(r.error_count(), 2);
}

#[test]
fn max_errors_boundary_exact_limit() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, Some(5));
    for i in 1..=5 {
        assert!(r.report_error(make_data_error(&format!("e{i}"))).is_ok());
    }
    assert!(r.report_error(make_data_error("e6")).is_err());
    assert_eq!(r.error_count(), 6);
}

#[test]
fn max_errors_not_affected_by_warnings() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, Some(2));
    for _ in 0..50 {
        r.report_warning(make_error(ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO, "blank"));
    }
    assert_eq!(r.warning_count(), 50);
    assert!(r.report_error(make_data_error("first")).is_ok());
    assert!(r.report_error(make_data_error("second")).is_ok());
    assert!(r.report_error(make_data_error("third")).is_err());
}

#[test]
fn max_errors_fatal_bypasses_limit_check() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, Some(100));
    assert!(
        r.report_error(make_error(ErrorCode::CBKP001_SYNTAX, "fatal"))
            .is_err()
    );
    assert_eq!(r.error_count(), 1);
}

// =========================================================================
// 4. Error summaries with multiple errors
// =========================================================================

#[test]
fn summary_mixed_errors_and_warnings() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    r.report_warning(make_error(ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO, "blank"));
    r.report_warning(make_error(ErrorCode::CBKC301_INVALID_EBCDIC_BYTE, "byte"));
    let _ = r.report_error(make_data_error("nibble"));
    let _ = r.report_error(make_error(ErrorCode::CBKD411_ZONED_BAD_SIGN, "sign"));

    let s = r.summary();
    assert_eq!(s.error_count(), 2);
    assert_eq!(s.warning_count(), 2);
    assert!(s.has_errors());
    assert!(s.has_warnings());
}

#[test]
fn summary_first_and_last_error_tracked() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    let _ = r.report_error(make_data_error("alpha"));
    let _ = r.report_error(make_error(ErrorCode::CBKD411_ZONED_BAD_SIGN, "beta"));
    let _ = r.report_error(make_error(ErrorCode::CBKD410_ZONED_OVERFLOW, "gamma"));

    let s = r.summary();
    assert_eq!(s.first_error.as_ref().unwrap().error.message, "alpha");
    assert_eq!(s.last_error.as_ref().unwrap().error.message, "gamma");
}

#[test]
fn summary_single_error_is_both_first_and_last() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    let _ = r.report_error(make_data_error("only one"));
    let s = r.summary();
    assert_eq!(
        s.first_error.as_ref().unwrap().error.code,
        s.last_error.as_ref().unwrap().error.code
    );
    assert_eq!(s.first_error.as_ref().unwrap().error.message, "only one");
}

#[test]
fn summary_many_errors_tracks_first_and_last() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    for i in 0..100 {
        let _ = r.report_error(make_data_error(&format!("error {i}")));
    }
    let s = r.summary();
    assert_eq!(s.first_error.as_ref().unwrap().error.message, "error 0");
    assert_eq!(s.last_error.as_ref().unwrap().error.message, "error 99");
    assert_eq!(r.error_count(), 100);
}

#[test]
fn summary_error_codes_counted_correctly() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    for _ in 0..5 {
        let _ = r.report_error(make_data_error("dup"));
    }
    for _ in 0..3 {
        let _ = r.report_error(make_error(ErrorCode::CBKD411_ZONED_BAD_SIGN, "sign"));
    }
    let codes = &r.summary().error_codes;
    assert_eq!(
        *codes.get(&ErrorCode::CBKD401_COMP3_INVALID_NIBBLE).unwrap(),
        5
    );
    assert_eq!(*codes.get(&ErrorCode::CBKD411_ZONED_BAD_SIGN).unwrap(), 3);
    assert_eq!(codes.len(), 2);
}

// =========================================================================
// 5. Structured output formatting
// =========================================================================

#[test]
fn generate_report_contains_required_sections() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    r.start_record(10);
    let _ = r.report_error(make_data_error("problem").with_record(3));
    r.report_warning(make_error(ErrorCode::CBKF104_RDW_SUSPECT_ASCII, "ascii"));

    let report = r.generate_report();
    assert!(report.contains("=== Error Summary ==="));
    assert!(report.contains("Total records processed: 10"));
    assert!(report.contains("Records with errors:"));
    assert!(report.contains("Error counts by severity:"));
    assert!(report.contains("Error counts by code:"));
    assert!(report.contains("Transfer corruption warnings: 1"));
    assert!(report.contains("First error:"));
}

#[test]
fn generate_report_empty_reporter_minimal() {
    let r = ErrorReporter::new(ErrorMode::Strict, None);
    let report = r.generate_report();
    assert!(report.contains("=== Error Summary ==="));
    assert!(report.contains("Total records processed: 0"));
    assert!(!report.contains("Error counts by severity:"));
    assert!(!report.contains("Error counts by code:"));
    assert!(!report.contains("Transfer corruption warnings"));
    assert!(!report.contains("First error:"));
}

#[test]
fn generate_report_corruption_section_only_when_nonzero() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    r.report_warning(make_error(ErrorCode::CBKF104_RDW_SUSPECT_ASCII, "ascii"));
    assert!(
        r.generate_report()
            .contains("Transfer corruption warnings: 1")
    );

    let mut r2 = ErrorReporter::new(ErrorMode::Lenient, None);
    r2.report_warning(make_error(ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO, "blank"));
    assert!(
        !r2.generate_report()
            .contains("Transfer corruption warnings")
    );
}

#[test]
fn generate_report_shows_first_error_message() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    let _ = r.report_error(make_data_error("the very first problem"));
    let _ = r.report_error(make_data_error("second problem"));
    let report = r.generate_report();
    assert!(report.contains("First error:"));
    assert!(report.contains("the very first problem"));
}

// =========================================================================
// 6. Error counts and categorization
// =========================================================================

#[test]
fn cbkp_parse_errors_all_fatal() {
    let codes = [
        ErrorCode::CBKP001_SYNTAX,
        ErrorCode::CBKP011_UNSUPPORTED_CLAUSE,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        ErrorCode::CBKP022_NESTED_ODO,
        ErrorCode::CBKP023_ODO_REDEFINES,
        ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC,
        ErrorCode::CBKP101_INVALID_PIC,
    ];
    for code in codes {
        let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
        assert!(
            r.report_error(make_error(code, "parse")).is_err(),
            "{code} must be fatal"
        );
    }
}

#[test]
fn cbks_schema_errors_all_fatal() {
    let codes = [
        ErrorCode::CBKS121_COUNTER_NOT_FOUND,
        ErrorCode::CBKS141_RECORD_TOO_LARGE,
        ErrorCode::CBKS601_RENAME_UNKNOWN_FROM,
        ErrorCode::CBKS602_RENAME_UNKNOWN_THRU,
        ErrorCode::CBKS603_RENAME_NOT_CONTIGUOUS,
        ErrorCode::CBKS701_PROJECTION_INVALID_ODO,
        ErrorCode::CBKS702_PROJECTION_UNRESOLVED_ALIAS,
        ErrorCode::CBKS703_PROJECTION_FIELD_NOT_FOUND,
    ];
    for code in codes {
        let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
        assert!(
            r.report_error(make_error(code, "schema")).is_err(),
            "{code} must be fatal"
        );
    }
}

#[test]
fn cbkd_data_errors_continue_in_lenient() {
    let codes = [
        ErrorCode::CBKD101_INVALID_FIELD_TYPE,
        ErrorCode::CBKD301_RECORD_TOO_SHORT,
        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
        ErrorCode::CBKD410_ZONED_OVERFLOW,
        ErrorCode::CBKD411_ZONED_BAD_SIGN,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
        ErrorCode::CBKD431_FLOAT_NAN,
        ErrorCode::CBKD432_FLOAT_INFINITY,
    ];
    for code in codes {
        let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
        assert!(
            r.report_error(make_error(code, "data")).is_ok(),
            "{code} should continue"
        );
        assert!(r.has_errors());
    }
}

#[test]
fn cbke_encode_errors_continue_in_lenient() {
    let codes = [
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
        ErrorCode::CBKE521_ARRAY_LEN_OOB,
        ErrorCode::CBKE530_SIGN_SEPARATE_ENCODE_ERROR,
        ErrorCode::CBKE531_FLOAT_ENCODE_OVERFLOW,
    ];
    for code in codes {
        let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
        assert!(
            r.report_error(make_error(code, "enc")).is_ok(),
            "{code} should continue"
        );
        assert!(r.has_errors());
    }
}

#[test]
fn cbke_fatal_encode_errors_halt_in_lenient() {
    let codes = [
        ErrorCode::CBKE505_SCALE_MISMATCH,
        ErrorCode::CBKE510_NUMERIC_OVERFLOW,
        ErrorCode::CBKE515_STRING_LENGTH_VIOLATION,
    ];
    for code in codes {
        let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
        assert!(
            r.report_error(make_error(code, "enc")).is_err(),
            "{code} must be fatal"
        );
    }
}

#[test]
fn warning_class_codes_are_warnings_not_errors() {
    let codes = [
        ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO,
        ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
        ErrorCode::CBKD423_EDITED_PIC_BLANK_WHEN_ZERO,
        ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
    ];
    for code in codes {
        let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
        assert!(r.report_error(make_error(code, "warn")).is_ok());
        assert!(!r.has_errors(), "{code} should not be error");
        assert!(r.has_warnings(), "{code} should be warning");
    }
}

#[test]
fn cbkf_file_format_errors_are_error_severity() {
    let codes = [
        ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
        ErrorCode::CBKF221_RDW_UNDERFLOW,
    ];
    for code in codes {
        let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
        assert!(r.report_error(make_error(code, "file")).is_ok());
        assert!(r.has_errors());
    }
}

#[test]
fn infrastructure_error_is_fatal() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    assert!(
        r.report_error(make_error(ErrorCode::CBKI001_INVALID_STATE, "state"))
            .is_err()
    );
}

// =========================================================================
// 7. Corruption warnings
// =========================================================================

#[test]
fn corruption_warnings_accumulated_correctly() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    for _ in 0..7 {
        r.report_warning(make_error(ErrorCode::CBKF104_RDW_SUSPECT_ASCII, "ascii"));
    }
    for _ in 0..3 {
        r.report_warning(make_error(ErrorCode::CBKC301_INVALID_EBCDIC_BYTE, "ebcdic"));
    }
    assert_eq!(r.summary().corruption_warnings, 10);
    assert_eq!(r.warning_count(), 10);
}

#[test]
fn non_corruption_warning_does_not_increment_corruption_count() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    r.report_warning(make_error(ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO, "blank"));
    r.report_warning(make_error(
        ErrorCode::CBKD423_EDITED_PIC_BLANK_WHEN_ZERO,
        "bwz",
    ));
    assert_eq!(r.summary().corruption_warnings, 0);
    assert_eq!(r.warning_count(), 2);
}

// =========================================================================
// 8. Record tracking
// =========================================================================

#[test]
fn start_record_updates_total_records() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    r.start_record(0);
    assert_eq!(r.summary().total_records, 0);
    r.start_record(42);
    assert_eq!(r.summary().total_records, 42);
    r.start_record(100);
    assert_eq!(r.summary().total_records, 100);
}

#[test]
fn records_with_errors_tracks_high_water_mark() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    let _ = r.report_error(make_data_error("a").with_record(5));
    let _ = r.report_error(make_data_error("b").with_record(3));
    let _ = r.report_error(make_data_error("c").with_record(10));
    assert_eq!(r.summary().records_with_errors, 10);
}

#[test]
fn warnings_do_not_affect_records_with_errors() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    r.report_warning(make_error(ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO, "blank").with_record(42));
    assert_eq!(r.summary().records_with_errors, 0);
}

// =========================================================================
// 9. Edge cases
// =========================================================================

#[test]
fn empty_reporter_is_completely_clean() {
    let r = ErrorReporter::new(ErrorMode::Lenient, None);
    assert!(!r.has_errors());
    assert!(!r.has_warnings());
    assert_eq!(r.error_count(), 0);
    assert_eq!(r.warning_count(), 0);

    let s = r.summary();
    assert!(s.first_error.is_none());
    assert!(s.last_error.is_none());
    assert_eq!(s.total_records, 0);
    assert_eq!(s.records_with_errors, 0);
    assert_eq!(s.corruption_warnings, 0);
}

#[test]
fn error_summary_default_is_clean() {
    let s = ErrorSummary::default();
    assert!(!s.has_errors());
    assert!(!s.has_warnings());
    assert_eq!(s.error_count(), 0);
    assert_eq!(s.warning_count(), 0);
    assert!(s.first_error.is_none());
    assert!(s.last_error.is_none());
    assert_eq!(s.records_with_errors, 0);
    assert_eq!(s.total_records, 0);
    assert_eq!(s.corruption_warnings, 0);
}

#[test]
fn very_long_error_message_handled() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    let long_msg = "x".repeat(10_000);
    let _ = r.report_error(make_data_error(&long_msg));
    assert_eq!(
        r.summary()
            .first_error
            .as_ref()
            .unwrap()
            .error
            .message
            .len(),
        10_000
    );
}

#[test]
fn error_with_full_context_reported() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    let err = make_data_error("ctx test")
        .with_record(42)
        .with_field("CUSTOMER.BALANCE")
        .with_offset(128);
    let _ = r.report_error(err);
    let first = r.summary().first_error.as_ref().unwrap();
    assert_eq!(first.error.code, ErrorCode::CBKD401_COMP3_INVALID_NIBBLE);
    let ctx = first.error.context.as_ref().unwrap();
    assert_eq!(ctx.record_index, Some(42));
    assert_eq!(ctx.field_path.as_deref(), Some("CUSTOMER.BALANCE"));
    assert_eq!(ctx.byte_offset, Some(128));
}

// =========================================================================
// 10. Display/Debug/trait tests
// =========================================================================

#[test]
fn error_severity_display_values() {
    assert_eq!(ErrorSeverity::Info.to_string(), "INFO");
    assert_eq!(ErrorSeverity::Warning.to_string(), "WARN");
    assert_eq!(ErrorSeverity::Error.to_string(), "ERROR");
    assert_eq!(ErrorSeverity::Fatal.to_string(), "FATAL");
}

#[test]
fn error_severity_ordering() {
    assert!(ErrorSeverity::Info < ErrorSeverity::Warning);
    assert!(ErrorSeverity::Warning < ErrorSeverity::Error);
    assert!(ErrorSeverity::Error < ErrorSeverity::Fatal);
}

#[test]
fn error_severity_equality() {
    assert_eq!(ErrorSeverity::Info, ErrorSeverity::Info);
    assert_ne!(ErrorSeverity::Info, ErrorSeverity::Fatal);
}

#[test]
fn error_severity_hash() {
    use std::collections::HashSet;
    let mut set = HashSet::new();
    set.insert(ErrorSeverity::Info);
    set.insert(ErrorSeverity::Warning);
    set.insert(ErrorSeverity::Error);
    set.insert(ErrorSeverity::Fatal);
    set.insert(ErrorSeverity::Info); // duplicate
    assert_eq!(set.len(), 4);
}

#[test]
fn error_mode_equality() {
    assert_eq!(ErrorMode::Strict, ErrorMode::Strict);
    assert_eq!(ErrorMode::Lenient, ErrorMode::Lenient);
    assert_ne!(ErrorMode::Strict, ErrorMode::Lenient);
}

#[test]
fn error_mode_debug_format() {
    let dbg = format!("{:?}", ErrorMode::Strict);
    assert!(dbg.contains("Strict"));
    let dbg = format!("{:?}", ErrorMode::Lenient);
    assert!(dbg.contains("Lenient"));
}

#[test]
fn error_severity_debug_format() {
    assert!(format!("{:?}", ErrorSeverity::Fatal).contains("Fatal"));
}

// =========================================================================
// 11. ErrorCode serde roundtrip (through reporter context)
// =========================================================================

#[test]
fn error_code_json_roundtrip() {
    let codes = [
        ErrorCode::CBKP001_SYNTAX,
        ErrorCode::CBKS121_COUNTER_NOT_FOUND,
        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
        ErrorCode::CBKR211_RDW_RESERVED_NONZERO,
        ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
    ];
    for code in codes {
        let json = serde_json::to_string(&code).unwrap();
        let back: ErrorCode = serde_json::from_str(&json).unwrap();
        assert_eq!(code, back, "roundtrip failed for {code}");
    }
}

#[test]
fn error_code_json_format_is_quoted_variant() {
    assert_eq!(
        serde_json::to_string(&ErrorCode::CBKP001_SYNTAX).unwrap(),
        r#""CBKP001_SYNTAX""#
    );
    assert_eq!(
        serde_json::to_string(&ErrorCode::CBKD401_COMP3_INVALID_NIBBLE).unwrap(),
        r#""CBKD401_COMP3_INVALID_NIBBLE""#,
    );
}

#[test]
fn summary_error_codes_map_serializable() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    let _ = r.report_error(make_data_error("a"));
    let _ = r.report_error(make_data_error("b"));
    let _ = r.report_error(make_error(ErrorCode::CBKD411_ZONED_BAD_SIGN, "c"));

    let codes = &r.summary().error_codes;
    assert_eq!(
        *codes.get(&ErrorCode::CBKD401_COMP3_INVALID_NIBBLE).unwrap(),
        2
    );
    assert_eq!(*codes.get(&ErrorCode::CBKD411_ZONED_BAD_SIGN).unwrap(), 1);

    for (code, count) in codes {
        let json = serde_json::to_string(code).unwrap();
        assert!(!json.is_empty());
        assert!(*count > 0);
    }
}

// =========================================================================
// 12. Builder pattern and configuration
// =========================================================================

#[test]
fn verbose_logging_builder_pattern() {
    let r = ErrorReporter::new(ErrorMode::Lenient, None).with_verbose_logging(false);
    assert!(!r.has_errors());
    assert_eq!(r.error_count(), 0);
}

#[test]
fn verbose_logging_true_by_default() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    // Should not panic or fail when logging is enabled
    let _ = r.report_error(make_data_error("test with verbose"));
    assert_eq!(r.error_count(), 1);
}

// =========================================================================
// 13. Additional edge cases
// =========================================================================

#[test]
fn only_warnings_no_errors_state() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    for _ in 0..20 {
        r.report_warning(make_error(ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO, "blank"));
    }
    assert!(!r.has_errors());
    assert!(r.has_warnings());
    assert_eq!(r.error_count(), 0);
    assert_eq!(r.warning_count(), 20);
}

#[test]
fn interleaved_errors_and_warnings() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    r.report_warning(make_error(ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO, "w1"));
    let _ = r.report_error(make_data_error("e1"));
    r.report_warning(make_error(ErrorCode::CBKF104_RDW_SUSPECT_ASCII, "w2"));
    let _ = r.report_error(make_error(ErrorCode::CBKD411_ZONED_BAD_SIGN, "e2"));
    r.report_warning(make_error(ErrorCode::CBKC301_INVALID_EBCDIC_BYTE, "w3"));

    assert_eq!(r.error_count(), 2);
    assert_eq!(r.warning_count(), 3);
    assert_eq!(r.summary().corruption_warnings, 2); // RDW_SUSPECT_ASCII + INVALID_EBCDIC_BYTE
}

#[test]
fn error_without_record_context_does_not_affect_records_with_errors() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    let _ = r.report_error(make_data_error("no record context"));
    assert_eq!(r.summary().records_with_errors, 0);
}

#[test]
fn multiple_distinct_error_codes_tracked() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    let codes = [
        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
        ErrorCode::CBKD411_ZONED_BAD_SIGN,
        ErrorCode::CBKD410_ZONED_OVERFLOW,
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
        ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
    ];
    for code in codes {
        let _ = r.report_error(make_error(code, "test"));
    }
    assert_eq!(r.summary().error_codes.len(), 5);
    assert_eq!(r.error_count(), 5);
}

#[test]
fn rdw_reserved_nonzero_mode_dependent() {
    // Lenient: warning
    let mut lenient = ErrorReporter::new(ErrorMode::Lenient, None);
    assert!(
        lenient
            .report_error(make_error(ErrorCode::CBKR211_RDW_RESERVED_NONZERO, "rdw"))
            .is_ok()
    );
    assert!(lenient.has_warnings());
    assert!(!lenient.has_errors());

    // Strict: fatal
    let mut strict = ErrorReporter::new(ErrorMode::Strict, None);
    assert!(
        strict
            .report_error(make_error(ErrorCode::CBKR211_RDW_RESERVED_NONZERO, "rdw"))
            .is_err()
    );
}
