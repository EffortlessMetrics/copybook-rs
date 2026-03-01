// SPDX-License-Identifier: AGPL-3.0-or-later
//! Integration tests for copybook-error-reporter.

#![allow(clippy::unwrap_used)]

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
// 1. Error reporting policies
// =========================================================================

#[test]
fn strict_mode_stops_on_first_data_error() {
    let mut r = ErrorReporter::new(ErrorMode::Strict, None);
    let result = r.report_error(make_data_error("first"));
    assert!(result.is_err(), "strict mode must halt on first error");
    assert_eq!(r.error_count(), 1);
}

#[test]
fn lenient_mode_continues_on_data_errors() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    for i in 0..5 {
        let result = r.report_error(make_data_error(&format!("err {i}")));
        assert!(result.is_ok(), "lenient mode must continue");
    }
    assert_eq!(r.error_count(), 5);
}

#[test]
fn strict_mode_stops_on_fatal_errors() {
    let mut r = ErrorReporter::new(ErrorMode::Strict, None);
    let result = r.report_error(make_error(ErrorCode::CBKP001_SYNTAX, "bad syntax"));
    assert!(result.is_err());
}

#[test]
fn lenient_mode_stops_on_fatal_errors() {
    // Fatal errors halt processing regardless of mode.
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    let result = r.report_error(make_error(ErrorCode::CBKP001_SYNTAX, "bad syntax"));
    assert!(
        result.is_err(),
        "fatal errors must halt even in lenient mode"
    );
    assert!(r.has_errors());
}

// =========================================================================
// 2. Error summaries
// =========================================================================

#[test]
fn summary_reflects_mixed_errors_and_warnings() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);

    // Warnings
    r.report_warning(make_error(ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO, "blank"));
    r.report_warning(make_error(
        ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
        "bad byte",
    ));

    // Data errors
    let _ = r.report_error(make_data_error("nibble1"));
    let _ = r.report_error(make_error(ErrorCode::CBKD411_ZONED_BAD_SIGN, "sign"));

    let s = r.summary();
    assert_eq!(s.error_count(), 2);
    assert_eq!(s.warning_count(), 2);
    assert!(s.has_errors());
    assert!(s.has_warnings());
}

#[test]
fn generate_report_text_format_contains_sections() {
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
fn generate_report_empty_reporter_has_no_counts_sections() {
    let r = ErrorReporter::new(ErrorMode::Strict, None);
    let report = r.generate_report();
    assert!(report.contains("=== Error Summary ==="));
    assert!(report.contains("Total records processed: 0"));
    assert!(!report.contains("Error counts by severity:"));
    assert!(!report.contains("Error counts by code:"));
    assert!(!report.contains("Transfer corruption warnings"));
    assert!(!report.contains("First error:"));
}

// =========================================================================
// 3. Max-errors threshold enforcement
// =========================================================================

#[test]
fn max_errors_allows_up_to_limit_then_stops() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, Some(3));

    for i in 1..=3 {
        let res = r.report_error(make_data_error(&format!("err {i}")));
        assert!(res.is_ok(), "error {i} should be under limit");
    }

    let res = r.report_error(make_data_error("err 4"));
    assert!(res.is_err(), "4th error exceeds max_errors=3");
    assert_eq!(r.error_count(), 4);
}

#[test]
fn max_errors_zero_rejects_immediately() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, Some(0));
    let res = r.report_error(make_data_error("first"));
    assert!(res.is_err(), "max_errors=0 must reject first error");
}

#[test]
fn max_errors_one_allows_exactly_one() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, Some(1));
    assert!(r.report_error(make_data_error("first")).is_ok());
    assert!(r.report_error(make_data_error("second")).is_err());
    assert_eq!(r.error_count(), 2);
}

#[test]
fn max_errors_not_affected_by_warnings() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, Some(1));

    // Warnings should not count against max_errors
    for _ in 0..20 {
        r.report_warning(make_error(ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO, "blank"));
    }
    assert_eq!(r.warning_count(), 20);
    assert_eq!(r.error_count(), 0);

    // First real error still passes
    assert!(r.report_error(make_data_error("first real")).is_ok());
    // Second exceeds limit
    assert!(r.report_error(make_data_error("second real")).is_err());
}

#[test]
fn max_errors_not_affected_by_fatal_errors() {
    // Fatal errors halt immediately but via their own path.
    // They do count in the error_count tally.
    let mut r = ErrorReporter::new(ErrorMode::Lenient, Some(10));
    let res = r.report_error(make_error(ErrorCode::CBKP001_SYNTAX, "fatal"));
    assert!(res.is_err());
    assert_eq!(r.error_count(), 1);
}

// =========================================================================
// 4. Error categories (CBK families) reported correctly
// =========================================================================

#[test]
fn cbkp_parse_errors_classified_as_fatal() {
    let parse_codes = [
        ErrorCode::CBKP001_SYNTAX,
        ErrorCode::CBKP011_UNSUPPORTED_CLAUSE,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        ErrorCode::CBKP022_NESTED_ODO,
        ErrorCode::CBKP023_ODO_REDEFINES,
        ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC,
        ErrorCode::CBKP101_INVALID_PIC,
    ];
    for code in parse_codes {
        let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
        assert!(
            r.report_error(make_error(code, "parse")).is_err(),
            "{code} must be fatal"
        );
    }
}

#[test]
fn cbks_schema_errors_classified_as_fatal() {
    let schema_codes = [
        ErrorCode::CBKS121_COUNTER_NOT_FOUND,
        ErrorCode::CBKS141_RECORD_TOO_LARGE,
        ErrorCode::CBKS601_RENAME_UNKNOWN_FROM,
        ErrorCode::CBKS602_RENAME_UNKNOWN_THRU,
        ErrorCode::CBKS603_RENAME_NOT_CONTIGUOUS,
        ErrorCode::CBKS701_PROJECTION_INVALID_ODO,
        ErrorCode::CBKS702_PROJECTION_UNRESOLVED_ALIAS,
        ErrorCode::CBKS703_PROJECTION_FIELD_NOT_FOUND,
    ];
    for code in schema_codes {
        let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
        assert!(
            r.report_error(make_error(code, "schema")).is_err(),
            "{code} must be fatal"
        );
    }
}

#[test]
fn cbkd_data_errors_are_error_severity_in_lenient() {
    let data_codes = [
        ErrorCode::CBKD101_INVALID_FIELD_TYPE,
        ErrorCode::CBKD301_RECORD_TOO_SHORT,
        ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED,
        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
        ErrorCode::CBKD410_ZONED_OVERFLOW,
        ErrorCode::CBKD411_ZONED_BAD_SIGN,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
        ErrorCode::CBKD431_FLOAT_NAN,
        ErrorCode::CBKD432_FLOAT_INFINITY,
    ];
    for code in data_codes {
        let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
        assert!(
            r.report_error(make_error(code, "data")).is_ok(),
            "{code} must allow continuation in lenient mode"
        );
        assert!(r.has_errors());
    }
}

#[test]
fn cbke_encode_errors_are_error_severity_in_lenient() {
    let encode_codes = [
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
        ErrorCode::CBKE521_ARRAY_LEN_OOB,
        ErrorCode::CBKE530_SIGN_SEPARATE_ENCODE_ERROR,
        ErrorCode::CBKE531_FLOAT_ENCODE_OVERFLOW,
    ];
    for code in encode_codes {
        let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
        assert!(
            r.report_error(make_error(code, "enc")).is_ok(),
            "{code} must allow continuation"
        );
        assert!(r.has_errors());
    }
}

#[test]
fn cbke_fatal_encode_errors_stop_in_lenient() {
    let fatal_encode = [
        ErrorCode::CBKE505_SCALE_MISMATCH,
        ErrorCode::CBKE510_NUMERIC_OVERFLOW,
        ErrorCode::CBKE515_STRING_LENGTH_VIOLATION,
    ];
    for code in fatal_encode {
        let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
        assert!(
            r.report_error(make_error(code, "enc")).is_err(),
            "{code} must be fatal"
        );
    }
}

#[test]
fn cbkf_file_format_errors_are_error_severity() {
    let file_codes = [
        ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
        ErrorCode::CBKF221_RDW_UNDERFLOW,
    ];
    for code in file_codes {
        let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
        assert!(
            r.report_error(make_error(code, "file")).is_ok(),
            "{code} should be Error severity in lenient"
        );
        assert!(r.has_errors());
    }
}

#[test]
fn cbki_infrastructure_error_is_fatal() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    assert!(
        r.report_error(make_error(ErrorCode::CBKI001_INVALID_STATE, "state"))
            .is_err()
    );
}

#[test]
fn warning_class_codes_produce_warnings_not_errors() {
    let warning_codes = [
        ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO,
        ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
        ErrorCode::CBKD423_EDITED_PIC_BLANK_WHEN_ZERO,
        ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
    ];
    for code in warning_codes {
        let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
        let res = r.report_error(make_error(code, "warn"));
        assert!(res.is_ok(), "{code} should continue");
        assert!(!r.has_errors(), "{code} should not count as error");
        assert!(r.has_warnings(), "{code} should be a warning");
    }
}

#[test]
fn odo_clipped_raised_mode_dependent_severity() {
    for code in [
        ErrorCode::CBKS301_ODO_CLIPPED,
        ErrorCode::CBKS302_ODO_RAISED,
    ] {
        // Lenient → warning
        let mut lenient = ErrorReporter::new(ErrorMode::Lenient, None);
        assert!(lenient.report_error(make_error(code, "odo")).is_ok());
        assert!(lenient.has_warnings());
        assert!(!lenient.has_errors());

        // Strict → fatal
        let mut strict = ErrorReporter::new(ErrorMode::Strict, None);
        assert!(strict.report_error(make_error(code, "odo")).is_err());
    }
}

// =========================================================================
// 5. Structured output
// =========================================================================

#[test]
fn error_code_json_round_trip() {
    let codes = [
        ErrorCode::CBKP001_SYNTAX,
        ErrorCode::CBKS121_COUNTER_NOT_FOUND,
        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
        ErrorCode::CBKR211_RDW_RESERVED_NONZERO,
        ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
        ErrorCode::CBKA001_BASELINE_ERROR,
        ErrorCode::CBKW001_SCHEMA_CONVERSION,
    ];
    for code in codes {
        let json = serde_json::to_string(&code).unwrap();
        let deserialized: ErrorCode = serde_json::from_str(&json).unwrap();
        assert_eq!(code, deserialized, "round-trip failed for {code}");
    }
}

#[test]
fn error_code_json_format_is_quoted_variant_name() {
    let pairs = [
        (ErrorCode::CBKP001_SYNTAX, r#""CBKP001_SYNTAX""#),
        (
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
            r#""CBKD401_COMP3_INVALID_NIBBLE""#,
        ),
        (
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            r#""CBKE501_JSON_TYPE_MISMATCH""#,
        ),
    ];
    for (code, expected) in pairs {
        assert_eq!(serde_json::to_string(&code).unwrap(), expected);
    }
}

#[test]
fn summary_error_codes_map_serializable() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    let _ = r.report_error(make_data_error("a"));
    let _ = r.report_error(make_error(ErrorCode::CBKD411_ZONED_BAD_SIGN, "b"));
    let _ = r.report_error(make_data_error("c"));

    let codes = &r.summary().error_codes;
    // Verify each entry serializes cleanly
    for (code, count) in codes {
        let json = serde_json::to_string(code).unwrap();
        assert!(!json.is_empty());
        assert!(*count > 0);
    }
    assert_eq!(
        *codes.get(&ErrorCode::CBKD401_COMP3_INVALID_NIBBLE).unwrap(),
        2
    );
    assert_eq!(*codes.get(&ErrorCode::CBKD411_ZONED_BAD_SIGN).unwrap(), 1);
}

// =========================================================================
// 6. Edge cases
// =========================================================================

#[test]
fn no_errors_reporter_is_clean() {
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
fn single_error_is_both_first_and_last() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    let _ = r.report_error(make_data_error("only one"));
    let s = r.summary();
    let first = s.first_error.as_ref().unwrap();
    let last = s.last_error.as_ref().unwrap();
    assert_eq!(first.error.code, last.error.code);
    assert_eq!(first.error.message, "only one");
    assert_eq!(last.error.message, "only one");
}

#[test]
fn many_errors_first_and_last_differ() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    for i in 0..50 {
        let _ = r.report_error(make_data_error(&format!("error {i}")));
    }
    let s = r.summary();
    assert_eq!(s.first_error.as_ref().unwrap().error.message, "error 0");
    assert_eq!(s.last_error.as_ref().unwrap().error.message, "error 49");
    assert_eq!(r.error_count(), 50);
}

#[test]
fn duplicate_error_codes_counted_correctly() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    for _ in 0..10 {
        let _ = r.report_error(make_data_error("dup"));
    }
    let codes = &r.summary().error_codes;
    assert_eq!(
        *codes.get(&ErrorCode::CBKD401_COMP3_INVALID_NIBBLE).unwrap(),
        10
    );
    assert_eq!(codes.len(), 1, "only one distinct code expected");
}

#[test]
fn corruption_warnings_accumulated() {
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
    assert_eq!(r.summary().corruption_warnings, 0);
    assert_eq!(r.warning_count(), 1);
}

#[test]
fn record_tracking_via_start_record() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    r.start_record(0);
    assert_eq!(r.summary().total_records, 0);
    r.start_record(99);
    assert_eq!(r.summary().total_records, 99);
}

#[test]
fn records_with_errors_tracks_highest_record_index() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    let _ = r.report_error(make_data_error("a").with_record(5));
    let _ = r.report_error(make_data_error("b").with_record(3));
    let _ = r.report_error(make_data_error("c").with_record(10));
    // Implementation tracks high-water mark
    assert_eq!(r.summary().records_with_errors, 10);
}

#[test]
fn warnings_do_not_affect_records_with_errors() {
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    r.report_warning(make_error(ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO, "blank").with_record(42));
    assert_eq!(r.summary().records_with_errors, 0);
}

#[test]
fn verbose_logging_builder_pattern() {
    let r = ErrorReporter::new(ErrorMode::Lenient, None).with_verbose_logging(false);
    assert!(!r.has_errors());
    assert_eq!(r.error_count(), 0);
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
fn error_mode_equality() {
    assert_eq!(ErrorMode::Strict, ErrorMode::Strict);
    assert_eq!(ErrorMode::Lenient, ErrorMode::Lenient);
    assert_ne!(ErrorMode::Strict, ErrorMode::Lenient);
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
    let _ = r.report_error(make_error(
        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
        &long_msg,
    ));
    let first = r.summary().first_error.as_ref().unwrap();
    assert_eq!(first.error.message.len(), 10_000);
}

#[test]
fn generate_report_corruption_section_only_when_nonzero() {
    // With corruption
    let mut r = ErrorReporter::new(ErrorMode::Lenient, None);
    r.report_warning(make_error(ErrorCode::CBKF104_RDW_SUSPECT_ASCII, "ascii"));
    assert!(
        r.generate_report()
            .contains("Transfer corruption warnings: 1")
    );

    // Without corruption
    let mut r2 = ErrorReporter::new(ErrorMode::Lenient, None);
    r2.report_warning(make_error(ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO, "blank"));
    assert!(
        !r2.generate_report()
            .contains("Transfer corruption warnings")
    );
}

#[test]
fn strict_mode_stops_on_all_data_error_codes() {
    let data_codes = [
        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
        ErrorCode::CBKD411_ZONED_BAD_SIGN,
        ErrorCode::CBKD410_ZONED_OVERFLOW,
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
        ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
    ];
    for code in data_codes {
        let mut r = ErrorReporter::new(ErrorMode::Strict, None);
        assert!(
            r.report_error(make_error(code, "data")).is_err(),
            "strict mode must stop on {code}"
        );
    }
}
