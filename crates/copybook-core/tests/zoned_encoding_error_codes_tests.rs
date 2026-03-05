// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used)]

//! Test scaffolding for zoned encoding error codes - Issue #48
//!
//! Tests error handling spec: SPEC.manifest.yml#error-codes-new-cbkd-codes
//!
//! This test suite validates:
//! - AC11: Enterprise error handling with new CBKD* error codes
//! - `CBKD413_ZONED_INVALID_ENCODING`: Invalid zoned decimal encoding format
//! - `CBKD414_ZONED_MIXED_ENCODING`: Mixed ASCII/EBCDIC encoding in single field
//! - `CBKD415_ZONED_ENCODING_DETECTION_FAILED`: Unable to detect encoding format
//!
//! Note: Runtime behavior tests are in copybook-codec crate tests since they depend on `DecodeOptions`

use copybook_core::error::{Error, ErrorCode};

/// AC11: Test `CBKD413_ZONED_INVALID_ENCODING` error code
/// Tests error handling spec: SPEC.manifest.yml#CBKD413-invalid-encoding
#[test]
fn test_cbkd413_zoned_invalid_encoding() {
    // Test existing error code works
    let existing_error = Error::new(
        ErrorCode::CBKD411_ZONED_BAD_SIGN,
        "Test existing error code",
    );
    assert_eq!(existing_error.code, ErrorCode::CBKD411_ZONED_BAD_SIGN);

    let overflow_error = Error::new(
        ErrorCode::CBKD410_ZONED_OVERFLOW,
        "Test overflow error code",
    );
    assert_eq!(overflow_error.code, ErrorCode::CBKD410_ZONED_OVERFLOW);

    // Test new error code
    let new_error = Error::new(
        ErrorCode::CBKD413_ZONED_INVALID_ENCODING,
        "Test new error code",
    );
    assert_eq!(new_error.code, ErrorCode::CBKD413_ZONED_INVALID_ENCODING);
}

/// AC11: Test `CBKD414_ZONED_MIXED_ENCODING` error code
/// Tests error handling spec: SPEC.manifest.yml#CBKD414-mixed-encoding
#[test]
fn test_cbkd414_zoned_mixed_encoding() {
    // Test existing error code works
    let existing_error = Error::new(
        ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO,
        "Test existing error code",
    );
    assert_eq!(existing_error.code, ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO);

    // Test new error code
    let new_error = Error::new(
        ErrorCode::CBKD414_ZONED_MIXED_ENCODING,
        "Test new error code",
    );
    assert_eq!(new_error.code, ErrorCode::CBKD414_ZONED_MIXED_ENCODING);
}

/// AC11: Test `CBKD415_ZONED_ENCODING_DETECTION_FAILED` error code
/// Tests error handling spec: SPEC.manifest.yml#CBKD415-detection-failed
#[test]
fn test_cbkd415_zoned_encoding_detection_failed() {
    // Test error code taxonomy consistency
    let error_string = format!("{}", ErrorCode::CBKD411_ZONED_BAD_SIGN);
    assert_eq!(error_string, "CBKD411_ZONED_BAD_SIGN");

    let overflow_string = format!("{}", ErrorCode::CBKD410_ZONED_OVERFLOW);
    assert_eq!(overflow_string, "CBKD410_ZONED_OVERFLOW");

    // Test new error code
    let new_error = Error::new(
        ErrorCode::CBKD415_ZONED_ENCODING_AMBIGUOUS,
        "Test new error code",
    );
    assert_eq!(new_error.code, ErrorCode::CBKD415_ZONED_ENCODING_AMBIGUOUS);
}

/// Test error context information for zoned encoding errors
/// Tests error handling spec: SPEC.manifest.yml#error-context-zoned-encoding
#[test]
fn test_zoned_encoding_error_context() {
    // Test error context creation patterns
    let mut error = Error::new(ErrorCode::CBKD411_ZONED_BAD_SIGN, "Test error with context");

    error = error
        .with_field("TEST-FIELD")
        .with_record(42)
        .with_offset(123);

    assert!(error.context.is_some());
    let context = error.context.unwrap();
    assert_eq!(context.field_path, Some("TEST-FIELD".to_string()));
    assert_eq!(context.record_index, Some(42));
    assert_eq!(context.byte_offset, Some(123));
}

/// Test that error messages are clear and actionable
/// Tests error handling spec: SPEC.manifest.yml#clear-error-messages
#[test]
fn test_zoned_encoding_error_messages_clarity() {
    // Test error message formatting with existing error codes
    let error = Error::new(
        ErrorCode::CBKD411_ZONED_BAD_SIGN,
        "Invalid zoned decimal sign nibble: found 0xE, expected 0xC (positive) or 0xD (negative)",
    );

    assert!(error.message.contains("Invalid zoned decimal sign nibble"));
    assert!(error.message.contains("found 0xE"));
    assert!(error.message.contains("expected 0xC"));

    // Test new error message patterns
    let new_error = Error::new(
        ErrorCode::CBKD413_ZONED_INVALID_ENCODING,
        "Invalid zoned encoding format: expected ASCII (0x3X) or EBCDIC (0xFX), found 0xAX",
    );
    assert!(new_error.message.contains("Invalid zoned encoding format"));
    assert!(new_error.message.contains("found 0xAX"));
}

/// Test error code stability and backward compatibility
/// Tests error handling spec: SPEC.manifest.yml#error-code-stability
#[test]
fn test_error_code_stability() {
    // Test that existing error codes still work
    let existing_error = Error::new(
        ErrorCode::CBKD411_ZONED_BAD_SIGN,
        "Test existing error code",
    );
    assert_eq!(existing_error.code, ErrorCode::CBKD411_ZONED_BAD_SIGN);

    // Verify error code string representation follows pattern
    let error_string = format!("{}", ErrorCode::CBKD411_ZONED_BAD_SIGN);
    assert_eq!(error_string, "CBKD411_ZONED_BAD_SIGN");

    let overflow_string = format!("{}", ErrorCode::CBKD410_ZONED_OVERFLOW);
    assert_eq!(overflow_string, "CBKD410_ZONED_OVERFLOW");

    // Test new error codes
    let _new_error_413 = Error::new(ErrorCode::CBKD413_ZONED_INVALID_ENCODING, "Test error");
    let error_413_string = format!("{}", ErrorCode::CBKD413_ZONED_INVALID_ENCODING);
    assert_eq!(error_413_string, "CBKD413_ZONED_INVALID_ENCODING");

    let _new_error_414 = Error::new(ErrorCode::CBKD414_ZONED_MIXED_ENCODING, "Test error");
    let error_414_string = format!("{}", ErrorCode::CBKD414_ZONED_MIXED_ENCODING);
    assert_eq!(error_414_string, "CBKD414_ZONED_MIXED_ENCODING");

    let _new_error_415 = Error::new(ErrorCode::CBKD415_ZONED_ENCODING_AMBIGUOUS, "Test error");
    let error_415_string = format!("{}", ErrorCode::CBKD415_ZONED_ENCODING_AMBIGUOUS);
    assert_eq!(error_415_string, "CBKD415_ZONED_ENCODING_AMBIGUOUS");
}
