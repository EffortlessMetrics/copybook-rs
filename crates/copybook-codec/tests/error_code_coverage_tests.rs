// SPDX-License-Identifier: AGPL-3.0-or-later
//! Error code coverage tests for error codes lacking dedicated test assertions.
//!
//! This module fills gaps identified by the error code audit:
//! - CBKD422_EDITED_PIC_SIGN_MISMATCH: Invalid sign character in edited PIC decode

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_codec::{Codepage, DecodeOptions, decode_record};
use copybook_core::error::ErrorCode;
use copybook_core::parse_copybook;

// =============================================================================
// CBKD422_EDITED_PIC_SIGN_MISMATCH: Invalid sign in edited PIC decode
// =============================================================================

/// Test CBKD422: Leading plus sign position contains invalid character
#[test]
fn test_cbkd422_leading_plus_invalid_char() {
    let copybook = "01 REC.\n   05 AMOUNT PIC +ZZZ9.";
    let schema = parse_copybook(copybook).unwrap();

    // Data: "X  12" - 'X' is invalid where '+' or ' ' is expected
    let data = b"X  12";
    let options = DecodeOptions::new().with_codepage(Codepage::ASCII);
    let result = decode_record(&schema, data, &options);

    assert!(result.is_err(), "Expected CBKD422 for invalid sign char");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
        "Expected CBKD422_EDITED_PIC_SIGN_MISMATCH, got {:?}",
        err.code
    );
}

/// Test CBKD422: Leading minus sign position contains invalid character
#[test]
fn test_cbkd422_leading_minus_invalid_char() {
    let copybook = "01 REC.\n   05 AMOUNT PIC -ZZZ9.";
    let schema = parse_copybook(copybook).unwrap();

    // Data: "X  12" - 'X' is invalid where '-' or ' ' is expected
    let data = b"X  12";
    let options = DecodeOptions::new().with_codepage(Codepage::ASCII);
    let result = decode_record(&schema, data, &options);

    assert!(result.is_err(), "Expected CBKD422 for invalid sign char");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
        "Expected CBKD422_EDITED_PIC_SIGN_MISMATCH, got {:?}",
        err.code
    );
}

/// Test CBKD422: CR sign field contains invalid characters
#[test]
fn test_cbkd422_cr_invalid_chars() {
    let copybook = "01 REC.\n   05 AMOUNT PIC ZZZ9CR.";
    let schema = parse_copybook(copybook).unwrap();

    // Data: "  12XX" - 'XX' is invalid where 'CR' or '  ' is expected
    let data = b"  12XX";
    let options = DecodeOptions::new().with_codepage(Codepage::ASCII);
    let result = decode_record(&schema, data, &options);

    assert!(
        result.is_err(),
        "Expected CBKD422 for invalid CR sign chars"
    );
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
        "Expected CBKD422_EDITED_PIC_SIGN_MISMATCH, got {:?}",
        err.code
    );
}

/// Test CBKD422: DB sign field contains invalid characters
#[test]
fn test_cbkd422_db_invalid_chars() {
    let copybook = "01 REC.\n   05 AMOUNT PIC ZZZ9DB.";
    let schema = parse_copybook(copybook).unwrap();

    // Data: "  12YZ" - 'YZ' is invalid where 'DB' or '  ' is expected
    let data = b"  12YZ";
    let options = DecodeOptions::new().with_codepage(Codepage::ASCII);
    let result = decode_record(&schema, data, &options);

    assert!(
        result.is_err(),
        "Expected CBKD422 for invalid DB sign chars"
    );
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
        "Expected CBKD422_EDITED_PIC_SIGN_MISMATCH, got {:?}",
        err.code
    );
}
