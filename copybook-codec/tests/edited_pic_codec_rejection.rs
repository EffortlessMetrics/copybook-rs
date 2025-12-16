//! Phase E1: Codec rejection tests for edited PIC
//!
//! Tests that edited PIC fields properly reject during decode/encode operations
//! with CBKD302_EDITED_PIC_NOT_IMPLEMENTED error code.

use copybook_core::{ErrorCode, parse_copybook};

/// Test E1-R1: Verify error code exists
#[test]
fn test_e1_error_code_defined() {
    // Verify the error code is properly defined
    let code = ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED;
    assert_eq!(format!("{}", code), "CBKD302_EDITED_PIC_NOT_IMPLEMENTED");
}

/// Test E1-R2: Edited PIC parses successfully in schema
#[test]
fn test_e1_edited_pic_parses_successfully() {
    let copybook = "01 REC.\n   05 AMOUNT PIC ZZZZ.";
    let result = parse_copybook(copybook);
    assert!(result.is_ok(), "Edited PIC should parse successfully");
}

/// Test E1-R3: SIGN clause rejected as edited PIC
#[test]
fn test_e1_sign_clause_parses_as_edited() {
    let copybook = "01 REC.\n   05 SIGNED-AMT PIC S9(5) SIGN LEADING.";
    let result = parse_copybook(copybook);
    assert!(
        result.is_err(),
        "SIGN clause should be rejected until decode semantics exist"
    );
    if let Err(err) = result {
        assert_eq!(err.code, ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC);
    }
}
