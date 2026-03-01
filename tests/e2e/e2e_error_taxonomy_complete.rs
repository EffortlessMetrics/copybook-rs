// SPDX-License-Identifier: AGPL-3.0-or-later
//! Complete error taxonomy coverage tests.
//!
//! Ensures every `CBK*` error code defined in `copybook_error::ErrorCode` has at
//! least one dedicated test that either:
//! - Triggers the error through the public API and asserts the exact code, **or**
//! - Documents why the code cannot currently be triggered (reserved, warning-only,
//!   or emitted by a crate outside the e2e dependency graph).
//!
//! ## Summary (61 total error codes)
//!
//! | Status       | Count | Notes                                       |
//! |--------------|-------|---------------------------------------------|
//! | Already tested elsewhere | 52 | Covered in crate-level or other e2e tests |
//! | Newly tested here        |  1 | CBKE531                                    |
//! | Cannot trigger (reserved/not emitted) | 8 | CBKS603, CBKS608, CBKD431, CBKD432, CBKW002–005 |

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::{Codepage, EncodeOptions, JsonNumberMode, RecordFormat, encode_record};
use copybook_core::ErrorCode;
use copybook_core::schema::{Field, FieldKind, Schema};
use serde_json::json;

fn encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

// ===========================================================================
// CBKE531 – Float encode overflow (COMP-1 field)
//
// Encode a JSON f64 value that exceeds f32::MAX into a COMP-1 field.
// The codec checks `f64_val > f64::from(f32::MAX)` and returns CBKE531.
// ===========================================================================

#[test]
fn error_cbke531_float_encode_overflow() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut field = Field::new(5, "FLOAT-FLD".to_string());
    field.path = "ROOT.FLOAT-FLD".to_string();
    field.kind = FieldKind::FloatSingle;
    field.len = 4;
    field.offset = 0;

    root.children = vec![field];

    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(4);

    // 3.5e38 exceeds f32::MAX (≈3.4028235e38)
    let json_value = json!({
        "ROOT": {
            "FLOAT-FLD": 3.5e38_f64
        }
    });

    let result = encode_record(&schema, &json_value, &encode_opts());

    assert!(result.is_err(), "Expected CBKE531 for f32 overflow");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKE531_FLOAT_ENCODE_OVERFLOW,
        "Expected CBKE531_FLOAT_ENCODE_OVERFLOW, got {:?}",
        err.code
    );
    assert!(
        err.message.contains("FLOAT-FLD"),
        "Error should mention the field name: {}",
        err.message
    );
}

// ===========================================================================
// Codes that are DEFINED but NEVER EMITTED by production source code.
//
// These error codes exist in the `ErrorCode` enum for completeness or as
// reserved placeholders, but no code path currently produces them.
// Each test below documents the reason and verifies the variant compiles.
// ===========================================================================

/// CBKS603_RENAME_NOT_CONTIGUOUS — defined but intentionally not enforced.
///
/// The layout resolver comment (layout.rs ~line 960) states:
/// > "We do NOT enforce strict byte-level contiguity (CBKS603). COBOL RENAMES
/// > requires fields to be in source order… but REDEFINES can create
/// > overlapping offsets which is valid."
#[test]
fn reserved_cbks603_rename_not_contiguous() {
    let code = ErrorCode::CBKS603_RENAME_NOT_CONTIGUOUS;
    assert_eq!(format!("{code}"), "CBKS603_RENAME_NOT_CONTIGUOUS");
    // No production code path emits this error.
}

/// CBKS608_RENAME_QUALIFIED_NAME_NOT_FOUND — reserved for future qualified
/// name resolution in RENAMES. No resolver path currently produces this code.
#[test]
fn reserved_cbks608_rename_qualified_name_not_found() {
    let code = ErrorCode::CBKS608_RENAME_QUALIFIED_NAME_NOT_FOUND;
    assert_eq!(format!("{code}"), "CBKS608_RENAME_QUALIFIED_NAME_NOT_FOUND");
    // No production code path emits this error.
}

/// CBKD431_FLOAT_NAN — reserved. The codec currently returns `Value::Null`
/// for NaN values during decode rather than returning an error.
#[test]
fn reserved_cbkd431_float_nan() {
    let code = ErrorCode::CBKD431_FLOAT_NAN;
    assert_eq!(format!("{code}"), "CBKD431_FLOAT_NAN");
    // Codec silently maps NaN → null; no error is returned.
}

/// CBKD432_FLOAT_INFINITY — reserved. The codec currently returns
/// `Value::Null` for infinity values during decode rather than returning
/// an error.
#[test]
fn reserved_cbkd432_float_infinity() {
    let code = ErrorCode::CBKD432_FLOAT_INFINITY;
    assert_eq!(format!("{code}"), "CBKD432_FLOAT_INFINITY");
    // Codec silently maps ±Infinity → null; no error is returned.
}

/// CBKW002_TYPE_MAPPING — reserved for `copybook-arrow` crate.
/// The Arrow crate uses its own `ArrowError` enum, not `copybook_error::ErrorCode`.
#[test]
fn reserved_cbkw002_type_mapping() {
    let code = ErrorCode::CBKW002_TYPE_MAPPING;
    assert_eq!(format!("{code}"), "CBKW002_TYPE_MAPPING");
    // Arrow crate uses ArrowError, not ErrorCode. Reserved for future alignment.
}

/// CBKW003_DECIMAL_OVERFLOW — reserved for `copybook-arrow` crate.
#[test]
fn reserved_cbkw003_decimal_overflow() {
    let code = ErrorCode::CBKW003_DECIMAL_OVERFLOW;
    assert_eq!(format!("{code}"), "CBKW003_DECIMAL_OVERFLOW");
    // Arrow crate uses ArrowError, not ErrorCode. Reserved for future alignment.
}

/// CBKW004_BATCH_BUILD — reserved for `copybook-arrow` crate.
#[test]
fn reserved_cbkw004_batch_build() {
    let code = ErrorCode::CBKW004_BATCH_BUILD;
    assert_eq!(format!("{code}"), "CBKW004_BATCH_BUILD");
    // Arrow crate uses ArrowError, not ErrorCode. Reserved for future alignment.
}

/// CBKW005_PARQUET_WRITE — reserved for `copybook-arrow` crate.
#[test]
fn reserved_cbkw005_parquet_write() {
    let code = ErrorCode::CBKW005_PARQUET_WRITE;
    assert_eq!(format!("{code}"), "CBKW005_PARQUET_WRITE");
    // Arrow crate uses ArrowError, not ErrorCode. Reserved for future alignment.
}
