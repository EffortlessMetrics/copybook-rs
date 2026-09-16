// SPDX-License-Identifier: AGPL-3.0-or-later
//! Shared exhaustive list of every [`ErrorCode`] variant.
//!
//! This module is the single source of truth for whole-taxonomy iteration in
//! the `copybook-error` integration tests. Keep it in sync with the enum
//! definition in `crates/copybook-error/src/lib.rs`: adding a variant without
//! listing it here weakens every completeness test that iterates the result.
//!
//! [`ErrorCode`]: copybook_error::ErrorCode

use copybook_error::ErrorCode;

/// Every [`ErrorCode`] variant, grouped by family.
///
/// [`ErrorCode`]: copybook_error::ErrorCode
#[inline]
pub fn all_error_codes() -> Vec<ErrorCode> {
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
        ErrorCode::CBKI002_TOO_MANY_ERRORS,
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
