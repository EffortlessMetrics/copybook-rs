#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Fixed-LRECL framing integration for schema-backed codec operations.
//!
//! `copybook-fixed` owns byte-stream framing and has no dependency on schema
//! types. This module is the codec boundary that resolves a copybook schema's
//! fixed LRECL before constructing those framing primitives.

use copybook_core::Schema;
use copybook_error::{Error, ErrorCode, ErrorContext, Result};
use copybook_fixed::FixedRecordReader;
use std::convert::TryFrom;
use std::io::Read;
use tracing::{debug, warn};

pub(crate) const FIXED_FORMAT_LRECL_MISSING: &str = "Fixed format requires a fixed record length (LRECL). Set schema.lrecl_fixed or use RecordFormat::Variable.";

/// Resolve the fixed record length required by a schema.
///
/// # Errors
/// Returns `CBKI001_INVALID_STATE` when the schema does not provide a fixed
/// record length.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn lrecl(schema: &Schema) -> Result<u32> {
    let lrecl = schema
        .lrecl_fixed
        .ok_or_else(|| Error::new(ErrorCode::CBKI001_INVALID_STATE, FIXED_FORMAT_LRECL_MISSING))?;

    if lrecl == 0 {
        return Err(Error::new(
            ErrorCode::CBKI001_INVALID_STATE,
            "LRECL must be greater than zero",
        ));
    }

    Ok(lrecl)
}

/// Construct a fixed reader after resolving the schema's LRECL in the codec.
///
/// The returned framing primitive accepts only the explicit LRECL value; no
/// schema type crosses into `copybook-fixed`.
///
/// # Errors
/// Returns an error when the schema has no fixed LRECL or its LRECL is invalid.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn reader<R: Read>(input: R, schema: &Schema) -> Result<FixedRecordReader<R>> {
    FixedRecordReader::with_lrecl(input, lrecl(schema)?)
}

/// Validate a fixed record against the codec's schema and framing contract.
///
/// This check intentionally lives beside schema-to-framing integration rather
/// than in `copybook-fixed`, so the framing crate remains schema-independent.
///
/// # Errors
/// Returns an error when the configured LRECL cannot be represented on the
/// current platform or when the record length does not match it.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn validate_record_length(
    schema: &Schema,
    configured_lrecl: u32,
    record_index: u64,
    record_data: &[u8],
) -> Result<()> {
    let lrecl_len = usize::try_from(configured_lrecl).map_err(|_| {
        Error::new(
            ErrorCode::CBKR101_FIXED_RECORD_ERROR,
            "LRECL exceeds platform addressable size",
        )
    })?;

    if record_data.len() != lrecl_len {
        return Err(Error::new(
            ErrorCode::CBKR101_FIXED_RECORD_ERROR,
            format!(
                "Record length mismatch: expected {}, got {}",
                configured_lrecl,
                record_data.len()
            ),
        )
        .with_context(ErrorContext {
            record_index: Some(record_index),
            field_path: None,
            byte_offset: None,
            line_number: None,
            details: Some("Fixed record length validation failed".to_string()),
        }));
    }

    if let Some(schema_lrecl) = schema.lrecl_fixed
        && configured_lrecl != schema_lrecl
    {
        warn!(
            "LRECL mismatch: codec configured for {}, schema expects {}",
            configured_lrecl, schema_lrecl
        );
    }

    if schema.tail_odo.is_some() {
        debug!("Record has ODO tail, variable length within fixed LRECL is expected");
    }

    Ok(())
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn resolves_schema_lrecl_at_codec_boundary() {
        let mut schema = Schema::new();
        schema.lrecl_fixed = Some(8);

        assert_eq!(lrecl(&schema).unwrap(), 8);
        let mut reader = reader(Cursor::new(b"ABCDEFGH"), &schema).unwrap();
        assert_eq!(reader.read_record().unwrap().unwrap(), b"ABCDEFGH");
    }

    #[test]
    fn rejects_schema_without_fixed_lrecl_at_codec_boundary() {
        let schema = Schema::new();

        let error = lrecl(&schema).unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKI001_INVALID_STATE);
    }

    #[test]
    fn rejects_zero_schema_lrecl_at_codec_boundary() {
        let mut schema = Schema::new();
        schema.lrecl_fixed = Some(0);

        let error = lrecl(&schema).unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKI001_INVALID_STATE);
        assert_eq!(error.message, "LRECL must be greater than zero");
    }

    #[test]
    fn validates_record_length_at_codec_boundary() {
        let mut schema = Schema::new();
        schema.lrecl_fixed = Some(8);

        validate_record_length(&schema, 8, 1, b"ABCDEFGH").unwrap();
        let error = validate_record_length(&schema, 8, 2, b"SHORT").unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKR101_FIXED_RECORD_ERROR);
    }
}
