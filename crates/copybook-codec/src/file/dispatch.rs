// SPDX-License-Identifier: AGPL-3.0-or-later
//! Operation-level fixed/RDW dispatch for codec callers.
//!
//! The framing crates own byte-level parsing and writing. This module owns the
//! format choice used by codec operations and keeps the legacy single-record
//! helpers available through [`crate::record`].

use crate::options::RecordFormat;
use copybook_error::{Error, ErrorCode, Result};
use std::io::{Read, Write};

pub use copybook_fixed::{FixedRecordReader, FixedRecordWriter};
pub use copybook_rdw::{RDWRecord, RDWRecordReader, RDWRecordWriter};

/// Read one record using the selected framing format.
///
/// # Errors
/// Returns an error when the delegated framing read fails or when fixed
/// framing is missing its LRECL.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn read_record(
    input: &mut impl Read,
    format: RecordFormat,
    lrecl: Option<u32>,
) -> Result<Option<Vec<u8>>> {
    match format {
        RecordFormat::Fixed => read_fixed_record(input, lrecl),
        RecordFormat::RDW => {
            read_rdw_record(input, false).map(|record| record.map(|record| record.payload))
        }
    }
}

#[inline]
fn read_fixed_record(input: &mut impl Read, lrecl: Option<u32>) -> Result<Option<Vec<u8>>> {
    let mut reader = FixedRecordReader::new(input, lrecl)?;
    reader.read_record()
}

/// Read one complete RDW record, preserving its header and reserved bytes.
///
/// Use [`read_record`] when the operation only needs the payload. This helper
/// is the lossless dispatch path for callers that need to inspect or preserve
/// the original RDW framing metadata.
///
/// # Errors
/// Returns an error when the delegated RDW framing read fails.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn read_rdw_record(input: &mut impl Read, strict_mode: bool) -> Result<Option<RDWRecord>> {
    let mut reader = RDWRecordReader::new(input, strict_mode);
    reader.read_record()
}

/// Write one record using the selected framing format.
///
/// # Errors
/// Returns an error when the delegated framing write fails.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn write_record(output: &mut impl Write, data: &[u8], format: RecordFormat) -> Result<()> {
    match format {
        RecordFormat::Fixed => {
            output.write_all(data).map_err(|e| {
                Error::new(
                    ErrorCode::CBKR101_FIXED_RECORD_ERROR,
                    format!("Write error: {e}"),
                )
            })?;
            Ok(())
        }
        RecordFormat::RDW => {
            let mut writer = RDWRecordWriter::new(output);
            writer.write_record_from_payload(data, None)
        }
    }
}
