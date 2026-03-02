#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::missing_inline_in_public_items)]
//! Streaming iterator primitives over raw fixed-length and RDW records.

use copybook_core::{Error, ErrorCode, Result, Schema};
use copybook_options::{DecodeOptions, RecordFormat};
use copybook_rdw::RdwHeader;
use std::io::{BufReader, Read};

const FIXED_FORMAT_LRECL_MISSING: &str = "Fixed format requires a fixed record length (LRECL). \
     Set `schema.lrecl_fixed` or use `RecordFormat::Variable`.";

/// Reads raw records from a `Read` source while tracking stream state.
pub struct RawRecordReader<R: Read> {
    reader: BufReader<R>,
    schema: Schema,
    options: DecodeOptions,
    record_index: u64,
    eof_reached: bool,
    buffer: Vec<u8>,
}

impl<R: Read> RawRecordReader<R> {
    /// Create a new raw record reader.
    #[must_use = "Handle the Result or propagate the error"]
    #[inline]
    pub fn new(reader: R, schema: &Schema, options: &DecodeOptions) -> Result<Self> {
        Ok(Self {
            reader: BufReader::new(reader),
            schema: schema.clone(),
            options: options.clone(),
            record_index: 0,
            eof_reached: false,
            buffer: Vec::new(),
        })
    }

    /// Get the current record index (1-based).
    #[must_use]
    #[inline]
    pub fn current_record_index(&self) -> u64 {
        self.record_index
    }

    /// Check whether EOF has been reached.
    #[must_use]
    #[inline]
    pub fn is_eof(&self) -> bool {
        self.eof_reached
    }

    /// Get the schema reference.
    #[must_use]
    #[inline]
    pub fn schema(&self) -> &Schema {
        &self.schema
    }

    /// Get decode options reference.
    #[must_use]
    #[inline]
    pub fn options(&self) -> &DecodeOptions {
        &self.options
    }

    /// Read the next raw record payload bytes.
    ///
    /// # Errors
    /// Returns an error if I/O fails or record framing is invalid.
    #[must_use = "Handle the Result or propagate the error"]
    pub fn read_raw_record(&mut self) -> Result<Option<Vec<u8>>> {
        if self.eof_reached {
            return Ok(None);
        }

        self.buffer.clear();

        let record_data = match self.options.format {
            RecordFormat::Fixed => {
                let lrecl = self.schema.lrecl_fixed.ok_or_else(|| {
                    Error::new(ErrorCode::CBKI001_INVALID_STATE, FIXED_FORMAT_LRECL_MISSING)
                })? as usize;
                self.buffer.resize(lrecl, 0);

                match self.reader.read_exact(&mut self.buffer) {
                    Ok(()) => {
                        self.record_index += 1;
                        Some(self.buffer.clone())
                    }
                    Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                        self.eof_reached = true;
                        return Ok(None);
                    }
                    Err(e) => {
                        return Err(Error::new(
                            ErrorCode::CBKD301_RECORD_TOO_SHORT,
                            format!("Failed to read fixed record: {e}"),
                        ));
                    }
                }
            }
            RecordFormat::RDW => {
                let mut rdw_header = [0u8; 4];
                match self.reader.read_exact(&mut rdw_header) {
                    Ok(()) => {}
                    Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                        self.eof_reached = true;
                        return Ok(None);
                    }
                    Err(e) => {
                        return Err(Error::new(
                            ErrorCode::CBKF221_RDW_UNDERFLOW,
                            format!("Failed to read RDW header: {e}"),
                        ));
                    }
                }

                let length = usize::from(RdwHeader::from_bytes(rdw_header).length());
                self.buffer.resize(length, 0);

                match self.reader.read_exact(&mut self.buffer) {
                    Ok(()) => {
                        self.record_index += 1;
                        Some(self.buffer.clone())
                    }
                    Err(e) => {
                        return Err(Error::new(
                            ErrorCode::CBKF221_RDW_UNDERFLOW,
                            format!("Failed to read RDW payload: {e}"),
                        ));
                    }
                }
            }
        };

        Ok(record_data)
    }
}

/// Create a raw record reader from any `Read` source.
#[must_use = "Handle the Result or propagate the error"]
#[inline]
pub fn raw_records<R: Read>(
    reader: R,
    schema: &Schema,
    options: &DecodeOptions,
) -> Result<RawRecordReader<R>> {
    RawRecordReader::new(reader, schema, options)
}

/// Create a raw record reader from a file path.
///
/// # Errors
/// Returns an error when opening the file fails.
#[must_use = "Handle the Result or propagate the error"]
#[inline]
pub fn raw_records_from_file<P: AsRef<std::path::Path>>(
    file_path: P,
    schema: &Schema,
    options: &DecodeOptions,
) -> Result<RawRecordReader<std::fs::File>> {
    let file = std::fs::File::open(file_path)
        .map_err(|e| Error::new(ErrorCode::CBKF104_RDW_SUSPECT_ASCII, e.to_string()))?;

    RawRecordReader::new(file, schema, options)
}
