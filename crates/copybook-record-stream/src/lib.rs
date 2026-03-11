// SPDX-License-Identifier: AGPL-3.0-or-later
#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]

//! Streaming raw record reader for fixed-length and RDW-framed data.

use copybook_core::{Error, ErrorCode, Result};
use copybook_rdw::RdwHeader;
use std::io::{BufReader, Read};

const FIXED_FORMAT_LRECL_MISSING: &str = "Fixed format requires a fixed record length (LRECL).";

/// Raw record framing formats.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RecordStreamFormat {
    /// Fixed-length records using a configured LRECL.
    Fixed,
    /// Variable-length records with a 4-byte RDW header.
    RDW,
}

/// Streaming raw record reader with bounded memory usage.
pub struct RawRecordStream<R: Read> {
    reader: BufReader<R>,
    format: RecordStreamFormat,
    fixed_lrecl: Option<usize>,
    record_index: u64,
    eof_reached: bool,
    buffer: Vec<u8>,
}

impl<R: Read> RawRecordStream<R> {
    /// Build a new stream.
    pub fn new(reader: R, format: RecordStreamFormat, fixed_lrecl: Option<usize>) -> Result<Self> {
        if format == RecordStreamFormat::Fixed && fixed_lrecl.is_none() {
            return Err(Error::new(
                ErrorCode::CBKI001_INVALID_STATE,
                FIXED_FORMAT_LRECL_MISSING,
            ));
        }

        Ok(Self {
            reader: BufReader::new(reader),
            format,
            fixed_lrecl,
            record_index: 0,
            eof_reached: false,
            buffer: Vec::new(),
        })
    }

    #[must_use]
    pub fn current_record_index(&self) -> u64 {
        self.record_index
    }

    #[must_use]
    pub fn is_eof(&self) -> bool {
        self.eof_reached
    }

    pub fn read_raw_record(&mut self) -> Result<Option<Vec<u8>>> {
        if self.eof_reached {
            return Ok(None);
        }

        self.buffer.clear();

        match self.format {
            RecordStreamFormat::Fixed => {
                let lrecl = match self.fixed_lrecl {
                    Some(length) => length,
                    None => {
                        return Err(Error::new(
                            ErrorCode::CBKI001_INVALID_STATE,
                            FIXED_FORMAT_LRECL_MISSING,
                        ));
                    }
                };
                self.buffer.resize(lrecl, 0);

                match self.reader.read_exact(&mut self.buffer) {
                    Ok(()) => {
                        self.record_index += 1;
                        Ok(Some(self.buffer.clone()))
                    }
                    Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                        self.eof_reached = true;
                        Ok(None)
                    }
                    Err(e) => Err(Error::new(
                        ErrorCode::CBKD301_RECORD_TOO_SHORT,
                        format!("Failed to read fixed record: {e}"),
                    )),
                }
            }
            RecordStreamFormat::RDW => {
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
                        Ok(Some(self.buffer.clone()))
                    }
                    Err(e) => Err(Error::new(
                        ErrorCode::CBKF221_RDW_UNDERFLOW,
                        format!("Failed to read RDW payload: {e}"),
                    )),
                }
            }
        }
    }
}
