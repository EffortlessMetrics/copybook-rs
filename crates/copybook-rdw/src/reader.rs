use copybook_core::Schema;
use copybook_error::{Error, ErrorCode, ErrorContext, Result};
use std::io::{BufRead, BufReader, Read};
use tracing::{debug, warn};

use crate::{
    RDW_HEADER_LEN, RDW_READER_BUF_CAPACITY, RDWRecord, rdw_is_suspect_ascii_corruption,
    rdw_read_len, rdw_slice_body, rdw_try_peek_len, rdw_validate_and_finish,
    schema_prefix::calculate_schema_fixed_prefix,
};

/// RDW (Record Descriptor Word) record reader for variable-length records.
#[derive(Debug)]
pub struct RDWRecordReader<R: Read> {
    input: BufReader<R>,
    record_count: u64,
    strict_mode: bool,
}

impl<R: Read> RDWRecordReader<R> {
    /// Create a new RDW record reader.
    #[inline]
    #[must_use]
    pub fn new(input: R, strict_mode: bool) -> Self {
        Self {
            input: BufReader::with_capacity(RDW_READER_BUF_CAPACITY, input),
            record_count: 0,
            strict_mode,
        }
    }

    #[inline]
    fn peek_header(&mut self) -> Result<Option<[u8; RDW_HEADER_LEN]>> {
        let peek = rdw_try_peek_len(&mut self.input).map_err(|error| {
            error.with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: Some(0),
                line_number: None,
                details: Some("Unable to peek RDW header".to_string()),
            })
        })?;

        if peek.is_none() {
            let buf = self.input.fill_buf().map_err(|e| {
                Error::new(
                    ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                    format!("I/O error reading RDW header: {e}"),
                )
                .with_context(ErrorContext {
                    record_index: Some(self.record_count + 1),
                    field_path: None,
                    byte_offset: Some(0),
                    line_number: None,
                    details: Some("Unable to read RDW header".to_string()),
                })
            })?;

            if buf.is_empty() {
                debug!("Reached EOF after {} RDW records", self.record_count);
                return Ok(None);
            }

            if self.strict_mode {
                return Err(Error::new(
                    ErrorCode::CBKF221_RDW_UNDERFLOW,
                    "Incomplete RDW header: expected 4 bytes".to_string(),
                )
                .with_context(ErrorContext {
                    record_index: Some(self.record_count + 1),
                    field_path: None,
                    byte_offset: Some(0),
                    line_number: None,
                    details: Some("File ends with incomplete RDW header".to_string()),
                }));
            }

            debug!(
                "Reached EOF after {} RDW records (truncated header ignored)",
                self.record_count
            );
            let remaining = buf.len();
            self.input.consume(remaining);
            return Ok(None);
        }

        let buf = self.input.fill_buf().map_err(|e| {
            Error::new(
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                format!("I/O error reading RDW header: {e}"),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: Some(0),
                line_number: None,
                details: Some("Unable to read RDW header".to_string()),
            })
        })?;

        if buf.len() < RDW_HEADER_LEN {
            if self.strict_mode {
                return Err(Error::new(
                    ErrorCode::CBKF221_RDW_UNDERFLOW,
                    "Incomplete RDW header: expected 4 bytes".to_string(),
                )
                .with_context(ErrorContext {
                    record_index: Some(self.record_count + 1),
                    field_path: None,
                    byte_offset: Some(0),
                    line_number: None,
                    details: Some("File ends with incomplete RDW header".to_string()),
                }));
            }

            debug!(
                "Reached EOF after {} RDW records (truncated header ignored)",
                self.record_count
            );
            let remaining = buf.len();
            self.input.consume(remaining);
            return Ok(None);
        }

        Ok(Some([buf[0], buf[1], buf[2], buf[3]]))
    }

    /// Read the next RDW record.
    ///
    /// # Errors
    /// Returns an error if the record cannot be read due to I/O errors or
    /// framing issues.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn read_record(&mut self) -> Result<Option<RDWRecord>> {
        let Some(header) = self.peek_header()? else {
            return Ok(None);
        };

        let length = match rdw_read_len(&mut self.input) {
            Ok(len) => len,
            Err(error) => {
                return Err(error.with_context(ErrorContext {
                    record_index: Some(self.record_count + 1),
                    field_path: None,
                    byte_offset: Some(0),
                    line_number: None,
                    details: Some("Unable to read RDW body length".to_string()),
                }));
            }
        };

        // Consume reserved bytes so the buffer now points at the body.
        self.input.consume(2);
        let reserved = u16::from_be_bytes([header[2], header[3]]);

        self.record_count += 1;
        debug!(
            "Read RDW header for record {}: length={}, reserved={:04X}",
            self.record_count,
            u32::from(length),
            reserved
        );

        if reserved != 0 {
            let error = Error::new(
                ErrorCode::CBKR211_RDW_RESERVED_NONZERO,
                format!("RDW reserved bytes are non-zero: {reserved:04X}"),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count),
                field_path: None,
                byte_offset: Some(2),
                line_number: None,
                details: Some(format!("Expected 0000, got {reserved:04X}")),
            });

            if self.strict_mode {
                return Err(error);
            }

            warn!(
                "RDW reserved bytes non-zero (record {}): {:04X}",
                self.record_count, reserved
            );
        }

        if Self::is_suspect_ascii_corruption(header) {
            warn!(
                "RDW appears to be ASCII-corrupted (record {}): {:02X} {:02X} {:02X} {:02X}",
                self.record_count, header[0], header[1], header[2], header[3]
            );

            return Err(Error::new(
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                format!(
                    "RDW appears to be ASCII-corrupted: {:02X} {:02X} {:02X} {:02X}",
                    header[0], header[1], header[2], header[3]
                ),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count),
                field_path: None,
                byte_offset: Some(0),
                line_number: None,
                details: Some("Suspected ASCII transfer corruption".to_string()),
            }));
        }

        if length == 0 {
            debug!("Zero-length RDW record {}", self.record_count);
            return Ok(Some(RDWRecord {
                header,
                payload: Vec::new(),
            }));
        }

        let payload_len = usize::from(length);
        let body_slice = match rdw_slice_body(&mut self.input, length) {
            Ok(slice) => slice,
            Err(error) => {
                return Err(error.with_context(ErrorContext {
                    record_index: Some(self.record_count),
                    field_path: None,
                    byte_offset: Some(4),
                    line_number: None,
                    details: Some("File ends with incomplete RDW payload".to_string()),
                }));
            }
        };

        let payload = rdw_validate_and_finish(body_slice).to_vec();
        self.input.consume(payload_len);

        debug!(
            "Read RDW record {} payload: {} bytes",
            self.record_count, length
        );
        Ok(Some(RDWRecord { header, payload }))
    }

    /// Validate a zero-length record against schema requirements.
    ///
    /// # Errors
    /// Returns an error when the schema requires non-zero fixed bytes.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn validate_zero_length_record(&self, schema: &Schema) -> Result<()> {
        let min_size = calculate_schema_fixed_prefix(schema);

        if min_size > 0 {
            return Err(Error::new(
                ErrorCode::CBKF221_RDW_UNDERFLOW,
                format!("Zero-length RDW record invalid: schema requires minimum {min_size} bytes"),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count),
                field_path: None,
                byte_offset: None,
                line_number: None,
                details: Some("Zero-length record with non-zero schema prefix".to_string()),
            }));
        }

        Ok(())
    }

    /// Number of RDW records consumed from the stream.
    #[inline]
    #[must_use]
    pub fn record_count(&self) -> u64 {
        self.record_count
    }

    #[inline]
    fn is_suspect_ascii_corruption(rdw_header: [u8; RDW_HEADER_LEN]) -> bool {
        rdw_is_suspect_ascii_corruption(rdw_header)
    }
}
