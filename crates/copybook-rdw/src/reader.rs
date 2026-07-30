use crate::{
    RDW_HEADER_LEN, RDWRecord, rdw_is_suspect_ascii_corruption, rdw_read_len, rdw_slice_body,
    rdw_try_peek_len, rdw_validate_and_finish,
};
use copybook_error::{Error, ErrorCode, ErrorContext, Result};
use std::io::{BufRead, BufReader, Read};
use tracing::{debug, warn};

const RDW_READER_BUF_CAPACITY: usize = (u16::MAX as usize) + RDW_HEADER_LEN;

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
            return self.handle_short_or_empty_header();
        }

        let buf = self.input.fill_buf().map_err(|e| {
            Error::new(
                ErrorCode::CBKR201_RDW_READ_ERROR,
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
            return self.handle_truncated_header();
        }

        Ok(Some([buf[0], buf[1], buf[2], buf[3]]))
    }

    fn handle_short_or_empty_header(&mut self) -> Result<Option<[u8; RDW_HEADER_LEN]>> {
        let context = self.header_context("Unable to read RDW header");
        let buf = self.input.fill_buf().map_err(|e| {
            Error::new(
                ErrorCode::CBKR201_RDW_READ_ERROR,
                format!("I/O error reading RDW header: {e}"),
            )
            .with_context(context)
        })?;

        if buf.is_empty() {
            debug!("Reached EOF after {} RDW records", self.record_count);
            return Ok(None);
        }

        self.handle_truncated_header()
    }

    fn handle_truncated_header(&mut self) -> Result<Option<[u8; RDW_HEADER_LEN]>> {
        if self.strict_mode {
            return Err(Error::new(
                ErrorCode::CBKF221_RDW_UNDERFLOW,
                "Incomplete RDW header: expected 4 bytes".to_string(),
            )
            .with_context(self.header_context("File ends with incomplete RDW header")));
        }

        debug!(
            "Reached EOF after {} RDW records (truncated header ignored)",
            self.record_count
        );
        let context = self.header_context("Unable to read RDW header");
        let remaining = self
            .input
            .fill_buf()
            .map_err(|e| {
                Error::new(
                    ErrorCode::CBKR201_RDW_READ_ERROR,
                    format!("I/O error reading RDW header: {e}"),
                )
                .with_context(context)
            })?
            .len();
        self.input.consume(remaining);
        Ok(None)
    }

    fn header_context(&self, details: impl Into<String>) -> ErrorContext {
        ErrorContext {
            record_index: Some(self.record_count + 1),
            field_path: None,
            byte_offset: Some(0),
            line_number: None,
            details: Some(details.into()),
        }
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

        self.validate_reserved(reserved)?;
        self.reject_ascii_corruption(header)?;

        if length == 0 {
            debug!("Zero-length RDW record {}", self.record_count);
            return Ok(Some(RDWRecord {
                header,
                payload: Vec::new(),
            }));
        }

        let payload = self.read_payload(length)?;

        debug!(
            "Read RDW record {} payload: {} bytes",
            self.record_count, length
        );
        Ok(Some(RDWRecord { header, payload }))
    }

    fn validate_reserved(&self, reserved: u16) -> Result<()> {
        if reserved == 0 {
            return Ok(());
        }

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
        Ok(())
    }

    fn reject_ascii_corruption(&self, header: [u8; RDW_HEADER_LEN]) -> Result<()> {
        if !Self::is_suspect_ascii_corruption(header) {
            return Ok(());
        }

        warn!(
            "RDW appears to be ASCII-corrupted (record {}): {:02X} {:02X} {:02X} {:02X}",
            self.record_count, header[0], header[1], header[2], header[3]
        );

        Err(Error::new(
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
        }))
    }

    fn read_payload(&mut self, length: u16) -> Result<Vec<u8>> {
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
        Ok(payload)
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
