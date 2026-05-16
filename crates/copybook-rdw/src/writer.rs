use copybook_error::{Error, ErrorCode, ErrorContext, Result};
use std::io::Write;
use tracing::debug;

use crate::{RDWRecord, RdwHeader};

/// RDW record writer for variable-length records.
#[derive(Debug)]
pub struct RDWRecordWriter<W: Write> {
    output: W,
    record_count: u64,
}

impl<W: Write> RDWRecordWriter<W> {
    /// Create a new RDW record writer.
    #[inline]
    #[must_use]
    pub fn new(output: W) -> Self {
        Self {
            output,
            record_count: 0,
        }
    }

    /// Write an RDW record.
    ///
    /// # Errors
    /// Returns an error if writing header or payload fails.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn write_record(&mut self, record: &RDWRecord) -> Result<()> {
        self.output.write_all(&record.header).map_err(|e| {
            Error::new(
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                format!("I/O error writing RDW header: {e}"),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: None,
                line_number: None,
                details: None,
            })
        })?;

        self.output.write_all(&record.payload).map_err(|e| {
            Error::new(
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                format!("I/O error writing RDW payload: {e}"),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: Some(4),
                line_number: None,
                details: None,
            })
        })?;

        self.record_count += 1;
        debug!(
            "Wrote RDW record {} with {} byte payload",
            self.record_count,
            record.payload.len()
        );
        Ok(())
    }

    /// Write an RDW record directly from payload.
    ///
    /// # Errors
    /// Returns an error if payload length exceeds `u16::MAX` or I/O fails.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn write_record_from_payload(
        &mut self,
        payload: &[u8],
        preserve_reserved: Option<u16>,
    ) -> Result<()> {
        let length = payload.len();
        let header =
            RdwHeader::from_payload_len(length, preserve_reserved.unwrap_or(0)).map_err(|_| {
                Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    format!(
                        "RDW payload too large: {length} bytes exceeds maximum of {}",
                        u16::MAX
                    ),
                )
                .with_context(ErrorContext {
                    record_index: Some(self.record_count + 1),
                    field_path: None,
                    byte_offset: None,
                    line_number: None,
                    details: Some("RDW length field is 16-bit".to_string()),
                })
            })?;

        let record = RDWRecord {
            header: header.bytes(),
            payload: payload.to_vec(),
        };
        self.write_record(&record)
    }

    /// Flush writer output.
    ///
    /// # Errors
    /// Returns an error when flush fails.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn flush(&mut self) -> Result<()> {
        self.output.flush().map_err(|e| {
            Error::new(
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                format!("I/O error flushing output: {e}"),
            )
        })
    }

    /// Number of written RDW records.
    #[inline]
    #[must_use]
    pub fn record_count(&self) -> u64 {
        self.record_count
    }
}
