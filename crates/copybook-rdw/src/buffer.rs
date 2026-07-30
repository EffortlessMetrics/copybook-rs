use copybook_error::{Error, ErrorCode, Result};
use std::io::BufRead;

/// Read a 2-byte big-endian RDW body length and consume those two bytes.
///
/// # Errors
/// Returns:
/// - `CBKR201_RDW_READ_ERROR` for I/O errors while peeking.
/// - `CBKF102_RECORD_LENGTH_INVALID` for incomplete headers.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn rdw_read_len<R: BufRead>(reader: &mut R) -> Result<u16> {
    let buf = reader.fill_buf().map_err(|e| {
        Error::new(
            ErrorCode::CBKR201_RDW_READ_ERROR,
            format!("I/O error peeking RDW length: {e}"),
        )
    })?;
    if buf.len() < 2 {
        return Err(Error::new(
            ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
            format!(
                "Incomplete RDW header: expected 2 bytes for length (have {})",
                buf.len()
            ),
        ));
    }

    let hi = buf[0];
    let lo = buf[1];
    reader.consume(2);
    Ok(u16::from_be_bytes([hi, lo]))
}

/// Borrow the RDW body slice for `len` bytes without consuming.
///
/// # Errors
/// Returns:
/// - `CBKR201_RDW_READ_ERROR` for I/O errors while peeking.
/// - `CBKF102_RECORD_LENGTH_INVALID` when fewer than `len` bytes are available.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn rdw_slice_body<R: BufRead>(reader: &mut R, len: u16) -> Result<&[u8]> {
    let need = usize::from(len);
    if need == 0 {
        return Ok(&[]);
    }

    let buf = reader.fill_buf().map_err(|e| {
        Error::new(
            ErrorCode::CBKR201_RDW_READ_ERROR,
            format!("I/O error reading RDW payload: {e}"),
        )
    })?;

    if buf.len() < need {
        return Err(Error::new(
            ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
            format!(
                "Incomplete RDW record payload: expected {} bytes (have {})",
                need,
                buf.len()
            ),
        ));
    }

    Ok(&buf[..need])
}

/// Placeholder for future RDW body validation hooks.
#[inline]
#[must_use]
pub const fn rdw_validate_and_finish(body: &[u8]) -> &[u8] {
    body
}

/// Probe if enough bytes exist to attempt RDW length parsing.
///
/// - `0` or `1` byte buffered => `Ok(None)`
/// - `>= 2` bytes buffered => `Ok(Some(()))`
///
/// # Errors
/// Returns `CBKR201_RDW_READ_ERROR` for I/O errors while peeking.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn rdw_try_peek_len<R: BufRead>(reader: &mut R) -> Result<Option<()>> {
    let buf = reader.fill_buf().map_err(|e| {
        Error::new(
            ErrorCode::CBKR201_RDW_READ_ERROR,
            format!("I/O error peeking RDW header: {e}"),
        )
    })?;
    if buf.len() <= 1 {
        return Ok(None);
    }
    Ok(Some(()))
}
