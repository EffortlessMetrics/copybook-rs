// SPDX-License-Identifier: AGPL-3.0-or-later
//! Line-delimited text framing for fixed-width records.
//!
//! Each record is one line: an `lrecl`-byte payload followed by an LF
//! terminator, with a lone CR before the LF accepted and stripped (CRLF).
//! Terminators are framing: they never reach field decoding or raw capture.
//!
//! Policy (evidenced against JRecord 0.93.2 `-IFS Text` in
//! `docs/evidence/differential-breadth/README.md`, lane 2):
//!
//! - LF and CRLF both terminate a line; a lone CR is data.
//! - A final line without any terminator is accepted, matching the oracle.
//! - Line length is strict where the oracle is lenient: a payload shorter
//!   or longer than `lrecl` is a classified `CBKR101` error naming expected
//!   and actual lengths, never a silent pad or truncation.
//! - Accumulation past the reviewed record bound fails with `CBKF226`
//!   before the line can grow without limit.

use copybook_error::{Error, ErrorCode, ErrorContext, Result};
use std::io::BufRead;

/// Read one text-framed payload from `reader` into `scratch`.
///
/// Returns `Ok(None)` on clean EOF (no bytes pending). A final line
/// without a terminator is accepted. The returned slice borrows `scratch`
/// and holds exactly `lrecl` payload bytes.
///
/// # Errors
///
/// Returns `CBKR101_FIXED_RECORD_ERROR` when the payload is shorter or
/// longer than `lrecl`, and `CBKF226_RECORD_BOUND_EXCEEDED` when the line
/// outgrows `bound` before any terminator arrives.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn read_text_record(
    reader: &mut impl BufRead,
    scratch: &mut Vec<u8>,
    lrecl: usize,
    bound: Option<u64>,
    record_index: u64,
) -> Result<Option<Vec<u8>>> {
    scratch.clear();
    let mut terminated = false;
    loop {
        let chunk = reader.fill_buf().map_err(|error| {
            Error::new(
                ErrorCode::CBKR101_FIXED_RECORD_ERROR,
                format!("Failed to read text record: {error}"),
            )
            .with_context(line_context(record_index, None))
        })?;
        if chunk.is_empty() {
            break;
        }
        let end = chunk
            .iter()
            .position(|byte| *byte == b'\n')
            .map_or(chunk.len(), |pos| {
                terminated = true;
                pos + 1
            });
        if let Some(cap) = bound {
            let pending = scratch.len() as u64 + end as u64;
            if pending > cap {
                return Err(Error::new(
                    ErrorCode::CBKF226_RECORD_BOUND_EXCEEDED,
                    format!(
                        "Text line {} exceeds the reviewed bound of {cap} bytes before any terminator",
                        record_index + 1,
                    ),
                )
                .with_context(line_context(record_index, None)));
            }
        }
        scratch.extend_from_slice(&chunk[..end]);
        reader.consume(end);
        if terminated {
            break;
        }
    }
    if scratch.is_empty() {
        return Ok(None);
    }
    if scratch.last() == Some(&b'\n') {
        scratch.pop();
    }
    if scratch.last() == Some(&b'\r') {
        scratch.pop();
    }
    let actual = scratch.len();
    if actual != lrecl {
        return Err(Error::new(
            ErrorCode::CBKR101_FIXED_RECORD_ERROR,
            format!(
                "Text line {} has {actual} bytes, expected {lrecl}",
                record_index + 1
            ),
        )
        .with_context(line_context(record_index, Some(actual))));
    }
    Ok(Some(scratch.clone()))
}

fn line_context(record_index: u64, actual: Option<usize>) -> ErrorContext {
    ErrorContext {
        record_index: Some(record_index + 1),
        field_path: None,
        byte_offset: None,
        line_number: u32::try_from(record_index + 1).ok(),
        details: actual.map(|seen| format!("Line payload is {seen} bytes")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    fn read_all(bytes: &[u8], lrecl: usize, bound: Option<u64>) -> Result<Vec<Vec<u8>>> {
        let mut reader = std::io::BufReader::new(Cursor::new(bytes));
        let mut scratch = Vec::new();
        let mut out = Vec::new();
        let mut index = 0u64;
        while let Some(payload) = read_text_record(&mut reader, &mut scratch, lrecl, bound, index)?
        {
            out.push(payload);
            index += 1;
        }
        Ok(out)
    }

    #[test]
    fn strips_lf_and_crlf_accepts_unterminated_final() {
        let out = read_all(b"ABCDEFGH\nABCDEFGH\r\nABCDEFGH", 8, None).expect("lines frame");
        assert_eq!(out, vec![b"ABCDEFGH".to_vec(); 3]);
    }

    #[test]
    fn clean_eof_yields_no_records() {
        let out = read_all(b"", 8, None).expect("empty file frames");
        assert!(out.is_empty());
    }

    #[test]
    fn lone_cr_is_data_not_a_terminator() {
        let out = read_all(b"ABC\rDEFG\n", 8, None).expect("lone CR stays in payload");
        assert_eq!(out, vec![b"ABC\rDEFG".to_vec()]);
    }

    #[test]
    fn short_line_names_expected_and_actual() {
        let error = read_all(b"ABCDEF\n", 8, None).expect_err("short line fails");
        assert_eq!(error.code(), ErrorCode::CBKR101_FIXED_RECORD_ERROR);
        assert!(error.to_string().contains('8'), "names expected: {error}");
        assert!(error.to_string().contains('6'), "names actual: {error}");
    }

    #[test]
    fn long_line_is_rejected_not_truncated() {
        let error = read_all(b"ABCDEFGHI\n", 8, None).expect_err("long line fails");
        assert_eq!(error.code(), ErrorCode::CBKR101_FIXED_RECORD_ERROR);
    }

    #[test]
    fn unbounded_line_hits_the_record_bound() {
        let error = read_all(b"AAAAAAAAAA", 8, Some(9)).expect_err("bound trips");
        assert_eq!(error.code(), ErrorCode::CBKF226_RECORD_BOUND_EXCEEDED);
    }
}
