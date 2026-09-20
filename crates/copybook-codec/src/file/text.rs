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
/// longer than `lrecl`, and `CBKF226_RECORD_BOUND_EXCEEDED` when the
/// payload outgrows `bound` (terminators never count against the bound).
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
        // The bound governs payload bytes, not framing: a profile cap at
        // exactly `lrecl` must accept every terminated line.
        let fresh = end - usize::from(terminated);
        let cr = usize::from(terminated && end >= 2 && chunk[end - 2] == b'\r');
        let pending = scratch.len().saturating_add(fresh).saturating_sub(cr) as u64;
        check_bound(pending, bound, record_index)?;
        scratch.extend_from_slice(&chunk[..end]);
        reader.consume(end);
        if terminated {
            break;
        }
    }
    if scratch.is_empty() {
        return Ok(None);
    }
    finish_text_payload(scratch, terminated, lrecl, record_index).map(Some)
}

/// Read one text-framed payload without buffering the caller stream.
///
/// Same contract as [`read_text_record`], but safe to call repeatedly on
/// one stream: a buffered reader would keep read-ahead bytes past the
/// newline and drop them when it goes out of scope. Byte-wise reads cost
/// one syscall per byte, which suits a single-shot helper.
///
/// # Errors
///
/// Same error contract as [`read_text_record`].
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn read_text_record_unbuffered(
    reader: &mut impl std::io::Read,
    scratch: &mut Vec<u8>,
    lrecl: usize,
    bound: Option<u64>,
    record_index: u64,
) -> Result<Option<Vec<u8>>> {
    scratch.clear();
    let mut terminated = false;
    let mut byte = [0u8];
    loop {
        let read = reader.read(&mut byte).map_err(|error| {
            Error::new(
                ErrorCode::CBKR101_FIXED_RECORD_ERROR,
                format!("Failed to read text record: {error}"),
            )
            .with_context(line_context(record_index, None))
        })?;
        if read == 0 {
            break;
        }
        if byte[0] == b'\n' {
            terminated = true;
            break;
        }
        scratch.push(byte[0]);
        check_bound(scratch.len() as u64, bound, record_index)?;
    }
    if scratch.is_empty() && !terminated {
        return Ok(None);
    }
    finish_text_payload(scratch, terminated, lrecl, record_index).map(Some)
}

fn check_bound(pending_payload: u64, bound: Option<u64>, record_index: u64) -> Result<()> {
    if let Some(cap) = bound
        && pending_payload > cap
    {
        return Err(Error::new(
            ErrorCode::CBKF226_RECORD_BOUND_EXCEEDED,
            format!(
                "Text line {} payload exceeds the reviewed bound of {cap} bytes",
                record_index + 1,
            ),
        )
        .with_context(line_context(record_index, None)));
    }
    Ok(())
}

/// Strip framing from an accumulated line and enforce the payload width.
///
/// A trailing CR only strips when a terminator was seen: on an
/// unterminated final line a CR is payload data. The buffered reader
/// retains its own `\n`; the unbuffered reader stops before it, so the
/// pop is conditional either way.
fn finish_text_payload(
    scratch: &mut Vec<u8>,
    terminated: bool,
    lrecl: usize,
    record_index: u64,
) -> Result<Vec<u8>> {
    // Only the buffered reader retains `\n` (the unbuffered reader stops
    // before it), so this pop is conditional either way.
    if scratch.last() == Some(&b'\n') {
        scratch.pop();
    }
    if terminated && scratch.last() == Some(&b'\r') {
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
    Ok(scratch.clone())
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

    #[test]
    fn bound_at_lrecl_accepts_terminated_lines() {
        // Terminators are framing: a cap at exactly `lrecl` must accept
        // every well-formed line.
        let out = read_all(b"ABCDEFGH\nABCDEFGH\r\n", 8, Some(8)).expect("lines frame");
        assert_eq!(out, vec![b"ABCDEFGH".to_vec(); 2]);
    }

    #[test]
    fn unterminated_final_cr_is_payload() {
        let out = read_all(b"ABCDEFG\r", 8, None).expect("trailing CR kept");
        assert_eq!(out, vec![b"ABCDEFG\r".to_vec()]);
    }

    #[test]
    fn unbuffered_reads_repeat_on_one_stream() {
        let mut stream = Cursor::new(b"AAAAAAAA\nBBBBBBBB\n");
        let mut scratch = Vec::new();
        let first =
            read_text_record_unbuffered(&mut stream, &mut scratch, 8, None, 0).expect("first line");
        let second = read_text_record_unbuffered(&mut stream, &mut scratch, 8, None, 1)
            .expect("second line");
        assert_eq!(first, Some(b"AAAAAAAA".to_vec()));
        assert_eq!(second, Some(b"BBBBBBBB".to_vec()));
        let end =
            read_text_record_unbuffered(&mut stream, &mut scratch, 8, None, 2).expect("clean eof");
        assert_eq!(end, None);
    }
}
