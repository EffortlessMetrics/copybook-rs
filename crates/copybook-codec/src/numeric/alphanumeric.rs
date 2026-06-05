// SPDX-License-Identifier: AGPL-3.0-or-later
//! Alphanumeric field encoding helpers.
//!
//! This module owns text-to-codepage conversion and fixed-width padding for
//! COBOL alphanumeric fields.

use crate::options::Codepage;
use copybook_core::{Error, ErrorCode, Result};

/// Encode an alphanumeric (PIC X) field with right-padding to the declared length.
///
/// Converts the UTF-8 input string to the target codepage encoding and then
/// right-pads with space characters (ASCII `0x20` or EBCDIC `0x40`) to fill
/// the declared field length.
///
/// # Arguments
/// * `text` - UTF-8 string value to encode
/// * `field_len` - Declared byte length of the COBOL field
/// * `codepage` - Target character encoding (ASCII or EBCDIC variant)
///
/// # Returns
/// A vector of exactly `field_len` bytes containing the encoded and padded text.
///
/// # Errors
/// * `CBKE501_JSON_TYPE_MISMATCH` - if the encoded text exceeds `field_len` bytes
///
/// # Examples
///
/// ```
/// use copybook_codec::numeric::encode_alphanumeric;
/// use copybook_codec::Codepage;
///
/// // Encode a 5-character string into a 10-byte ASCII field.
/// let result = encode_alphanumeric("HELLO", 10, Codepage::ASCII).unwrap();
/// assert_eq!(result.len(), 10);
/// assert_eq!(&result[..5], b"HELLO");
/// assert_eq!(&result[5..], b"     ");
/// ```
///
/// # See Also
/// * [`crate::charset::utf8_to_ebcdic`] - Underlying character conversion
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn encode_alphanumeric(text: &str, field_len: usize, codepage: Codepage) -> Result<Vec<u8>> {
    let encoded_bytes = crate::charset::utf8_to_ebcdic(text, codepage)?;

    if encoded_bytes.len() > field_len {
        return Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            format!(
                "Text length {} exceeds field length {}",
                encoded_bytes.len(),
                field_len
            ),
        ));
    }

    let mut result = encoded_bytes;
    let space_byte = match codepage {
        Codepage::ASCII => b' ',
        _ => 0x40,
    };

    result.resize(field_len, space_byte);
    Ok(result)
}
