// SPDX-License-Identifier: AGPL-3.0-or-later
//! Binary integer codecs for COBOL COMP/BINARY fields.

use copybook_core::{Error, ErrorCode, Result};

/// Decode binary integer field (COMP-4, COMP-5, BINARY)
///
/// Decodes big-endian binary integer fields. Supports 16-bit, 32-bit, and 64-bit
/// widths. All binary integers use big-endian byte order as per COBOL specification.
///
/// # Arguments
/// * `data` - Raw byte data containing the binary integer
/// * `bits` - Bit width of the field (16, 32, or 64)
/// * `signed` - Whether the field is signed (true) or unsigned (false)
///
/// # Returns
/// The decoded integer value as `i64`
///
/// # Errors
/// Returns an error if the binary data is invalid or the field size is unsupported.
///
/// # Examples
///
/// ## 16-bit Signed Integer
///
/// ```no_run
/// use copybook_codec::numeric::{decode_binary_int};
///
/// // 16-bit signed: -12345 = [0xCF, 0xC7] (big-endian)
/// let data = [0xCF, 0xC7];
/// let result = decode_binary_int(&data, 16, true)?;
/// assert_eq!(result, -12345);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## 16-bit Unsigned Integer
///
/// ```no_run
/// use copybook_codec::numeric::{decode_binary_int};
///
/// // 16-bit unsigned: 54321 = [0xD4, 0x31]
/// let data = [0xD4, 0x31];
/// let result = decode_binary_int(&data, 16, false)?;
/// assert_eq!(result, 54321);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## 32-bit Signed Integer
///
/// ```no_run
/// use copybook_codec::numeric::{decode_binary_int};
///
/// // 32-bit signed: -987654321 = [0xC5, 0x7D, 0x3C, 0x21]
/// let data = [0xC5, 0x7D, 0x3C, 0x21];
/// let result = decode_binary_int(&data, 32, true)?;
/// assert_eq!(result, -987654321);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## 64-bit Signed Integer
///
/// ```no_run
/// use copybook_codec::numeric::{decode_binary_int};
///
/// // 64-bit signed: 9223372036854775807 = [0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07]
/// let data = [0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07];
/// let result = decode_binary_int(&data, 64, true)?;
/// assert_eq!(result, 9223372036854775807);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// # See Also
/// * [`encode_binary_int`] - For encoding binary integers
/// * [`get_binary_width_from_digits`] - For mapping digit count to width
/// * [`validate_explicit_binary_width`] - For validating explicit BINARY(n) widths
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_binary_int(data: &[u8], bits: u16, signed: bool) -> Result<i64> {
    let expected_bytes = usize::from(bits / 8);
    if data.len() != expected_bytes {
        return Err(Error::new(
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE, // Reusing error code for binary validation
            format!(
                "Binary data length {} doesn't match expected {} bytes for {} bits",
                data.len(),
                expected_bytes,
                bits
            ),
        ));
    }

    match bits {
        16 => {
            if data.len() != 2 {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    "16-bit binary field requires exactly 2 bytes",
                ));
            }
            let value = u16::from_be_bytes([data[0], data[1]]);
            if signed {
                Ok(i64::from(i16::from_be_bytes([data[0], data[1]])))
            } else {
                Ok(i64::from(value))
            }
        }
        32 => {
            if data.len() != 4 {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    "32-bit binary field requires exactly 4 bytes",
                ));
            }
            let value = u32::from_be_bytes([data[0], data[1], data[2], data[3]]);
            if signed {
                Ok(i64::from(i32::from_be_bytes([
                    data[0], data[1], data[2], data[3],
                ])))
            } else {
                Ok(i64::from(value))
            }
        }
        64 => {
            if data.len() != 8 {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    "64-bit binary field requires exactly 8 bytes",
                ));
            }
            let bytes: [u8; 8] = data.try_into().map_err(|_| {
                Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    "Failed to convert data to 8-byte array",
                )
            })?;
            if signed {
                Ok(i64::from_be_bytes(bytes))
            } else {
                // For unsigned 64-bit, we need to be careful about overflow
                let value = u64::from_be_bytes(bytes);
                let max_i64 = u64::try_from(i64::MAX).unwrap_or(u64::MAX);
                if value > max_i64 {
                    return Err(Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        format!("Unsigned 64-bit value {value} exceeds i64::MAX"),
                    ));
                }
                i64::try_from(value).map_err(|_| {
                    Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        format!("Unsigned 64-bit value {value} exceeds i64::MAX"),
                    )
                })
            }
        }
        _ => Err(Error::new(
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
            format!("Unsupported binary field width: {bits} bits"),
        )),
    }
}

/// Encode binary integer field (COMP-4, COMP-5, BINARY)
///
/// Encodes integer values to big-endian binary format. Supports 16-bit, 32-bit, and 64-bit
/// widths. All binary integers use big-endian byte order as per COBOL specification.
///
/// # Arguments
/// * `value` - Integer value to encode
/// * `bits` - Bit width of field (16, 32, or 64)
/// * `signed` - Whether the field is signed (true) or unsigned (false)
///
/// # Returns
/// A vector of bytes containing the encoded binary integer
///
/// # Errors
/// Returns an error if the value is out of range for the specified bit width.
///
/// # Examples
///
/// ## 16-bit Signed Integer
///
/// ```no_run
/// use copybook_codec::numeric::{encode_binary_int};
///
/// // Encode -12345 as 16-bit signed
/// let encoded = encode_binary_int(-12345, 16, true)?;
/// assert_eq!(encoded, [0xCF, 0xC7]); // Big-endian
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## 16-bit Unsigned Integer
///
/// ```no_run
/// use copybook_codec::numeric::{encode_binary_int};
///
/// // Encode 54321 as 16-bit unsigned
/// let encoded = encode_binary_int(54321, 16, false)?;
/// assert_eq!(encoded, [0xD4, 0x31]);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## 32-bit Signed Integer
///
/// ```no_run
/// use copybook_codec::numeric::{encode_binary_int};
///
/// // Encode -987654321 as 32-bit signed
/// let encoded = encode_binary_int(-987654321, 32, true)?;
/// assert_eq!(encoded, [0xC5, 0x7D, 0x3C, 0x21]);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## 64-bit Signed Integer
///
/// ```no_run
/// use copybook_codec::numeric::{encode_binary_int};
///
/// // Encode 9223372036854775807 as 64-bit signed
/// let encoded = encode_binary_int(9223372036854775807, 64, true)?;
/// assert_eq!(encoded, [0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07]);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// # See Also
/// * [`decode_binary_int`] - For decoding binary integers
/// * [`get_binary_width_from_digits`] - For mapping digit count to width
/// * [`validate_explicit_binary_width`] - For validating explicit BINARY(n) widths
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn encode_binary_int(value: i64, bits: u16, signed: bool) -> Result<Vec<u8>> {
    match bits {
        16 => {
            if signed {
                let int_value = i16::try_from(value).map_err(|_| {
                    Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} out of range for signed 16-bit integer"),
                    )
                })?;
                Ok(int_value.to_be_bytes().to_vec())
            } else {
                let int_value = u16::try_from(value).map_err(|_| {
                    Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} out of range for unsigned 16-bit integer"),
                    )
                })?;
                Ok(int_value.to_be_bytes().to_vec())
            }
        }
        32 => {
            if signed {
                let int_value = i32::try_from(value).map_err(|_| {
                    Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} out of range for signed 32-bit integer"),
                    )
                })?;
                Ok(int_value.to_be_bytes().to_vec())
            } else {
                let int_value = u32::try_from(value).map_err(|_| {
                    Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} out of range for unsigned 32-bit integer"),
                    )
                })?;
                Ok(int_value.to_be_bytes().to_vec())
            }
        }
        64 => {
            if signed {
                Ok(value.to_be_bytes().to_vec())
            } else {
                let int_value = u64::try_from(value).map_err(|_| {
                    Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} cannot be negative for unsigned 64-bit integer"),
                    )
                })?;
                Ok(int_value.to_be_bytes().to_vec())
            }
        }
        _ => Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            format!("Unsupported binary field width: {bits} bits"),
        )),
    }
}

/// Map a COBOL PIC digit count to the corresponding USAGE BINARY storage width
/// in bits (NORMATIVE).
///
/// Follows the COBOL standard mapping:
/// - 1-4 digits  -> 16 bits (2 bytes, halfword)
/// - 5-9 digits  -> 32 bits (4 bytes, fullword)
/// - 10-18 digits -> 64 bits (8 bytes, doubleword)
///
/// # Arguments
/// * `digits` - Number of digit positions declared in the PIC clause
///
/// # Returns
/// The storage width in bits: 16, 32, or 64.
///
/// # See Also
/// * [`validate_explicit_binary_width`] - For explicit `USAGE BINARY(n)` declarations
/// * [`decode_binary_int`] - Decodes binary integer data at the determined width
#[inline]
#[must_use]
pub fn get_binary_width_from_digits(digits: u16) -> u16 {
    match digits {
        1..=4 => 16, // 2 bytes
        5..=9 => 32, // 4 bytes
        _ => 64,     // 8 bytes for larger values
    }
}

/// Validate an explicit `USAGE BINARY(n)` byte-width declaration and return
/// the equivalent bit width (NORMATIVE).
///
/// Only the widths 1, 2, 4, and 8 bytes are valid.  Each is converted to
/// the corresponding bit count (8, 16, 32, 64) for downstream codec use.
///
/// # Arguments
/// * `width_bytes` - The explicit byte width from the copybook (1, 2, 4, or 8)
///
/// # Returns
/// The equivalent width in bits: 8, 16, 32, or 64.
///
/// # Errors
/// * `CBKE501_JSON_TYPE_MISMATCH` - if `width_bytes` is not 1, 2, 4, or 8
///
/// # See Also
/// * [`get_binary_width_from_digits`] - Infers width from PIC digit count
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn validate_explicit_binary_width(width_bytes: u8) -> Result<u16> {
    match width_bytes {
        1 => Ok(8),  // 1 byte = 8 bits
        2 => Ok(16), // 2 bytes = 16 bits
        4 => Ok(32), // 4 bytes = 32 bits
        8 => Ok(64), // 8 bytes = 64 bits
        _ => Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            format!("Invalid explicit binary width: {width_bytes} bytes. Must be 1, 2, 4, or 8"),
        )),
    }
}

/// Decode a COBOL binary integer (USAGE BINARY / COMP) with optimized fast
/// paths for 16-, 32-, and 64-bit widths.
///
/// For the three common widths this function reads the big-endian bytes
/// directly into the native integer type, avoiding the generic loop in
/// [`decode_binary_int`].  Uncommon widths fall back to that generic
/// implementation.
///
/// # Arguments
/// * `data` - Raw big-endian byte data containing the binary integer
/// * `bits` - Expected field width in bits (16, 32, or 64 for fast path)
/// * `signed` - Whether the field is signed (PIC S9 COMP) or unsigned (PIC 9 COMP)
///
/// # Returns
/// The decoded integer value as `i64`.
///
/// # Errors
/// * `CBKD401_COMP3_INVALID_NIBBLE` - if an unsigned 64-bit value exceeds `i64::MAX`
/// * Delegates to [`decode_binary_int`] for unsupported widths, which may
///   return its own errors.
///
/// # See Also
/// * [`decode_binary_int`] - General-purpose binary integer decoder
/// * [`encode_binary_int`] - Binary integer encoder
/// * [`super::format_binary_int_to_string_with_scratch`] - Formats the decoded value to a string
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_binary_int_fast(data: &[u8], bits: u16, signed: bool) -> Result<i64> {
    // Optimized paths for common binary widths
    match (bits, data.len()) {
        (16, 2) => {
            // 16-bit integer - most common case
            let bytes = [data[0], data[1]];
            if signed {
                Ok(i64::from(i16::from_be_bytes(bytes)))
            } else {
                Ok(i64::from(u16::from_be_bytes(bytes)))
            }
        }
        (32, 4) => {
            // 32-bit integer - common case
            let bytes = [data[0], data[1], data[2], data[3]];
            if signed {
                Ok(i64::from(i32::from_be_bytes(bytes)))
            } else {
                Ok(i64::from(u32::from_be_bytes(bytes)))
            }
        }
        (64, 8) => {
            // 64-bit integer
            let bytes = [
                data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
            ];
            if signed {
                Ok(i64::from_be_bytes(bytes))
            } else {
                let value = u64::from_be_bytes(bytes);
                let max_i64 = u64::try_from(i64::MAX).unwrap_or(u64::MAX);
                if value > max_i64 {
                    return Err(Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        format!("Unsigned 64-bit value {value} exceeds i64::MAX"),
                    ));
                }
                i64::try_from(value).map_err(|_| {
                    Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        format!("Unsigned 64-bit value {value} exceeds i64::MAX"),
                    )
                })
            }
        }
        _ => {
            // Fallback to general implementation
            decode_binary_int(data, bits, signed)
        }
    }
}
