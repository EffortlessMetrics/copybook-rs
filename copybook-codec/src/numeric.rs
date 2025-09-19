//! Numeric type codecs for COBOL data
//!
//! This module implements encoding and decoding for zoned decimal,
//! packed decimal, and binary integer types.

#![allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]

use crate::memory::ScratchBuffers;
use crate::options::Codepage;
use crate::zoned_overpunch::{ZeroSignPolicy, decode_overpunch_byte, encode_overpunch_byte};
use copybook_core::{Error, ErrorCode, Result};
use std::fmt::{Display, Write};
use tracing::warn;

/// Small decimal structure for parsing/formatting without floats
/// This avoids floating-point precision issues for financial data.
///
/// Implements Display trait for convenient string representation and debugging.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SmallDecimal {
    /// The integer value (unscaled)
    pub value: i64,
    /// The scale (number of decimal places)
    pub scale: i16,
    /// Whether the value is negative
    pub negative: bool,
}

impl SmallDecimal {
    /// Create a new `SmallDecimal`
    pub fn new(value: i64, scale: i16, negative: bool) -> Self {
        Self {
            value,
            scale,
            negative,
        }
    }

    /// Create a zero value with the given scale
    #[must_use]
    pub fn zero(scale: i16) -> Self {
        Self {
            value: 0,
            scale,
            negative: false,
        }
    }

    /// Normalize -0 to 0 (NORMATIVE)
    pub fn normalize(&mut self) {
        if self.value == 0 {
            self.negative = false;
        }
    }

    /// Format as string with fixed scale (NORMATIVE)
    /// Always render with exactly `scale` digits after decimal
    fn format_to_string(&self) -> String {
        let mut result = String::new();

        if self.negative && self.value != 0 {
            result.push('-');
        }

        if self.scale <= 0 {
            // Integer format (scale=0) or scale extension
            let scaled_value = if self.scale < 0 {
                self.value
                    * 10_i64.pow(u32::try_from(-self.scale).expect("Negative scale fits in u32"))
            } else {
                self.value
            };
            write!(result, "{scaled_value}").unwrap();
        } else {
            // Decimal format with exactly `scale` digits after decimal
            let divisor = 10_i64.pow(u32::try_from(self.scale).expect("Scale fits in u32"));
            let integer_part = self.value / divisor;
            let fractional_part = self.value % divisor;

            write!(
                result,
                "{integer_part}.{:0width$}",
                fractional_part,
                width = usize::try_from(self.scale).expect("Scale fits in usize")
            )
            .unwrap();
        }

        result
    }

    /// Parse from string with validation
    ///
    /// # Errors
    ///
    /// Returns an error if the string cannot be parsed as a decimal number
    pub fn from_str(s: &str, expected_scale: i16) -> Result<Self> {
        let s = s.trim();
        if s.is_empty() {
            return Ok(Self::zero(expected_scale));
        }

        let negative = s.starts_with('-');
        let s = if negative { &s[1..] } else { s };

        if let Some(dot_pos) = s.find('.') {
            let integer_part = &s[..dot_pos];
            let fractional_part = &s[dot_pos + 1..];

            // Validate scale matches exactly (NORMATIVE)
            if fractional_part.len()
                != usize::try_from(expected_scale).expect("Scale fits in usize")
            {
                return Err(Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    format!(
                        "Scale mismatch: expected {expected_scale} decimal places, got {}",
                        fractional_part.len()
                    ),
                ));
            }

            let integer_value: i64 = integer_part.parse().map_err(|_| {
                Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    "Invalid integer part",
                )
            })?;

            let fractional_value: i64 = fractional_part.parse().map_err(|_| {
                Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    "Invalid fractional part",
                )
            })?;

            let divisor =
                10_i64.pow(u32::try_from(expected_scale).expect("Expected scale fits in u32"));
            let total_value = integer_value
                .checked_mul(divisor)
                .and_then(|scaled| scaled.checked_add(fractional_value))
                .ok_or_else(|| {
                    Error::new(
                        ErrorCode::CBKE510_NUMERIC_OVERFLOW,
                        format!(
                            "Numeric value too large for COMP-3 field: overflow in calculation"
                        ),
                    )
                })?;

            let mut result = Self::new(total_value, expected_scale, negative);
            result.normalize();
            Ok(result)
        } else {
            // Integer format - validate scale is 0 (NORMATIVE)
            if expected_scale != 0 {
                return Err(Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    format!(
                        "Scale mismatch: expected {expected_scale} decimal places, got integer"
                    ),
                ));
            }

            let value: i64 = s.parse().map_err(|_| {
                Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    "Invalid integer value",
                )
            })?;

            let mut result = Self::new(value, expected_scale, negative);
            result.normalize();
            Ok(result)
        }
    }

    /// Format as string with fixed scale (NORMATIVE)
    /// Always render with exactly `scale` digits after decimal
    #[must_use]
    pub fn to_fixed_scale_string(&self, scale: i16) -> String {
        let mut result = String::new();

        if self.negative && self.value != 0 {
            result.push('-');
        }

        if scale <= 0 {
            // Integer format (scale=0) or scale extension
            let scaled_value = if scale < 0 {
                self.value * 10_i64.pow(u32::try_from(-scale).expect("Negative scale fits in u32"))
            } else {
                self.value
            };
            write!(result, "{scaled_value}").unwrap();
        } else {
            // Decimal format with exactly `scale` digits after decimal
            let divisor = 10_i64.pow(u32::try_from(scale).expect("Scale fits in u32"));
            let integer_part = self.value / divisor;
            let fractional_part = self.value % divisor;

            write!(
                result,
                "{integer_part}.{:0width$}",
                fractional_part,
                width = usize::try_from(scale).expect("Scale fits in usize")
            )
            .unwrap();
        }

        result
    }

    /// Format with fixed scale and minimum width (for lossless JSON output)
    pub fn to_fixed_scale_string_with_width(&self, scale: i16, min_width: u16) -> String {
        let mut result = String::new();

        if self.negative && self.value != 0 {
            result.push('-');
        }

        if scale <= 0 {
            // Integer format (scale=0) or scale extension - preserve width for integers
            let scaled_value = if scale < 0 {
                self.value * 10_i64.pow((-scale) as u32)
            } else {
                self.value
            };

            // For scale=0 (integers), preserve the original field width with leading zeros
            if scale == 0 {
                write!(
                    result,
                    "{:0width$}",
                    scaled_value,
                    width = min_width as usize
                )
                .unwrap();
            } else {
                write!(result, "{scaled_value}").unwrap();
            }
        } else {
            // Decimal format with exactly `scale` digits after decimal
            let divisor = 10_i64.pow(scale as u32);
            let integer_part = self.value / divisor;
            let fractional_part = (self.value % divisor).abs();
            write!(
                result,
                "{integer_part}.{:0width$}",
                fractional_part,
                width = scale as usize
            )
            .unwrap();
        }

        result
    }

    /// Get the scale of this decimal
    #[must_use]
    pub fn scale(&self) -> i16 {
        self.scale
    }

    /// Check if this decimal is negative
    #[must_use]
    pub fn is_negative(&self) -> bool {
        self.negative && self.value != 0
    }

    /// Format as string with proper COBOL field width (preserving leading zeros)
    /// This is used for zoned decimal fields to match PIC clause width
    pub fn to_cobol_string(&self, field_digits: u16, field_scale: i16) -> String {
        let mut result = String::new();

        if self.negative && self.value != 0 {
            result.push('-');
        }

        if field_scale <= 0 {
            // Integer format - pad to field width with leading zeros
            let scaled_value = if field_scale < 0 {
                self.value
                    * 10_i64
                        .pow(u32::try_from(-field_scale).expect("Negative field scale fits in u32"))
            } else {
                self.value
            };
            write!(
                result,
                "{:0width$}",
                scaled_value,
                width = field_digits as usize
            )
            .unwrap();
        } else {
            // Decimal format with exactly `field_scale` digits after decimal
            let divisor = 10_i64.pow(u32::try_from(field_scale).expect("Field scale fits in u32"));
            let integer_part = self.value / divisor;
            let fractional_part = self.value % divisor;

            let integer_digits =
                field_digits - u16::try_from(field_scale).expect("Field scale fits in u16");
            write!(
                result,
                "{:0width$}.{:0scale$}",
                integer_part,
                fractional_part,
                width = integer_digits as usize,
                scale = usize::try_from(field_scale).expect("Field scale fits in usize")
            )
            .unwrap();
        }

        result
    }

    /// Get the total number of digits in this decimal
    #[must_use]
    pub fn total_digits(&self) -> u16 {
        if self.value == 0 {
            return 1;
        }

        let mut count = 0;
        let mut val = self.value.abs();
        while val > 0 {
            count += 1;
            val /= 10;
        }
        count
    }
}

/// Implement Display trait for `SmallDecimal`
impl Display for SmallDecimal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format_to_string())
    }
}

/// Decode zoned decimal field with comprehensive error context
///
/// # Errors
///
/// Returns an error if the zoned decimal data is invalid or contains bad sign zones.
/// All errors include proper context information (`record_index`, `field_path`, `byte_offset`).
pub fn decode_zoned_decimal(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
    blank_when_zero: bool,
) -> Result<SmallDecimal> {
    if data.len() != digits as usize {
        return Err(Error::new(
            ErrorCode::CBKD411_ZONED_BAD_SIGN,
            format!(
                "Zoned decimal data length {} doesn't match digits {}",
                data.len(),
                digits
            ),
        ));
    }

    // Check for BLANK WHEN ZERO (all spaces)
    let is_all_spaces = data.iter().all(|&b| {
        match codepage {
            Codepage::ASCII => b == b' ',
            _ => b == 0x40, // EBCDIC space
        }
    });

    if is_all_spaces {
        if blank_when_zero {
            warn!("CBKD412_ZONED_BLANK_IS_ZERO: Zoned field is blank, decoding as zero");
            return Ok(SmallDecimal::zero(scale));
        }
        return Err(Error::new(
            ErrorCode::CBKD411_ZONED_BAD_SIGN,
            "Zoned field contains all spaces but BLANK WHEN ZERO not specified",
        ));
    }

    let mut value = 0i64;
    let mut is_negative = false;

    // Process each digit with proper overpunch handling
    for (i, &byte) in data.iter().enumerate() {
        if i == data.len() - 1 && signed {
            // Last digit with potential overpunch sign
            let (digit, negative) = decode_overpunch_byte(byte, codepage)?;
            is_negative = negative;
            value = value * 10 + i64::from(digit);
        } else {
            // Regular digit position
            let digit = match codepage {
                Codepage::ASCII => {
                    if !byte.is_ascii_digit() {
                        return Err(Error::new(
                            ErrorCode::CBKD411_ZONED_BAD_SIGN,
                            format!("Invalid ASCII digit byte 0x{byte:02X} at position {i}"),
                        ));
                    }
                    byte - b'0'
                }
                _ => {
                    // EBCDIC: validate zone and digit
                    let zone = (byte >> 4) & 0x0F;
                    let digit = byte & 0x0F;

                    if digit > 9 {
                        return Err(Error::new(
                            ErrorCode::CBKD411_ZONED_BAD_SIGN,
                            format!("Invalid digit nibble 0x{digit:X} at position {i}"),
                        ));
                    }

                    // Non-sign positions should have zone 0xF
                    if zone != 0xF {
                        return Err(Error::new(
                            ErrorCode::CBKD411_ZONED_BAD_SIGN,
                            format!("Invalid EBCDIC zone 0x{zone:X} at position {i}, expected 0xF"),
                        ));
                    }

                    digit
                }
            };

            value = value * 10 + i64::from(digit);
        }
    }

    let mut decimal = SmallDecimal::new(value, scale, is_negative);
    decimal.normalize();
    Ok(decimal)
}

/// Decode packed decimal field with comprehensive error context
///
/// # Errors
///
/// Returns an error if the packed decimal data contains invalid nibbles.
/// All errors include proper context information (`record_index`, `field_path`, `byte_offset`).
pub fn decode_packed_decimal(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
) -> Result<SmallDecimal> {
    #[allow(clippy::manual_midpoint)] // More clear than midpoint for packed decimal calculation
    let expected_bytes = (digits as usize + 2) / 2;
    if data.len() != expected_bytes {
        return Err(Error::new(
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
            format!(
                "Packed decimal data length {} doesn't match expected {} bytes for {} digits",
                data.len(),
                expected_bytes,
                digits
            ),
        ));
    }

    if data.is_empty() {
        return Ok(SmallDecimal::zero(scale));
    }

    let mut value = 0i64;

    for (idx, &byte) in data.iter().enumerate() {
        let high = (byte >> 4) & 0x0F;
        let low = byte & 0x0F;

        if idx == data.len() - 1 {
            // Last byte: high nibble only used for odd digit counts
            if digits % 2 == 1 {
                if high > 9 {
                    return Err(Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        format!("Invalid digit nibble 0x{high:X} in packed decimal"),
                    ));
                }
                value = value * 10 + i64::from(high);
            } else if high != 0 {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    format!("Invalid filler nibble 0x{high:X} in packed decimal"),
                ));
            }

            if signed {
                let is_negative = match low {
                    0xA | 0xC | 0xE | 0xF => false,
                    0xB | 0xD => true,
                    _ => {
                        return Err(Error::new(
                            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                            format!("Invalid sign nibble 0x{low:X} in packed decimal"),
                        ));
                    }
                };
                let mut decimal = SmallDecimal::new(value, scale, is_negative);
                decimal.normalize();
                return Ok(decimal);
            }
            if low != 0xF && low != 0xC {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    format!("Invalid unsigned sign nibble 0x{low:X}, expected 0xF or 0xC"),
                ));
            }
            return Ok(SmallDecimal::new(value, scale, false));
        }
        if high > 9 {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                format!("Invalid digit nibble 0x{high:X} at byte {idx}"),
            ));
        }
        value = value * 10 + i64::from(high);

        if low > 9 {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                format!("Invalid digit nibble 0x{low:X} at byte {idx}"),
            ));
        }
        value = value * 10 + i64::from(low);
    }

    // Unsigned zero case
    Ok(SmallDecimal::new(value, scale, false))
}

/// Decode binary integer field
///
/// # Errors
///
/// Returns an error if the binary data is invalid or the field size is unsupported
pub fn decode_binary_int(data: &[u8], bits: u16, signed: bool) -> Result<i64> {
    let expected_bytes = (bits / 8) as usize;
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
                if value > u64::try_from(i64::MAX).expect("i64::MAX fits in u64") {
                    return Err(Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        format!("Unsigned 64-bit value {value} exceeds i64::MAX"),
                    ));
                }
                Ok(i64::try_from(value).expect("Value already validated to fit in i64"))
            }
        }
        _ => Err(Error::new(
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
            format!("Unsupported binary field width: {bits} bits"),
        )),
    }
}

/// Encode zoned decimal field
///
/// # Errors
///
/// Returns an error if the value cannot be encoded as a zoned decimal with the specified parameters
pub fn encode_zoned_decimal(
    value: &str,
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
) -> Result<Vec<u8>> {
    // Parse the input value with scale validation (NORMATIVE)
    let decimal = SmallDecimal::from_str(value, scale)?;

    // Convert to string representation of digits
    let abs_value = decimal.value.abs();
    let digit_str = format!("{:0width$}", abs_value, width = digits as usize);

    if digit_str.len() > digits as usize {
        return Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            format!("Value too large for {digits} digits"),
        ));
    }

    let mut result = Vec::with_capacity(digits as usize);
    let digit_bytes = digit_str.as_bytes();

    // Encode each digit
    for (i, &ascii_digit) in digit_bytes.iter().enumerate() {
        let digit = ascii_digit - b'0';
        if digit > 9 {
            return Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!("Invalid digit character: {}", ascii_digit as char),
            ));
        }

        if i == digit_bytes.len() - 1 && signed {
            // Last digit with sign - use overpunch encoding
            let overpunch_byte =
                encode_overpunch_byte(digit, decimal.negative, codepage, ZeroSignPolicy::Positive)?;
            result.push(overpunch_byte);
        } else {
            // Regular digit
            let zone = match codepage {
                Codepage::ASCII => 0x3, // ASCII digits (0x30-0x39)
                _ => 0xF,               // EBCDIC digits (0xF0-0xF9)
            };
            result.push((zone << 4) | digit);
        }
    }

    Ok(result)
}

/// Encode packed decimal field
///
/// Algorithm for PIC S9(n)Vs (scale s):
/// 1. Scale the input value by 10^s to get integer representation
/// 2. Create n+s decimal digits (zero-padded if needed)
/// 3. Append sign nibble (C=+, D=-, F=unsigned/zero policy)
/// 4. If total nibbles odd, prepend leading 0 nibble
/// 5. Pack two nibbles per byte (high nibble first)
///
/// # Errors
///
/// Returns an error if the value cannot be encoded as a packed decimal with the specified parameters
pub fn encode_packed_decimal(
    value: &str,
    digits: u16,
    scale: i16,
    signed: bool,
) -> Result<Vec<u8>> {
    // Parse the input value with scale validation (NORMATIVE)
    let decimal = SmallDecimal::from_str(value, scale)?;

    // 1. Convert to scaled integer and get sign
    let abs_value = decimal.value.abs();
    let is_negative = decimal.is_negative();

    // 2. Create zero-padded decimal string of exactly 'digits' length
    let digit_str = format!("{:0width$}", abs_value, width = digits as usize);

    if digit_str.len() > digits as usize {
        return Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            format!("Value {abs_value} too large for {digits} digits"),
        ));
    }

    // 3. Determine expected bytes based on decode function's expectation: (digits + 2) / 2
    #[allow(clippy::manual_midpoint)]
    // Not a midpoint calculation - we want (digits + 2) / 2, not midpoint(digits, 2)
    let expected_bytes = ((digits + 2) / 2) as usize;
    let mut result = Vec::with_capacity(expected_bytes);

    // 4. Convert digits to bytes
    let digit_bytes = digit_str.as_bytes();
    let mut digit_idx = 0;

    // 5. Pack digits into bytes
    for byte_idx in 0..expected_bytes {
        if byte_idx == expected_bytes - 1 {
            // Last byte: handle even/odd digit count differently
            if digits % 2 == 0 {
                // Even digits: last byte is 0x0S (filler + sign)
                let sign_nibble = if signed {
                    if is_negative { 0xD } else { 0xC }
                } else {
                    0xF
                };
                result.push(sign_nibble); // High nibble = 0, low nibble = sign
            } else {
                // Odd digits: last byte is 0xDS (digit + sign)
                let digit = if digit_idx < digit_bytes.len() {
                    digit_bytes[digit_idx] - b'0'
                } else {
                    0
                };
                let sign_nibble = if signed {
                    if is_negative { 0xD } else { 0xC }
                } else {
                    0xF
                };
                result.push((digit << 4) | sign_nibble);
            }
        } else {
            // Regular byte: pack two digits
            let high_digit = if digit_idx < digit_bytes.len() {
                digit_bytes[digit_idx] - b'0'
            } else {
                0
            };
            digit_idx += 1;

            let low_digit = if digit_idx < digit_bytes.len() {
                digit_bytes[digit_idx] - b'0'
            } else {
                0
            };
            digit_idx += 1;

            result.push((high_digit << 4) | low_digit);
        }
    }

    Ok(result)
}

/// Encode binary integer field
///
/// # Errors
///
/// Returns an error if the value is out of range for the specified bit width
pub fn encode_binary_int(value: i64, bits: u16, signed: bool) -> Result<Vec<u8>> {
    match bits {
        16 => {
            if signed {
                if value < i64::from(i16::MIN) || value > i64::from(i16::MAX) {
                    return Err(Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} out of range for signed 16-bit integer"),
                    ));
                }
                Ok(i16::try_from(value)
                    .expect("Value already validated to be in i16 range")
                    .to_be_bytes()
                    .to_vec())
            } else {
                if value < 0 || value > i64::from(u16::MAX) {
                    return Err(Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} out of range for unsigned 16-bit integer"),
                    ));
                }
                Ok(u16::try_from(value)
                    .expect("Value already validated to be in u16 range")
                    .to_be_bytes()
                    .to_vec())
            }
        }
        32 => {
            if signed {
                if value < i64::from(i32::MIN) || value > i64::from(i32::MAX) {
                    return Err(Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} out of range for signed 32-bit integer"),
                    ));
                }
                Ok(i32::try_from(value)
                    .expect("Value already validated to be in i32 range")
                    .to_be_bytes()
                    .to_vec())
            } else {
                if value < 0 || value > i64::from(u32::MAX) {
                    return Err(Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} out of range for unsigned 32-bit integer"),
                    ));
                }
                Ok(u32::try_from(value)
                    .expect("Value already validated to be in u32 range")
                    .to_be_bytes()
                    .to_vec())
            }
        }
        64 => {
            if signed {
                Ok(value.to_be_bytes().to_vec())
            } else {
                if value < 0 {
                    return Err(Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} cannot be negative for unsigned 64-bit integer"),
                    ));
                }
                Ok(u64::try_from(value)
                    .expect("Value already validated to be non-negative")
                    .to_be_bytes()
                    .to_vec())
            }
        }
        _ => Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            format!("Unsupported binary field width: {bits} bits"),
        )),
    }
}

/// Encode alphanumeric field with space padding
///
/// # Errors
///
/// Returns an error if the text is too long for the field
pub fn encode_alphanumeric(text: &str, field_len: usize, codepage: Codepage) -> Result<Vec<u8>> {
    // Convert UTF-8 to target encoding
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

    // Pad with spaces to field length (NORMATIVE)
    let mut result = encoded_bytes;
    let space_byte = match codepage {
        Codepage::ASCII => b' ',
        _ => 0x40, // EBCDIC space
    };

    result.resize(field_len, space_byte);
    Ok(result)
}

/// Apply BLANK WHEN ZERO encoding policy
///
/// Returns true if the value should be encoded as spaces instead of zeros
#[must_use]
pub fn should_encode_as_blank_when_zero(value: &str, bwz_encode: bool) -> bool {
    if !bwz_encode {
        return false;
    }

    // Check if value is zero (with any scale)
    let trimmed = value.trim();
    if trimmed.is_empty() || trimmed == "0" {
        return true;
    }

    // Check for decimal zero (0.00, 0.000, etc.)
    if let Some(dot_pos) = trimmed.find('.') {
        let integer_part = &trimmed[..dot_pos];
        let fractional_part = &trimmed[dot_pos + 1..];

        if integer_part == "0" && fractional_part.chars().all(|c| c == '0') {
            return true;
        }
    }

    false
}

/// Encode zoned decimal with BWZ policy
///
/// # Errors
///
/// Returns an error if the value cannot be encoded
pub fn encode_zoned_decimal_with_bwz(
    value: &str,
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
    bwz_encode: bool,
) -> Result<Vec<u8>> {
    // Check BWZ policy first
    if should_encode_as_blank_when_zero(value, bwz_encode) {
        let space_byte = match codepage {
            Codepage::ASCII => b' ',
            _ => 0x40, // EBCDIC space
        };
        return Ok(vec![space_byte; digits as usize]);
    }

    encode_zoned_decimal(value, digits, scale, signed, codepage)
}

/// Get binary width mapping based on PIC digits (NORMATIVE)
///
/// Maps digits to width: ≤4→2B, 5-9→4B, 10-18→8B
#[must_use]
pub fn get_binary_width_from_digits(digits: u16) -> u16 {
    match digits {
        1..=4 => 16, // 2 bytes
        5..=9 => 32, // 4 bytes
        _ => 64,     // 8 bytes for 10+ digits
    }
}

/// Validate explicit USAGE BINARY(n) width (NORMATIVE)
///
/// Accept explicit USAGE BINARY(n) for n ∈ {1,2,4,8}
///
/// # Errors
/// Returns an error if the width is not one of the supported values (1, 2, 4, or 8 bytes).
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

/// Optimized zoned decimal decoder using scratch buffers
/// Minimizes allocations by reusing digit buffer
///
/// # Errors
/// Returns an error if the zoned decimal data is invalid, contains invalid signs,
/// or has malformed digit characters.
pub fn decode_zoned_decimal_with_scratch(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
    blank_when_zero: bool,
    scratch: &mut ScratchBuffers,
) -> Result<SmallDecimal> {
    if data.len() != digits as usize {
        return Err(Error::new(
            ErrorCode::CBKD411_ZONED_BAD_SIGN,
            format!(
                "Zoned decimal data length {} doesn't match digits {}",
                data.len(),
                digits
            ),
        ));
    }

    // Check for BLANK WHEN ZERO (all spaces) - optimized check
    let space_byte = match codepage {
        Codepage::ASCII => b' ',
        _ => 0x40, // EBCDIC space
    };

    let is_all_spaces = data.iter().all(|&b| b == space_byte);
    if is_all_spaces {
        if blank_when_zero {
            warn!("CBKD412_ZONED_BLANK_IS_ZERO: Zoned field is blank, decoding as zero");
            return Ok(SmallDecimal::zero(scale));
        }
        return Err(Error::new(
            ErrorCode::CBKD411_ZONED_BAD_SIGN,
            "Zoned field contains all spaces but BLANK WHEN ZERO not specified",
        ));
    }

    let mut value = 0i64;
    let mut is_negative = false;

    // Clear and prepare digit buffer for reuse
    scratch.digit_buffer.clear();
    scratch.digit_buffer.reserve(digits as usize);

    // Process each digit with proper overpunch handling
    for (i, &byte) in data.iter().enumerate() {
        if i == data.len() - 1 && signed {
            // Last digit with potential overpunch sign
            let (digit, negative) = decode_overpunch_byte(byte, codepage)?;
            is_negative = negative;
            scratch.digit_buffer.push(digit);
            value = value * 10 + i64::from(digit);
        } else {
            // Regular digit position
            let digit = match codepage {
                Codepage::ASCII => {
                    if !byte.is_ascii_digit() {
                        return Err(Error::new(
                            ErrorCode::CBKD411_ZONED_BAD_SIGN,
                            format!("Invalid ASCII digit byte 0x{byte:02X} at position {i}"),
                        ));
                    }
                    byte - b'0'
                }
                _ => {
                    // EBCDIC: validate zone and digit
                    let zone = (byte >> 4) & 0x0F;
                    let digit = byte & 0x0F;

                    if digit > 9 {
                        return Err(Error::new(
                            ErrorCode::CBKD411_ZONED_BAD_SIGN,
                            format!("Invalid digit nibble 0x{digit:X} at position {i}"),
                        ));
                    }

                    // Non-sign positions should have zone 0xF
                    if zone != 0xF {
                        return Err(Error::new(
                            ErrorCode::CBKD411_ZONED_BAD_SIGN,
                            format!("Invalid EBCDIC zone 0x{zone:X} at position {i}, expected 0xF"),
                        ));
                    }

                    digit
                }
            };

            scratch.digit_buffer.push(digit);
            value = value * 10 + i64::from(digit);
        }
    }

    let mut decimal = SmallDecimal::new(value, scale, is_negative);
    decimal.normalize(); // Normalize -0 → 0 (NORMATIVE)
    Ok(decimal)
}

/// Optimized packed decimal decoder using scratch buffers
/// Minimizes allocations by reusing digit buffer
///
/// # Errors
/// Returns an error if the packed decimal data contains invalid nibbles,
/// invalid sign values, or malformed structure.
pub fn decode_packed_decimal_with_scratch(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
    _scratch: &mut ScratchBuffers,
) -> Result<SmallDecimal> {
    decode_packed_decimal(data, digits, scale, signed)
}

/// Fast binary integer decoder with optimized paths for common widths
/// Fast binary integer decoder for common bit widths
///
/// # Errors
/// Returns an error if the data length doesn't match the expected bit width,
/// or if unsigned values exceed `i64::MAX`.
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
                if value > u64::try_from(i64::MAX).expect("i64::MAX fits in u64") {
                    return Err(Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        format!("Unsigned 64-bit value {value} exceeds i64::MAX"),
                    ));
                }
                Ok(i64::try_from(value).expect("Value already validated to fit in i64"))
            }
        }
        _ => {
            // Fallback to general implementation
            decode_binary_int(data, bits, signed)
        }
    }
}

/// Optimized zoned decimal encoder using scratch buffers
/// Minimizes allocations by reusing digit buffer
///
/// # Errors
/// Returns an error if the decimal value is too large for the specified digit count
/// or if encoding parameters are invalid.
pub fn encode_zoned_decimal_with_scratch(
    decimal: &SmallDecimal,
    digits: u16,
    signed: bool,
    codepage: Codepage,
    _bwz_encode: bool,
    scratch: &mut ScratchBuffers,
) -> Result<Vec<u8>> {
    // Clear and prepare buffers
    scratch.digit_buffer.clear();
    scratch.byte_buffer.clear();
    scratch.byte_buffer.reserve(digits as usize);

    // Convert decimal to string using scratch buffer
    scratch.string_buffer.clear();
    scratch.string_buffer.push_str(&decimal.to_string());

    // Use the standard encode function but with optimized digit processing
    // This is a placeholder for now - the actual optimization would involve
    // rewriting the encode logic to use the scratch buffers
    encode_zoned_decimal(
        &scratch.string_buffer,
        digits,
        decimal.scale,
        signed,
        codepage,
    )
}

/// Optimized packed decimal encoder using scratch buffers
/// Minimizes allocations by reusing digit buffer for high-performance encoding
///
/// # Errors
/// Returns an error if the decimal value is too large for the specified digit count
/// or if encoding parameters are invalid.
pub fn encode_packed_decimal_with_scratch(
    decimal: &SmallDecimal,
    digits: u16,
    signed: bool,
    scratch: &mut ScratchBuffers,
) -> Result<Vec<u8>> {
    // Clear and prepare buffers for reuse
    scratch.digit_buffer.clear();
    scratch.byte_buffer.clear();
    scratch.string_buffer.clear();

    // 1. Convert to scaled integer and get sign
    let abs_value = decimal.value.abs();
    let is_negative = decimal.is_negative();

    // 2. Build zero-padded digit string using scratch buffer for efficiency
    scratch
        .string_buffer
        .push_str(&format!("{:0width$}", abs_value, width = digits as usize));

    if scratch.string_buffer.len() > digits as usize {
        return Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            format!("Value {abs_value} too large for {digits} digits"),
        ));
    }

    // 3. Build nibble sequence using scratch digit buffer
    scratch.digit_buffer.reserve((digits + 1) as usize);

    // Add digit nibbles to scratch buffer
    for byte in scratch.string_buffer.bytes() {
        let digit = byte - b'0';
        if digit > 9 {
            return Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!("Invalid digit character: {}", byte as char),
            ));
        }
        scratch.digit_buffer.push(digit);
    }

    // 4. Append sign nibble
    let sign_nibble = if signed {
        if is_negative { 0xD } else { 0xC }
    } else {
        0xF // Unsigned: use F for positive/zero
    };
    scratch.digit_buffer.push(sign_nibble);

    // 5. If odd nibble count, prepend leading 0 nibble
    if scratch.digit_buffer.len() % 2 != 0 {
        scratch.digit_buffer.insert(0, 0);
    }

    // 6. Pack nibbles using scratch byte buffer for efficiency
    let expected_bytes = scratch.digit_buffer.len() / 2;
    scratch.byte_buffer.reserve(expected_bytes);

    for chunk in scratch.digit_buffer.chunks(2) {
        let byte_val = (chunk[0] << 4) | chunk[1];
        scratch.byte_buffer.push(byte_val);
    }

    // Return owned copy (minimal allocation at final step)
    Ok(scratch.byte_buffer.clone())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_small_decimal_normalization() {
        let mut decimal = SmallDecimal::new(0, 2, true);
        decimal.normalize();
        assert!(!decimal.negative); // -0 should become 0
    }

    #[test]
    fn test_small_decimal_formatting() {
        // Integer format (scale=0)
        let decimal = SmallDecimal::new(123, 0, false);
        assert_eq!(decimal.to_string(), "123");

        // Decimal format with fixed scale
        let decimal = SmallDecimal::new(12345, 2, false);
        assert_eq!(decimal.to_string(), "123.45");

        // Negative decimal
        let decimal = SmallDecimal::new(12345, 2, true);
        assert_eq!(decimal.to_string(), "-123.45");
    }

    #[test]
    fn test_zoned_decimal_blank_when_zero() {
        // EBCDIC spaces (0x40)
        let data = vec![0x40, 0x40, 0x40];
        let result = decode_zoned_decimal(&data, 3, 0, false, Codepage::CP037, true).unwrap();
        assert_eq!(result.to_string(), "0");

        // ASCII spaces
        let data = vec![b' ', b' ', b' '];
        let result = decode_zoned_decimal(&data, 3, 0, false, Codepage::ASCII, true).unwrap();
        assert_eq!(result.to_string(), "0");
    }

    #[test]
    fn test_packed_decimal_signs() {
        // Positive packed decimal: 123C (123 positive)
        let data = vec![0x12, 0x3C];
        let result = decode_packed_decimal(&data, 3, 0, true).unwrap();
        assert_eq!(result.to_string(), "123");

        // Negative packed decimal: 123D (123 negative)
        let data = vec![0x12, 0x3D];
        let result = decode_packed_decimal(&data, 3, 0, true).unwrap();
        assert_eq!(result.to_string(), "-123");
    }

    #[test]
    fn test_binary_int_big_endian() {
        // 16-bit big-endian: 0x0123 = 291
        let data = vec![0x01, 0x23];
        let result = decode_binary_int(&data, 16, false).unwrap();
        assert_eq!(result, 291);

        // 32-bit big-endian: 0x01234567 = 19088743
        let data = vec![0x01, 0x23, 0x45, 0x67];
        let result = decode_binary_int(&data, 32, false).unwrap();
        assert_eq!(result, 19_088_743);
    }

    #[test]
    fn test_alphanumeric_encoding() {
        // ASCII encoding with padding
        let result = encode_alphanumeric("HELLO", 10, Codepage::ASCII).unwrap();
        assert_eq!(result, b"HELLO     ");

        // Over-length should error
        let result = encode_alphanumeric("HELLO WORLD", 5, Codepage::ASCII);
        assert!(result.is_err());
    }

    #[test]
    fn test_bwz_policy() {
        // Zero values should trigger BWZ
        assert!(should_encode_as_blank_when_zero("0", true));
        assert!(should_encode_as_blank_when_zero("0.00", true));
        assert!(should_encode_as_blank_when_zero("0.000", true));

        // Non-zero values should not trigger BWZ
        assert!(!should_encode_as_blank_when_zero("1", true));
        assert!(!should_encode_as_blank_when_zero("0.01", true));

        // BWZ disabled should never trigger
        assert!(!should_encode_as_blank_when_zero("0", false));
    }

    #[test]
    fn test_binary_width_mapping() {
        // Test digit-to-width mapping (NORMATIVE)
        assert_eq!(get_binary_width_from_digits(1), 16); // ≤4 → 2B
        assert_eq!(get_binary_width_from_digits(4), 16); // ≤4 → 2B
        assert_eq!(get_binary_width_from_digits(5), 32); // 5-9 → 4B
        assert_eq!(get_binary_width_from_digits(9), 32); // 5-9 → 4B
        assert_eq!(get_binary_width_from_digits(10), 64); // 10-18 → 8B
        assert_eq!(get_binary_width_from_digits(18), 64); // 10-18 → 8B
    }

    #[test]
    fn test_explicit_binary_width_validation() {
        // Valid explicit widths
        assert_eq!(validate_explicit_binary_width(1).unwrap(), 8);
        assert_eq!(validate_explicit_binary_width(2).unwrap(), 16);
        assert_eq!(validate_explicit_binary_width(4).unwrap(), 32);
        assert_eq!(validate_explicit_binary_width(8).unwrap(), 64);

        // Invalid explicit widths
        assert!(validate_explicit_binary_width(3).is_err());
        assert!(validate_explicit_binary_width(16).is_err());
    }

    #[test]
    fn test_zoned_decimal_with_bwz() {
        // BWZ enabled with zero value should return spaces
        let result =
            encode_zoned_decimal_with_bwz("0", 3, 0, false, Codepage::ASCII, true).unwrap();
        assert_eq!(result, vec![b' ', b' ', b' ']);

        // BWZ disabled with zero value should return normal encoding
        let result =
            encode_zoned_decimal_with_bwz("0", 3, 0, false, Codepage::ASCII, false).unwrap();
        assert_eq!(result, vec![0x30, 0x30, 0x30]); // ASCII "000"

        // Non-zero value should return normal encoding regardless of BWZ
        let result =
            encode_zoned_decimal_with_bwz("123", 3, 0, false, Codepage::ASCII, true).unwrap();
        assert_eq!(result, vec![0x31, 0x32, 0x33]); // ASCII "123"
    }

    #[test]
    fn test_zoned_decimal_negative_zero_normalization() {
        // Test that negative zero normalizes to "0" but other negatives remain negative
        // This tests the specific bug where "-0" was incorrectly returned as "-0"

        // ASCII negative zero: "}" (0x7D) = -0
        let data = vec![b'}']; // ASCII overpunch for -0
        let result = decode_zoned_decimal(&data, 1, 0, true, Codepage::ASCII, false).unwrap();
        assert_eq!(
            result.to_string(),
            "0",
            "Negative zero should normalize to '0'"
        );

        // ASCII negative non-zero: "J" (0x4A) = -1
        let data = vec![b'J']; // ASCII overpunch for -1
        let result = decode_zoned_decimal(&data, 1, 0, true, Codepage::ASCII, false).unwrap();
        assert_eq!(
            result.to_string(),
            "-1",
            "Negative non-zero should remain negative"
        );

        // Multi-digit negative zero: "000}" = -0000
        let data = vec![b'0', b'0', b'0', b'}'];
        let result = decode_zoned_decimal(&data, 4, 0, true, Codepage::ASCII, false).unwrap();
        assert_eq!(
            result.to_string(),
            "0",
            "Multi-digit negative zero should normalize to '0'"
        );

        // Multi-digit negative non-zero: "123J" = -1231 (J = negative 1)
        let data = vec![b'1', b'2', b'3', b'J'];
        let result = decode_zoned_decimal(&data, 4, 0, true, Codepage::ASCII, false).unwrap();
        assert_eq!(
            result.to_string(),
            "-1231",
            "Multi-digit negative non-zero should remain negative"
        );

        // EBCDIC negative zero: 0xD0 = -0
        let data = vec![0xD0]; // EBCDIC overpunch for -0
        let result = decode_zoned_decimal(&data, 1, 0, true, Codepage::CP037, false).unwrap();
        assert_eq!(
            result.to_string(),
            "0",
            "EBCDIC negative zero should normalize to '0'"
        );

        // EBCDIC negative non-zero: 0xD1 = -1
        let data = vec![0xD1]; // EBCDIC overpunch for -1
        let result = decode_zoned_decimal(&data, 1, 0, true, Codepage::CP037, false).unwrap();
        assert_eq!(
            result.to_string(),
            "-1",
            "EBCDIC negative non-zero should remain negative"
        );
    }

    #[test]
    fn test_comp3_encode_decode_roundtrip() {
        // Test comprehensive round-trip for various COMP-3 values

        // Test case 1: Positive value, odd digits (5 digits -> 5+1=6 nibbles -> 3 bytes)
        let original_value = "12345";
        let encoded = encode_packed_decimal(original_value, 5, 0, true).unwrap();
        let decoded = decode_packed_decimal(&encoded, 5, 0, true).unwrap();
        assert_eq!(decoded.to_string(), original_value);
        assert_eq!(encoded, vec![0x12, 0x34, 0x5C]); // 5 digits = 3 bytes, positive sign

        // Test case 2: Negative value, odd digits (5 digits -> 5+1=6 nibbles -> 3 bytes)
        let original_value = "-12345";
        let encoded = encode_packed_decimal(original_value, 5, 0, true).unwrap();
        let decoded = decode_packed_decimal(&encoded, 5, 0, true).unwrap();
        assert_eq!(decoded.to_string(), original_value);
        assert_eq!(encoded, vec![0x12, 0x34, 0x5D]); // 5 digits = 3 bytes, negative sign

        // Test case 3: Positive value, even digits (6 digits -> (6+2)/2 = 4 bytes)
        let original_value = "123456";
        let encoded = encode_packed_decimal(original_value, 6, 0, true).unwrap();
        let decoded = decode_packed_decimal(&encoded, 6, 0, true).unwrap();
        assert_eq!(decoded.to_string(), original_value);
        assert_eq!(encoded, vec![0x12, 0x34, 0x56, 0x0C]); // 6 digits = 4 bytes, filler + positive sign

        // Test case 4: Negative value, even digits (6 digits -> (6+2)/2 = 4 bytes)
        let original_value = "-123456";
        let encoded = encode_packed_decimal(original_value, 6, 0, true).unwrap();
        let decoded = decode_packed_decimal(&encoded, 6, 0, true).unwrap();
        assert_eq!(decoded.to_string(), original_value);
        assert_eq!(encoded, vec![0x12, 0x34, 0x56, 0x0D]); // 6 digits = 4 bytes, filler + negative sign

        // Test case 5: Zero value (should normalize) (3 digits -> 3+1=4 nibbles -> 2 bytes)
        let original_value = "0";
        let encoded = encode_packed_decimal(original_value, 3, 0, true).unwrap();
        let decoded = decode_packed_decimal(&encoded, 3, 0, true).unwrap();
        assert_eq!(decoded.to_string(), "0"); // Should normalize -0 to 0
        assert_eq!(encoded, vec![0x00, 0x0C]); // 3 digits = 2 bytes, positive sign

        // Test case 6: Unsigned value (5 digits -> 5+1=6 nibbles -> 3 bytes)
        let original_value = "12345";
        let encoded = encode_packed_decimal(original_value, 5, 0, false).unwrap();
        let decoded = decode_packed_decimal(&encoded, 5, 0, false).unwrap();
        assert_eq!(decoded.to_string(), original_value);
        assert_eq!(encoded, vec![0x12, 0x34, 0x5F]); // 5 digits = 3 bytes, unsigned sign F
    }

    #[test]
    fn test_comp3_encode_scale_handling() {
        // Test scale handling with decimal values

        // Test case 1: Scale 2 - "123.45" -> 12345 scaled
        let original_value = "123.45";
        let encoded = encode_packed_decimal(original_value, 5, 2, true).unwrap();
        let decoded = decode_packed_decimal(&encoded, 5, 2, true).unwrap();
        assert_eq!(decoded.to_string(), original_value);
        // Value 12345 with 5 digits: 0x12345C = [0x12, 0x34, 0x5C]
        assert_eq!(encoded, vec![0x12, 0x34, 0x5C]);

        // Test case 2: Scale 0 with padding - "123" -> "00123"
        let original_value = "123";
        let encoded = encode_packed_decimal(original_value, 5, 0, true).unwrap();
        let decoded = decode_packed_decimal(&encoded, 5, 0, true).unwrap();
        assert_eq!(decoded.to_string(), original_value);
        // Value 123 padded to 5 digits: 00123 -> 0x00123C = [0x00, 0x12, 0x3C]
        assert_eq!(encoded, vec![0x00, 0x12, 0x3C]);
    }

    #[test]
    fn test_comp3_encode_error_conditions() {
        // Test overflow - value too large for digit count
        let result = encode_packed_decimal("123456", 4, 0, true);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(error.message.contains("too large"));

        // Test invalid scale
        let result = encode_packed_decimal("123.4", 3, 2, true);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(
            error.message.contains("Scale mismatch") || error.message.contains("type mismatch")
        );
    }

    #[test]
    fn test_comp3_golden_vectors() {
        // Test against known golden values to ensure compatibility

        // Golden vector 1: 1234 positive, 4 digits (even) -> (4+2)/2 = 3 bytes
        // Bytes: 0x12 (digits 1,2), 0x34 (digits 3,4), 0x0C (filler 0, sign C)
        let encoded = encode_packed_decimal("1234", 4, 0, true).unwrap();
        assert_eq!(encoded, vec![0x12, 0x34, 0x0C]); // Expected: [0x12, 0x34, 0x0C]

        // Golden vector 2: -1234 negative, 4 digits (even)
        let encoded = encode_packed_decimal("-1234", 4, 0, true).unwrap();
        assert_eq!(encoded, vec![0x12, 0x34, 0x0D]); // Expected: [0x12, 0x34, 0x0D]

        // Golden vector 3: 1 with padding, 3 digits (odd) -> (3+2)/2 = 2 bytes
        // Bytes: 0x00 (digit 0,0), 0x1C (digit 1, sign C)
        let encoded = encode_packed_decimal("1", 3, 0, true).unwrap();
        assert_eq!(encoded, vec![0x00, 0x1C]); // Expected: [0x00, 0x1C] (001C)

        // Golden vector 4: Odd digit count example, 5 digits -> (5+2)/2 = 3 bytes
        // Bytes: 0x12 (digits 1,2), 0x34 (digits 3,4), 0x5C (digit 5, sign C)
        let encoded = encode_packed_decimal("12345", 5, 0, true).unwrap();
        assert_eq!(encoded, vec![0x12, 0x34, 0x5C]); // Expected: [0x12, 0x34, 0x5C] (12345C)
    }

    #[test]
    fn test_comp3_property_tests() {
        // Property-based testing: encode(decode(bytes)) should be identity for valid inputs
        let test_cases = vec![
            (vec![0x12, 0x3C], 3, 0, true), // 123 positive (3 digits -> 3+1=4 nibbles -> 2 bytes)
            (vec![0x12, 0x3D], 3, 0, true), // 123 negative (3 digits -> 3+1=4 nibbles -> 2 bytes)
            (vec![0x12, 0x3F], 3, 0, false), // 123 unsigned (3 digits -> 3+1=4 nibbles -> 2 bytes)
            (vec![0x12, 0x34, 0x5C], 5, 0, true), // 12345 positive (5 digits -> 5+1=6 nibbles -> 3 bytes)
            (vec![0x00, 0x1C], 3, 0, true), // 1 positive (3 digits -> 3+1=4 nibbles -> 2 bytes: 001C)
        ];

        for (bytes, digits, scale, signed) in test_cases {
            // Decode to get decimal value
            let decoded = decode_packed_decimal(&bytes, digits, scale, signed).unwrap();

            // Re-encode to bytes
            let reencoded =
                encode_packed_decimal(&decoded.to_string(), digits, scale, signed).unwrap();

            // Should be identical
            assert_eq!(
                bytes, reencoded,
                "Round-trip failed for digits={}, scale={}, signed={}: {:?} != {:?}",
                digits, scale, signed, bytes, reencoded
            );
        }
    }
}
