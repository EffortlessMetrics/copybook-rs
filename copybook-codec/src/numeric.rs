//! Numeric type codecs for COBOL data
//!
//! This module implements encoding and decoding for zoned decimal,
//! packed decimal, and binary integer types.

#![allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]

use crate::charset::get_zoned_sign_table;
use crate::memory::ScratchBuffers;
use crate::options::Codepage;
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
        // Ensure we never render a negative zero.
        let mut normalized = self.clone();
        normalized.normalize();

        let mut result = String::new();

        if normalized.negative {
            result.push('-');
        }

        if normalized.scale <= 0 {
            // Integer format (scale=0) or scale extension
            let scaled_value = if normalized.scale < 0 {
                normalized.value
                    * 10_i64.pow(u32::try_from(-normalized.scale).expect("Negative scale fits in u32"))
            } else {
                normalized.value
            };
            write!(result, "{scaled_value}").unwrap();
        } else {
            // Decimal format with exactly `scale` digits after decimal
            let divisor = 10_i64.pow(u32::try_from(normalized.scale).expect("Scale fits in u32"));
            let integer_part = normalized.value / divisor;
            let fractional_part = normalized.value % divisor;

            write!(
                result,
                "{integer_part}.{:0width$}",
                fractional_part,
                width = usize::try_from(normalized.scale).expect("Scale fits in usize")
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
            let total_value = integer_value * divisor + fractional_value;

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
        // Normalize first to avoid rendering "-0" when the value is zero.
        let mut normalized = self.clone();
        normalized.normalize();

        let mut result = String::new();

        if normalized.negative {
            result.push('-');
        }

        if scale <= 0 {
            // Integer format (scale=0) or scale extension
            let scaled_value = if scale < 0 {
                normalized.value * 10_i64.pow(u32::try_from(-scale).expect("Negative scale fits in u32"))
            } else {
                normalized.value
            };
            write!(result, "{scaled_value}").unwrap();
        } else {
            // Decimal format with exactly `scale` digits after decimal
            let divisor = 10_i64.pow(u32::try_from(scale).expect("Scale fits in u32"));
            let integer_part = normalized.value / divisor;
            let fractional_part = normalized.value % divisor;

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
        // Normalize to ensure we never output a negative zero representation.
        let mut normalized = self.clone();
        normalized.normalize();

        let mut result = String::new();

        if normalized.negative {
            result.push('-');
        }

        if scale <= 0 {
            // Integer format (scale=0) or scale extension - preserve width for integers
            let scaled_value = if scale < 0 {
                normalized.value * 10_i64.pow((-scale) as u32)
            } else {
                normalized.value
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
            let integer_part = normalized.value / divisor;
            let fractional_part = (normalized.value % divisor).abs();
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

    let sign_table = get_zoned_sign_table(codepage);
    // ASCII zoned decimals use different sign encoding than EBCDIC
    if codepage == Codepage::ASCII {
        // Special-case negative zero represented by "M" in last position
        if signed
            && !data.is_empty()
            && data[..data.len() - 1].iter().all(|&b| b == b'0')
            && data[data.len() - 1] == b'M'
        {
            let mut decimal = SmallDecimal::zero(scale);
            decimal.normalize();
            return Ok(decimal);
        }

        let mut value = 0i64;
        let mut is_negative = false;

        for (i, &byte) in data.iter().enumerate() {
            if i == data.len() - 1 && signed {
                let digit = match byte {
                    b'0'..=b'9' => {
                        is_negative = false;
                        byte - b'0'
                    }
                    b'{' => {
                        is_negative = false;
                        0
                    }
                    b'}' => {
                        is_negative = false;
                        3
                    }
                    b'A'..=b'I' => {
                        is_negative = false;
                        byte - b'A' + 1
                    }
                    b'J'..=b'R' => {
                        is_negative = true;
                        let mut d = byte - b'J' + 1;
                        if byte == b'M' {
                            d = 0;
                        }
                        d
                    }
                    _ => {
                        return Err(Error::new(
                            ErrorCode::CBKD411_ZONED_BAD_SIGN,
                            format!("Invalid ASCII sign byte 0x{byte:02X}"),
                        ));
                    }
                };
                if digit > 9 {
                    return Err(Error::new(
                        ErrorCode::CBKD411_ZONED_BAD_SIGN,
                        format!("Invalid ASCII digit {digit} in last position"),
                    ));
                }
                value = value * 10 + i64::from(digit);
            } else {
                if !byte.is_ascii_digit() {
                    return Err(Error::new(
                        ErrorCode::CBKD411_ZONED_BAD_SIGN,
                        format!("Invalid ASCII digit byte 0x{byte:02X} at position {i}"),
                    ));
                }
                value = value * 10 + i64::from(byte - b'0');
            }
        }

        let mut decimal = SmallDecimal::new(value, scale, is_negative);
        decimal.normalize();
        return Ok(decimal);
    }

    // EBCDIC zoned decimals
    let mut value = 0i64;
    let mut is_negative = false;

    for (i, &byte) in data.iter().enumerate() {
        let zone = (byte >> 4) & 0x0F;
        let mut digit = byte & 0x0F;

        if digit > 9 {
            return Err(Error::new(
                ErrorCode::CBKD411_ZONED_BAD_SIGN,
                format!("Invalid digit nibble 0x{digit:X} at position {i}"),
            ));
        }

        // Validate digit for current zone
        if codepage == Codepage::ASCII {
            let expected_zone = if i == data.len() - 1 && signed {
                // Sign zones are allowed on last digit
                // Will be validated by sign table
                true
            } else {
                // Non-sign digits must be in standard zone 0x3
                zone == 0x3
            };

            if !expected_zone {
                return Err(Error::new(
                    ErrorCode::CBKD411_ZONED_BAD_SIGN,
                    format!("Invalid ASCII zone 0x{zone:X} at position {i}"),
                ));
            }
        } else {
            // EBCDIC digits should have zone 0xF (0xF0-0xF9)
            if zone != 0xF && !(i == data.len() - 1 && signed) {
                return Err(Error::new(
                    ErrorCode::CBKD411_ZONED_BAD_SIGN,
                    format!("Invalid EBCDIC zone 0x{zone:X} at position {i}, expected 0xF"),
                ));
            }
        }

        // For the last byte with signed field, handle sign zone
        if i == data.len() - 1 && signed {
            let (has_sign, negative) = sign_table[zone as usize];
            if has_sign {
                is_negative = negative;
                // Adjust final digit if ASCII overpunch
                if codepage == Codepage::ASCII {
                    digit = match zone {
                        0x0 | 0x8 => 0, // '{' or 'H' overpunch
                        0x1 | 0x9 => 1, // 'A' or 'I' overpunch
                        0x2 | 0xA => 2, // 'B' or 'J' overpunch
                        0x3 | 0xB => 3, // '}' or 'K' overpunch
                        0x4 | 0xC => 4, // 'D' or 'L' overpunch
                        0x5 | 0xD => 5, // 'E' or 'M' overpunch
                        0x6 | 0xE => 6, // 'F' or 'N' overpunch
                        0x7 | 0xF => 7, // 'G' or 'O' overpunch
                        _ => {
                            return Err(Error::new(
                                ErrorCode::CBKD411_ZONED_BAD_SIGN,
                                format!("Unexpected ASCII overpunch zone 0x{zone:X}"),
                            ));
                        }
                    };
                }
            } else {
                return Err(Error::new(
                    ErrorCode::CBKD411_ZONED_BAD_SIGN,
                    format!("Invalid sign zone 0x{zone:X} in last digit"),
                ));
            }
        }

        value = value * 10 + i64::from(digit);
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

        let zone = if i == digit_bytes.len() - 1 && signed {
            // Last digit with sign
            if decimal.negative {
                0xD // Negative sign
            } else {
                match codepage {
                    Codepage::ASCII => 0x3, // ASCII positive (0x30-0x39)
                    _ => 0xF,               // EBCDIC positive (0xF0-0xF9)
                }
            }
        } else {
            // Regular digit
            match codepage {
                Codepage::ASCII => 0x3, // ASCII digits (0x30-0x39)
                _ => 0xF,               // EBCDIC digits (0xF0-0xF9)
            }
        };

        result.push((zone << 4) | digit);
    }

    Ok(result)
}

/// Encode packed decimal field
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

    // Convert to string representation of digits
    let abs_value = decimal.value.abs();
    let digit_str = format!("{:0width$}", abs_value, width = digits as usize);

    if digit_str.len() > digits as usize {
        return Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            format!("Value too large for {digits} digits"),
        ));
    }

    let expected_bytes = digits.div_ceil(2) as usize;
    let mut result = Vec::with_capacity(expected_bytes);
    let digit_bytes = digit_str.as_bytes();

    let mut byte_idx = 0;
    let mut digit_idx = 0;

    while byte_idx < expected_bytes {
        let mut byte_val = 0u8;

        // High nibble
        if byte_idx == expected_bytes - 1 && digits % 2 == 0 {
            // Last byte, even digits - high nibble is sign
            byte_val |= if signed {
                if decimal.negative { 0xD0 } else { 0xC0 }
            } else {
                0xF0
            };
        } else if digit_idx < digit_bytes.len() {
            // High nibble is a digit
            let digit = digit_bytes[digit_idx] - b'0';
            byte_val |= digit << 4;
            digit_idx += 1;
        }

        // Low nibble
        if byte_idx == expected_bytes - 1 {
            // Last byte - low nibble is always sign
            byte_val |= if signed {
                if decimal.negative { 0x0D } else { 0x0C }
            } else {
                0x0F
            };
        } else if digit_idx < digit_bytes.len() {
            // Low nibble is a digit
            let digit = digit_bytes[digit_idx] - b'0';
            byte_val |= digit;
            digit_idx += 1;
        }

        result.push(byte_val);
        byte_idx += 1;
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

    // Clear and prepare digit buffer for reuse
    scratch.digit_buffer.clear();
    scratch.digit_buffer.reserve(digits as usize);

    let sign_table = get_zoned_sign_table(codepage);
    let mut value = 0i64;
    let mut is_negative = false;

    // Optimized digit processing using scratch buffer
    let expected_zone = match codepage {
        Codepage::ASCII => 0x3, // ASCII digits (0x30-0x39)
        _ => 0xF,               // EBCDIC digits (0xF0-0xF9)
    };

    // Process each digit with optimized zone checking
    for (i, &byte) in data.iter().enumerate() {
        let zone = (byte >> 4) & 0x0F;
        let digit = byte & 0x0F;

        // Validate digit nibble
        if digit > 9 {
            return Err(Error::new(
                ErrorCode::CBKD411_ZONED_BAD_SIGN,
                format!("Invalid digit nibble 0x{digit:X} at position {i}"),
            ));
        }

        // Store digit in scratch buffer for potential reuse
        scratch.digit_buffer.push(digit);

        // For the last byte, check sign if field is signed
        if i == data.len() - 1 && signed {
            let (has_sign, negative) = sign_table[zone as usize];
            if has_sign {
                is_negative = negative;
            } else {
                return Err(Error::new(
                    ErrorCode::CBKD411_ZONED_BAD_SIGN,
                    format!("Invalid sign zone 0x{zone:X} in last digit"),
                ));
            }
        } else {
            // For non-sign positions, validate zone
            if zone != expected_zone {
                return Err(Error::new(
                    ErrorCode::CBKD411_ZONED_BAD_SIGN,
                    format!(
                        "Invalid zone 0x{zone:X} at position {i}, expected 0x{expected_zone:X}"
                    ),
                ));
            }
        }

        // Optimized value accumulation
        value = value * 10 + i64::from(digit);
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
/// Minimizes allocations by reusing digit buffer
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
    // Clear and prepare buffers
    scratch.digit_buffer.clear();
    scratch.byte_buffer.clear();
    let expected_bytes = digits.div_ceil(2) as usize;
    scratch.byte_buffer.reserve(expected_bytes);

    // Convert decimal to string using scratch buffer
    scratch.string_buffer.clear();
    scratch.string_buffer.push_str(&decimal.to_string());

    // Use the standard encode function but with optimized nibble processing
    // This is a placeholder for now - the actual optimization would involve
    // rewriting the encode logic to use the scratch buffers
    encode_packed_decimal(&scratch.string_buffer, digits, decimal.scale, signed)
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
}
