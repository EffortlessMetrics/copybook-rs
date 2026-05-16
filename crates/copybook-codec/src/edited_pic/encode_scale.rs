// SPDX-License-Identifier: AGPL-3.0-or-later
//! Input parsing and digit scaling for edited PIC encoding.
//!
//! Parses a decimal numeric string into a `ParsedNumeric`, then adjusts its
//! digit vector to match the target scale (the number of fractional digits
//! required by the pattern).

use copybook_core::{Error, ErrorCode, Result};

use super::Sign;

/// Parsed numeric value extracted from the input string.
#[derive(Debug, Clone)]
pub(super) struct ParsedNumeric {
    /// Sign of the number.
    pub sign: Sign,
    /// All digits without decimal point (e.g. `[1,2,3,4,5]` for `123.45`).
    pub digits: Vec<u8>,
    /// Position of decimal point counted from the right.
    pub decimal_places: usize,
}

/// Parse a numeric string into its components for encoding.
pub(super) fn parse(value: &str) -> Result<ParsedNumeric> {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return Err(Error::new(
            ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
            "Empty numeric value",
        ));
    }

    let mut chars = trimmed.chars().peekable();
    let sign = if chars.peek() == Some(&'-') {
        chars.next();
        Sign::Negative
    } else if chars.peek() == Some(&'+') {
        chars.next();
        Sign::Positive
    } else {
        Sign::Positive
    };

    let mut digits = Vec::new();
    let mut found_decimal = false;
    let mut decimal_places = 0;
    let mut found_digit = false;

    for ch in chars {
        if ch.is_ascii_digit() {
            digits.push(ch as u8 - b'0');
            if found_decimal {
                decimal_places += 1;
            }
            found_digit = true;
        } else if ch == '.' {
            if found_decimal {
                return Err(Error::new(
                    ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                    format!("Multiple decimal points in value: {value}"),
                ));
            }
            found_decimal = true;
        } else {
            return Err(Error::new(
                ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                format!("Invalid character '{ch}' in numeric value: {value}"),
            ));
        }
    }

    if !found_digit {
        return Err(Error::new(
            ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
            format!("No digits found in value: {value}"),
        ));
    }

    Ok(ParsedNumeric {
        sign,
        digits,
        decimal_places,
    })
}

/// Pad with trailing zeros or truncate so the digit vector has exactly
/// `target_scale` fractional digits.
pub(super) fn adjust_to_scale(
    mut digits: Vec<u8>,
    source_scale: usize,
    target_scale: usize,
) -> Vec<u8> {
    if target_scale > source_scale {
        digits.extend(std::iter::repeat_n(0, target_scale - source_scale));
    } else if target_scale < source_scale {
        let to_remove = source_scale - target_scale;
        digits.truncate(digits.len().saturating_sub(to_remove));
    }
    digits
}

/// True if every digit in the slice is zero.
pub(super) fn is_all_zero(digits: &[u8]) -> bool {
    digits.iter().all(|&d| d == 0)
}

/// Determine the effective sign: an all-zero magnitude is always positive.
pub(super) fn effective_sign(is_zero: bool, parsed_sign: Sign) -> Sign {
    if is_zero { Sign::Positive } else { parsed_sign }
}
