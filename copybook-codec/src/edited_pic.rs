//! Edited PIC (Phase E2) decode support
//!
//! This module implements decode for edited numeric PICTURE clauses following IBM COBOL specifications.
//! Edited PICs include formatting symbols like Z (zero suppression), $ (currency), comma, decimal point,
//! and sign editing (+, -, CR, DB).
//!
//! The decode algorithm walks the input string and PIC pattern in lockstep, extracting numeric digits
//! and validating formatting symbols.

use copybook_core::{Error, ErrorCode, Result};

/// Pattern tokens for edited PIC clauses
#[derive(Debug, Clone, PartialEq)]
pub enum PicToken {
    /// Numeric digit (9) - always displays
    Digit,
    /// Zero suppression (Z) - displays space if leading zero
    ZeroSuppress,
    /// Zero insert (0) - always displays '0'
    ZeroInsert,
    /// Asterisk fill (*) - displays '*' for leading zeros
    AsteriskFill,
    /// Blank space (B)
    Space,
    /// Literal comma
    Comma,
    /// Literal slash
    Slash,
    /// Decimal point
    DecimalPoint,
    /// Currency symbol ($)
    Currency,
    /// Leading plus sign
    LeadingPlus,
    /// Leading minus sign
    LeadingMinus,
    /// Trailing plus sign
    TrailingPlus,
    /// Trailing minus sign
    TrailingMinus,
    /// Credit (CR) - two characters
    Credit,
    /// Debit (DB) - two characters
    Debit,
}

impl PicToken {
    /// Returns true if this token represents a numeric position
    #[inline]
    #[allow(dead_code)] // May be used in future optimizations
    const fn is_numeric(&self) -> bool {
        matches!(
            self,
            Self::Digit | Self::ZeroSuppress | Self::ZeroInsert | Self::AsteriskFill
        )
    }

    /// Returns true if this token is a sign editing symbol
    #[inline]
    #[allow(dead_code)] // May be used in future optimizations
    const fn is_sign(&self) -> bool {
        matches!(
            self,
            Self::LeadingPlus
                | Self::LeadingMinus
                | Self::TrailingPlus
                | Self::TrailingMinus
                | Self::Credit
                | Self::Debit
        )
    }
}

/// Sign extracted from edited PIC
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Sign {
    /// Positive or unsigned
    Positive,
    /// Negative
    Negative,
}

/// Tokenize an edited PIC pattern into tokens
///
/// # Errors
/// Returns error if the PIC pattern is malformed
pub fn tokenize_edited_pic(pic_str: &str) -> Result<Vec<PicToken>> {
    let mut tokens = Vec::new();
    let mut chars = pic_str.chars().peekable();
    let mut found_decimal = false;

    // Skip leading 'S' if present (sign is handled separately via sign editing symbols)
    if chars.peek() == Some(&'S') || chars.peek() == Some(&'s') {
        chars.next();
    }

    while let Some(ch) = chars.next() {
        match ch.to_ascii_uppercase() {
            '9' => {
                let count = parse_repetition(&mut chars)?;
                for _ in 0..count {
                    tokens.push(PicToken::Digit);
                }
            }
            'Z' => {
                let count = parse_repetition(&mut chars)?;
                for _ in 0..count {
                    tokens.push(PicToken::ZeroSuppress);
                }
            }
            '0' => {
                let count = parse_repetition(&mut chars)?;
                for _ in 0..count {
                    tokens.push(PicToken::ZeroInsert);
                }
            }
            '*' => {
                let count = parse_repetition(&mut chars)?;
                for _ in 0..count {
                    tokens.push(PicToken::AsteriskFill);
                }
            }
            'B' => {
                let count = parse_repetition(&mut chars)?;
                for _ in 0..count {
                    tokens.push(PicToken::Space);
                }
            }
            ',' => tokens.push(PicToken::Comma),
            '/' => tokens.push(PicToken::Slash),
            '.' => {
                if found_decimal {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        format!("Multiple decimal points in edited PIC: {pic_str}"),
                    ));
                }
                found_decimal = true;
                tokens.push(PicToken::DecimalPoint);
            }
            '$' => tokens.push(PicToken::Currency),
            '+' => {
                // Check if at beginning (leading) or end (trailing)
                if tokens.is_empty() {
                    tokens.push(PicToken::LeadingPlus);
                } else {
                    tokens.push(PicToken::TrailingPlus);
                }
            }
            '-' => {
                // Check if at beginning (leading) or end (trailing)
                if tokens.is_empty() {
                    tokens.push(PicToken::LeadingMinus);
                } else {
                    tokens.push(PicToken::TrailingMinus);
                }
            }
            'C' => {
                // Check for CR
                if let Some(&next_ch) = chars.peek()
                    && (next_ch == 'R' || next_ch == 'r')
                {
                    chars.next(); // consume 'R'
                    tokens.push(PicToken::Credit);
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        format!("Invalid character 'C' in edited PIC: {pic_str}"),
                    ));
                }
            }
            'D' => {
                // Check for DB
                if let Some(&next_ch) = chars.peek()
                    && (next_ch == 'B' || next_ch == 'b')
                {
                    chars.next(); // consume 'B'
                    tokens.push(PicToken::Debit);
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        format!("Invalid character 'D' in edited PIC: {pic_str}"),
                    ));
                }
            }
            'V' => {
                // V is implicit decimal point - don't add to tokens (doesn't affect display)
            }
            ' ' | '\t' => {
                // Skip whitespace
            }
            _ => {
                // Unknown character - skip for now (could be part of SIGN clause, etc.)
            }
        }
    }

    if tokens.is_empty() {
        return Err(Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!("Empty or invalid edited PIC pattern: {pic_str}"),
        ));
    }

    Ok(tokens)
}

/// Parse repetition count from chars like (5)
fn parse_repetition<I>(chars: &mut std::iter::Peekable<I>) -> Result<usize>
where
    I: Iterator<Item = char>,
{
    if chars.peek() == Some(&'(') {
        chars.next(); // consume '('
        let mut count_str = String::new();
        while let Some(&ch) = chars.peek() {
            if ch == ')' {
                chars.next(); // consume ')'
                break;
            } else if ch.is_ascii_digit() {
                count_str.push(ch);
                chars.next();
            } else {
                return Err(Error::new(
                    ErrorCode::CBKP001_SYNTAX,
                    format!("Invalid repetition count: {count_str}"),
                ));
            }
        }
        count_str.parse::<usize>().map_err(|_| {
            Error::new(
                ErrorCode::CBKP001_SYNTAX,
                format!("Invalid repetition count: {count_str}"),
            )
        })
    } else {
        Ok(1)
    }
}

/// Decoded numeric value from edited PIC
#[derive(Debug, Clone, PartialEq)]
pub struct NumericValue {
    /// Sign of the number
    pub sign: Sign,
    /// Digits without decimal point (e.g., "12345" for 123.45 with scale=2)
    pub digits: String,
    /// Number of decimal places
    pub scale: u16,
}

impl NumericValue {
    /// Format as decimal string for JSON output
    #[must_use]
    pub fn to_decimal_string(&self) -> String {
        if self.digits.is_empty() || self.digits.chars().all(|c| c == '0') {
            return "0".to_string();
        }

        let sign_prefix = match self.sign {
            Sign::Positive => "",
            Sign::Negative => "-",
        };

        if self.scale == 0 {
            // Integer - just return digits with sign
            format!("{sign_prefix}{}", self.digits)
        } else {
            // Decimal - insert decimal point
            let scale = self.scale as usize;
            let digits_len = self.digits.len();

            if scale >= digits_len {
                // Need leading zeros (e.g., 0.0123)
                let zeros = "0".repeat(scale - digits_len);
                format!("{sign_prefix}0.{zeros}{}", self.digits)
            } else {
                // Split at decimal point
                let (int_part, frac_part) = self.digits.split_at(digits_len - scale);
                if int_part.is_empty() {
                    format!("{sign_prefix}0.{frac_part}")
                } else {
                    format!("{sign_prefix}{int_part}.{frac_part}")
                }
            }
        }
    }
}

/// Decode edited numeric string according to PICTURE pattern
///
/// # Errors
/// Returns error if the input doesn't match the pattern
pub fn decode_edited_numeric(
    input: &str,
    pattern: &[PicToken],
    scale: u16,
    blank_when_zero: bool,
) -> Result<NumericValue> {
    // Check for BLANK WHEN ZERO
    if blank_when_zero && input.chars().all(|c| c == ' ') {
        return Ok(NumericValue {
            sign: Sign::Positive,
            digits: "0".to_string(),
            scale,
        });
    }

    let input_chars: Vec<char> = input.chars().collect();
    let mut pattern_idx = 0;
    let mut input_idx = 0;
    let mut digits = String::new();
    let mut sign = Sign::Positive;
    let mut found_non_zero = false;

    // Extract sign from pattern and input
    while pattern_idx < pattern.len() {
        let token = &pattern[pattern_idx];

        if input_idx >= input_chars.len() {
            return Err(Error::new(
                ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                format!(
                    "Input too short for edited PIC pattern (expected {} characters, got {})",
                    pattern.len(),
                    input.len()
                ),
            ));
        }

        let input_char = input_chars[input_idx];

        match token {
            PicToken::Digit
            | PicToken::ZeroSuppress
            | PicToken::ZeroInsert
            | PicToken::AsteriskFill => {
                // Expect digit or space or asterisk
                if input_char.is_ascii_digit() {
                    let digit_val = input_char;
                    if digit_val != '0' {
                        found_non_zero = true;
                    }
                    if found_non_zero || matches!(token, PicToken::Digit | PicToken::ZeroInsert) {
                        digits.push(digit_val);
                    } else {
                        // Leading zero suppression - push 0 to maintain position
                        digits.push('0');
                    }
                } else if input_char == ' ' {
                    // Space for zero suppression
                    if matches!(token, PicToken::Digit) {
                        // Required digit position cannot be space
                        return Err(Error::new(
                            ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                            format!("Expected digit but found space at position {input_idx}"),
                        ));
                    }
                    digits.push('0');
                } else if input_char == '*' {
                    // Asterisk fill for check protection
                    if !matches!(token, PicToken::AsteriskFill) {
                        return Err(Error::new(
                            ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                            format!("Unexpected asterisk at position {input_idx}"),
                        ));
                    }
                    digits.push('0');
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                        format!(
                            "Expected digit, space, or asterisk but found '{}' at position {}",
                            input_char, input_idx
                        ),
                    ));
                }
                input_idx += 1;
            }
            PicToken::Space => {
                if input_char != ' ' && input_char != 'B' {
                    return Err(Error::new(
                        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                        format!("Expected space but found '{input_char}' at position {input_idx}"),
                    ));
                }
                input_idx += 1;
            }
            PicToken::Comma => {
                if input_char != ',' {
                    return Err(Error::new(
                        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                        format!("Expected comma but found '{input_char}' at position {input_idx}"),
                    ));
                }
                input_idx += 1;
            }
            PicToken::Slash => {
                if input_char != '/' {
                    return Err(Error::new(
                        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                        format!("Expected slash but found '{input_char}' at position {input_idx}"),
                    ));
                }
                input_idx += 1;
            }
            PicToken::DecimalPoint => {
                if input_char != '.' {
                    return Err(Error::new(
                        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                        format!(
                            "Expected decimal point but found '{input_char}' at position {input_idx}"
                        ),
                    ));
                }
                input_idx += 1;
            }
            PicToken::Currency => {
                if input_char != '$' && input_char != ' ' {
                    return Err(Error::new(
                        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                        format!(
                            "Expected currency symbol but found '{input_char}' at position {input_idx}"
                        ),
                    ));
                }
                input_idx += 1;
            }
            PicToken::LeadingPlus => {
                if input_char == '+' {
                    sign = Sign::Positive;
                } else if input_char == ' ' {
                    sign = Sign::Positive;
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
                        format!("Expected '+' for leading plus but found '{input_char}'"),
                    ));
                }
                input_idx += 1;
            }
            PicToken::LeadingMinus => {
                if input_char == '-' {
                    sign = Sign::Negative;
                } else if input_char == ' ' {
                    sign = Sign::Positive;
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
                        format!("Expected '-' for leading minus but found '{input_char}'"),
                    ));
                }
                input_idx += 1;
            }
            PicToken::TrailingPlus => {
                if input_char == '+' {
                    sign = Sign::Positive;
                } else if input_char == ' ' {
                    sign = Sign::Positive;
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
                        format!("Expected '+' for trailing plus but found '{input_char}'"),
                    ));
                }
                input_idx += 1;
            }
            PicToken::TrailingMinus => {
                if input_char == '-' {
                    sign = Sign::Negative;
                } else if input_char == ' ' {
                    sign = Sign::Positive;
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
                        format!("Expected '-' for trailing minus but found '{input_char}'"),
                    ));
                }
                input_idx += 1;
            }
            PicToken::Credit => {
                // CR requires two characters
                if input_idx + 1 >= input_chars.len() {
                    return Err(Error::new(
                        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                        "Input too short for CR symbol".to_string(),
                    ));
                }
                let cr_str: String = input_chars[input_idx..input_idx + 2].iter().collect();
                if cr_str == "CR" {
                    sign = Sign::Negative;
                } else if cr_str == "  " {
                    sign = Sign::Positive;
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
                        format!("Expected 'CR' or spaces but found '{cr_str}'"),
                    ));
                }
                input_idx += 2;
            }
            PicToken::Debit => {
                // DB requires two characters
                if input_idx + 1 >= input_chars.len() {
                    return Err(Error::new(
                        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                        "Input too short for DB symbol".to_string(),
                    ));
                }
                let db_str: String = input_chars[input_idx..input_idx + 2].iter().collect();
                if db_str == "DB" {
                    sign = Sign::Negative;
                } else if db_str == "  " {
                    sign = Sign::Positive;
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
                        format!("Expected 'DB' or spaces but found '{db_str}'"),
                    ));
                }
                input_idx += 2;
            }
        }

        pattern_idx += 1;
    }

    // Check if we consumed all input
    if input_idx != input_chars.len() {
        return Err(Error::new(
            ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
            format!(
                "Input longer than expected (pattern consumed {} chars, input has {} chars)",
                input_idx,
                input_chars.len()
            ),
        ));
    }

    // Clean up digits - remove leading zeros but preserve at least one digit
    let digits = digits.trim_start_matches('0');
    let digits = if digits.is_empty() {
        "0".to_string()
    } else {
        digits.to_string()
    };

    // If all zeros, force positive sign
    if digits == "0" {
        sign = Sign::Positive;
    }

    Ok(NumericValue {
        sign,
        digits,
        scale,
    })
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_simple_z() {
        let tokens = tokenize_edited_pic("ZZZ9").unwrap();
        assert_eq!(
            tokens,
            vec![
                PicToken::ZeroSuppress,
                PicToken::ZeroSuppress,
                PicToken::ZeroSuppress,
                PicToken::Digit
            ]
        );
    }

    #[test]
    fn test_tokenize_with_decimal() {
        let tokens = tokenize_edited_pic("ZZZ9.99").unwrap();
        assert_eq!(
            tokens,
            vec![
                PicToken::ZeroSuppress,
                PicToken::ZeroSuppress,
                PicToken::ZeroSuppress,
                PicToken::Digit,
                PicToken::DecimalPoint,
                PicToken::Digit,
                PicToken::Digit
            ]
        );
    }

    #[test]
    fn test_tokenize_currency() {
        let tokens = tokenize_edited_pic("$ZZ,ZZZ.99").unwrap();
        assert_eq!(tokens[0], PicToken::Currency);
        assert!(tokens.contains(&PicToken::Comma));
        assert!(tokens.contains(&PicToken::DecimalPoint));
    }

    #[test]
    fn test_decode_simple() {
        let pattern = tokenize_edited_pic("ZZZ9").unwrap();
        let result = decode_edited_numeric("  12", &pattern, 0, false).unwrap();
        assert_eq!(result.sign, Sign::Positive);
        assert_eq!(result.digits, "12");
        assert_eq!(result.to_decimal_string(), "12");
    }

    #[test]
    fn test_decode_with_decimal() {
        let pattern = tokenize_edited_pic("ZZZ9.99").unwrap();
        let result = decode_edited_numeric("  12.34", &pattern, 2, false).unwrap();
        assert_eq!(result.sign, Sign::Positive);
        assert_eq!(result.digits, "1234");
        assert_eq!(result.scale, 2);
        assert_eq!(result.to_decimal_string(), "12.34");
    }

    #[test]
    fn test_decode_blank_when_zero() {
        let pattern = tokenize_edited_pic("ZZZ9").unwrap();
        let result = decode_edited_numeric("    ", &pattern, 0, true).unwrap();
        assert_eq!(result.to_decimal_string(), "0");
    }

    #[test]
    fn test_decode_with_currency() {
        let pattern = tokenize_edited_pic("$ZZZ.99").unwrap();
        let result = decode_edited_numeric("$ 12.34", &pattern, 2, false).unwrap();
        assert_eq!(result.to_decimal_string(), "12.34");
    }

    #[test]
    fn test_decode_trailing_cr() {
        let pattern = tokenize_edited_pic("ZZZ9CR").unwrap();
        let result = decode_edited_numeric("  12CR", &pattern, 0, false).unwrap();
        assert_eq!(result.sign, Sign::Negative);
        assert_eq!(result.to_decimal_string(), "-12");
    }

    #[test]
    fn test_decode_trailing_db() {
        let pattern = tokenize_edited_pic("ZZZ9DB").unwrap();
        let result = decode_edited_numeric("  12DB", &pattern, 0, false).unwrap();
        assert_eq!(result.sign, Sign::Negative);
        assert_eq!(result.to_decimal_string(), "-12");
    }
}
