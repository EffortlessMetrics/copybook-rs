//! PIC clause parsing and validation for COBOL data types
//!
//! This module handles parsing of COBOL PICTURE clauses, which define the format
//! and storage characteristics of data items. COBOL PIC clauses specify:
//!
//! - **Data Type**: Alphanumeric (X), Numeric (9), or Alphabetic (A)
//! - **Size**: Number of characters or digits
//! - **Decimal Places**: Position of implied decimal point (V)
//! - **Sign**: Whether numeric values can be signed (S)
//!
//! ## Common COBOL PIC Examples
//!
//! ```cobol
//! PIC X(10)        -- 10 alphanumeric characters
//! PIC 9(5)         -- 5-digit unsigned integer
//! PIC S9(5)        -- 5-digit signed integer
//! PIC 9(7)V99      -- 7 digits with 2 decimal places (e.g. 12345.67)
//! PIC S9(5)V9(2)   -- Signed decimal with 5 integer and 2 fractional digits
//! ```
//!
//! This module validates supported formats and rejects edited pictures
//! (currency symbols, etc.) which require separate handling.

use crate::error::ErrorCode;
use crate::{Error, Result};
use std::fmt;

/// Parsed PIC clause information
#[derive(Debug, Clone, PartialEq)]
pub struct PicClause {
    pub kind: PicKind,
    pub signed: bool,
    pub digits: u16,
    pub scale: i16, // Decimal places (negative for implied scaling)
}

/// Types of PIC clauses
#[derive(Debug, Clone, PartialEq)]
pub enum PicKind {
    /// Alphanumeric field (X)
    Alphanumeric,
    /// Numeric display field (9)
    NumericDisplay,
    /// Edited picture (rejected)
    Edited,
}

impl PicClause {
    /// Parse a PIC clause string
    ///
    /// # Errors
    /// Returns an error if the PIC clause is invalid or uses unsupported features.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn parse(pic_str: &str) -> Result<Self> {
        let pic_str = pic_str.trim();
        let pic_upper = pic_str.to_ascii_uppercase();

        // SIGN clauses are not yet supported in edited PIC decoding
        if pic_upper.contains("SIGN") {
            return Err(Error::new(
                ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC,
                format!("SIGN clause is not supported: {pic_str}"),
            ));
        }

        // Check for edited PIC patterns first (Phase E2: parse with scale computation)
        if is_edited_pic(pic_str) {
            let width = compute_edited_pic_width(pic_str)?;
            let signed = has_sign_editing(pic_str);
            let scale = compute_edited_pic_scale(pic_str)?;
            return Ok(PicClause {
                kind: PicKind::Edited,
                signed,
                digits: width,
                scale,
            });
        }

        let mut chars = pic_str.chars().peekable();
        let mut signed = false;
        let mut digits = 0u16;
        let mut scale = 0i16;
        let mut kind = None;
        let mut found_v = false;

        // Check for leading S (signed)
        if chars.peek() == Some(&'S') || chars.peek() == Some(&'s') {
            signed = true;
            chars.next();
        }

        while let Some(ch) = chars.next() {
            match ch.to_ascii_uppercase() {
                'X' => {
                    if kind.is_some() && kind != Some(PicKind::Alphanumeric) {
                        return Err(Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("Mixed PIC types not allowed: {}", pic_str),
                        ));
                    }
                    kind = Some(PicKind::Alphanumeric);
                    digits += 1;
                }
                '9' => {
                    if kind.is_some() && kind != Some(PicKind::NumericDisplay) {
                        return Err(Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("Mixed PIC types not allowed: {}", pic_str),
                        ));
                    }
                    kind = Some(PicKind::NumericDisplay);
                    digits += 1;
                    if found_v {
                        scale += 1;
                    }
                }
                'V' => {
                    if found_v {
                        return Err(Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("Multiple V positions not allowed: {}", pic_str),
                        ));
                    }
                    if kind != Some(PicKind::NumericDisplay) {
                        return Err(Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("V only allowed in numeric PIC: {}", pic_str),
                        ));
                    }
                    found_v = true;
                }
                '(' => {
                    // Parse repetition count
                    let mut count_str = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch == ')' {
                            chars.next(); // consume ')'
                            break;
                        }
                        if ch.is_ascii_digit() {
                            if let Some(digit_char) = chars.next() {
                                count_str.push(digit_char);
                            } else {
                                return Err(Error::new(
                                    ErrorCode::CBKP001_SYNTAX,
                                    format!("Unexpected end of digit sequence in PIC: {}", pic_str),
                                ));
                            }
                        } else {
                            return Err(Error::new(
                                ErrorCode::CBKP001_SYNTAX,
                                format!("Invalid repetition count: {}", pic_str),
                            ));
                        }
                    }

                    let count: u16 = count_str.parse().map_err(|_| {
                        Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("Invalid repetition count: {}", count_str),
                        )
                    })?;

                    if count == 0 {
                        return Err(Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            "Repetition count cannot be zero".to_string(),
                        ));
                    }

                    // Subtract 1 because we already counted the character before '('
                    digits = digits.saturating_sub(1) + count;
                    if found_v {
                        scale = scale.saturating_sub(1) + count as i16;
                    }
                }
                ' ' | '\t' => {
                    // Skip whitespace
                }
                _ => {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        format!("Invalid character in PIC clause: {}", ch),
                    ));
                }
            }
        }

        let kind = kind
            .ok_or_else(|| Error::new(ErrorCode::CBKP001_SYNTAX, "Empty PIC clause".to_string()))?;

        // Validate constraints
        if digits == 0 {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "PIC clause must have at least one digit/character".to_string(),
            ));
        }

        // Note: digits is u16, so max value is already 65535 - no need to check

        // Alphanumeric fields cannot be signed
        if signed && kind == PicKind::Alphanumeric {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "Alphanumeric PIC cannot be signed".to_string(),
            ));
        }

        Ok(PicClause {
            kind,
            signed,
            digits,
            scale,
        })
    }

    /// Get the byte length of this field when stored
    pub fn byte_length(&self) -> u32 {
        match self.kind {
            PicKind::Alphanumeric => self.digits as u32,
            PicKind::NumericDisplay => self.digits as u32,
            PicKind::Edited => 0, // Should never reach here
        }
    }
}

impl fmt::Display for PicClause {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sign_prefix = if self.signed { "S" } else { "" };

        match self.kind {
            PicKind::Alphanumeric => {
                if self.digits == 1 {
                    write!(f, "{}X", sign_prefix)
                } else {
                    write!(f, "{}X({})", sign_prefix, self.digits)
                }
            }
            PicKind::NumericDisplay => {
                if self.scale == 0 {
                    if self.digits == 1 {
                        write!(f, "{}9", sign_prefix)
                    } else {
                        write!(f, "{}9({})", sign_prefix, self.digits)
                    }
                } else {
                    let integer_digits = self.digits - self.scale as u16;
                    if integer_digits == 1 && self.scale == 1 {
                        write!(f, "{sign_prefix}9V9")
                    } else if integer_digits == 1 {
                        write!(f, "{}9V9({})", sign_prefix, self.scale)
                    } else if self.scale == 1 {
                        write!(f, "{sign_prefix}9({integer_digits})V9")
                    } else {
                        write!(f, "{}9({})V9({})", sign_prefix, integer_digits, self.scale)
                    }
                }
            }
            PicKind::Edited => write!(f, "EDITED"),
        }
    }
}

/// Check if a PIC string contains edited picture characters
fn is_edited_pic(pic_str: &str) -> bool {
    // Edited PIC characters: Z, /, comma, $, +, -, *
    // Note: '0' (zero insertion) is handled separately to avoid false positives
    // from count specifiers like 9(10) which contain '0' inside parentheses
    let edited_chars = ['Z', 'z', '/', ',', '$', '+', '-', '*'];

    // Track parentheses depth to distinguish count digits from edited characters
    let mut paren_depth: u32 = 0;
    for ch in pic_str.chars() {
        match ch {
            '(' => paren_depth += 1,
            ')' => paren_depth = paren_depth.saturating_sub(1),
            '0' => {
                // Zero is only an edited character when outside parentheses
                // (zero insertion like PIC 0999, not count like PIC 9(10))
                if paren_depth == 0 {
                    return true;
                }
            }
            _ => {
                if paren_depth == 0 && edited_chars.contains(&ch) {
                    return true;
                }
            }
        }
    }

    // Check for multi-character edited symbols
    pic_str.contains("CR")
        || pic_str.contains("DB")
        || pic_str.contains("cr")
        || pic_str.contains("db")
}

/// Compute display width from edited PIC string
/// Example: "ZZ,ZZZ.99" → 8 (including comma and decimal point)
fn compute_edited_pic_width(pic_str: &str) -> Result<u16> {
    let mut width = 0u16;
    let mut chars = pic_str.chars().peekable();

    while let Some(ch) = chars.next() {
        match ch.to_ascii_uppercase() {
            // Digit positions (including zero insertion)
            '9' | 'Z' | '*' | '0' => {
                // Check for repetition count
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
                                format!("Invalid repetition count in edited PIC: {}", pic_str),
                            ));
                        }
                    }
                    let count: u16 = count_str.parse().map_err(|_| {
                        Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("Invalid repetition count: {}", count_str),
                        )
                    })?;
                    width = width.saturating_add(count);
                } else {
                    width = width.saturating_add(1);
                }
            }
            // Insertion characters (commas, slashes, etc.)
            ',' | '/' | '.' => {
                width = width.saturating_add(1);
            }
            // Currency symbol
            '$' => {
                width = width.saturating_add(1);
            }
            // Sign symbols
            '+' | '-' => {
                width = width.saturating_add(1);
            }
            // CR/DB handling (2 characters)
            'C' | 'D' => {
                if let Some(&next_ch) = chars.peek()
                    && ((ch == 'C' && (next_ch == 'R' || next_ch == 'r'))
                        || (ch == 'D' && (next_ch == 'B' || next_ch == 'b')))
                {
                    chars.next(); // consume second character
                    width = width.saturating_add(2);
                }
            }
            // V is non-display (implied decimal)
            'V' => {
                // Don't add to width
            }
            // S prefix is non-display
            'S' => {
                // Don't add to width
            }
            // Whitespace
            ' ' | '\t' => {
                // Skip
            }
            _ => {
                // Unknown character - for now, just skip it
            }
        }
    }

    if width == 0 {
        return Err(Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!("Edited PIC has zero display width: {}", pic_str),
        ));
    }

    Ok(width)
}

/// Check if edited PIC has sign editing (CR, DB, +, -)
fn has_sign_editing(pic_str: &str) -> bool {
    pic_str.contains("CR")
        || pic_str.contains("DB")
        || pic_str.contains("cr")
        || pic_str.contains("db")
        || pic_str.contains('+')
        || pic_str.contains('-')
}

/// Compute decimal scale (number of digits after decimal point) for edited PIC
/// Examples:
/// - "ZZZ9" → 0
/// - "ZZZ9.99" → 2
/// - "$ZZ,ZZZ.99" → 2
/// - "9(5)V99" → 2 (V is implicit decimal, . is explicit)
fn compute_edited_pic_scale(pic_str: &str) -> Result<i16> {
    let mut chars = pic_str.chars().peekable();
    let mut found_decimal = false;
    let mut scale = 0i16;

    // Skip leading 'S' if present
    if chars.peek() == Some(&'S') || chars.peek() == Some(&'s') {
        chars.next();
    }

    while let Some(ch) = chars.next() {
        match ch.to_ascii_uppercase() {
            '.' => {
                if found_decimal {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        format!("Multiple decimal points in edited PIC: {pic_str}"),
                    ));
                }
                found_decimal = true;
            }
            'V' => {
                // Implicit decimal point
                if found_decimal {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        format!("Both V and . in edited PIC: {pic_str}"),
                    ));
                }
                found_decimal = true;
            }
            '9' | 'Z' | '*' | '0' => {
                // Check for repetition count
                let count = if chars.peek() == Some(&'(') {
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
                                format!("Invalid repetition count in edited PIC: {pic_str}"),
                            ));
                        }
                    }
                    count_str.parse::<i16>().map_err(|_| {
                        Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("Invalid repetition count: {count_str}"),
                        )
                    })?
                } else {
                    1
                };

                if found_decimal {
                    scale = scale.saturating_add(count);
                }
            }
            // Other characters (comma, slash, $, +, -, etc.) don't affect scale
            _ => {}
        }
    }

    Ok(scale)
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_alphanumeric_pic() {
        let pic = PicClause::parse("X(10)").unwrap();
        assert_eq!(pic.kind, PicKind::Alphanumeric);
        assert!(!pic.signed);
        assert_eq!(pic.digits, 10);
        assert_eq!(pic.scale, 0);
        assert_eq!(pic.byte_length(), 10);
    }

    #[test]
    fn test_numeric_display_pic() {
        let pic = PicClause::parse("9(5)").unwrap();
        assert_eq!(pic.kind, PicKind::NumericDisplay);
        assert!(!pic.signed);
        assert_eq!(pic.digits, 5);
        assert_eq!(pic.scale, 0);
    }

    #[test]
    fn test_signed_numeric_pic() {
        let pic = PicClause::parse("S9(7)V99").unwrap();
        assert_eq!(pic.kind, PicKind::NumericDisplay);
        assert!(pic.signed);
        assert_eq!(pic.digits, 9);
        assert_eq!(pic.scale, 2);
    }

    #[test]
    fn test_edited_pic_parses() {
        // Phase E1: edited PIC should now parse successfully
        let result = PicClause::parse("ZZ,ZZZ.99");
        assert!(result.is_ok());
        let pic = result.unwrap();
        assert_eq!(pic.kind, PicKind::Edited);
        assert_eq!(pic.digits, 9); // ZZ,ZZZ.99 = 2 + 1 + 3 + 1 + 2 = 9 display positions
        assert!(!pic.signed); // No sign editing
    }

    #[test]
    fn test_sign_clause_rejected() {
        let result = PicClause::parse("S9(5) SIGN LEADING");
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC);
    }

    #[test]
    fn test_mixed_types_error() {
        let result = PicClause::parse("X9");
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().code,
            ErrorCode::CBKP001_SYNTAX
        ));
    }

    #[test]
    fn test_signed_alphanumeric_error() {
        let result = PicClause::parse("SX(10)");
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().code,
            ErrorCode::CBKP001_SYNTAX
        ));
    }

    #[test]
    fn test_pic_display() {
        assert_eq!(PicClause::parse("X(10)").unwrap().to_string(), "X(10)");
        assert_eq!(PicClause::parse("9(5)").unwrap().to_string(), "9(5)");
        assert_eq!(
            PicClause::parse("S9(7)V99").unwrap().to_string(),
            "S9(7)V9(2)"
        );
        assert_eq!(PicClause::parse("9V9").unwrap().to_string(), "9V9");
    }
}
