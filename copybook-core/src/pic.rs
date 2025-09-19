//! PIC clause parsing and validation
//!
//! This module handles parsing of COBOL PICTURE clauses, including validation
//! of supported formats and rejection of edited pictures.

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
    ///
    /// Returns an error if:
    /// - The PIC clause contains unsupported edited patterns (e.g., currency symbols, decimal points)
    /// - The PIC clause contains SIGN clauses (treated as edited)
    /// - The PIC clause is empty after trimming
    /// - The PIC clause contains invalid characters
    /// - Multiple V positions are specified
    /// - V is used in non-numeric PIC clauses
    /// - Invalid repetition counts are specified
    /// - The total number of digits exceeds 38 (maximum supported)
    ///
    /// # Panics
    ///
    /// May panic if the internal character iterator is in an inconsistent state during parsing.
    /// This should not happen in normal usage with well-formed input.
    #[allow(clippy::too_many_lines)]
    pub fn parse(pic_str: &str) -> Result<Self> {
        let pic_str = pic_str.trim();

        // Check for edited PIC patterns first
        if is_edited_pic(pic_str) {
            return Err(Error::new(
                ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC,
                format!("edited PIC clause not supported: {pic_str}"),
            ));
        }

        // Check for SIGN clauses (also treated as edited)
        if pic_str.contains("SIGN") {
            return Err(Error::new(
                ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC,
                format!("SIGN clause treated as edited PIC: {pic_str}"),
            ));
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
                            format!("Mixed PIC types not allowed: {pic_str}"),
                        ));
                    }
                    kind = Some(PicKind::Alphanumeric);
                    digits += 1;
                }
                '9' => {
                    if kind.is_some() && kind != Some(PicKind::NumericDisplay) {
                        return Err(Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("Mixed PIC types not allowed: {pic_str}"),
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
                            format!("Multiple V positions not allowed: {pic_str}"),
                        ));
                    }
                    if kind != Some(PicKind::NumericDisplay) {
                        return Err(Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("V only allowed in numeric PIC: {pic_str}"),
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
                            count_str.push(chars.next().unwrap());
                        } else {
                            return Err(Error::new(
                                ErrorCode::CBKP001_SYNTAX,
                                format!("Invalid repetition count: {pic_str}"),
                            ));
                        }
                    }

                    let count: u16 = count_str.parse().map_err(|_| {
                        Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("Invalid repetition count: {count_str}"),
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
                        scale = scale.saturating_sub(1) + i16::try_from(count).unwrap_or(0);
                    }
                }
                ' ' | '\t' => {
                    // Skip whitespace
                }
                _ => {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        format!("Invalid character in PIC clause: {ch}"),
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

        if kind == PicKind::NumericDisplay && digits > 38 {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                format!("PIC clause too long: {digits} digits (max 38)"),
            ));
        }

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
    #[must_use]
    pub fn byte_length(&self) -> u32 {
        match self.kind {
            PicKind::Alphanumeric | PicKind::NumericDisplay => u32::from(self.digits),
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
                    write!(f, "{sign_prefix}X")
                } else {
                    write!(f, "{sign_prefix}X({})", self.digits)
                }
            }
            PicKind::NumericDisplay => {
                if self.scale == 0 {
                    if self.digits == 1 {
                        write!(f, "{sign_prefix}9")
                    } else {
                        write!(f, "{sign_prefix}9({})", self.digits)
                    }
                } else {
                    let integer_digits = self.digits - u16::try_from(self.scale).unwrap_or(0);
                    if integer_digits == 1 && self.scale == 1 {
                        write!(f, "{sign_prefix}9V9")
                    } else if integer_digits == 1 {
                        write!(f, "{sign_prefix}9V9({})", self.scale)
                    } else if self.scale == 1 {
                        write!(f, "{sign_prefix}9({integer_digits})V9")
                    } else {
                        write!(f, "{sign_prefix}9({integer_digits})V9({})", self.scale)
                    }
                }
            }
            PicKind::Edited => write!(f, "EDITED"),
        }
    }
}

/// Check if a PIC string contains edited picture characters
fn is_edited_pic(pic_str: &str) -> bool {
    // Edited PIC characters: Z, B, /, comma, $, +, -, *, CR, DB, etc.
    let edited_chars = ['Z', 'z', 'B', 'b', '/', ',', '$', '+', '-', '*'];

    for ch in pic_str.chars() {
        if edited_chars.contains(&ch) {
            return true;
        }
    }

    // Check for multi-character edited symbols
    pic_str.contains("CR")
        || pic_str.contains("DB")
        || pic_str.contains("cr")
        || pic_str.contains("db")
}

#[cfg(test)]
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
    fn test_edited_pic_rejection() {
        let result = PicClause::parse("ZZ,ZZZ.99");
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().code,
            ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC
        ));
    }

    #[test]
    fn test_sign_clause_rejection() {
        let result = PicClause::parse("S9(5) SIGN LEADING");
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().code,
            ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC
        ));
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
