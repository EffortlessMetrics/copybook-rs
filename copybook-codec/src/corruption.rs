//! Transfer corruption detection utilities
//!
//! This module provides heuristics and detection methods for common data transfer
//! corruption patterns, particularly ASCII conversion of binary data.

use copybook_core::{Error, ErrorCode};

/// Heuristics for detecting ASCII transfer corruption in RDW headers
/// 
/// This function implements the CBKF104_RDW_SUSPECT_ASCII detection logic
/// by looking for patterns that suggest binary data was corrupted during
/// ASCII transfer (e.g., EBCDIC->ASCII conversion of binary fields).
pub fn detect_rdw_ascii_corruption(rdw_bytes: &[u8]) -> Option<Error> {
    if rdw_bytes.len() < 4 {
        return None;
    }

    // Extract the length field (first 2 bytes, big-endian)
    let length_bytes = [rdw_bytes[0], rdw_bytes[1]];
    let length = u16::from_be_bytes(length_bytes);

    // Heuristic 1: Length field contains ASCII digits
    // If the length appears to be ASCII-encoded digits, this suggests corruption
    if is_ascii_digits(&length_bytes) {
        return Some(Error::new(
            ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
            format!(
                "RDW length field appears to contain ASCII digits: 0x{:02X}{:02X} ('{}{}')",
                rdw_bytes[0], rdw_bytes[1],
                ascii_char_or_dot(rdw_bytes[0]), ascii_char_or_dot(rdw_bytes[1])
            )
        ));
    }

    // Heuristic 2: Unreasonably large length values that could be ASCII
    // ASCII digits in binary positions create very large numbers
    if length > 0x3030 && length <= 0x3939 {
        // Range covers ASCII '00' to '99' when interpreted as binary
        return Some(Error::new(
            ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
            format!(
                "RDW length field suspiciously large ({}), may be ASCII-corrupted: 0x{:04X}",
                length, length
            )
        ));
    }

    // Heuristic 3: Reserved bytes contain ASCII-like patterns
    let reserved_bytes = [rdw_bytes[2], rdw_bytes[3]];
    if is_ascii_printable(&reserved_bytes) && reserved_bytes != [0x00, 0x00] {
        return Some(Error::new(
            ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
            format!(
                "RDW reserved bytes contain ASCII-like data: 0x{:02X}{:02X} ('{}{}')",
                rdw_bytes[2], rdw_bytes[3],
                ascii_char_or_dot(rdw_bytes[2]), ascii_char_or_dot(rdw_bytes[3])
            )
        ));
    }

    None
}

/// Detect potential EBCDIC corruption in text fields
/// 
/// This looks for patterns that suggest EBCDIC data was not properly converted
/// or was double-converted during transfer.
pub fn detect_ebcdic_corruption(data: &[u8], field_path: &str) -> Vec<Error> {
    let mut errors = Vec::new();

    // Look for common EBCDIC corruption patterns
    for (i, &byte) in data.iter().enumerate() {
        // Check for invalid EBCDIC bytes that might indicate corruption
        if is_likely_corrupted_ebcdic(byte) {
            let error = Error::new(
                ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
                format!(
                    "Potentially corrupted EBCDIC byte 0x{:02X} at position {} in field {}",
                    byte, i, field_path
                )
            ).with_field(field_path).with_offset(i as u64);
            
            errors.push(error);
        }
    }

    errors
}

/// Detect patterns in packed decimal that suggest corruption
pub fn detect_packed_corruption(data: &[u8], field_path: &str) -> Vec<Error> {
    let mut errors = Vec::new();

    // Check for ASCII digits in packed decimal fields
    for (i, &byte) in data.iter().enumerate() {
        let high_nibble = (byte >> 4) & 0x0F;
        let low_nibble = byte & 0x0F;

        // ASCII digits '0'-'9' have values 0x30-0x39
        // Only flag as corruption if we see a complete ASCII digit byte
        // 0x34 is valid packed decimal (digits 3 and 4), so we need to be more specific
        // We'll only flag bytes that are exactly ASCII digit values (0x30-0x39)
        // and appear in suspicious patterns (like multiple consecutive ASCII bytes)
        if byte >= 0x30 && byte <= 0x39 {
            // Check if this looks like a complete ASCII digit corruption
            // For now, we'll be conservative and only flag obvious ASCII patterns
            // This is a heuristic and may need refinement based on real-world data
            
            // Skip this check for now - packed decimal validation should be done elsewhere
            // This corruption detection is meant for obvious transfer corruption, not validation
        }

        // Check for invalid nibbles (A, B, E are invalid in packed decimal)
        if high_nibble == 0xA || high_nibble == 0xB || high_nibble == 0xE {
            let error = Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                format!(
                    "invalid high nibble 0x{:X} in packed decimal at byte {} (full byte: 0x{:02X})",
                    high_nibble, i, byte
                )
            ).with_field(field_path).with_offset(i as u64);

            errors.push(error);
        }

        // For the last byte, check the sign nibble (low nibble)
        if i == data.len() - 1 {
            // Valid sign nibbles: C, D, F (positive), and sometimes A, B, E (negative)
            // Invalid: 0-9 (these would be digits, not signs)
            if low_nibble <= 0x9 {
                let error = Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    format!(
                        "invalid sign nibble 0x{:X} in packed decimal (should be C/D/F), byte {} (full byte: 0x{:02X})",
                        low_nibble, i, byte
                    )
                ).with_field(field_path).with_offset(i as u64);

                errors.push(error);
            }
        } else if low_nibble == 0xA || low_nibble == 0xB || low_nibble == 0xE {
            let error = Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                format!(
                    "invalid low nibble 0x{:X} in packed decimal at byte {} (full byte: 0x{:02X})",
                    low_nibble, i, byte
                )
            ).with_field(field_path).with_offset(i as u64);

            errors.push(error);
        }
    }

    errors
}

/// Check if bytes contain ASCII digits
fn is_ascii_digits(bytes: &[u8]) -> bool {
    bytes.iter().all(|&b| b >= b'0' && b <= b'9')
}

/// Check if bytes contain ASCII printable characters
fn is_ascii_printable(bytes: &[u8]) -> bool {
    bytes.iter().all(|&b| b >= 0x20 && b <= 0x7E)
}

/// Check if a byte is likely a corrupted EBCDIC character
fn is_likely_corrupted_ebcdic(byte: u8) -> bool {
    // This is a heuristic - look for bytes that are uncommon in valid EBCDIC
    // but might appear due to corruption
    match byte {
        // Control characters that shouldn't appear in normal text
        0x00..=0x1F => true,
        0x7F..=0x9F => true,
        // Other suspicious patterns can be added here
        _ => false,
    }
}

/// Convert byte to ASCII character or '.' if not printable
fn ascii_char_or_dot(byte: u8) -> char {
    if byte >= 0x20 && byte <= 0x7E {
        byte as char
    } else {
        '.'
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rdw_ascii_corruption_detection() {
        // Test ASCII digits in length field
        let rdw_with_ascii = [b'1', b'2', 0x00, 0x00];
        let result = detect_rdw_ascii_corruption(&rdw_with_ascii);
        assert!(result.is_some());
        assert_eq!(result.unwrap().code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);

        // Test normal RDW
        let normal_rdw = [0x00, 0x50, 0x00, 0x00]; // Length 80, no reserved data
        let result = detect_rdw_ascii_corruption(&normal_rdw);
        assert!(result.is_none());

        // Test ASCII in reserved bytes
        let rdw_with_reserved_ascii = [0x00, 0x50, b'A', b'B'];
        let result = detect_rdw_ascii_corruption(&rdw_with_reserved_ascii);
        assert!(result.is_some());
    }

    #[test]
    fn test_packed_corruption_detection() {
        // Test invalid sign nibble
        let invalid_sign = [0x12, 0x34, 0x56]; // Last nibble 6 is invalid for sign
        let errors = detect_packed_corruption(&invalid_sign, "TEST.FIELD");
        assert!(!errors.is_empty());
        assert!(errors.iter().any(|e| e.message.contains("Invalid sign nibble")));

        // Test invalid high nibble
        let invalid_high = [0xA2, 0x34, 0x5C]; // High nibble A is invalid
        let errors = detect_packed_corruption(&invalid_high, "TEST.FIELD");
        assert!(!errors.is_empty());
        assert!(errors.iter().any(|e| e.message.contains("Invalid high nibble")));

        // Test valid packed decimal
        let valid_packed = [0x12, 0x34, 0x5C]; // Valid with positive sign
        let errors = detect_packed_corruption(&valid_packed, "TEST.FIELD");
        assert!(errors.is_empty(), "Valid packed decimal should not generate errors: {:?}", errors);
    }

    #[test]
    fn test_ebcdic_corruption_detection() {
        // Test with control characters that suggest corruption
        let corrupted_data = [0xC1, 0x00, 0x7F, 0xC2]; // Valid EBCDIC 'A', control chars, valid 'B'
        let errors = detect_ebcdic_corruption(&corrupted_data, "TEXT.FIELD");
        assert_eq!(errors.len(), 2); // Two control characters detected

        // Test with normal EBCDIC data
        let normal_data = [0xC1, 0xC2, 0xC3]; // EBCDIC 'ABC'
        let errors = detect_ebcdic_corruption(&normal_data, "TEXT.FIELD");
        assert!(errors.is_empty());
    }

    #[test]
    fn test_ascii_char_or_dot() {
        assert_eq!(ascii_char_or_dot(b'A'), 'A');
        assert_eq!(ascii_char_or_dot(0x00), '.');
        assert_eq!(ascii_char_or_dot(0x7F), '.');
        assert_eq!(ascii_char_or_dot(b' '), ' ');
    }
}