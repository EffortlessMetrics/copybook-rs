// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used)]
//! Common test helpers for copybook-codec tests
//!
//! This module provides reusable test utilities to reduce code duplication
//! across test files. It consolidates common patterns like creating
//! DecodeOptions, test schemas, and test data.

use copybook_codec::{Codepage, DecodeOptions, EncodeOptions, RecordFormat};
use copybook_core::parse_copybook;

/// Creates a default DecodeOptions for fixed-length records
#[inline]
#[must_use]
pub fn default_fixed_options() -> DecodeOptions {
    DecodeOptions::new().with_format(RecordFormat::Fixed)
}

/// Creates a default DecodeOptions for RDW variable-length records
#[inline]
#[must_use]
pub fn default_rdw_options() -> DecodeOptions {
    DecodeOptions::new().with_format(RecordFormat::RDW)
}

/// Creates a DecodeOptions with the specified codepage for fixed-length records
#[inline]
#[must_use]
pub fn fixed_options_with_codepage(codepage: Codepage) -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(codepage)
}

/// Creates a DecodeOptions with CP037 codepage for fixed-length records
#[inline]
#[must_use]
pub fn fixed_options_cp037() -> DecodeOptions {
    fixed_options_with_codepage(Codepage::CP037)
}

/// Creates a DecodeOptions with ASCII codepage for fixed-length records
#[inline]
#[must_use]
pub fn fixed_options_ascii() -> DecodeOptions {
    fixed_options_with_codepage(Codepage::ASCII)
}

/// Creates a default EncodeOptions
#[inline]
#[must_use]
pub fn default_encode_options() -> EncodeOptions {
    EncodeOptions::new()
}

/// Creates an EncodeOptions with the specified codepage
#[inline]
#[must_use]
pub fn encode_options_with_codepage(codepage: Codepage) -> EncodeOptions {
    EncodeOptions::new().with_codepage(codepage)
}

/// Creates a simple test schema with a single alphanumeric field
#[inline]
pub fn simple_schema(field_name: &str, field_len: usize) -> copybook_core::Schema {
    let copybook_text = format!(
        r"
            01 RECORD.
               05 {} PIC X({}).
        ",
        field_name, field_len
    );
    parse_copybook(&copybook_text).expect("Failed to parse simple schema")
}

/// Creates a simple test schema with a numeric field
#[inline]
pub fn numeric_schema(field_name: &str, digits: usize) -> copybook_core::Schema {
    let copybook_text = format!(
        r"
            01 RECORD.
               05 {} PIC 9({}).
        ",
        field_name, digits
    );
    parse_copybook(&copybook_text).expect("Failed to parse numeric schema")
}

/// Creates a simple test schema with both alphanumeric and numeric fields
#[inline]
pub fn mixed_schema() -> copybook_core::Schema {
    let copybook_text = r"
        01 RECORD.
           05 ID       PIC 9(6).
           05 NAME     PIC X(30).
           05 AMOUNT   PIC S9(7)V99 COMP-3.
    ";
    parse_copybook(copybook_text).expect("Failed to parse mixed schema")
}

/// Creates a simple test schema with COMP-3 packed decimal field
#[inline]
pub fn comp3_schema(field_name: &str, digits: usize, scale: usize) -> copybook_core::Schema {
    let copybook_text = format!(
        r"
            01 RECORD.
               05 {} PIC S9({})V{} COMP-3.
        ",
        field_name, digits, scale
    );
    parse_copybook(&copybook_text).expect("Failed to parse COMP-3 schema")
}

/// Creates a simple test schema with zoned decimal field
#[inline]
pub fn zoned_schema(field_name: &str, digits: usize, scale: usize) -> copybook_core::Schema {
    let copybook_text = format!(
        r"
            01 RECORD.
               05 {} PIC S9({})V{}.
        ",
        field_name, digits, scale
    );
    parse_copybook(&copybook_text).expect("Failed to parse zoned schema")
}

/// Creates test data by repeating a byte pattern
#[inline]
#[must_use]
pub fn repeat_byte(byte: u8, count: usize) -> Vec<u8> {
    vec![byte; count]
}

/// Creates test data from a string (ASCII)
#[inline]
#[must_use]
pub fn ascii_data(s: &str) -> Vec<u8> {
    s.as_bytes().to_vec()
}

/// Creates EBCDIC test data from an ASCII string
#[inline]
#[must_use]
pub fn ebcdic_data(s: &str) -> Vec<u8> {
    // Simple ASCII to EBCDIC conversion for common characters
    s.chars()
        .map(|c| match c {
            'A'..='Z' => 0xC1 + (c as u8) - b'A',
            '0'..='9' => 0xF0 + (c as u8) - b'0',
            ' ' => 0x40,
            _ => c as u8, // Fallback to ASCII
        })
        .collect()
}

/// Creates a simple customer record schema
#[inline]
pub fn customer_schema() -> copybook_core::Schema {
    let copybook_text = r"
        01 CUSTOMER-RECORD.
           05 CUSTOMER-ID      PIC 9(6).
           05 CUSTOMER-NAME    PIC X(30).
           05 CUSTOMER-ADDRESS PIC X(50).
           05 BALANCE          PIC S9(7)V99 COMP-3.
    ";
    parse_copybook(copybook_text).expect("Failed to parse customer schema")
}

/// Creates a simple transaction record schema
#[inline]
pub fn transaction_schema() -> copybook_core::Schema {
    let copybook_text = r"
        01 TRANSACTION.
           05 TRAN-ID        PIC 9(10).
           05 TRAN-AMOUNT    PIC S9(9)V99 COMP-3.
           05 TRAN-DESC      PIC X(100).
    ";
    parse_copybook(copybook_text).expect("Failed to parse transaction schema")
}

/// Creates a simple employee record schema
#[inline]
pub fn employee_schema() -> copybook_core::Schema {
    let copybook_text = r"
        01 EMPLOYEE-RECORD.
           05 EMP-ID         PIC 9(6).
           05 EMP-NAME       PIC X(30).
           05 EMP-SALARY     PIC S9(7)V99 COMP-3.
           05 EMP-DEPT       PIC X(10).
    ";
    parse_copybook(copybook_text).expect("Failed to parse employee schema")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_fixed_options() {
        let options = default_fixed_options();
        assert_eq!(options.format, RecordFormat::Fixed);
    }

    #[test]
    fn test_default_rdw_options() {
        let options = default_rdw_options();
        assert_eq!(options.format, RecordFormat::RDW);
    }

    #[test]
    fn test_fixed_options_cp037() {
        let options = fixed_options_cp037();
        assert_eq!(options.format, RecordFormat::Fixed);
        assert_eq!(options.codepage, Codepage::CP037);
    }

    #[test]
    fn test_fixed_options_ascii() {
        let options = fixed_options_ascii();
        assert_eq!(options.format, RecordFormat::Fixed);
        assert_eq!(options.codepage, Codepage::ASCII);
    }

    #[test]
    fn test_simple_schema() {
        let schema = simple_schema("FIELD", 10);
        assert_eq!(schema.fields.len(), 1);
        assert_eq!(schema.fields[0].name, "RECORD");
    }

    #[test]
    fn test_numeric_schema() {
        let schema = numeric_schema("NUM", 5);
        assert_eq!(schema.fields.len(), 1);
    }

    #[test]
    fn test_repeat_byte() {
        let data = repeat_byte(0x41, 5);
        assert_eq!(data, vec![0x41, 0x41, 0x41, 0x41, 0x41]);
    }

    #[test]
    fn test_ascii_data() {
        let data = ascii_data("HELLO");
        assert_eq!(data, b"HELLO".to_vec());
    }
}
