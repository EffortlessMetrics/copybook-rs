// SPDX-License-Identifier: AGPL-3.0-or-later
//! Error taxonomy completeness tests.
//!
//! Validates that every `ErrorCode` variant has a unique string representation,
//! follows the CBK* naming convention, maps to the correct family, and
//! serializes/deserializes correctly.

#![allow(clippy::unwrap_used)]

use copybook_error::{Error, ErrorCode, ErrorContext};
use std::collections::{HashMap, HashSet};

mod all_codes;
use all_codes::all_error_codes;

/// Validate that a string matches `CBK[A-Z][0-9]{3}_[A-Z0-9_]+`
fn matches_cbk_pattern(s: &str) -> bool {
    let bytes = s.as_bytes();
    if bytes.len() < 9 {
        return false;
    }
    bytes[0] == b'C'
        && bytes[1] == b'B'
        && bytes[2] == b'K'
        && bytes[3].is_ascii_uppercase()
        && bytes[4].is_ascii_digit()
        && bytes[5].is_ascii_digit()
        && bytes[6].is_ascii_digit()
        && bytes[7] == b'_'
        && bytes[8..]
            .iter()
            .all(|&b| b.is_ascii_uppercase() || b.is_ascii_digit() || b == b'_')
}

// ====================================================================
// 1. Display string follows CBK<letter><digits>_<NAME> pattern
// ====================================================================

#[test]
fn every_display_string_matches_cbk_pattern() {
    for code in all_error_codes() {
        let display = format!("{code}");
        assert!(
            matches_cbk_pattern(&display),
            "ErrorCode display '{display}' does not match CBK<letter><digits>_<NAME> pattern"
        );
    }
}

// ====================================================================
// 2. No duplicate display strings (uniqueness)
// ====================================================================

#[test]
fn no_duplicate_display_strings() {
    let codes = all_error_codes();
    let mut seen = HashSet::with_capacity(codes.len());
    for code in &codes {
        let display = format!("{code}");
        assert!(
            seen.insert(display.clone()),
            "Duplicate display string: {display}"
        );
    }
    assert_eq!(seen.len(), codes.len());
}

// ====================================================================
// 3. Display string starts with its family_prefix
// ====================================================================

#[test]
fn display_string_prefix_equals_family_prefix() {
    for code in all_error_codes() {
        let display = format!("{code}");
        let prefix = code.family_prefix();
        assert!(
            display.starts_with(prefix),
            "{display} does not start with family prefix {prefix}"
        );
    }
}

// ====================================================================
// 4. Family prefix to category mapping is exhaustive
// ====================================================================

#[test]
fn family_prefix_maps_to_known_category() {
    let known = [
        "CBKP", "CBKS", "CBKR", "CBKC", "CBKD", "CBKI", "CBKE", "CBKF", "CBKA", "CBKW",
    ];
    for code in all_error_codes() {
        let prefix = code.family_prefix();
        assert!(
            known.contains(&prefix),
            "Unknown family prefix '{prefix}' for {code}"
        );
    }
}

// ====================================================================
// 5. Family categories contain the expected members
// ====================================================================

#[test]
fn family_category_member_counts() {
    let mut counts: HashMap<&str, usize> = HashMap::new();
    for code in all_error_codes() {
        *counts.entry(code.family_prefix()).or_default() += 1;
    }
    // At least the minimum expected counts per family
    assert!(
        *counts.get("CBKP").unwrap_or(&0) >= 7,
        "CBKP should have ≥7 codes"
    );
    assert!(
        *counts.get("CBKS").unwrap_or(&0) >= 19,
        "CBKS should have ≥19 codes"
    );
    assert!(
        *counts.get("CBKD").unwrap_or(&0) >= 15,
        "CBKD should have ≥15 codes"
    );
    assert!(
        *counts.get("CBKE").unwrap_or(&0) >= 7,
        "CBKE should have ≥7 codes"
    );
    assert!(
        *counts.get("CBKW").unwrap_or(&0) >= 5,
        "CBKW should have ≥5 codes"
    );
}

// ====================================================================
// 6. Serde round-trip for every single variant
// ====================================================================

#[test]
fn serde_roundtrip_every_variant() {
    for code in all_error_codes() {
        let json = serde_json::to_string(&code).unwrap();
        let back: ErrorCode = serde_json::from_str(&json).unwrap();
        assert_eq!(back, code, "serde roundtrip failed for {code}");
    }
}

// ====================================================================
// 7. Serde serialized form equals Display form (quoted)
// ====================================================================

#[test]
fn serde_json_string_equals_display() {
    for code in all_error_codes() {
        let display = format!("{code}");
        let json = serde_json::to_string(&code).unwrap();
        assert_eq!(json, format!("\"{display}\""), "mismatch for {code}");
    }
}

// ====================================================================
// 8. Debug output contains the variant name
// ====================================================================

#[test]
fn debug_output_contains_variant_name() {
    for code in all_error_codes() {
        let display = format!("{code}");
        let debug = format!("{code:?}");
        assert!(
            debug.contains(&display),
            "Debug output '{debug}' should contain '{display}'"
        );
    }
}

// ====================================================================
// 9. Error Display includes code and message
// ====================================================================

#[test]
fn error_display_format_includes_code_and_message() {
    for code in all_error_codes() {
        let msg = format!("test message for {code}");
        let err = Error::new(code, msg.clone());
        let display = format!("{err}");
        let code_str = format!("{code}");
        assert!(
            display.starts_with(&code_str),
            "display should start with code: {display}"
        );
        assert!(
            display.contains(&msg),
            "display should contain message: {display}"
        );
    }
}

// ====================================================================
// 10. Error Display with context appends parenthesized context
// ====================================================================

#[test]
fn error_display_with_context_is_parenthesized() {
    let err = Error::new(ErrorCode::CBKP001_SYNTAX, "msg")
        .with_field("FIELD-A")
        .with_record(1);
    let display = format!("{err}");
    assert!(display.contains('('), "context should be in parentheses");
    assert!(display.contains(')'), "context should be in parentheses");
    assert!(display.contains("field FIELD-A"));
    assert!(display.contains("record 1"));
}

// ====================================================================
// 11. ErrorContext Display ordering is stable
// ====================================================================

#[test]
fn error_context_display_ordering_record_field_offset_line_details() {
    let ctx = ErrorContext {
        record_index: Some(1),
        field_path: Some("A".into()),
        byte_offset: Some(2),
        line_number: Some(3),
        details: Some("D".into()),
    };
    let display = format!("{ctx}");
    let parts: Vec<&str> = display.split(", ").collect();
    assert_eq!(parts.len(), 5);
    assert!(parts[0].starts_with("record "));
    assert!(parts[1].starts_with("field "));
    assert!(parts[2].starts_with("offset "));
    assert!(parts[3].starts_with("line "));
    assert_eq!(parts[4], "D");
}

// ====================================================================
// 12. Numeric portion in display string is exactly three digits
// ====================================================================

#[test]
fn display_string_numeric_portion_is_three_digits() {
    for code in all_error_codes() {
        let display = format!("{code}");
        let bytes = display.as_bytes();
        // Positions 4,5,6 must be digits (after CBK + letter)
        assert!(bytes.len() >= 7, "too short: {display}");
        assert!(
            bytes[4].is_ascii_digit() && bytes[5].is_ascii_digit() && bytes[6].is_ascii_digit(),
            "Expected 3-digit number at positions 4-6 in {display}"
        );
    }
}

// ====================================================================
// 13. Error::code() accessor matches stored code
// ====================================================================

#[test]
fn code_accessor_matches_for_all_variants() {
    for code in all_error_codes() {
        let err = Error::new(code, "test");
        assert_eq!(err.code(), code);
    }
}

// ====================================================================
// 14. Error::family_prefix() accessor matches code's family_prefix
// ====================================================================

#[test]
fn family_prefix_accessor_matches_for_all_variants() {
    for code in all_error_codes() {
        let err = Error::new(code, "test");
        assert_eq!(err.family_prefix(), code.family_prefix());
    }
}

// ====================================================================
// 15. Serde rejects invalid strings and non-string values
// ====================================================================

#[test]
fn serde_rejects_empty_string() {
    let result: Result<ErrorCode, _> = serde_json::from_str(r#""""#);
    assert!(result.is_err());
}

#[test]
fn serde_rejects_partial_code_name() {
    let result: Result<ErrorCode, _> = serde_json::from_str(r#""CBKP001""#);
    assert!(result.is_err());
}

#[test]
fn serde_rejects_lowercase_variant() {
    let result: Result<ErrorCode, _> = serde_json::from_str(r#""cbkp001_syntax""#);
    assert!(result.is_err());
}

#[test]
fn serde_rejects_null() {
    let result: Result<ErrorCode, _> = serde_json::from_str("null");
    assert!(result.is_err());
}

#[test]
fn serde_rejects_boolean() {
    let result: Result<ErrorCode, _> = serde_json::from_str("true");
    assert!(result.is_err());
}

#[test]
fn error_new_accepts_string_and_str() {
    let err1 = Error::new(ErrorCode::CBKP001_SYNTAX, "static");
    let err2 = Error::new(ErrorCode::CBKP001_SYNTAX, String::from("dynamic"));
    assert_eq!(err1.message, "static");
    assert_eq!(err2.message, "dynamic");
}

#[test]
fn error_context_display_record_only() {
    let ctx = ErrorContext {
        record_index: Some(1),
        field_path: None,
        byte_offset: None,
        line_number: None,
        details: None,
    };
    assert_eq!(format!("{ctx}"), "record 1");
}

#[test]
fn error_code_hash_unique() {
    let codes = all_error_codes();
    let set: HashSet<ErrorCode> = codes.iter().copied().collect();
    assert_eq!(
        set.len(),
        codes.len(),
        "all codes should be unique in HashSet"
    );
}
