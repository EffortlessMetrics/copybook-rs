//! Custom generators for property testing
//!
//! This module provides proptest strategies for generating:
//! - Copybook syntax
//! - Binary data with various encodings
//! - JSON data for encoding
//! - PIC clause patterns
//! - OCCURS/ODO structures
//! - REDEFINES configurations

use proptest::prelude::*;
use proptest::collection::vec;

/// Strategy for generating valid PIC clause patterns
pub fn pic_clause_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        // Numeric PIC clauses
        Just("9".to_string()),
        Just("9(1)".to_string()),
        Just("9(5)".to_string()),
        Just("9(18)".to_string()),
        // Signed numeric PIC clauses
        Just("S9".to_string()),
        Just("S9(1)".to_string()),
        Just("S9(5)".to_string()),
        Just("S9(9)".to_string()),
        // Decimal PIC clauses
        Just("9V9".to_string()),
        Just("9(5)V9".to_string()),
        Just("9(9)V9(2)".to_string()),
        Just("S9V9".to_string()),
        Just("S9(5)V9(2)".to_string()),
        // Alphanumeric PIC clauses
        Just("X".to_string()),
        Just("X(1)".to_string()),
        Just("X(10)".to_string()),
        Just("X(50)".to_string()),
        // Edited PIC clauses
        Just("ZZZZ9".to_string()),
        Just("$$$,$$9.99".to_string()),
        Just("+999,999.99".to_string()),
        Just("-999,999.99".to_string()),
        // COMP-3 (packed decimal)
        Just("9(5) COMP-3".to_string()),
        Just("S9(7)V99 COMP-3".to_string()),
        // Binary
        Just("9(4) BINARY".to_string()),
        Just("S9(9) BINARY".to_string()),
    ]
}

/// Strategy for generating field names (valid COBOL identifiers)
pub fn field_name_strategy() -> impl Strategy<Value = String> {
    "[A-Z][A-Z0-9-]{0,29}".prop_map(|s| s.to_uppercase())
}

/// Strategy for generating field level numbers
pub fn level_number_strategy() -> impl Strategy<Value = u8> {
    prop_oneof![
        Just(1u8),   // Record level
        5u8..49u8,  // Group level
        49u8..88u8, // Elementary level (excluding 88)
    ]
}

/// Strategy for generating OCCURS clauses
pub fn occurs_strategy() -> impl Strategy<Value = Option<String>> {
    prop_oneof![
        // No OCCURS
        Just(None),
        // Fixed OCCURS
        "[1-9][0-9]{0,2}".prop_map(|s| Some(format!("OCCURS {} TIMES", s))),
        // OCCURS with DEPENDING ON
        (
            "[1-9][0-9]{0,2}".prop_map(|s| s.parse::<usize>().unwrap()),
            field_name_strategy()
        ).prop_map(|(count, name)| Some(format!("OCCURS {} TO {} TIMES DEPENDING ON {}", 1, count, name))),
    ]
}

/// Strategy for generating REDEFINES clauses
pub fn redefines_strategy() -> impl Strategy<Value = Option<String>> {
    prop_oneof![
        // No REDEFINES
        Just(None),
        // With REDEFINES
        field_name_strategy().prop_map(|name| Some(format!("REDEFINES {}", name))),
    ]
}

/// Strategy for generating a simple field definition
pub fn field_definition_strategy() -> impl Strategy<Value = String> {
    (
        level_number_strategy(),
        field_name_strategy(),
        pic_clause_strategy(),
        occurs_strategy(),
        redefines_strategy(),
    ).prop_map(|(level, name, pic, occurs, redefines)| {
        let mut def = format!("      {:02}  {} PIC {}.", level, name, pic);
        if let Some(o) = occurs {
            def = format!("      {:02}  {} PIC {} {}.", level, name, pic, o);
        }
        if let Some(r) = redefines {
            def = format!("{} {}", def.trim_end_matches('.'), r);
        }
        def
    })
}

/// Strategy for generating a simple copybook
pub fn simple_copybook_strategy() -> impl Strategy<Value = String> {
    vec(field_definition_strategy(), 1..=10).prop_map(|fields| {
        let mut copybook = "       01  RECORD.\n".to_string();
        for field in fields {
            copybook.push_str(&field);
            copybook.push('\n');
        }
        copybook
    })
}

/// Strategy for generating ASCII zoned decimal data
pub fn ascii_zoned_strategy(length: usize) -> impl Strategy<Value = Vec<u8>> {
    vec(0u8..=9, length).prop_map(|digits| {
        digits.iter().map(|&d| b'0' + d).collect()
    })
}

/// Strategy for generating EBCDIC zoned decimal data
pub fn ebcdic_zoned_strategy(length: usize) -> impl Strategy<Value = Vec<u8>> {
    vec(0u8..=9, length).prop_map(|digits| {
        digits.iter().map(|&d| 0xF0 + d).collect()
    })
}

/// Strategy for generating signed ASCII zoned decimal data (with overpunch)
pub fn ascii_signed_zoned_strategy(length: usize) -> impl Strategy<Value = Vec<u8>> {
    (
        vec(0u8..=9, length.saturating_sub(1)),
        any::<bool>(),
    ).prop_map(|(mut digits, negative)| {
        let last_digit = digits.pop().unwrap_or(0);
        let overpunch = if negative {
            // Negative overpunch: p, q, r, s, t, u, v, w, x, y for 0-9
            match last_digit {
                0 => b'p', 1 => b'q', 2 => b'r', 3 => b's', 4 => b't',
                5 => b'u', 6 => b'v', 7 => b'w', 8 => b'x', _ => b'y',
            }
        } else {
            // Positive overpunch: {, A, B, C, D, E, F, G, H, I for 0-9
            match last_digit {
                0 => b'{', 1 => b'A', 2 => b'B', 3 => b'C', 4 => b'D',
                5 => b'E', 6 => b'F', 7 => b'G', 8 => b'H', _ => b'I',
            }
        };
        let mut result: Vec<u8> = digits.iter().map(|&d| b'0' + d).collect();
        result.push(overpunch);
        result
    })
}

/// Strategy for generating signed EBCDIC zoned decimal data (with overpunch)
pub fn ebcdic_signed_zoned_strategy(length: usize) -> impl Strategy<Value = Vec<u8>> {
    (
        vec(0u8..=9, length.saturating_sub(1)),
        any::<bool>(),
    ).prop_map(|(mut digits, negative)| {
        let last_digit = digits.pop().unwrap_or(0);
        let overpunch = if negative {
            // Negative overpunch: D0-D9 for 0-9
            0xD0 + last_digit
        } else {
            // Positive overpunch: C0-C9 for 0-9
            0xC0 + last_digit
        };
        let mut result: Vec<u8> = digits.iter().map(|&d| 0xF0 + d).collect();
        result.push(overpunch);
        result
    })
}

/// Strategy for generating COMP-3 packed decimal data
pub fn comp3_strategy(total_digits: usize, decimal_places: usize) -> impl Strategy<Value = Vec<u8>> {
    (
        vec(0u8..=9, total_digits.saturating_sub(decimal_places)),
        vec(0u8..=9, decimal_places),
        any::<bool>(),
    ).prop_map(|(int_part, dec_part, negative)| {
        let mut result = Vec::new();
        for digit in int_part {
            result.push(digit);
        }
        for digit in dec_part {
            result.push(digit);
        }
        let sign_nibble = if negative { 0x0D } else { 0x0C };
        let last_byte = result.pop().unwrap_or(0);
        result.push((last_byte << 4) | sign_nibble);
        result
    })
}

/// Strategy for generating alphanumeric ASCII data
pub fn alphanumeric_ascii_strategy(length: usize) -> impl Strategy<Value = Vec<u8>> {
    vec(0x20u8..=0x7E, length) // Printable ASCII
}

/// Strategy for generating alphanumeric EBCDIC data
pub fn alphanumeric_ebcdic_strategy(length: usize) -> impl Strategy<Value = Vec<u8>> {
    vec(0x40u8..=0xFF, length) // Printable EBCDIC
}

/// Strategy for generating JSON data for encoding
pub fn json_value_strategy() -> impl Strategy<Value = serde_json::Value> {
    prop_oneof![
        // String values
        "[A-Za-z0-9 ]{1,50}".prop_map(serde_json::Value::String),
        // Numeric values
        any::<i64>().prop_map(|n| serde_json::json!(n)),
        // Boolean values
        any::<bool>().prop_map(serde_json::Value::Bool),
        // Null
        Just(serde_json::Value::Null),
    ]
}

/// Strategy for generating Level-88 condition values
pub fn level_88_strategy() -> impl Strategy<Value = Option<String>> {
    prop_oneof![
        // No Level-88
        Just(None),
        // Single value
        (
            field_name_strategy(),
            "[A-Z0-9]{1,10}".prop_map(|s| s.to_string())
        ).prop_map(|(name, value)| Some(format!(
            "                88  {} VALUE '{}'.", name, value
        ))),
        // Multiple values
        (
            field_name_strategy(),
            vec("[A-Z0-9]{1,10}", 2..=5).prop_map(|vals| {
                vals.join(" ")
            })
        ).prop_map(|(name, values)| Some(format!(
            "                88  {} VALUE {}.", name, values
        ))),
    ]
}

/// Strategy for generating a field with optional Level-88 conditions
pub fn field_with_level_88_strategy() -> impl Strategy<Value = String> {
    (
        level_number_strategy(),
        field_name_strategy(),
        pic_clause_strategy(),
        level_88_strategy(),
    ).prop_map(|(level, name, pic, level_88)| {
        let mut result = format!("      {:02}  {} PIC {}.\n", level, name, pic);
        if let Some(l88) = level_88 {
            result.push_str(&l88);
            result.push('\n');
        }
        result
    })
}

/// Strategy for generating ODO (OCCURS DEPENDING ON) structures
pub fn odo_strategy() -> impl Strategy<Value = String> {
    (
        field_name_strategy(),
        "[1-9][0-9]{0,2}".prop_map(|s| s.parse::<usize>().unwrap()),
        field_name_strategy(),
        field_name_strategy(),
        pic_clause_strategy(),
    ).prop_map(|(array_name, max_count, count_field, element_name, pic)| {
        format!(
            "      05  {} PIC 9(3).\n\
             05  {} OCCURS 1 TO {} TIMES DEPENDING ON {}.\n\
             10  {} PIC {}.",
            count_field, array_name, max_count, count_field, element_name, pic
        )
    })
}

/// Strategy for generating REDEFINES configurations
pub fn redefines_config_strategy() -> impl Strategy<Value = String> {
    (
        field_name_strategy(),
        pic_clause_strategy(),
        field_name_strategy(),
        pic_clause_strategy(),
    ).prop_map(|(base_name, base_pic, redef_name, redef_pic)| {
        format!(
            "      05  {} PIC {}.\n\
             05  {} PIC {} REDEFINES {}.",
            base_name, base_pic, redef_name, redef_pic, base_name
        )
    })
}

/// Strategy for generating copybook with Level-88 conditions
pub fn copybook_with_level_88_strategy() -> impl Strategy<Value = String> {
    vec(field_with_level_88_strategy(), 1..=10).prop_map(|fields| {
        let mut copybook = "       01  RECORD.\n".to_string();
        for field in fields {
            copybook.push_str(&field);
        }
        copybook
    })
}

/// Strategy for generating copybook with ODO structures
pub fn copybook_with_odo_strategy() -> impl Strategy<Value = String> {
    (
        simple_copybook_strategy(),
        prop::collection::vec(odo_strategy(), 1..=3),
    ).prop_map(|(base, odo_fields)| {
        let mut copybook = base;
        for odo in odo_fields {
            copybook.push_str("\n");
            copybook.push_str(&odo);
        }
        copybook
    })
}

/// Strategy for generating copybook with REDEFINES
pub fn copybook_with_redefines_strategy() -> impl Strategy<Value = String> {
    (
        simple_copybook_strategy(),
        prop::collection::vec(redefines_config_strategy(), 1..=3),
    ).prop_map(|(base, redefines)| {
        let mut copybook = base;
        for redef in redefines {
            copybook.push_str("\n");
            copybook.push_str(&redef);
        }
        copybook
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pic_clause_generation() {
        let mut runner = TestRunner::default();
        runner.run(&pic_clause_strategy(), |pic| {
            assert!(!pic.is_empty());
            Ok(())
        }).unwrap();
    }

    #[test]
    fn test_field_name_generation() {
        let mut runner = TestRunner::default();
        runner.run(&field_name_strategy(), |name| {
            assert!(name.chars().next().unwrap().is_ascii_uppercase());
            assert!(name.len() <= 30);
            Ok(())
        }).unwrap();
    }

    #[test]
    fn test_simple_copybook_generation() {
        let mut runner = TestRunner::default();
        runner.run(&simple_copybook_strategy(), |copybook| {
            assert!(copybook.contains("01  RECORD."));
            Ok(())
        }).unwrap();
    }

    #[test]
    fn test_ascii_zoned_generation() {
        let mut runner = TestRunner::default();
        runner.run(&ascii_zoned_strategy(5), |data| {
            assert_eq!(data.len(), 5);
            for byte in data {
                assert!(byte.is_ascii_digit());
            }
            Ok(())
        }).unwrap();
    }

    #[test]
    fn test_ebcdic_zoned_generation() {
        let mut runner = TestRunner::default();
        runner.run(&ebcdic_zoned_strategy(5), |data| {
            assert_eq!(data.len(), 5);
            for byte in data {
                assert!(byte >= 0xF0 && byte <= 0xF9);
            }
            Ok(())
        }).unwrap();
    }
}
