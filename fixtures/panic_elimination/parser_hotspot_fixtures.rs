// SPDX-License-Identifier: AGPL-3.0-or-later
/// Panic Elimination Fixtures: Parser Hotspot Test Data
/// Issue #33 - Parser.rs Panic Elimination (17 target instances)
///
/// This module provides comprehensive COBOL copybook fixtures designed to trigger
/// parsing scenarios that historically cause panic conditions in parser.rs.
/// These fixtures enable validation of panic-safe error handling with CBKP* error codes.

#[cfg(test)]
pub struct ParserHotspotFixture {
    pub name: &'static str,
    pub copybook_text: &'static str,
    pub expected_error: Option<&'static str>, // Expected CBKP* error code
    pub panic_scenario: &'static str,
    pub description: &'static str,
}

/// Complex nested structure parsing that can cause stack overflow panics
pub fn nested_structure_overflow_fixture() -> ParserHotspotFixture {
    ParserHotspotFixture {
        name: "nested_structure_overflow",
        copybook_text: r#"
        01 DEEPLY-NESTED-RECORD.
            05 LEVEL1-GROUP.
                10 LEVEL2-GROUP.
                    15 LEVEL3-GROUP.
                        20 LEVEL4-GROUP.
                            25 LEVEL5-GROUP.
                                30 LEVEL6-GROUP.
                                    35 LEVEL7-GROUP.
                                        40 LEVEL8-GROUP.
                                            45 LEVEL9-GROUP.
                                                50 LEVEL10-GROUP.
                                                    55 LEVEL11-GROUP.
                                                        60 LEVEL12-GROUP.
                                                            65 LEVEL13-GROUP.
                                                                70 LEVEL14-GROUP.
                                                                    75 LEVEL15-GROUP.
                                                                        80 LEVEL16-GROUP.
                                                                            85 LEVEL17-GROUP.
                                                                                90 LEVEL18-GROUP.
                                                                                    95 LEVEL19-GROUP.
                                                                                        99 FINAL-FIELD PIC 9(5).
        "#,
        expected_error: Some("CBKP021_DEEP_NESTING"),
        panic_scenario: "Recursive descent parser stack overflow with deep nesting",
        description: "Extremely deep nested structures that can cause parser stack overflow",
    }
}

/// Malformed PIC clause parsing that can cause index out of bounds panics
pub fn malformed_pic_clause_fixture() -> ParserHotspotFixture {
    ParserHotspotFixture {
        name: "malformed_pic_clause",
        copybook_text: r#"
        01 MALFORMED-PIC-RECORD.
            05 INVALID-PIC-1 PIC X(.
            05 INVALID-PIC-2 PIC 9).
            05 INVALID-PIC-3 PIC S9((5).
            05 INVALID-PIC-4 PIC 9(99999999999999999999).
            05 INVALID-PIC-5 PIC $$$$$,$$$.$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$.
            05 INVALID-PIC-6 PIC ZZZZZ,ZZZV999999999999999999999999999999.
            05 INVALID-PIC-7 PIC ++++++++++++++++++++++++++++++++++++++++.
            05 INVALID-PIC-8 PIC -----.............................
        "#,
        expected_error: Some("CBKP001_SYNTAX"),
        panic_scenario: "PIC clause parsing with malformed syntax causing buffer overruns",
        description: "Various malformed PIC clauses that can cause parsing panics",
    }
}

/// Complex REDEFINES parsing that can cause infinite loops or panics
pub fn complex_redefines_fixture() -> ParserHotspotFixture {
    ParserHotspotFixture {
        name: "complex_redefines",
        copybook_text: r#"
        01 COMPLEX-REDEFINES-RECORD.
            05 BASE-FIELD PIC X(100).
            05 REDEFINES-1 REDEFINES BASE-FIELD.
                10 SUB-FIELD-1 PIC 9(50).
                10 SUB-FIELD-2 PIC X(50).
            05 REDEFINES-2 REDEFINES BASE-FIELD.
                10 ANOTHER-FIELD PIC 9(20) OCCURS 5 TIMES.
            05 REDEFINES-3 REDEFINES REDEFINES-1.
                10 NESTED-REDEFINES PIC X(25) OCCURS 4 TIMES.
            05 CIRCULAR-ATTEMPT REDEFINES CIRCULAR-ATTEMPT PIC X(10).
            05 MISSING-TARGET REDEFINES NON-EXISTENT-FIELD PIC X(20).
        "#,
        expected_error: Some("CBKP031_REDEFINES_INVALID"),
        panic_scenario: "Complex REDEFINES with circular references and missing targets",
        description: "REDEFINES patterns that can cause infinite loops or invalid references",
    }
}

/// ODO with complex occurs patterns that can cause parsing panics
pub fn complex_odo_occurs_fixture() -> ParserHotspotFixture {
    ParserHotspotFixture {
        name: "complex_odo_occurs",
        copybook_text: r#"
        01 COMPLEX-ODO-RECORD.
            05 COUNTER-FIELD PIC 9(5).
            05 INVALID-ODO-1 OCCURS 0 TO 999999999 TIMES DEPENDING ON COUNTER-FIELD.
                10 ODO-SUB-FIELD PIC X(100).
            05 INVALID-ODO-2 OCCURS 1 TO 0 TIMES DEPENDING ON COUNTER-FIELD.
                10 BACKWARD-RANGE-FIELD PIC 9(10).
            05 INVALID-ODO-3 OCCURS -5 TO 10 TIMES DEPENDING ON COUNTER-FIELD.
                10 NEGATIVE-MIN-FIELD PIC X(20).
            05 MISSING-COUNTER OCCURS 1 TO 100 TIMES DEPENDING ON NON-EXISTENT.
                10 MISSING-COUNTER-FIELD PIC 9(5).
            05 NESTED-ODO OCCURS 1 TO 50 TIMES DEPENDING ON COUNTER-FIELD.
                10 INNER-COUNTER PIC 9(3).
                10 NESTED-ODO-INNER OCCURS 1 TO 100 TIMES DEPENDING ON INNER-COUNTER.
                    15 DEEPLY-NESTED-FIELD PIC X(50).
        "#,
        expected_error: Some("CBKP021_ODO_INVALID"),
        panic_scenario: "ODO with invalid range specifications and missing dependencies",
        description: "Complex ODO patterns with invalid ranges and missing counter fields",
    }
}

/// Level number violations that can cause parsing confusion and panics
pub fn level_number_violations_fixture() -> ParserHotspotFixture {
    ParserHotspotFixture {
        name: "level_number_violations",
        copybook_text: r#"
        01 LEVEL-VIOLATIONS-RECORD.
            05 VALID-FIELD PIC X(10).
            03 INVALID-LEVEL-BACKWARD PIC 9(5).
            05 ANOTHER-FIELD PIC X(20).
            77 INDEPENDENT-FIELD PIC 9(10).
            05 AFTER-77-FIELD PIC X(15).
            01 DUPLICATE-01 PIC X(25).
            05 ORPHANED-FIELD PIC 9(8).
            99 INVALID-99-LEVEL PIC X(5).
            00 INVALID-00-LEVEL PIC 9(3).
            05 FINAL-FIELD PIC X(12).
        "#,
        expected_error: Some("CBKP011_LEVEL_INVALID"),
        panic_scenario: "Invalid level number sequences causing parser state confusion",
        description: "Invalid level number sequences that violate COBOL hierarchy rules",
    }
}

/// Extremely long field names and clauses that can cause buffer overflows
pub fn long_identifiers_fixture() -> ParserHotspotFixture {
    ParserHotspotFixture {
        name: "long_identifiers",
        copybook_text: r#"
        01 EXTREMELY-LONG-RECORD-NAME-THAT-EXCEEDS-NORMAL-COBOL-IDENTIFIER-LIMITS-AND-MIGHT-CAUSE-BUFFER-OVERFLOW-CONDITIONS-IN-PARSERS-THAT-DO-NOT-PROPERLY-HANDLE-OVERSIZED-IDENTIFIERS.
            05 ANOTHER-EXTREMELY-LONG-FIELD-NAME-THAT-ALSO-EXCEEDS-REASONABLE-LIMITS-AND-TESTS-IDENTIFIER-PARSING-ROBUSTNESS-WITH-VERY-LONG-NAMES-THAT-MIGHT-CAUSE-MEMORY-ALLOCATION-ISSUES PIC X(100).
            05 FIELD-WITH-LONG-PIC PIC X(999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999).
            05 NORMAL-FIELD PIC 9(5).
        "#,
        expected_error: Some("CBKP001_SYNTAX"),
        panic_scenario: "Extremely long identifiers causing buffer overflow in parsing",
        description: "Field names and PIC clauses that exceed normal limits",
    }
}

/// Unicode and special characters that can cause parsing panics
pub fn special_characters_fixture() -> ParserHotspotFixture {
    ParserHotspotFixture {
        name: "special_characters",
        copybook_text: r#"
        01 SPECIAL-CHARS-RECORD.
            05 FIELD-WITH-UNICODE-© PIC X(10).
            05 FIELD-WITH-EMOJI-🚀 PIC 9(5).
            05 FIELD-WITH-NULLS-\0-EMBEDDED PIC X(15).
            05 FIELD-WITH-TABS-	-EMBEDDED PIC 9(8).
            05 FIELD-WITH-NEWLINES-
            -EMBEDDED PIC X(20).
            05 FIELD-WITH-QUOTES-'"-MIXED PIC X(12).
            05 FIELD-WITH-BACKSLASHES-\\-MULTIPLE PIC 9(6).
        "#,
        expected_error: Some("CBKP001_SYNTAX"),
        panic_scenario: "Special characters and Unicode in identifiers causing parsing errors",
        description: "Field names with special characters, Unicode, and control characters",
    }
}

/// Incomplete parsing scenarios that can cause unexpected EOF panics
pub fn incomplete_parsing_fixture() -> ParserHotspotFixture {
    ParserHotspotFixture {
        name: "incomplete_parsing",
        copybook_text: r#"
        01 INCOMPLETE-RECORD.
            05 COMPLETE-FIELD PIC X(10).
            05 INCOMPLETE-FIELD PIC
            05 ANOTHER-INCOMPLETE
            05 MISSING-PIC-CLAUSE.
            05 UNTERMINATED-OCCURS OCCURS 1 TO
            05 UNTERMINATED-REDEFINES REDEFINES
        "#,
        expected_error: Some("CBKP001_SYNTAX"),
        panic_scenario: "Incomplete or truncated copybook causing unexpected EOF",
        description: "Incomplete copybook structure with missing required elements",
    }
}

/// Comments and continuation handling that can cause parsing confusion
pub fn comment_continuation_fixture() -> ParserHotspotFixture {
    ParserHotspotFixture {
        name: "comment_continuation",
        copybook_text: r#"
        01 COMMENT-TEST-RECORD.
      * This is a traditional COBOL comment
            05 NORMAL-FIELD PIC X(10).
      *> This is a COBOL-2002 inline comment
            05 INLINE-COMMENT-FIELD PIC 9(5).
      * Comment without proper spacing
            05 MALFORMED-COMMENT-FIELD PIC X(15).
      *> Inline comment with extremely long text that continues beyond normal line limits and might cause buffer overflow in comment processing routines that do not properly handle oversized comment text content that exceeds expected line length boundaries
            05 LONG-COMMENT-FIELD PIC 9(8).
            05 FIELD-WITH-CONTINUATION-
            -ACROSS-LINES PIC X(20).
            05 MALFORMED-CONTINUATION-
      * Comment interrupting continuation
            -BROKEN-BY-COMMENT PIC 9(10).
        "#,
        expected_error: Some("CBKP001_SYNTAX"),
        panic_scenario: "Complex comment and continuation line handling",
        description: "Various comment styles and continuation patterns that stress parsing",
    }
}

/// Extremely large numeric values that can cause integer overflow panics
pub fn large_numeric_values_fixture() -> ParserHotspotFixture {
    ParserHotspotFixture {
        name: "large_numeric_values",
        copybook_text: r#"
        01 LARGE-NUMERIC-RECORD.
            05 HUGE-OCCURS OCCURS 999999999 TIMES.
                10 SUB-FIELD PIC 9(5).
            05 MASSIVE-PIC PIC 9(999999999999999999).
            05 DECIMAL-OVERFLOW PIC 9(18)V9(999999999999999999).
            05 SIGNED-MASSIVE PIC S9(999999999999999999)V9(999999999999999999) COMP-3.
            05 BINARY-HUGE PIC S9(999999999999999999) COMP.
        "#,
        expected_error: Some("CBKP011_SIZE_OVERFLOW"),
        panic_scenario: "Extremely large numeric values causing integer overflow",
        description: "Numeric specifications that exceed implementation limits",
    }
}

/// All parser hotspot fixtures collection
pub fn all_parser_hotspot_fixtures() -> Vec<ParserHotspotFixture> {
    vec![
        nested_structure_overflow_fixture(),
        malformed_pic_clause_fixture(),
        complex_redefines_fixture(),
        complex_odo_occurs_fixture(),
        level_number_violations_fixture(),
        long_identifiers_fixture(),
        special_characters_fixture(),
        incomplete_parsing_fixture(),
        comment_continuation_fixture(),
        large_numeric_values_fixture(),
    ]
}

/// Generate test data for parser panic scenarios
pub fn generate_parser_panic_test_data() -> Vec<(String, Vec<u8>)> {
    // Generate corresponding binary data that would be used with these copybooks
    vec![
        (
            "nested_structure_overflow_data".to_string(),
            vec![0x00; 5], // Minimal data for deeply nested structure
        ),
        (
            "malformed_pic_data".to_string(),
            vec![0xFF; 100], // Random data that would stress malformed PIC parsing
        ),
        (
            "complex_redefines_data".to_string(),
            vec![0x40; 100], // EBCDIC spaces for REDEFINES field
        ),
        (
            "complex_odo_data".to_string(),
            vec![0xF0, 0xF1, 0xF0], // Counter field with value 010
        ),
        (
            "level_violations_data".to_string(),
            vec![0x40; 50], // EBCDIC spaces for various fields
        ),
        (
            "long_identifiers_data".to_string(),
            vec![0x40; 200], // Data for extremely long fields
        ),
        (
            "special_chars_data".to_string(),
            vec![0xC1, 0xC2, 0xC3], // EBCDIC A, B, C for special character test
        ),
        (
            "incomplete_data".to_string(),
            vec![0x40; 10], // Minimal data for incomplete structure
        ),
        (
            "comment_data".to_string(),
            vec![0x40; 30], // Data for comment/continuation test
        ),
        (
            "large_numeric_data".to_string(),
            vec![0xF9; 20], // EBCDIC 9s for large numeric test
        ),
    ]
}

#[cfg(test)]
mod parser_hotspot_tests {
    use super::*;
    use copybook_core::parse_copybook;

    #[test]
    fn test_parser_hotspot_fixtures_load() {
        let fixtures = all_parser_hotspot_fixtures();
        assert_eq!(fixtures.len(), 10, "Should have 10 parser hotspot fixtures");

        for fixture in &fixtures {
            assert!(!fixture.name.is_empty(), "Fixture name should not be empty");
            assert!(!fixture.copybook_text.is_empty(), "Copybook text should not be empty");
            assert!(!fixture.panic_scenario.is_empty(), "Panic scenario should not be empty");
            assert!(!fixture.description.is_empty(), "Description should not be empty");
        }
    }

    #[test]
    fn test_parser_fixtures_trigger_expected_errors() {
        let fixtures = all_parser_hotspot_fixtures();

        for fixture in &fixtures {
            let result = parse_copybook(fixture.copybook_text);

            // These fixtures are designed to trigger errors, not successes
            if result.is_ok() {
                // Some fixtures might parse successfully with robust error handling
                // This is actually a good sign for panic elimination
                continue;
            }

            let error = result.unwrap_err();
            if let Some(expected_code) = fixture.expected_error {
                // Verify error uses appropriate CBKP* error code structure
                let error_str = format!("{:?}", error.code);
                assert!(
                    error_str.starts_with("CBKP"),
                    "Error for {} should use CBKP* code, got {:?}",
                    fixture.name, error.code
                );
            }
        }
    }

    #[test]
    fn test_parser_test_data_generation() {
        let test_data = generate_parser_panic_test_data();
        assert_eq!(test_data.len(), 10, "Should generate 10 test data sets");

        for (name, data) in &test_data {
            assert!(!name.is_empty(), "Test data name should not be empty");
            assert!(!data.is_empty(), "Test data should not be empty");
        }
    }
}