//! COBOL Parsing Edge Case Fixtures for copybook-core Panic Elimination
//!
//! This module provides comprehensive test fixtures for eliminating 63 .unwrap()/.expect() calls
//! in copybook-core production code. Tests target parser.rs (16), layout.rs (8), pic.rs (7),
//! and audit modules (32) with enterprise-safe error handling patterns.
//!
//! **Issue #63 - AC Traceability:**
//! - AC1: Complete elimination of 63 .unwrap()/.expect() calls in copybook-core
//! - AC2: Zero breaking changes to existing public APIs
//! - AC3: Integration with CBKP*/CBKS*/CBKD*/CBKE* error taxonomy
//! - AC4: Performance impact <5% on enterprise benchmarks
//! - AC7: Comprehensive test coverage with // AC:ID tags
//! - AC10: Memory safety preserved with zero unsafe code

use std::sync::LazyLock;

/// Parser State Edge Cases - Token stream bounds, position management, stack operations
pub struct ParserEdgeCaseFixture {
    pub copybook_text: &'static str,
    pub description: &'static str,
    pub expected_error_pattern: Option<&'static str>,
    pub panic_scenario: &'static str,
    pub ac_tag: &'static str,
}

/// Schema Validation Edge Cases - Field layout, ODO counters, REDEFINES resolution
pub struct SchemaValidationFixture {
    pub copybook_text: &'static str,
    pub description: &'static str,
    pub expected_error_code: &'static str,
    pub validation_scenario: &'static str,
    pub ac_tag: &'static str,
}

/// PIC Clause Edge Cases - Character access, digit parsing, size validation
pub struct PicClauseFixture {
    pub copybook_text: &'static str,
    pub description: &'static str,
    pub pic_clause: &'static str,
    pub edge_case_type: &'static str,
    pub ac_tag: &'static str,
}

/// Enterprise Audit Edge Cases - Complex structures, metadata validation
pub struct AuditStructureFixture {
    pub copybook_text: &'static str,
    pub description: &'static str,
    pub audit_complexity: &'static str,
    pub performance_target_ms: u64,
    pub ac_tag: &'static str,
}

/// COBOL Parsing Edge Case Test Data
pub static PARSER_EDGE_CASES: LazyLock<Vec<ParserEdgeCaseFixture>> = LazyLock::new(|| {
    vec![
        // AC:63-1-1 - Token access bounds checking
        ParserEdgeCaseFixture {
            copybook_text: "",
            description: "Empty copybook causing token access beyond bounds",
            expected_error_pattern: Some("CBKP001_SYNTAX"),
            panic_scenario: "Parser token access with empty token stream",
            ac_tag: "AC:63-1-1",
        },
        ParserEdgeCaseFixture {
            copybook_text: "   \n\n  \t  ",
            description: "Whitespace-only copybook with no tokens",
            expected_error_pattern: Some("CBKP001_SYNTAX"),
            panic_scenario: "Parser token access with whitespace-only input",
            ac_tag: "AC:63-1-1",
        },

        // AC:63-1-2 - Parser position bounds safety
        ParserEdgeCaseFixture {
            copybook_text: "01 INCOMPLETE",
            description: "Incomplete field definition causing position overflow",
            expected_error_pattern: Some("CBKP001_SYNTAX"),
            panic_scenario: "Parser position advance beyond token bounds",
            ac_tag: "AC:63-1-2",
        },
        ParserEdgeCaseFixture {
            copybook_text: "01 FIELD PIC",
            description: "Truncated PIC clause causing lookahead overflow",
            expected_error_pattern: Some("CBKP001_SYNTAX"),
            panic_scenario: "Parser position advance beyond available tokens",
            ac_tag: "AC:63-1-2",
        },

        // AC:63-1-3 - Parser stack underflow protection
        ParserEdgeCaseFixture {
            copybook_text: "01 FIELD PIC X((10)).",
            description: "Nested parentheses causing stack operations stress",
            expected_error_pattern: None, // May parse successfully with error recovery
            panic_scenario: "Parser stack operations with complex nesting",
            ac_tag: "AC:63-1-3",
        },
        ParserEdgeCaseFixture {
            copybook_text: "01 FIELD PIC )(X(10.",
            description: "Malformed parentheses causing stack underflow",
            expected_error_pattern: Some("CBKP001_SYNTAX"),
            panic_scenario: "Parser stack underflow on malformed input",
            ac_tag: "AC:63-1-3",
        },

        // AC:63-1-4 - Parser lookahead bounds safety
        ParserEdgeCaseFixture {
            copybook_text: "01 FIELD PIC X",
            description: "Missing PIC size causing lookahead beyond stream",
            expected_error_pattern: None, // May parse with default
            panic_scenario: "Parser lookahead beyond token stream end",
            ac_tag: "AC:63-1-4",
        },
        ParserEdgeCaseFixture {
            copybook_text: "01 ROOT.\n    05",
            description: "Incomplete level number causing lookahead overflow",
            expected_error_pattern: Some("CBKP001_SYNTAX"),
            panic_scenario: "Parser lookahead with incomplete structure",
            ac_tag: "AC:63-1-4",
        },

        // AC:63-1-5 - Parser recursion depth protection
        ParserEdgeCaseFixture {
            copybook_text: &format!("01 ROOT.\n{}",
                (1..=100).map(|i| format!("    {:02} LEVEL-{} PIC X(1).", 5 + i, i))
                         .collect::<Vec<_>>().join("\n")),
            description: "Deeply nested structure stressing parser recursion (100 levels)",
            expected_error_pattern: None, // Should handle gracefully
            panic_scenario: "Parser recursion depth with deep structure",
            ac_tag: "AC:63-1-5",
        },

        // Extreme parsing stress cases
        ParserEdgeCaseFixture {
            copybook_text: "01\n02\n03\n04\n05\n06\n07\n08\n09\n10\n",
            description: "Sequential level numbers without field names",
            expected_error_pattern: Some("CBKP001_SYNTAX"),
            panic_scenario: "Parser field name resolution with missing identifiers",
            ac_tag: "AC:63-1-6",
        },
        ParserEdgeCaseFixture {
            copybook_text: "01 FIELD-" + &"X".repeat(1000) + " PIC X(10).",
            description: "Extremely long field name (1000+ characters)",
            expected_error_pattern: None, // Should handle long identifiers
            panic_scenario: "Parser identifier handling with extreme lengths",
            ac_tag: "AC:63-1-7",
        },
    ]
});

/// Schema Validation Edge Cases for Layout Calculation Safety
pub static SCHEMA_VALIDATION_CASES: LazyLock<Vec<SchemaValidationFixture>> = LazyLock::new(|| {
    vec![
        // AC:63-2-1 - Field index bounds safety
        SchemaValidationFixture {
            copybook_text: r"
            01 RECORD.
                05 FIELD-A PIC X(10).
                05 FIELD-B PIC X(20).
                05 FIELD-C PIC X(30).
            ",
            description: "Multiple fields for index bounds testing",
            expected_error_code: "SUCCESS", // Should parse successfully
            validation_scenario: "Field index access beyond bounds protection",
            ac_tag: "AC:63-2-1",
        },

        // AC:63-2-2 - REDEFINES target resolution safety
        SchemaValidationFixture {
            copybook_text: r"
            01 RECORD.
                05 FIELD-A PIC X(10).
                05 FIELD-B REDEFINES NONEXISTENT PIC 9(10).
            ",
            description: "REDEFINES with missing target causing lookup failure",
            expected_error_code: "CBKP001_SYNTAX",
            validation_scenario: "REDEFINES target resolution with missing reference",
            ac_tag: "AC:63-2-2",
        },
        SchemaValidationFixture {
            copybook_text: r"
            01 RECORD.
                05 FIELD-A PIC X(10).
                05 FIELD-B REDEFINES FIELD-A PIC X(5).
                05 FIELD-C REDEFINES FIELD-B PIC 9(10).
            ",
            description: "Chained REDEFINES causing complex resolution",
            expected_error_code: "SUCCESS", // Should handle chained REDEFINES
            validation_scenario: "Complex REDEFINES chain resolution",
            ac_tag: "AC:63-2-2",
        },

        // AC:63-2-3 - Field offset calculation overflow protection
        SchemaValidationFixture {
            copybook_text: r"
            01 LARGE-RECORD.
                05 HUGE-FIELD-1 PIC X(32767).
                05 HUGE-FIELD-2 PIC X(32767).
                05 HUGE-FIELD-3 PIC X(32767).
            ",
            description: "Large fields that could cause offset overflow",
            expected_error_code: "SUCCESS", // Should handle large offsets safely
            validation_scenario: "Field offset calculation with potential overflow",
            ac_tag: "AC:63-2-3",
        },
        SchemaValidationFixture {
            copybook_text: r"
            01 EXTREME-RECORD.
                05 MASSIVE-FIELD PIC X(65535).
            ",
            description: "Maximum size field testing offset bounds",
            expected_error_code: "SUCCESS",
            validation_scenario: "Maximum field size offset calculation",
            ac_tag: "AC:63-2-3",
        },

        // AC:63-2-4 - ODO counter field lookup safety
        SchemaValidationFixture {
            copybook_text: r"
            01 RECORD.
                05 ARRAY-DATA OCCURS 1 TO 100 TIMES DEPENDING ON MISSING-COUNTER.
                    10 ITEM PIC X(5).
            ",
            description: "ODO with invalid counter field causing lookup failure",
            expected_error_code: "CBKS121_COUNTER_NOT_FOUND",
            validation_scenario: "ODO counter field lookup with missing reference",
            ac_tag: "AC:63-2-4",
        },
        SchemaValidationFixture {
            copybook_text: r"
            01 RECORD.
                05 COUNT PIC 9(3).
                05 ARRAY-DATA OCCURS 1 TO 100 TIMES DEPENDING ON COUNT.
                    10 ITEM PIC X(5).
                05 ANOTHER-FIELD PIC X(10).
            ",
            description: "ODO not at tail position causing validation error",
            expected_error_code: "CBKP021_ODO_NOT_TAIL",
            validation_scenario: "ODO tail position validation",
            ac_tag: "AC:63-2-4",
        },

        // AC:63-2-5 - Schema validation bounds checking
        SchemaValidationFixture {
            copybook_text: r"
            01 COMPLEX-RECORD.
                05 HEADER.
                    10 ID PIC 9(10).
                    10 COUNT PIC 9(3).
                05 DATA-ARRAY OCCURS 1 TO 999 TIMES DEPENDING ON COUNT.
                    10 ITEM-ID PIC 9(5).
                    10 ITEM-DATA PIC X(100).
                    10 ITEM-FLAGS.
                        15 FLAG-A PIC X.
                        15 FLAG-B PIC X.
            ",
            description: "Complex schema with ODO, nested structures, and large arrays",
            expected_error_code: "SUCCESS",
            validation_scenario: "Complex schema validation with multiple structural elements",
            ac_tag: "AC:63-2-5",
        },
    ]
});

/// PIC Clause Edge Cases for Character and Digit Access Safety
pub static PIC_CLAUSE_CASES: LazyLock<Vec<PicClauseFixture>> = LazyLock::new(|| {
    vec![
        // AC:63-3-1 - PIC string character access safety
        PicClauseFixture {
            copybook_text: "01 FIELD PIC X",
            description: "Incomplete PIC clause without size specification",
            pic_clause: "X",
            edge_case_type: "Incomplete size specification",
            ac_tag: "AC:63-3-1",
        },
        PicClauseFixture {
            copybook_text: "01 FIELD PIC",
            description: "Truncated PIC clause missing type",
            pic_clause: "",
            edge_case_type: "Missing PIC type",
            ac_tag: "AC:63-3-1",
        },

        // AC:63-3-2 - PIC digit parsing safety
        PicClauseFixture {
            copybook_text: "01 FIELD PIC 9(ABC).",
            description: "Non-numeric characters in PIC size",
            pic_clause: "9(ABC)",
            edge_case_type: "Invalid digit characters in size",
            ac_tag: "AC:63-3-2",
        },
        PicClauseFixture {
            copybook_text: "01 FIELD PIC 9(999999999999999999999).",
            description: "Extremely large numeric size causing overflow",
            pic_clause: "9(999999999999999999999)",
            edge_case_type: "Size overflow in digit parsing",
            ac_tag: "AC:63-3-2",
        },

        // AC:63-3-3 - PIC parentheses parsing safety
        PicClauseFixture {
            copybook_text: "01 FIELD PIC X(10.",
            description: "Unmatched opening parenthesis",
            pic_clause: "X(10.",
            edge_case_type: "Missing closing parenthesis",
            ac_tag: "AC:63-3-3",
        },
        PicClauseFixture {
            copybook_text: "01 FIELD PIC X10).",
            description: "Unmatched closing parenthesis",
            pic_clause: "X10)",
            edge_case_type: "Missing opening parenthesis",
            ac_tag: "AC:63-3-3",
        },
        PicClauseFixture {
            copybook_text: "01 FIELD PIC X(((10))).",
            description: "Deeply nested parentheses",
            pic_clause: "X(((10)))",
            edge_case_type: "Nested parentheses parsing",
            ac_tag: "AC:63-3-3",
        },

        // AC:63-3-4 - PIC size validation safety
        PicClauseFixture {
            copybook_text: "01 FIELD PIC X(0).",
            description: "Zero size PIC clause",
            pic_clause: "X(0)",
            edge_case_type: "Zero size validation",
            ac_tag: "AC:63-3-4",
        },
        PicClauseFixture {
            copybook_text: "01 FIELD PIC 9().",
            description: "Empty parentheses in PIC clause",
            pic_clause: "9()",
            edge_case_type: "Empty size specification",
            ac_tag: "AC:63-3-4",
        },

        // AC:63-3-5 - PIC type validation safety
        PicClauseFixture {
            copybook_text: "01 FIELD PIC X(10) COMP.",
            description: "Invalid combination: alphanumeric with COMP",
            pic_clause: "X(10) COMP",
            edge_case_type: "Invalid type combination",
            ac_tag: "AC:63-3-5",
        },
        PicClauseFixture {
            copybook_text: "01 FIELD PIC 9(5)V99V99.",
            description: "Multiple decimal points in PIC clause",
            pic_clause: "9(5)V99V99",
            edge_case_type: "Multiple decimal indicators",
            ac_tag: "AC:63-3-5",
        },
        PicClauseFixture {
            copybook_text: "01 FIELD PIC SS9(5).",
            description: "Multiple sign indicators",
            pic_clause: "SS9(5)",
            edge_case_type: "Multiple sign indicators",
            ac_tag: "AC:63-3-5",
        },

        // Complex edited PIC clauses
        PicClauseFixture {
            copybook_text: "01 FIELD PIC $$$,$$9.99.",
            description: "Complex edited PIC with currency and formatting",
            pic_clause: "$$$,$$9.99",
            edge_case_type: "Complex edited PIC parsing",
            ac_tag: "AC:63-3-6",
        },
        PicClauseFixture {
            copybook_text: "01 FIELD PIC +ZZZ,ZZ9.99-.",
            description: "Complex signed edited PIC with zero suppression",
            pic_clause: "+ZZZ,ZZ9.99-",
            edge_case_type: "Complex sign and zero suppression",
            ac_tag: "AC:63-3-6",
        },
    ]
});

/// Enterprise Audit Structure Edge Cases
pub static AUDIT_STRUCTURE_CASES: LazyLock<Vec<AuditStructureFixture>> = LazyLock::new(|| {
    vec![
        // AC:63-4-1 - Audit event serialization safety
        AuditStructureFixture {
            copybook_text: r"
            01 AUDIT-EVENT-RECORD.
                05 EVENT-HEADER.
                    10 EVENT-ID PIC 9(15).
                    10 EVENT-TYPE PIC X(20).
                    10 TIMESTAMP PIC 9(14).
                    10 USER-ID PIC X(8).
                05 EVENT-PAYLOAD.
                    10 OPERATION-CODE PIC X(10).
                    10 TABLE-NAME PIC X(30).
                    10 RECORD-KEY PIC X(50).
                    10 BEFORE-IMAGE PIC X(1000).
                    10 AFTER-IMAGE PIC X(1000).
                05 EVENT-METADATA.
                    10 COMPLIANCE-FLAGS PIC X(20).
                    10 RETENTION-PERIOD PIC 9(4).
                    10 CLASSIFICATION PIC X(15).
            ",
            description: "Enterprise audit event structure for SOX compliance",
            audit_complexity: "Complex audit event with metadata",
            performance_target_ms: 10,
            ac_tag: "AC:63-4-1",
        },

        // AC:63-4-2 - Audit context creation safety
        AuditStructureFixture {
            copybook_text: r"
            01 AUDIT-CONTEXT-RECORD.
                05 CONTEXT-HEADER.
                    10 SESSION-ID PIC X(32).
                    10 TRANSACTION-ID PIC X(16).
                    10 REQUEST-ID PIC X(24).
                05 SECURITY-CONTEXT.
                    10 USER-ROLES OCCURS 10 TIMES.
                        15 ROLE-NAME PIC X(20).
                        15 ROLE-PERMISSIONS PIC X(100).
                    10 ACCESS-LEVEL PIC 9(2).
                    10 CLEARANCE-LEVEL PIC X(15).
                05 OPERATIONAL-CONTEXT.
                    10 SOURCE-SYSTEM PIC X(20).
                    10 TARGET-SYSTEM PIC X(20).
                    10 OPERATION-CATEGORY PIC X(30).
                    10 RISK-SCORE PIC 9(3).
            ",
            description: "Complex audit context with security and operational data",
            audit_complexity: "Multi-level security context",
            performance_target_ms: 15,
            ac_tag: "AC:63-4-2",
        },

        // AC:63-4-3 - Audit metadata validation safety
        AuditStructureFixture {
            copybook_text: r"
            01 METADATA-INTENSIVE-AUDIT.
                05 SCHEMA-METADATA.
                    10 SCHEMA-VERSION PIC 9(3).
                    10 SCHEMA-HASH PIC X(64).
                    10 VALIDATION-RULES OCCURS 50 TIMES.
                        15 RULE-ID PIC X(10).
                        15 RULE-TYPE PIC X(20).
                        15 RULE-EXPRESSION PIC X(200).
                05 BUSINESS-METADATA OCCURS 1 TO 100 TIMES DEPENDING ON SCHEMA-VERSION.
                    10 METADATA-KEY PIC X(50).
                    10 METADATA-VALUE PIC X(500).
                    10 METADATA-TYPE PIC X(20).
                    10 VALIDATION-STATUS PIC X.
            ",
            description: "Metadata-intensive structure with variable arrays",
            audit_complexity: "Metadata validation with ODO",
            performance_target_ms: 25,
            ac_tag: "AC:63-4-3",
        },

        // AC:63-4-4 - Audit performance metrics safety
        AuditStructureFixture {
            copybook_text: r"
            01 PERFORMANCE-AUDIT-RECORD.
                05 PERFORMANCE-HEADER.
                    10 MEASUREMENT-ID PIC X(20).
                    10 START-TIMESTAMP PIC 9(18).
                    10 END-TIMESTAMP PIC 9(18).
                05 RESOURCE-METRICS.
                    10 CPU-UTILIZATION PIC 9(5)V99.
                    10 MEMORY-USAGE PIC 9(12).
                    10 DISK-IO-BYTES PIC 9(15).
                    10 NETWORK-IO-BYTES PIC 9(15).
                05 OPERATION-METRICS OCCURS 1000 TIMES.
                    10 OPERATION-NAME PIC X(30).
                    10 OPERATION-COUNT PIC 9(10).
                    10 TOTAL-DURATION PIC 9(12).
                    10 AVG-DURATION PIC 9(8)V99.
                    10 MAX-DURATION PIC 9(8)V99.
                    10 ERROR-COUNT PIC 9(6).
            ",
            description: "Performance metrics structure with large arrays",
            audit_complexity: "Large-scale performance data",
            performance_target_ms: 50,
            ac_tag: "AC:63-4-4",
        },
    ]
});

/// Enterprise Integration Test Fixtures
pub static ENTERPRISE_INTEGRATION_CASES: LazyLock<Vec<ParserEdgeCaseFixture>> = LazyLock::new(|| {
    vec![
        // Comprehensive stress test for all 63 panic points
        ParserEdgeCaseFixture {
            copybook_text: r"
            01 COMPREHENSIVE-PANIC-TEST.
                05 PARSER-STRESS-SECTION.
                    10 COMPLEX-PIC PIC $$$,$$9.99.
                    10 INVALID-REFERENCE REDEFINES NONEXISTENT PIC X(10).
                    10 DEEP-NESTING.
                        15 LEVEL-A.
                            20 LEVEL-B.
                                25 LEVEL-C PIC X(1).
                05 LAYOUT-STRESS-SECTION.
                    10 HUGE-FIELD PIC X(10000).
                    10 ODO-ARRAY OCCURS 1 TO 1000 TIMES DEPENDING ON MISSING-COUNTER.
                        15 ODO-ITEM PIC 9(10).
                05 PIC-STRESS-SECTION.
                    10 ZERO-SIZE PIC X(0).
                    10 EXTREME-SIZE PIC 9(999).
                    10 INVALID-DIGITS PIC 9(ABC).
                05 AUDIT-STRESS-SECTION.
                    10 AUDIT-EVENT PIC X(1000).
                    10 METADATA-BLOB PIC X(5000).
            ",
            description: "Comprehensive stress test covering all 63 identified panic points",
            expected_error_pattern: Some("CBKP001_SYNTAX|CBKS121_COUNTER_NOT_FOUND"),
            panic_scenario: "Multi-module panic elimination validation",
            ac_tag: "AC:63-5-1",
        },

        // Banking industry production pattern
        ParserEdgeCaseFixture {
            copybook_text: r"
            01 BANKING-TRANSACTION-RECORD.
                05 TRANSACTION-HEADER.
                    10 TRANSACTION-ID PIC X(20).
                    10 ACCOUNT-NUMBER PIC 9(16).
                    10 TRANSACTION-TYPE PIC X(4).
                    10 TRANSACTION-DATE PIC 9(8).
                    10 TRANSACTION-TIME PIC 9(6).
                05 TRANSACTION-AMOUNTS.
                    10 AMOUNT-DOLLARS PIC S9(13)V99 COMP-3.
                    10 CURRENCY-CODE PIC X(3).
                    10 EXCHANGE-RATE PIC 9(7)V9999.
                05 ACCOUNT-BALANCES OCCURS 5 TIMES.
                    10 BALANCE-TYPE PIC X(10).
                    10 BALANCE-AMOUNT PIC S9(15)V99 COMP-3.
                    10 AVAILABLE-AMOUNT PIC S9(15)V99 COMP-3.
            ",
            description: "Banking transaction record with COMP-3 amounts and arrays",
            expected_error_pattern: None,
            panic_scenario: "Enterprise banking data processing",
            ac_tag: "AC:63-5-2",
        },
    ]
});

#[cfg(test)]
mod tests {
    use super::*;
    use copybook_core::parse_copybook;

    #[test]
    fn test_parser_edge_case_fixtures_load() {
        assert!(!PARSER_EDGE_CASES.is_empty(), "Parser edge cases should be loaded");
        assert!(PARSER_EDGE_CASES.len() >= 10, "Should have comprehensive parser edge cases");

        // Validate each fixture has required fields
        for fixture in PARSER_EDGE_CASES.iter() {
            assert!(!fixture.copybook_text.is_empty() || fixture.description.contains("Empty"),
                   "Fixture should have copybook text or be empty test");
            assert!(!fixture.description.is_empty(), "Fixture should have description");
            assert!(!fixture.panic_scenario.is_empty(), "Fixture should have panic scenario");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_schema_validation_fixtures_load() {
        assert!(!SCHEMA_VALIDATION_CASES.is_empty(), "Schema validation cases should be loaded");

        for fixture in SCHEMA_VALIDATION_CASES.iter() {
            assert!(!fixture.copybook_text.trim().is_empty(), "Fixture should have copybook text");
            assert!(!fixture.expected_error_code.is_empty(), "Fixture should have error code");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_pic_clause_fixtures_load() {
        assert!(!PIC_CLAUSE_CASES.is_empty(), "PIC clause cases should be loaded");

        for fixture in PIC_CLAUSE_CASES.iter() {
            assert!(!fixture.description.is_empty(), "Fixture should have description");
            assert!(!fixture.edge_case_type.is_empty(), "Fixture should have edge case type");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_audit_structure_fixtures_load() {
        assert!(!AUDIT_STRUCTURE_CASES.is_empty(), "Audit structure cases should be loaded");

        for fixture in AUDIT_STRUCTURE_CASES.iter() {
            assert!(!fixture.copybook_text.trim().is_empty(), "Fixture should have copybook text");
            assert!(fixture.performance_target_ms > 0, "Fixture should have performance target");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_fixture_parsing_safety() {
        // Test that all fixtures can be parsed without panicking
        let all_fixtures = PARSER_EDGE_CASES.iter()
            .chain(SCHEMA_VALIDATION_CASES.iter().map(|f| &ParserEdgeCaseFixture {
                copybook_text: f.copybook_text,
                description: f.description,
                expected_error_pattern: Some(f.expected_error_code),
                panic_scenario: f.validation_scenario,
                ac_tag: f.ac_tag,
            }))
            .chain(ENTERPRISE_INTEGRATION_CASES.iter());

        for fixture in all_fixtures {
            let result = std::panic::catch_unwind(|| {
                parse_copybook(fixture.copybook_text)
            });

            assert!(result.is_ok(),
                   "Fixture should not panic during parsing: {} - {}",
                   fixture.ac_tag, fixture.description);
        }
    }
}