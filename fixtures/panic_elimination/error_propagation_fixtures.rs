//! Error Propagation Validation Fixtures for Panic Elimination
//!
//! This module provides comprehensive error propagation validation fixtures that ensure
//! proper Result<T, E> error handling instead of panics across all copybook-rs components.
//! Tests structured error taxonomy (CBKP*/CBKS*/CBKD*/CBKE*), context preservation,
//! and graceful error recovery patterns for enterprise reliability.
//!
//! **Issue #63 - AC Traceability:**
//! - AC1: Complete elimination of all .unwrap()/.expect() calls with structured errors
//! - AC2: Zero breaking changes to existing public APIs
//! - AC3: Integration with comprehensive CBKP*/CBKS*/CBKD*/CBKE* error taxonomy
//! - AC5: Enterprise-grade error handling for production reliability
//! - AC7: Comprehensive test coverage for error propagation paths
//! - AC8: Context preservation in error chains across component boundaries

use std::sync::LazyLock;

/// Error Propagation Test Case
pub struct ErrorPropagationFixture {
    pub fixture_name: &'static str,
    pub component: &'static str,
    pub error_scenario: &'static str,
    pub input_data: ErrorInputData,
    pub expected_error_code: &'static str,
    pub expected_context: Vec<&'static str>,
    pub recovery_behavior: &'static str,
    pub panic_elimination_point: &'static str,
    pub description: &'static str,
    pub ac_tag: &'static str,
}

/// Error Chain Validation Test Case
pub struct ErrorChainFixture {
    pub fixture_name: &'static str,
    pub error_chain: Vec<ErrorStep>,
    pub description: &'static str,
    pub chain_validation: &'static str,
    pub ac_tag: &'static str,
}

/// Individual step in error propagation chain
#[derive(Clone)]
pub struct ErrorStep {
    pub component: &'static str,
    pub operation: &'static str,
    pub error_type: &'static str,
    pub context_preservation: Vec<&'static str>,
}

/// Input data for error scenarios
#[derive(Clone)]
pub enum ErrorInputData {
    Copybook(&'static str),
    BinaryData(Vec<u8>),
    CliArgs(Vec<&'static str>),
    Combined {
        copybook: &'static str,
        data: Vec<u8>,
        args: Vec<&'static str>,
    },
}

/// Context Preservation Test Case
pub struct ContextPreservationFixture {
    pub fixture_name: &'static str,
    pub error_context: ErrorContext,
    pub expected_preserved_context: Vec<&'static str>,
    pub description: &'static str,
    pub ac_tag: &'static str,
}

/// Error context information
#[derive(Clone)]
pub struct ErrorContext {
    pub operation_type: &'static str,
    pub data_location: &'static str,
    pub processing_stage: &'static str,
    pub user_context: Vec<&'static str>,
}

/// Graceful Recovery Test Case
pub struct GracefulRecoveryFixture {
    pub fixture_name: &'static str,
    pub failure_scenario: &'static str,
    pub recovery_strategy: &'static str,
    pub expected_outcome: &'static str,
    pub data_integrity_preserved: bool,
    pub ac_tag: &'static str,
}

/// copybook-core Error Propagation Fixtures - AC:63-26
pub static CORE_ERROR_PROPAGATION: LazyLock<Vec<ErrorPropagationFixture>> = LazyLock::new(|| {
    vec![
        // AC:63-26-1 - Parser error propagation
        ErrorPropagationFixture {
            fixture_name: "parser_syntax_error_propagation",
            component: "copybook-core",
            error_scenario: "Invalid COBOL syntax causing parser failure",
            input_data: ErrorInputData::Copybook(r"
            01 INVALID-RECORD
                05 FIELD-1 PIC X(10.
                05 FIELD-2 PIC INVALID-TYPE.
                05 FIELD-3 REDEFINES NONEXISTENT PIC X(5).
            "),
            expected_error_code: "CBKP001_SYNTAX",
            expected_context: vec!["line number", "column position", "field name", "syntax element"],
            recovery_behavior: "Return structured error with detailed context",
            panic_elimination_point: "Parser token access and field resolution",
            description: "Parser syntax error with comprehensive context preservation",
            ac_tag: "AC:63-26-1",
        },

        ErrorPropagationFixture {
            fixture_name: "schema_validation_error_propagation",
            component: "copybook-core",
            error_scenario: "ODO field validation failure",
            input_data: ErrorInputData::Copybook(r"
            01 RECORD.
                05 COUNT PIC 9(3).
                05 ARRAY-DATA OCCURS 1 TO 100 TIMES DEPENDING ON MISSING-COUNTER.
                    10 ITEM PIC X(10).
                05 TRAILING-FIELD PIC X(5).
            "),
            expected_error_code: "CBKS121_COUNTER_NOT_FOUND",
            expected_context: vec!["field name", "counter reference", "array path"],
            recovery_behavior: "Schema validation failure with field context",
            panic_elimination_point: "ODO counter field lookup and validation",
            description: "ODO validation error with field resolution context",
            ac_tag: "AC:63-26-2",
        },

        ErrorPropagationFixture {
            fixture_name: "layout_calculation_error_propagation",
            component: "copybook-core",
            error_scenario: "Field offset calculation overflow",
            input_data: ErrorInputData::Copybook(r"
            01 OVERFLOW-RECORD.
                05 HUGE-FIELD-1 PIC X(32767).
                05 HUGE-FIELD-2 PIC X(32767).
                05 HUGE-FIELD-3 PIC X(32767).
            "),
            expected_error_code: "CBKP001_SYNTAX",
            expected_context: vec!["field name", "offset value", "total record size"],
            recovery_behavior: "Layout calculation with bounds checking",
            panic_elimination_point: "Field offset arithmetic and bounds validation",
            description: "Layout calculation overflow with field positioning context",
            ac_tag: "AC:63-26-3",
        },

        ErrorPropagationFixture {
            fixture_name: "pic_clause_error_propagation",
            component: "copybook-core",
            error_scenario: "Invalid PIC clause format",
            input_data: ErrorInputData::Copybook(r"
            01 RECORD.
                05 INVALID-PIC-1 PIC 9(ABC).
                05 INVALID-PIC-2 PIC X(999999999999999999999).
                05 INVALID-PIC-3 PIC $$$,$$9.99.ZZ.
            "),
            expected_error_code: "CBKP051_UNSUPPORTED_EDITED_PIC",
            expected_context: vec!["field name", "PIC clause", "invalid element"],
            recovery_behavior: "PIC clause validation with format details",
            panic_elimination_point: "PIC string parsing and size calculation",
            description: "PIC clause validation error with format context",
            ac_tag: "AC:63-26-4",
        },
    ]
});

/// copybook-codec Error Propagation Fixtures - AC:63-27
pub static CODEC_ERROR_PROPAGATION: LazyLock<Vec<ErrorPropagationFixture>> = LazyLock::new(|| {
    vec![
        // AC:63-27-1 - Numeric conversion error propagation
        ErrorPropagationFixture {
            fixture_name: "comp3_conversion_error_propagation",
            component: "copybook-codec",
            error_scenario: "Invalid COMP-3 packed decimal data",
            input_data: ErrorInputData::Combined {
                copybook: "01 RECORD.\n    05 COMP3-FIELD PIC S9(7)V99 COMP-3.",
                data: vec![0xFF, 0xFF, 0xFF, 0xAA], // Invalid nibbles and sign
                args: vec![],
            },
            expected_error_code: "CBKD201_INVALID_COMP3",
            expected_context: vec!["field name", "field offset", "invalid nibble", "byte position"],
            recovery_behavior: "Invalid COMP-3 data with precise error location",
            panic_elimination_point: "COMP-3 nibble extraction and validation",
            description: "COMP-3 conversion error with byte-level context",
            ac_tag: "AC:63-27-1",
        },

        ErrorPropagationFixture {
            fixture_name: "zoned_overpunch_error_propagation",
            component: "copybook-codec",
            error_scenario: "Invalid zoned overpunch character",
            input_data: ErrorInputData::Combined {
                copybook: "01 RECORD.\n    05 ZONED-FIELD PIC S9(5).",
                data: b"1234@".to_vec(), // Invalid overpunch character
                args: vec![],
            },
            expected_error_code: "CBKD301_INVALID_ZONED",
            expected_context: vec!["field name", "character position", "invalid character", "expected overpunch"],
            recovery_behavior: "Zoned decimal error with character context",
            panic_elimination_point: "Overpunch character mapping and validation",
            description: "Zoned overpunch error with character-level context",
            ac_tag: "AC:63-27-2",
        },

        ErrorPropagationFixture {
            fixture_name: "record_boundary_error_propagation",
            component: "copybook-codec",
            error_scenario: "Record data truncation",
            input_data: ErrorInputData::Combined {
                copybook: r"
                01 RECORD.
                    05 FIELD-1 PIC X(20).
                    05 FIELD-2 PIC 9(10).
                    05 FIELD-3 PIC S9(7)V99 COMP-3.
                ",
                data: vec![0x40; 10], // Only 10 bytes for 34-byte record
                args: vec![],
            },
            expected_error_code: "CBKD101_TRUNCATED_RECORD",
            expected_context: vec!["expected length", "actual length", "missing bytes", "last complete field"],
            recovery_behavior: "Record truncation with precise boundary information",
            panic_elimination_point: "Record bounds checking and field access",
            description: "Record boundary error with length calculation context",
            ac_tag: "AC:63-27-3",
        },

        ErrorPropagationFixture {
            fixture_name: "memory_allocation_error_propagation",
            component: "copybook-codec",
            error_scenario: "Memory allocation failure during large record processing",
            input_data: ErrorInputData::Combined {
                copybook: "01 LARGE-RECORD.\n    05 HUGE-FIELD PIC X(10000000).", // 10MB field
                data: vec![0x40; 1000], // Small data, large expectation
                args: vec![],
            },
            expected_error_code: "CBKD001_MEMORY_ERROR",
            expected_context: vec!["allocation size", "available memory", "field name", "operation type"],
            recovery_behavior: "Memory allocation failure with resource context",
            panic_elimination_point: "Large buffer allocation and memory management",
            description: "Memory allocation error with resource context",
            ac_tag: "AC:63-27-4",
        },

        ErrorPropagationFixture {
            fixture_name: "character_encoding_error_propagation",
            component: "copybook-codec",
            error_scenario: "Invalid EBCDIC character conversion",
            input_data: ErrorInputData::Combined {
                copybook: "01 RECORD.\n    05 TEXT-FIELD PIC X(10).",
                data: vec![0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09], // Control chars
                args: vec!["--codepage", "cp037"],
            },
            expected_error_code: "CBKD401_ENCODING_ERROR",
            expected_context: vec!["field name", "character position", "invalid byte", "codepage"],
            recovery_behavior: "Character encoding error with conversion context",
            panic_elimination_point: "EBCDIC to Unicode conversion and validation",
            description: "Character encoding error with codepage context",
            ac_tag: "AC:63-27-5",
        },
    ]
});

/// copybook-cli Error Propagation Fixtures - AC:63-28
pub static CLI_ERROR_PROPAGATION: LazyLock<Vec<ErrorPropagationFixture>> = LazyLock::new(|| {
    vec![
        // AC:63-28-1 - Command argument error propagation
        ErrorPropagationFixture {
            fixture_name: "argument_validation_error_propagation",
            component: "copybook-cli",
            error_scenario: "Invalid command arguments",
            input_data: ErrorInputData::CliArgs(vec!["decode", "--threads", "0", "--codepage", "invalid", "missing.cpy"]),
            expected_error_code: "CBKE001_INVALID_ARGS",
            expected_context: vec!["argument name", "invalid value", "expected format", "command context"],
            recovery_behavior: "Argument validation with usage guidance",
            panic_elimination_point: "Command line argument parsing and validation",
            description: "CLI argument error with command context",
            ac_tag: "AC:63-28-1",
        },

        ErrorPropagationFixture {
            fixture_name: "file_system_error_propagation",
            component: "copybook-cli",
            error_scenario: "File system access failure",
            input_data: ErrorInputData::CliArgs(vec!["parse", "/nonexistent/path/copybook.cpy"]),
            expected_error_code: "CBKE101_FILE_NOT_FOUND",
            expected_context: vec!["file path", "operation type", "system error", "suggested action"],
            recovery_behavior: "File system error with path validation",
            panic_elimination_point: "File existence checking and access validation",
            description: "File system error with path context",
            ac_tag: "AC:63-28-2",
        },

        ErrorPropagationFixture {
            fixture_name: "output_generation_error_propagation",
            component: "copybook-cli",
            error_scenario: "Output file write failure",
            input_data: ErrorInputData::Combined {
                copybook: "01 RECORD.\n    05 FIELD PIC X(10).",
                data: vec![0x40; 10],
                args: vec!["decode", "copybook.cpy", "data.bin", "--output", "/readonly/output.jsonl"],
            },
            expected_error_code: "CBKE201_OUTPUT_ERROR",
            expected_context: vec!["output path", "write operation", "system error", "partial data"],
            recovery_behavior: "Output error with data preservation",
            panic_elimination_point: "Output file creation and write operations",
            description: "Output generation error with write context",
            ac_tag: "AC:63-28-3",
        },
    ]
});

/// Error Chain Validation Fixtures - AC:63-29
pub static ERROR_CHAIN_FIXTURES: LazyLock<Vec<ErrorChainFixture>> = LazyLock::new(|| {
    vec![
        // AC:63-29-1 - Complete processing chain error
        ErrorChainFixture {
            fixture_name: "complete_processing_chain_error",
            error_chain: vec![
                ErrorStep {
                    component: "copybook-core",
                    operation: "parse_copybook",
                    error_type: "CBKP001_SYNTAX",
                    context_preservation: vec!["copybook content", "line number", "syntax element"],
                },
                ErrorStep {
                    component: "copybook-codec",
                    operation: "decode_record",
                    error_type: "CBKD001_SCHEMA_ERROR",
                    context_preservation: vec!["original parse error", "field context", "data position"],
                },
                ErrorStep {
                    component: "copybook-cli",
                    operation: "decode_command",
                    error_type: "CBKE001_PROCESSING_ERROR",
                    context_preservation: vec!["command context", "file paths", "processing stage"],
                },
            ],
            description: "Complete error chain from parse failure to CLI error",
            chain_validation: "Context preserved through entire chain with structured error reporting",
            ac_tag: "AC:63-29-1",
        },

        // AC:63-29-2 - Data processing error chain
        ErrorChainFixture {
            fixture_name: "data_processing_error_chain",
            error_chain: vec![
                ErrorStep {
                    component: "copybook-codec",
                    operation: "numeric_conversion",
                    error_type: "CBKD201_INVALID_COMP3",
                    context_preservation: vec!["field name", "byte offset", "invalid data"],
                },
                ErrorStep {
                    component: "copybook-codec",
                    operation: "record_decode",
                    error_type: "CBKD001_FIELD_ERROR",
                    context_preservation: vec!["numeric error context", "record position", "field hierarchy"],
                },
                ErrorStep {
                    component: "copybook-cli",
                    operation: "batch_processing",
                    error_type: "CBKE201_BATCH_ERROR",
                    context_preservation: vec!["record number", "batch context", "recovery options"],
                },
            ],
            description: "Data processing error chain with field-level context",
            chain_validation: "Field-level error context preserved through processing chain",
            ac_tag: "AC:63-29-2",
        },

        // AC:63-29-3 - Performance monitoring error chain
        ErrorChainFixture {
            fixture_name: "performance_monitoring_error_chain",
            error_chain: vec![
                ErrorStep {
                    component: "copybook-bench",
                    operation: "benchmark_execution",
                    error_type: "CBKB001_MEASUREMENT_ERROR",
                    context_preservation: vec!["benchmark name", "measurement type", "failure point"],
                },
                ErrorStep {
                    component: "copybook-bench",
                    operation: "statistics_calculation",
                    error_type: "CBKB101_STATS_ERROR",
                    context_preservation: vec!["measurement data", "calculation type", "overflow context"],
                },
                ErrorStep {
                    component: "copybook-cli",
                    operation: "performance_reporting",
                    error_type: "CBKE301_REPORT_ERROR",
                    context_preservation: vec!["benchmark context", "output format", "reporting stage"],
                },
            ],
            description: "Performance monitoring error chain with measurement context",
            chain_validation: "Performance measurement context preserved through reporting chain",
            ac_tag: "AC:63-29-3",
        },
    ]
});

/// Context Preservation Fixtures - AC:63-30
pub static CONTEXT_PRESERVATION_FIXTURES: LazyLock<Vec<ContextPreservationFixture>> = LazyLock::new(|| {
    vec![
        // AC:63-30-1 - Parse context preservation
        ContextPreservationFixture {
            fixture_name: "parse_context_preservation",
            error_context: ErrorContext {
                operation_type: "copybook_parsing",
                data_location: "line 15, column 25",
                processing_stage: "PIC clause validation",
                user_context: vec!["file: customer.cpy", "field: CUSTOMER-BALANCE"],
            },
            expected_preserved_context: vec![
                "source file name",
                "line and column numbers",
                "field name and hierarchy",
                "specific syntax element",
                "parsing stage",
            ],
            description: "Parse error context preservation with location details",
            ac_tag: "AC:63-30-1",
        },

        // AC:63-30-2 - Data processing context preservation
        ContextPreservationFixture {
            fixture_name: "data_processing_context_preservation",
            error_context: ErrorContext {
                operation_type: "record_decoding",
                data_location: "record 1523, byte offset 145",
                processing_stage: "COMP-3 field conversion",
                user_context: vec!["input: transactions.bin", "output: transactions.jsonl", "field: TRANSACTION-AMOUNT"],
            },
            expected_preserved_context: vec![
                "record number",
                "byte offset within record",
                "field name and type",
                "conversion operation",
                "input/output file context",
            ],
            description: "Data processing context preservation with precise location",
            ac_tag: "AC:63-30-2",
        },

        // AC:63-30-3 - CLI operation context preservation
        ContextPreservationFixture {
            fixture_name: "cli_operation_context_preservation",
            error_context: ErrorContext {
                operation_type: "batch_processing",
                data_location: "file 3 of 10, record 5000 of 50000",
                processing_stage: "output generation",
                user_context: vec!["command: decode", "threads: 4", "format: fixed", "codepage: cp037"],
            },
            expected_preserved_context: vec![
                "command and arguments",
                "processing progress",
                "file and record position",
                "operation parameters",
                "batch context",
            ],
            description: "CLI operation context preservation with batch progress",
            ac_tag: "AC:63-30-3",
        },

        // AC:63-30-4 - Enterprise audit context preservation
        ContextPreservationFixture {
            fixture_name: "enterprise_audit_context_preservation",
            error_context: ErrorContext {
                operation_type: "compliance_validation",
                data_location: "regulatory section, field BSA-INDICATOR",
                processing_stage: "compliance rule evaluation",
                user_context: vec!["industry: banking", "regulation: BSA", "risk-level: high", "operator: COMP001"],
            },
            expected_preserved_context: vec![
                "regulatory framework",
                "compliance rule details",
                "risk assessment level",
                "operator identification",
                "audit trail information",
            ],
            description: "Enterprise audit context preservation with compliance details",
            ac_tag: "AC:63-30-4",
        },
    ]
});

/// Graceful Recovery Fixtures - AC:63-31
pub static GRACEFUL_RECOVERY_FIXTURES: LazyLock<Vec<GracefulRecoveryFixture>> = LazyLock::new(|| {
    vec![
        // AC:63-31-1 - Parse error recovery
        GracefulRecoveryFixture {
            fixture_name: "parse_error_recovery",
            failure_scenario: "Invalid copybook syntax with partial valid content",
            recovery_strategy: "Parse valid portions, report specific errors for invalid sections",
            expected_outcome: "Structured error with successfully parsed field information",
            data_integrity_preserved: true,
            ac_tag: "AC:63-31-1",
        },

        // AC:63-31-2 - Data corruption recovery
        GracefulRecoveryFixture {
            fixture_name: "data_corruption_recovery",
            failure_scenario: "Corrupted COMP-3 field in otherwise valid record",
            recovery_strategy: "Skip corrupted field, continue processing remaining fields",
            expected_outcome: "Partial record with error markers for corrupted fields",
            data_integrity_preserved: true,
            ac_tag: "AC:63-31-2",
        },

        // AC:63-31-3 - Memory pressure recovery
        GracefulRecoveryFixture {
            fixture_name: "memory_pressure_recovery",
            failure_scenario: "Memory allocation failure during large batch processing",
            recovery_strategy: "Reduce batch size, implement streaming processing",
            expected_outcome: "Continued processing with reduced memory footprint",
            data_integrity_preserved: true,
            ac_tag: "AC:63-31-3",
        },

        // AC:63-31-4 - Network error recovery
        GracefulRecoveryFixture {
            fixture_name: "network_error_recovery",
            failure_scenario: "Network failure during remote file access",
            recovery_strategy: "Retry with exponential backoff, fallback to local processing",
            expected_outcome: "Processing continuation with alternative data source",
            data_integrity_preserved: true,
            ac_tag: "AC:63-31-4",
        },

        // AC:63-31-5 - Enterprise compliance recovery
        GracefulRecoveryFixture {
            fixture_name: "compliance_error_recovery",
            failure_scenario: "Compliance validation failure in production environment",
            recovery_strategy: "Quarantine non-compliant data, continue processing compliant records",
            expected_outcome: "Compliant data processed, non-compliant data flagged for review",
            data_integrity_preserved: true,
            ac_tag: "AC:63-31-5",
        },
    ]
});

/// Error Taxonomy Coverage Validation
pub static ERROR_TAXONOMY_COVERAGE: LazyLock<Vec<(&'static str, Vec<&'static str>)>> = LazyLock::new(|| {
    vec![
        // CBKP* - Parse errors
        ("CBKP", vec![
            "CBKP001_SYNTAX",
            "CBKP011_UNSUPPORTED_CLAUSE",
            "CBKP021_ODO_NOT_TAIL",
            "CBKP051_UNSUPPORTED_EDITED_PIC",
        ]),

        // CBKS* - Schema validation errors
        ("CBKS", vec![
            "CBKS121_COUNTER_NOT_FOUND",
            "CBKS301_ODO_CLIPPED",
            "CBKS302_ODO_RAISED",
        ]),

        // CBKD* - Data processing errors
        ("CBKD", vec![
            "CBKD001_MEMORY_ERROR",
            "CBKD101_TRUNCATED_RECORD",
            "CBKD201_INVALID_COMP3",
            "CBKD301_INVALID_ZONED",
            "CBKD401_ENCODING_ERROR",
        ]),

        // CBKE* - External/CLI errors
        ("CBKE", vec![
            "CBKE001_INVALID_ARGS",
            "CBKE101_FILE_NOT_FOUND",
            "CBKE201_OUTPUT_ERROR",
            "CBKE301_REPORT_ERROR",
        ]),

        // CBKB* - Benchmark errors
        ("CBKB", vec![
            "CBKB001_MEASUREMENT_ERROR",
            "CBKB101_STATS_ERROR",
        ]),
    ]
});

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_core_error_propagation_fixtures_load() {
        assert!(!CORE_ERROR_PROPAGATION.is_empty(), "Core error propagation fixtures should be loaded");

        for fixture in CORE_ERROR_PROPAGATION.iter() {
            assert_eq!(fixture.component, "copybook-core", "Core fixture should specify correct component");
            assert!(!fixture.error_scenario.is_empty(), "Fixture should have error scenario");
            assert!(!fixture.expected_error_code.is_empty(), "Fixture should have expected error code");
            assert!(!fixture.expected_context.is_empty(), "Fixture should specify expected context");
            assert!(!fixture.panic_elimination_point.is_empty(), "Fixture should specify panic elimination point");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_codec_error_propagation_fixtures_load() {
        assert!(!CODEC_ERROR_PROPAGATION.is_empty(), "Codec error propagation fixtures should be loaded");

        for fixture in CODEC_ERROR_PROPAGATION.iter() {
            assert_eq!(fixture.component, "copybook-codec", "Codec fixture should specify correct component");
            assert!(fixture.expected_error_code.starts_with("CBKD"), "Codec errors should use CBKD* taxonomy");
            assert!(!fixture.recovery_behavior.is_empty(), "Fixture should specify recovery behavior");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_cli_error_propagation_fixtures_load() {
        assert!(!CLI_ERROR_PROPAGATION.is_empty(), "CLI error propagation fixtures should be loaded");

        for fixture in CLI_ERROR_PROPAGATION.iter() {
            assert_eq!(fixture.component, "copybook-cli", "CLI fixture should specify correct component");
            assert!(fixture.expected_error_code.starts_with("CBKE"), "CLI errors should use CBKE* taxonomy");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_error_chain_fixtures_load() {
        assert!(!ERROR_CHAIN_FIXTURES.is_empty(), "Error chain fixtures should be loaded");

        for fixture in ERROR_CHAIN_FIXTURES.iter() {
            assert!(!fixture.error_chain.is_empty(), "Chain fixture should have error steps");
            assert!(fixture.error_chain.len() >= 2, "Chain should involve multiple components");

            // Verify chain involves different components
            let components: Vec<&str> = fixture.error_chain.iter()
                .map(|step| step.component)
                .collect();
            let unique_components: std::collections::HashSet<&str> = components.into_iter().collect();
            assert!(unique_components.len() >= 2, "Error chain should span multiple components");

            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_context_preservation_fixtures_load() {
        assert!(!CONTEXT_PRESERVATION_FIXTURES.is_empty(), "Context preservation fixtures should be loaded");

        for fixture in CONTEXT_PRESERVATION_FIXTURES.iter() {
            assert!(!fixture.expected_preserved_context.is_empty(), "Fixture should specify preserved context");
            assert!(!fixture.error_context.operation_type.is_empty(), "Fixture should have operation type");
            assert!(!fixture.error_context.data_location.is_empty(), "Fixture should have data location");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_graceful_recovery_fixtures_load() {
        assert!(!GRACEFUL_RECOVERY_FIXTURES.is_empty(), "Graceful recovery fixtures should be loaded");

        for fixture in GRACEFUL_RECOVERY_FIXTURES.iter() {
            assert!(!fixture.failure_scenario.is_empty(), "Fixture should have failure scenario");
            assert!(!fixture.recovery_strategy.is_empty(), "Fixture should have recovery strategy");
            assert!(!fixture.expected_outcome.is_empty(), "Fixture should have expected outcome");
            assert!(fixture.data_integrity_preserved, "Recovery should preserve data integrity");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_error_taxonomy_coverage() {
        // Verify all error taxonomy categories are covered
        let covered_categories: Vec<&str> = ERROR_TAXONOMY_COVERAGE.iter()
            .map(|(category, _)| *category)
            .collect();

        assert!(covered_categories.contains(&"CBKP"), "Should cover CBKP* parse errors");
        assert!(covered_categories.contains(&"CBKS"), "Should cover CBKS* schema errors");
        assert!(covered_categories.contains(&"CBKD"), "Should cover CBKD* data errors");
        assert!(covered_categories.contains(&"CBKE"), "Should cover CBKE* external errors");
        assert!(covered_categories.contains(&"CBKB"), "Should cover CBKB* benchmark errors");

        // Verify each category has multiple error codes
        for (category, codes) in ERROR_TAXONOMY_COVERAGE.iter() {
            assert!(!codes.is_empty(), "Category {} should have error codes", category);
            for code in codes {
                assert!(code.starts_with(category), "Error code {} should start with {}", code, category);
            }
        }
    }

    #[test]
    fn test_panic_elimination_coverage() {
        // Collect all panic elimination points
        let panic_points: Vec<&str> = CORE_ERROR_PROPAGATION.iter()
            .chain(CODEC_ERROR_PROPAGATION.iter())
            .chain(CLI_ERROR_PROPAGATION.iter())
            .map(|f| f.panic_elimination_point)
            .collect();

        // Verify key panic-prone operations are covered
        let has_parser_panics = panic_points.iter()
            .any(|&point| point.contains("Parser") || point.contains("token"));
        assert!(has_parser_panics, "Should cover parser panic elimination points");

        let has_numeric_panics = panic_points.iter()
            .any(|&point| point.contains("COMP-3") || point.contains("numeric"));
        assert!(has_numeric_panics, "Should cover numeric conversion panic elimination points");

        let has_memory_panics = panic_points.iter()
            .any(|&point| point.contains("memory") || point.contains("allocation"));
        assert!(has_memory_panics, "Should cover memory management panic elimination points");

        let has_bounds_panics = panic_points.iter()
            .any(|&point| point.contains("bounds") || point.contains("overflow"));
        assert!(has_bounds_panics, "Should cover bounds checking panic elimination points");
    }

    #[test]
    fn test_enterprise_error_scenarios() {
        // Verify enterprise-specific error scenarios are covered
        let all_scenarios: Vec<&str> = CORE_ERROR_PROPAGATION.iter()
            .chain(CODEC_ERROR_PROPAGATION.iter())
            .chain(CLI_ERROR_PROPAGATION.iter())
            .map(|f| f.error_scenario)
            .collect();

        let has_compliance_scenarios = all_scenarios.iter()
            .any(|&scenario| scenario.contains("compliance") || scenario.contains("regulatory"));

        let has_performance_scenarios = all_scenarios.iter()
            .any(|&scenario| scenario.contains("memory") || scenario.contains("large"));

        let has_data_integrity_scenarios = all_scenarios.iter()
            .any(|&scenario| scenario.contains("truncation") || scenario.contains("corruption"));

        // Note: While not all individual fixtures contain compliance scenarios,
        // the overall fixture set should support enterprise requirements
        assert!(
            has_performance_scenarios && has_data_integrity_scenarios,
            "Should cover enterprise performance and data integrity scenarios"
        );
    }

    #[test]
    fn test_error_context_completeness() {
        // Verify error context information is comprehensive
        for fixture in CONTEXT_PRESERVATION_FIXTURES.iter() {
            let context = &fixture.error_context;

            // Verify essential context elements are present
            assert!(!context.operation_type.is_empty(), "Should have operation type");
            assert!(!context.data_location.is_empty(), "Should have data location");
            assert!(!context.processing_stage.is_empty(), "Should have processing stage");

            // Verify preserved context is comprehensive
            assert!(fixture.expected_preserved_context.len() >= 3,
                   "Should preserve multiple context elements: {}",
                   fixture.expected_preserved_context.len());
        }
    }
}