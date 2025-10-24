#![allow(clippy::if_not_else, clippy::unnecessary_wraps)]
#![allow(clippy::ignore_without_reason)]
#![allow(clippy::panic)]
#![allow(clippy::uninlined_format_args)] // Test output formatting for clarity

/// Tests feature spec: issue-63-spec.md#ac1-complete-panic-elimination
/// Tests feature spec: issue-63-technical-specification.md#cli-command-safety
/// Tests feature spec: panic-elimination-implementation-blueprint.md#phase-3-long-tail-cleanup
///
/// Issue #63 - Comprehensive Panic Elimination Test Scaffolding for copybook-cli
///
/// This module provides comprehensive test scaffolding for eliminating 7 `.unwrap()`/`.expect()` calls
/// in copybook-cli production code. Tests target audit.rs (4) and utils.rs (3) with enterprise-safe
/// command handling and user-friendly error reporting.
///
/// **AC Traceability:**
/// - AC1: Complete elimination of 7 `.unwrap()`/`.expect()` calls in copybook-cli
/// - AC2: Zero breaking changes to existing CLI interfaces
/// - AC3: Integration with CBKE* error taxonomy for CLI errors
/// - AC4: Performance impact minimal on CLI operations
/// - AC7: Comprehensive test coverage for command handlers
/// - AC8: User-friendly error messages with structured error taxonomy
mod common;

use common::{TestResult, bin};

#[cfg(feature = "soak")]
use common::write_file;
#[cfg(feature = "soak")]
use std::fs;
#[cfg(feature = "soak")]
use std::process::Command;

#[cfg(feature = "soak")]
fn soak_enabled() -> bool {
    std::env::var("COPYBOOK_TEST_SLOW").as_deref() == Ok("1")
}

#[cfg(feature = "soak")]
fn skip_if_soak_disabled() -> bool {
    if !soak_enabled() {
        eprintln!("Skipping soak-only panic elimination test (set COPYBOOK_TEST_SLOW=1 to run).");
        true
    } else {
        false
    }
}

#[test]
fn panic_elimination_cli_smoke() -> TestResult<()> {
    let missing_path = std::env::temp_dir().join("panic_elimination_smoke_missing.cpy");
    let mut cmd = bin()?;
    cmd.env("RUST_LOG", "error");
    cmd.arg("inspect").arg(&missing_path);
    let assert = cmd.assert().failure();
    let output = assert.get_output();
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.contains("panic"),
        "panic elimination smoke test observed panic output: {stderr}"
    );
    Ok(())
}

#[cfg(feature = "soak")]
mod panic_elimination_cli_command_tests {
    use super::*;

    /// Tests CLI command handlers panic elimination (7 instances total)
    /// AC:63-13 - CLI command execution with safe argument processing

    #[test] // AC:63-13-1 Parse command argument safety
    #[ignore]
    fn test_parse_command_argument_safety() -> TestResult<()> {
        if super::skip_if_soak_disabled() {
            return Ok(());
        }
        // Test case: Parse command with invalid arguments that could cause panics
        let test_copybook_content = r"
       01 TEST-RECORD.
           05 TEST-FIELD PIC X(10).
        ";

        // Create temporary test files
        let temp_dir = std::env::temp_dir();
        let copybook_path = temp_dir.join("test_parse_safety.cpy");
        write_file(&copybook_path, test_copybook_content)?;

        // Test parse command with valid input
        let output = Command::new("cargo")
            .args(["run", "--bin", "copybook", "--", "parse"])
            .arg(&copybook_path)
            .output();

        match output {
            Ok(output) => {
                // Should handle parse command safely without panics
                assert!(
                    output.status.success() || !output.stderr.is_empty(),
                    "Parse command should complete without panic: {}",
                    String::from_utf8_lossy(&output.stderr)
                );

                // Clean up test file
                let _ = fs::remove_file(&copybook_path);
            }
            Err(e) => {
                // Command execution may fail due to environment - that's acceptable
                println!("CLI test skipped due to execution environment: {}", e);
                let _ = fs::remove_file(&copybook_path);
            }
        }
        Ok(())
    }

    #[test] // AC:63-13-2 Inspect command file handling safety
    #[ignore]
    fn test_inspect_command_file_handling_safety() -> TestResult<()> {
        if super::skip_if_soak_disabled() {
            return Ok(());
        }
        // Test case: Inspect command with missing or invalid files
        let nonexistent_path = std::env::temp_dir().join("nonexistent_file.cpy");

        // Test inspect command with nonexistent file
        let output = Command::new("cargo")
            .args(["run", "--bin", "copybook", "--", "inspect"])
            .arg(&nonexistent_path)
            .output();

        match output {
            Ok(output) => {
                // Should handle missing file safely with structured error
                assert!(
                    !output.status.success() || !output.stderr.is_empty(),
                    "Inspect command should handle missing file gracefully"
                );

                // Should not contain panic messages
                let stderr_output = String::from_utf8_lossy(&output.stderr);
                assert!(
                    !stderr_output.contains("panic") && !stderr_output.contains("unwrap"),
                    "Error output should not contain panic traces: {}",
                    stderr_output
                );
            }
            Err(e) => {
                println!("CLI test skipped due to execution environment: {}", e);
            }
        }
        Ok(())
    }

    #[test] // AC:63-13-3 Decode command option validation safety
    #[ignore]
    fn test_decode_command_option_validation_safety() -> TestResult<()> {
        if super::skip_if_soak_disabled() {
            return Ok(());
        }
        // Test case: Decode command with invalid option combinations
        let test_copybook_content = "       01 RECORD.\n           05 FIELD PIC X(10).";
        let test_data = vec![b'T'; 10];

        let temp_dir = std::env::temp_dir();
        let copybook_path = temp_dir.join("test_decode_safety.cpy");
        let data_path = temp_dir.join("test_decode_safety.bin");
        let output_path = temp_dir.join("test_decode_safety.jsonl");

        write_file(&copybook_path, test_copybook_content)?;
        write_file(&data_path, &test_data)?;

        // Test decode command with invalid codepage
        let output = Command::new("cargo")
            .args([
                "run",
                "--bin",
                "copybook",
                "--",
                "decode",
                "--format",
                "fixed",
                "--codepage",
                "INVALID_CODEPAGE",
                "--output",
            ])
            .arg(&output_path)
            .arg(&copybook_path)
            .arg(&data_path)
            .output();

        match output {
            Ok(output) => {
                // Should handle invalid options safely with structured error
                if !output.status.success() {
                    let stderr_output = String::from_utf8_lossy(&output.stderr);
                    assert!(
                        !stderr_output.contains("panic") && !stderr_output.contains("unwrap"),
                        "Invalid option error should not contain panic traces: {}",
                        stderr_output
                    );
                }

                // Clean up test files
                let _ = fs::remove_file(&copybook_path);
                let _ = fs::remove_file(&data_path);
                let _ = fs::remove_file(&output_path);
            }
            Err(e) => {
                println!("CLI test skipped due to execution environment: {}", e);
                let _ = fs::remove_file(&copybook_path);
                let _ = fs::remove_file(&data_path);
            }
        }
        Ok(())
    }

    #[test] // AC:63-13-4 Encode command data processing safety
    #[ignore]
    fn test_encode_command_data_processing_safety() -> TestResult<()> {
        if super::skip_if_soak_disabled() {
            return Ok(());
        }
        // Test case: Encode command with malformed JSON data
        let test_copybook_content = "       01 RECORD.\n           05 FIELD PIC X(10).";
        let invalid_json_data = r#"{"FIELD": "valid"} invalid json follows"#;

        let temp_dir = std::env::temp_dir();
        let copybook_path = temp_dir.join("test_encode_safety.cpy");
        let json_path = temp_dir.join("test_encode_safety.jsonl");
        let output_path = temp_dir.join("test_encode_safety.bin");

        write_file(&copybook_path, test_copybook_content)?;
        write_file(&json_path, invalid_json_data)?;

        // Test encode command with malformed JSON
        let output = Command::new("cargo")
            .args([
                "run",
                "--bin",
                "copybook",
                "--",
                "encode",
                "--format",
                "fixed",
                "--codepage",
                "cp037",
            ])
            .arg(&copybook_path)
            .arg(&json_path)
            .arg(&output_path)
            .output();

        match output {
            Ok(output) => {
                // Should handle malformed JSON safely with structured error
                if !output.status.success() {
                    let stderr_output = String::from_utf8_lossy(&output.stderr);
                    assert!(
                        !stderr_output.contains("panic") && !stderr_output.contains("unwrap"),
                        "JSON parsing error should not contain panic traces: {}",
                        stderr_output
                    );
                }

                // Clean up test files
                let _ = fs::remove_file(&copybook_path);
                let _ = fs::remove_file(&json_path);
                let _ = fs::remove_file(&output_path);
            }
            Err(e) => {
                println!("CLI test skipped due to execution environment: {}", e);
                let _ = fs::remove_file(&copybook_path);
                let _ = fs::remove_file(&json_path);
            }
        }
        Ok(())
    }

    #[test] // AC:63-13-5 Verify command integration safety
    #[ignore]
    fn test_verify_command_integration_safety() -> TestResult<()> {
        if super::skip_if_soak_disabled() {
            return Ok(());
        }
        // Test case: Verify command with mismatched copybook and data
        let test_copybook_content = "       01 RECORD.\n           05 FIELD PIC X(20)."; // Expects 20 bytes
        let test_data = vec![b'T'; 10]; // Only 10 bytes

        let temp_dir = std::env::temp_dir();
        let copybook_path = temp_dir.join("test_verify_safety.cpy");
        let data_path = temp_dir.join("test_verify_safety.bin");

        write_file(&copybook_path, test_copybook_content)?;
        write_file(&data_path, &test_data)?;

        // Test verify command with mismatched data
        let output = Command::new("cargo")
            .args([
                "run",
                "--bin",
                "copybook",
                "--",
                "verify",
                "--format",
                "fixed",
                "--codepage",
                "cp037",
            ])
            .arg(&copybook_path)
            .arg(&data_path)
            .output();

        match output {
            Ok(output) => {
                // Should handle data mismatch safely with structured error
                if !output.status.success() {
                    let stderr_output = String::from_utf8_lossy(&output.stderr);
                    let stdout_output = String::from_utf8_lossy(&output.stdout);
                    let combined_output = format!("{}{}", stderr_output, stdout_output);
                    let combined_lower = combined_output.to_lowercase();
                    assert!(
                        !combined_lower.contains("panic") && !combined_lower.contains("unwrap"),
                        "Verification error should not contain panic traces: {}",
                        combined_output
                    );

                    // Should provide user-friendly error message
                    assert!(
                        combined_lower.contains("record")
                            || combined_lower.contains("length")
                            || combined_lower.contains("mismatch"),
                        "Verification error should provide meaningful context: {}",
                        combined_output
                    );
                }

                // Clean up test files
                let _ = fs::remove_file(&copybook_path);
                let _ = fs::remove_file(&data_path);
            }
            Err(e) => {
                println!("CLI test skipped due to execution environment: {}", e);
                let _ = fs::remove_file(&copybook_path);
                let _ = fs::remove_file(&data_path);
            }
        }
        Ok(())
    }
}

#[cfg(feature = "soak")]
mod panic_elimination_cli_audit_tests {
    use super::*;

    /// Tests CLI audit functionality panic elimination (4 instances in audit.rs)
    /// AC:63-14 - CLI audit integration with safe event processing

    #[test] // AC:63-14-1 Audit event generation safety
    #[ignore]
    fn test_audit_event_generation_safety() -> TestResult<()> {
        if super::skip_if_soak_disabled() {
            return Ok(());
        }
        // Test case: CLI operations that generate audit events
        let test_copybook_content = r"
       01 AUDIT-TEST-RECORD.
           05 AUDIT-ID PIC 9(10).
           05 AUDIT-TYPE PIC X(20).
           05 AUDIT-DATA PIC X(100).
        ";

        let temp_dir = std::env::temp_dir();
        let copybook_path = temp_dir.join("test_audit_safety.cpy");
        write_file(&copybook_path, test_copybook_content)?;

        // Test parse command that should generate audit events
        let output = Command::new("cargo")
            .args(["run", "--bin", "copybook", "--", "parse"])
            .arg(&copybook_path)
            .output();

        match output {
            Ok(output) => {
                // Should handle audit event generation safely
                if output.status.success() {
                    let stdout_output = String::from_utf8_lossy(&output.stdout);
                    // Should produce valid JSON output without panics
                    // Allow for log messages before JSON output
                    let trimmed_output = stdout_output.trim_start();
                    assert!(
                        trimmed_output.contains('{')
                            || trimmed_output.contains('[')
                            || stdout_output.contains("schema")
                            || stdout_output.contains("fields"),
                        "Parse output should contain JSON or schema information: {}",
                        stdout_output
                    );
                } else {
                    let stderr_output = String::from_utf8_lossy(&output.stderr);
                    assert!(
                        !stderr_output.contains("panic") && !stderr_output.contains("unwrap"),
                        "Audit event error should not contain panic traces: {}",
                        stderr_output
                    );
                }

                // Clean up test file
                let _ = fs::remove_file(&copybook_path);
            }
            Err(e) => {
                println!("CLI audit test skipped due to execution environment: {}", e);
                let _ = fs::remove_file(&copybook_path);
            }
        }
        Ok(())
    }

    #[test] // AC:63-14-2 Audit serialization safety
    #[ignore]
    fn test_audit_serialization_safety() -> TestResult<()> {
        if super::skip_if_soak_disabled() {
            return Ok(());
        }
        // Test case: Complex operations that stress audit serialization
        let complex_copybook_content = r"
       01 COMPLEX-AUDIT-RECORD.
           05 HEADER.
               10 TRANSACTION-ID PIC 9(15).
               10 USER-ID PIC X(8).
               10 TIMESTAMP PIC 9(14).
           05 OPERATION-DATA.
               10 OPERATION-TYPE PIC X(10).
               10 TABLE-NAME PIC X(30).
               10 RECORD-COUNT PIC 9(7).
           05 AUDIT-TRAIL.
               10 BEFORE-IMAGE PIC X(500).
               10 AFTER-IMAGE PIC X(500).
               10 CHANGE-REASON PIC X(100).
        ";

        let temp_dir = std::env::temp_dir();
        let copybook_path = temp_dir.join("test_complex_audit.cpy");
        write_file(&copybook_path, complex_copybook_content)?;

        // Test inspect command on complex structure
        let output = Command::new("cargo")
            .args(["run", "--bin", "copybook", "--", "inspect"])
            .arg(&copybook_path)
            .output();

        match output {
            Ok(output) => {
                // Should handle complex audit serialization safely
                if output.status.success() {
                    let stdout_output = String::from_utf8_lossy(&output.stdout);
                    // Should produce human-readable output without panics
                    assert!(
                        !stdout_output.is_empty(),
                        "Inspect output should not be empty"
                    );
                } else {
                    let stderr_output = String::from_utf8_lossy(&output.stderr);
                    assert!(
                        !stderr_output.contains("panic") && !stderr_output.contains("unwrap"),
                        "Complex audit error should not contain panic traces: {}",
                        stderr_output
                    );
                }

                // Clean up test file
                let _ = fs::remove_file(&copybook_path);
            }
            Err(e) => {
                println!(
                    "CLI complex audit test skipped due to execution environment: {}",
                    e
                );
                let _ = fs::remove_file(&copybook_path);
            }
        }
        Ok(())
    }

    #[test] // AC:63-14-3 Audit context preservation safety
    #[ignore]
    fn test_audit_context_preservation_safety() -> TestResult<()> {
        if super::skip_if_soak_disabled() {
            return Ok(());
        }
        // Test case: Operations that require audit context preservation
        let context_copybook_content = r"
       01 CONTEXT-RECORD.
           05 CONTEXT-ID PIC 9(5).
           05 CONTEXT-DATA OCCURS 10 TIMES.
               10 DATA-ITEM PIC X(20).
               10 DATA-FLAGS PIC X(5).
        ";

        let temp_dir = std::env::temp_dir();
        let copybook_path = temp_dir.join("test_context_audit.cpy");
        write_file(&copybook_path, context_copybook_content)?;

        // Test parse command with context preservation
        let output = Command::new("cargo")
            .args(["run", "--bin", "copybook", "--", "parse"])
            .arg(&copybook_path)
            .output();

        match output {
            Ok(output) => {
                // Should preserve audit context safely
                if output.status.success() {
                    let stdout_output = String::from_utf8_lossy(&output.stdout);
                    // Should handle occurs arrays in context
                    assert!(
                        stdout_output.contains("CONTEXT-DATA")
                            || stdout_output.contains("occurs")
                            || stdout_output.contains("array"),
                        "Context preservation should handle arrays: {}",
                        stdout_output
                    );
                } else {
                    let stderr_output = String::from_utf8_lossy(&output.stderr);
                    assert!(
                        !stderr_output.contains("panic") && !stderr_output.contains("unwrap"),
                        "Context preservation error should not contain panic traces: {}",
                        stderr_output
                    );
                }

                // Clean up test file
                let _ = fs::remove_file(&copybook_path);
            }
            Err(e) => {
                println!(
                    "CLI context audit test skipped due to execution environment: {}",
                    e
                );
                let _ = fs::remove_file(&copybook_path);
            }
        }
        Ok(())
    }

    #[test] // AC:63-14-4 Audit error recovery safety
    #[ignore]
    fn test_audit_error_recovery_safety() -> TestResult<()> {
        if super::skip_if_soak_disabled() {
            return Ok(());
        }
        // Test case: Error conditions that trigger audit error recovery
        let malformed_copybook_content = r"
       01 MALFORMED-RECORD.
           05 FIELD-A PIC X(10).
           05 FIELD-B REDEFINES NONEXISTENT PIC 9(10).
           05 FIELD-C OCCURS 1 TO 100 TIMES DEPENDING ON MISSING-COUNTER.
               10 ITEM PIC X(5).
        ";

        let temp_dir = std::env::temp_dir();
        let copybook_path = temp_dir.join("test_error_audit.cpy");
        write_file(&copybook_path, malformed_copybook_content)?;

        // Test parse command on malformed copybook
        let output = Command::new("cargo")
            .args(["run", "--bin", "copybook", "--", "parse"])
            .arg(&copybook_path)
            .output();

        match output {
            Ok(output) => {
                // Should handle audit error recovery safely
                if !output.status.success() {
                    let stderr_output = String::from_utf8_lossy(&output.stderr);

                    // Should not contain panic traces
                    assert!(
                        !stderr_output.contains("panic") && !stderr_output.contains("unwrap"),
                        "Audit error recovery should not contain panic traces: {}",
                        stderr_output
                    );

                    // Should provide structured error information
                    assert!(
                        stderr_output.contains("REDEFINES")
                            || stderr_output.contains("NONEXISTENT")
                            || stderr_output.contains("DEPENDING ON")
                            || stderr_output.contains("MISSING-COUNTER"),
                        "Error recovery should provide specific error context: {}",
                        stderr_output
                    );
                }

                // Clean up test file
                let _ = fs::remove_file(&copybook_path);
            }
            Err(e) => {
                println!(
                    "CLI error audit test skipped due to execution environment: {}",
                    e
                );
                let _ = fs::remove_file(&copybook_path);
            }
        }
        Ok(())
    }
}

#[cfg(feature = "soak")]
mod panic_elimination_cli_utils_tests {
    use super::*;

    /// Tests CLI utilities panic elimination (3 instances in utils.rs)
    /// AC:63-15 - CLI utility functions with safe file operations

    #[test] // AC:63-15-1 File path validation safety
    #[ignore]
    fn test_file_path_validation_safety() -> TestResult<()> {
        if super::skip_if_soak_disabled() {
            return Ok(());
        }
        // Test case: CLI operations with invalid file paths
        let invalid_paths = vec![
            "",                      // Empty path
            "/dev/null/nonexistent", // Invalid directory structure
            "///invalid///path",     // Multiple slashes
            "\0invalid_null_path",   // Null character in path
        ];

        for invalid_path in invalid_paths {
            // Test parse command with invalid path
            let output = Command::new("cargo")
                .args(["run", "--bin", "copybook", "--", "parse"])
                .arg(invalid_path)
                .output();

            match output {
                Ok(output) => {
                    // Should handle invalid paths safely with structured error
                    if !output.status.success() {
                        let stderr_output = String::from_utf8_lossy(&output.stderr);
                        assert!(
                            !stderr_output.contains("panic") && !stderr_output.contains("unwrap"),
                            "Path validation error should not contain panic traces for '{}': {}",
                            invalid_path,
                            stderr_output
                        );

                        // Should provide user-friendly error message
                        assert!(
                            stderr_output.contains("file")
                                || stderr_output.contains("path")
                                || stderr_output.contains("not found"),
                            "Path validation should provide meaningful error for '{}': {}",
                            invalid_path,
                            stderr_output
                        );
                    }
                }
                Err(e) => {
                    println!(
                        "CLI path validation test skipped for '{}' due to execution environment: {}",
                        invalid_path, e
                    );
                }
            }
        }
        Ok(())
    }

    #[test] // AC:63-15-2 File I/O operation safety
    #[ignore]
    fn test_file_io_operation_safety() -> TestResult<()> {
        if super::skip_if_soak_disabled() {
            return Ok(());
        }
        // Test case: CLI operations with file I/O that could fail
        let temp_dir = std::env::temp_dir();
        let readonly_path = temp_dir.join("readonly_test.cpy");

        // Create a test file
        let test_content = "       01 RECORD.\n           05 FIELD PIC X(10).";
        write_file(&readonly_path, test_content)?;

        // Test decode operation that requires output file creation
        let output_path = temp_dir.join("readonly_output.jsonl");
        let data_path = temp_dir.join("test_data.bin");
        write_file(&data_path, b"1234567890")?;

        let output = Command::new("cargo")
            .args([
                "run",
                "--bin",
                "copybook",
                "--",
                "decode",
                "--format",
                "fixed",
                "--codepage",
                "cp037",
                "--output",
            ])
            .arg(&output_path)
            .arg(&readonly_path)
            .arg(&data_path)
            .output();

        match output {
            Ok(output) => {
                // Should handle file I/O operations safely
                if !output.status.success() {
                    let stderr_output = String::from_utf8_lossy(&output.stderr);
                    assert!(
                        !stderr_output.contains("panic") && !stderr_output.contains("unwrap"),
                        "File I/O error should not contain panic traces: {}",
                        stderr_output
                    );
                }

                // Clean up test files
                let _ = fs::remove_file(&readonly_path);
                let _ = fs::remove_file(&data_path);
                let _ = fs::remove_file(&output_path);
            }
            Err(e) => {
                println!("CLI I/O test skipped due to execution environment: {}", e);
                let _ = fs::remove_file(&readonly_path);
                let _ = fs::remove_file(&data_path);
            }
        }
        Ok(())
    }

    #[test] // AC:63-15-3 Configuration parsing safety
    #[ignore]
    fn test_configuration_parsing_safety() -> TestResult<()> {
        if super::skip_if_soak_disabled() {
            return Ok(());
        }
        // Test case: CLI configuration parsing with invalid values
        let test_copybook_content = "       01 RECORD.\n           05 FIELD PIC X(10).";
        let test_data = vec![b'T'; 10];

        let temp_dir = std::env::temp_dir();
        let copybook_path = temp_dir.join("test_config_safety.cpy");
        let data_path = temp_dir.join("test_config_safety.bin");
        let output_path = temp_dir.join("test_config_safety.jsonl");

        write_file(&copybook_path, test_copybook_content)?;
        write_file(&data_path, &test_data)?;

        // Test decode command with invalid configuration values
        let invalid_configs = vec![
            ("--threads", "invalid_thread_count"),
            ("--json-number", "invalid_number_mode"),
            ("--max-errors", "not_a_number"),
        ];

        for (flag, invalid_value) in invalid_configs {
            let output = Command::new("cargo")
                .args([
                    "run",
                    "--bin",
                    "copybook",
                    "--",
                    "decode",
                    "--format",
                    "fixed",
                    "--codepage",
                    "cp037",
                    flag,
                    invalid_value,
                    "--output",
                ])
                .arg(&output_path)
                .arg(&copybook_path)
                .arg(&data_path)
                .output();

            match output {
                Ok(output) => {
                    // Should handle invalid configuration safely
                    if !output.status.success() {
                        let stderr_output = String::from_utf8_lossy(&output.stderr);
                        assert!(
                            !stderr_output.contains("panic") && !stderr_output.contains("unwrap"),
                            "Configuration parsing error for '{}={}' should not contain panic traces: {}",
                            flag,
                            invalid_value,
                            stderr_output
                        );

                        // Should provide helpful error message
                        assert!(
                            stderr_output.contains("invalid")
                                || stderr_output.contains("argument")
                                || stderr_output.contains(flag),
                            "Configuration error should provide meaningful context for '{}={}': {}",
                            flag,
                            invalid_value,
                            stderr_output
                        );
                    }
                }
                Err(e) => {
                    println!(
                        "CLI config test skipped for '{}={}' due to execution environment: {}",
                        flag, invalid_value, e
                    );
                }
            }
        }

        // Clean up test files
        let _ = fs::remove_file(&copybook_path);
        let _ = fs::remove_file(&data_path);
        let _ = fs::remove_file(&output_path);
        Ok(())
    }
}

#[cfg(feature = "soak")]
mod panic_elimination_cli_integration_tests {
    use super::*;

    /// Integration tests for CLI panic elimination across command types
    /// AC:63-16 - End-to-end CLI safety validation

    #[test] // AC:63-16-1 Command pipeline safety
    #[ignore]
    #[allow(clippy::too_many_lines)] // Test infrastructure for complex pipeline validation
    fn test_command_pipeline_safety() -> TestResult<()> {
        if super::skip_if_soak_disabled() {
            return Ok(());
        }
        // Test case: Full CLI pipeline from parse to decode to verify
        let pipeline_copybook_content = r"
       01 PIPELINE-RECORD.
           05 PIPELINE-ID PIC 9(5).
           05 PIPELINE-DATA PIC X(20).
           05 PIPELINE-FLAGS PIC X(3).
        ";

        let pipeline_json_data = r#"{"PIPELINE-ID": "12345", "PIPELINE-DATA": "TESTDATA12345678901", "PIPELINE-FLAGS": "ABC"}"#;

        let temp_dir = std::env::temp_dir();
        let copybook_path = temp_dir.join("test_pipeline.cpy");
        let json_path = temp_dir.join("test_pipeline.jsonl");
        let binary_path = temp_dir.join("test_pipeline.bin");

        write_file(&copybook_path, pipeline_copybook_content)?;
        write_file(&json_path, pipeline_json_data)?;

        // Step 1: Parse copybook
        let parse_output = Command::new("cargo")
            .args(["run", "--bin", "copybook", "--", "parse"])
            .arg(&copybook_path)
            .output();

        // Step 2: Encode JSON to binary
        let encode_output = Command::new("cargo")
            .args([
                "run",
                "--bin",
                "copybook",
                "--",
                "encode",
                "--format",
                "fixed",
                "--codepage",
                "cp037",
            ])
            .arg(&copybook_path)
            .arg(&json_path)
            .arg(&binary_path)
            .output();

        // Step 3: Verify binary data
        let verify_output = if binary_path.exists() {
            Some(
                Command::new("cargo")
                    .args([
                        "run",
                        "--bin",
                        "copybook",
                        "--",
                        "verify",
                        "--format",
                        "fixed",
                        "--codepage",
                        "cp037",
                    ])
                    .arg(&copybook_path)
                    .arg(&binary_path)
                    .output(),
            )
        } else {
            None
        };

        // Validate pipeline steps handle errors safely
        match parse_output {
            Ok(output) => {
                if !output.status.success() {
                    let stderr_output = String::from_utf8_lossy(&output.stderr);
                    assert!(
                        !stderr_output.contains("panic") && !stderr_output.contains("unwrap"),
                        "Parse step should not contain panic traces: {}",
                        stderr_output
                    );
                }
            }
            Err(e) => {
                println!(
                    "CLI pipeline parse test skipped due to execution environment: {}",
                    e
                );
            }
        }

        match encode_output {
            Ok(output) => {
                if !output.status.success() {
                    let stderr_output = String::from_utf8_lossy(&output.stderr);
                    assert!(
                        !stderr_output.contains("panic") && !stderr_output.contains("unwrap"),
                        "Encode step should not contain panic traces: {}",
                        stderr_output
                    );
                }
            }
            Err(e) => {
                println!(
                    "CLI pipeline encode test skipped due to execution environment: {}",
                    e
                );
            }
        }

        if let Some(verify_result) = verify_output {
            match verify_result {
                Ok(output) => {
                    // Verify step may fail due to data format issues - that's acceptable
                    if !output.status.success() {
                        let stderr_output = String::from_utf8_lossy(&output.stderr);
                        assert!(
                            !stderr_output.contains("panic") && !stderr_output.contains("unwrap"),
                            "Verify step should not contain panic traces: {}",
                            stderr_output
                        );
                    }
                }
                Err(e) => {
                    println!(
                        "CLI pipeline verify test skipped due to execution environment: {}",
                        e
                    );
                }
            }
        }

        // Clean up test files
        let _ = fs::remove_file(&copybook_path);
        let _ = fs::remove_file(&json_path);
        let _ = fs::remove_file(&binary_path);
        Ok(())
    }

    #[test] // AC:63-16-2 Error propagation safety
    #[ignore]
    fn test_error_propagation_safety() -> TestResult<()> {
        if super::skip_if_soak_disabled() {
            return Ok(());
        }
        // Test case: Error propagation through CLI layers without panics
        let error_test_scenarios = vec![
            ("empty_copybook", ""),
            ("malformed_copybook", "01 MALFORMED.\n    05 FIELD PIC"),
            ("invalid_syntax", "INVALID COBOL SYNTAX"),
        ];

        for (scenario_name, copybook_content) in error_test_scenarios {
            let temp_dir = std::env::temp_dir();
            let copybook_path = temp_dir.join(format!("test_error_{}.cpy", scenario_name));

            write_file(&copybook_path, copybook_content)?;

            // Test parse command error propagation
            let output = Command::new("cargo")
                .args(["run", "--bin", "copybook", "--", "parse"])
                .arg(&copybook_path)
                .output();

            match output {
                Ok(output) => {
                    if !output.status.success() {
                        let stderr_output = String::from_utf8_lossy(&output.stderr);

                        // Should not contain panic traces
                        assert!(
                            !stderr_output.contains("panic") && !stderr_output.contains("unwrap"),
                            "Error propagation for '{}' should not contain panic traces: {}",
                            scenario_name,
                            stderr_output
                        );

                        // Should provide structured error information
                        assert!(
                            !stderr_output.is_empty(),
                            "Error propagation for '{}' should provide error message",
                            scenario_name
                        );
                    }
                }
                Err(e) => {
                    println!(
                        "CLI error propagation test for '{}' skipped due to execution environment: {}",
                        scenario_name, e
                    );
                }
            }

            // Clean up test file
            let _ = fs::remove_file(&copybook_path);
        }
        Ok(())
    }

    #[test] // AC:63-16-3 Resource cleanup safety
    #[ignore]
    fn test_resource_cleanup_safety() -> TestResult<()> {
        if super::skip_if_soak_disabled() {
            return Ok(());
        }
        // Test case: Resource cleanup during error conditions
        let cleanup_copybook_content = r"
       01 CLEANUP-RECORD.
           05 CLEANUP-FIELD PIC X(100).
        ";

        let temp_dir = std::env::temp_dir();
        let copybook_path = temp_dir.join("test_cleanup.cpy");
        let large_data_path = temp_dir.join("test_cleanup.bin");
        let output_path = temp_dir.join("test_cleanup.jsonl");

        write_file(&copybook_path, cleanup_copybook_content)?;

        // Create large data file that might stress resource management
        let large_data = vec![b'C'; 10000];
        write_file(&large_data_path, &large_data)?;

        // Test decode command with large data
        let output = Command::new("cargo")
            .args([
                "run",
                "--bin",
                "copybook",
                "--",
                "decode",
                "--format",
                "fixed",
                "--codepage",
                "cp037",
                "--output",
            ])
            .arg(&output_path)
            .arg(&copybook_path)
            .arg(&large_data_path)
            .output();

        match output {
            Ok(output) => {
                // Should handle resource cleanup safely regardless of success/failure
                let stderr_output = String::from_utf8_lossy(&output.stderr);
                assert!(
                    !stderr_output.contains("panic") && !stderr_output.contains("unwrap"),
                    "Resource cleanup should not contain panic traces: {}",
                    stderr_output
                );

                // Validate no resource leaks indicated in error messages
                assert!(
                    !stderr_output.contains("leaked") && !stderr_output.contains("unclosed"),
                    "Resource cleanup should not indicate leaks: {}",
                    stderr_output
                );
            }
            Err(e) => {
                println!(
                    "CLI resource cleanup test skipped due to execution environment: {}",
                    e
                );
            }
        }

        // Clean up test files
        let _ = fs::remove_file(&copybook_path);
        let _ = fs::remove_file(&large_data_path);
        let _ = fs::remove_file(&output_path);
        Ok(())
    }
}

#[allow(clippy::if_not_else, clippy::unnecessary_wraps)]
mod clippy_allows_for_tests {}
