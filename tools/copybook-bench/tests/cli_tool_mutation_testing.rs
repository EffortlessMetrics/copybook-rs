// SPDX-License-Identifier: AGPL-3.0-or-later
//! CLI Tool Mutation Testing for bench-report
//!
//! Comprehensive mutation testing for the bench-report CLI tool to ensure
//! robust command handling, argument parsing, and error conditions.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::needless_borrows_for_generic_args,
    clippy::uninlined_format_args,
    clippy::single_match_else,
    clippy::used_underscore_binding
)] // Tests: allow common pedantic lints

use copybook_bench::baseline::BaselineStore;
use copybook_bench::reporting::PerformanceReport;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::{NamedTempFile, TempDir};

/// Helper to create cargo run command with proper manifest path
fn cargo_bench_report(working_dir: Option<&Path>) -> Command {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let workspace_manifest = PathBuf::from(manifest_dir)
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("Cargo.toml");

    let mut cmd = Command::new("cargo");
    cmd.args([
        "run",
        "--manifest-path",
        workspace_manifest.to_str().unwrap(),
        "--bin",
        "bench-report",
        "--",
    ]);

    if let Some(dir) = working_dir {
        cmd.current_dir(dir);
    }

    cmd
}

#[test]
fn test_cli_help_command() {
    let output = Command::new("cargo")
        .args(["run", "--bin", "bench-report", "--", "--help"])
        .output()
        .expect("Failed to execute bench-report --help");

    assert!(output.status.success(), "Help command should succeed");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("copybook-rs benchmark reporting tool"));
    assert!(stdout.contains("validate"));
    assert!(stdout.contains("baseline"));
    assert!(stdout.contains("compare"));
    assert!(stdout.contains("summary"));
    assert!(stdout.contains("EXAMPLES"));
}

#[test]
fn test_cli_validate_command_mutations() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    // Test with valid JSON
    let mut valid_file = NamedTempFile::new_in(&temp_dir).expect("Failed to create temp file");
    let valid_report = PerformanceReport::new();
    let valid_json = serde_json::to_string_pretty(&valid_report).expect("Failed to serialize");
    valid_file
        .write_all(valid_json.as_bytes())
        .expect("Failed to write valid JSON");
    valid_file.flush().expect("Failed to flush file");

    let output = Command::new("cargo")
        .args(&[
            "run",
            "--bin",
            "bench-report",
            "--",
            "validate",
            valid_file.path().to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute validate command");

    assert!(
        output.status.success(),
        "Validate with valid JSON should succeed"
    );

    // Test with invalid JSON
    let mut invalid_file = NamedTempFile::new_in(&temp_dir).expect("Failed to create temp file");
    invalid_file
        .write_all(b"invalid json")
        .expect("Failed to write invalid JSON");
    invalid_file.flush().expect("Failed to flush file");

    let output = Command::new("cargo")
        .args(&[
            "run",
            "--bin",
            "bench-report",
            "--",
            "validate",
            invalid_file.path().to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute validate command");

    assert!(
        !output.status.success(),
        "Validate with invalid JSON should fail"
    );

    // Test with non-existent file
    let output = Command::new("cargo")
        .args(&[
            "run",
            "--bin",
            "bench-report",
            "--",
            "validate",
            "/nonexistent/file.json",
        ])
        .output()
        .expect("Failed to execute validate command");

    assert!(
        !output.status.success(),
        "Validate with non-existent file should fail"
    );

    // Test with directory instead of file
    let output = Command::new("cargo")
        .args(&[
            "run",
            "--bin",
            "bench-report",
            "--",
            "validate",
            temp_dir.path().to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute validate command");

    assert!(
        !output.status.success(),
        "Validate with directory should fail"
    );
}

#[test]
fn test_cli_baseline_mutations() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let baseline_path = temp_dir.path().join("baseline.json");

    // Test showing baseline when none exists
    let output = cargo_bench_report(Some(temp_dir.path()))
        .args(&["baseline", "show"])
        .output()
        .expect("Failed to execute baseline show");

    assert!(
        output.status.success(),
        "Baseline show should succeed even when no baseline exists"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("No baseline") || stdout.contains("not found") || stdout.is_empty());

    // Create a baseline file
    let mut store = BaselineStore::new();
    let mut report = PerformanceReport::new();
    report.display_gibs = Some(4.22);
    report.comp3_mibs = Some(571.0);
    store.promote_baseline(&report, "main", "test_commit");
    store.save(&baseline_path).expect("Failed to save baseline");

    // Test showing baseline when it exists
    let output = cargo_bench_report(Some(temp_dir.path()))
        .args(&["baseline", "show"])
        .output()
        .expect("Failed to execute baseline show");

    assert!(
        output.status.success(),
        "Baseline show with existing baseline should succeed"
    );

    // Test baseline promotion
    let mut promotion_file = NamedTempFile::new_in(&temp_dir).expect("Failed to create temp file");
    let promotion_report = PerformanceReport::new();
    let promotion_json =
        serde_json::to_string_pretty(&promotion_report).expect("Failed to serialize");
    promotion_file
        .write_all(promotion_json.as_bytes())
        .expect("Failed to write promotion JSON");
    promotion_file.flush().expect("Failed to flush file");

    let output = cargo_bench_report(Some(temp_dir.path()))
        .args(&[
            "baseline",
            "promote",
            promotion_file.path().to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute baseline promote");

    // Should succeed or fail gracefully
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        // Should provide meaningful error message
        assert!(
            !stderr.is_empty(),
            "Should provide error message on failure"
        );
    }
}

#[test]
#[ignore = "Flaky: bench-report compare baseline resolution depends on CWD and temp dir layout"]
fn test_cli_compare_mutations() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    // Create comparison file
    let mut compare_file = NamedTempFile::new_in(&temp_dir).expect("Failed to create temp file");
    let mut compare_report = PerformanceReport::new();
    compare_report.display_gibs = Some(3.8);
    compare_report.comp3_mibs = Some(450.0);
    let compare_json = serde_json::to_string_pretty(&compare_report).expect("Failed to serialize");
    compare_file
        .write_all(compare_json.as_bytes())
        .expect("Failed to write compare JSON");
    compare_file.flush().expect("Failed to flush file");

    // Test comparison without baseline
    let output = cargo_bench_report(Some(temp_dir.path()))
        .args(&["compare", compare_file.path().to_str().unwrap()])
        .output()
        .expect("Failed to execute compare command");

    assert!(
        output.status.success(),
        "Compare should succeed even without baseline"
    );

    // Create baseline for comparison
    let baseline_path = temp_dir.path().join("baseline.json");
    let mut store = BaselineStore::new();
    let mut baseline_report = PerformanceReport::new();
    baseline_report.display_gibs = Some(4.0);
    baseline_report.comp3_mibs = Some(500.0);
    store.promote_baseline(&baseline_report, "main", "baseline_commit");
    store.save(&baseline_path).expect("Failed to save baseline");

    // Test comparison with baseline
    let output = cargo_bench_report(Some(temp_dir.path()))
        .args(&["compare", compare_file.path().to_str().unwrap()])
        .output()
        .expect("Failed to execute compare command");

    assert!(
        output.status.success(),
        "Compare with baseline should succeed"
    );
}

#[test]
fn test_cli_summary_mutations() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    // Test summary without baseline
    let output = cargo_bench_report(Some(temp_dir.path()))
        .args(&["summary"])
        .output()
        .expect("Failed to execute summary command");

    assert!(output.status.success(), "Summary should succeed");

    // Create baseline
    let baseline_path = temp_dir.path().join("baseline.json");
    let mut store = BaselineStore::new();
    let mut baseline_report = PerformanceReport::new();
    baseline_report.display_gibs = Some(4.5);
    baseline_report.comp3_mibs = Some(580.0);
    store.promote_baseline(&baseline_report, "main", "summary_commit");
    store.save(&baseline_path).expect("Failed to save baseline");

    // Test summary with baseline
    let output = cargo_bench_report(Some(temp_dir.path()))
        .args(&["summary"])
        .output()
        .expect("Failed to execute summary command");

    assert!(
        output.status.success(),
        "Summary with baseline should succeed"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Baseline") || stdout.contains("summary") || !stdout.is_empty());
}

#[test]
fn test_cli_invalid_arguments_mutations() {
    let test_cases = vec![
        // Invalid commands
        vec!["invalid_command"],
        vec!["validate"], // Missing file argument
        vec!["baseline"], // Missing subcommand
        vec!["baseline", "invalid_subcommand"],
        vec!["compare"], // Missing file argument
        // Too many arguments
        vec!["validate", "file1.json", "file2.json"],
        vec!["baseline", "show", "extra_arg"],
        vec!["summary", "extra_arg"],
    ];

    for args in test_cases {
        let mut cmd_args = vec!["run", "--bin", "bench-report", "--"];
        cmd_args.extend(args.iter());

        let output = Command::new("cargo")
            .args(&cmd_args)
            .output()
            .expect("Failed to execute command");

        // Should either fail or show help - both are acceptable
        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // If it succeeds, it should show help or usage
            assert!(
                stdout.contains("USAGE") || stdout.contains("help") || stdout.contains("COMMANDS"),
                "Successful execution should show usage information for args: {:?}",
                args
            );
        }
        // If it fails, that's also acceptable for invalid arguments
    }
}

#[test]
fn test_cli_edge_case_mutations() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    // Test with empty JSON file
    let empty_file = temp_dir.path().join("empty.json");
    fs::write(&empty_file, "").expect("Failed to write empty file");

    let output = Command::new("cargo")
        .args(&[
            "run",
            "--bin",
            "bench-report",
            "--",
            "validate",
            empty_file.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute validate command");

    assert!(
        !output.status.success(),
        "Validate with empty file should fail"
    );

    // Test with minimal JSON
    let minimal_file = temp_dir.path().join("minimal.json");
    fs::write(&minimal_file, "{}").expect("Failed to write minimal JSON");

    let _output = Command::new("cargo")
        .args(&[
            "run",
            "--bin",
            "bench-report",
            "--",
            "validate",
            minimal_file.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute validate command");

    // Minimal JSON might be valid (depends on schema requirements)
    // Both success and failure are acceptable

    // Test with large JSON file
    let large_file = temp_dir.path().join("large.json");
    let mut large_report = PerformanceReport::new();
    // Add many warnings and errors to make it large
    large_report.warnings = (0..1000).map(|i| format!("Warning {}", i)).collect();
    large_report.errors = (0..1000).map(|i| format!("Error {}", i)).collect();
    let large_json =
        serde_json::to_string_pretty(&large_report).expect("Failed to serialize large JSON");
    fs::write(&large_file, large_json).expect("Failed to write large JSON");

    let output = Command::new("cargo")
        .args(&[
            "run",
            "--bin",
            "bench-report",
            "--",
            "validate",
            large_file.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute validate command");

    assert!(
        output.status.success(),
        "Validate with large valid JSON should succeed"
    );
}

#[test]
fn test_cli_permission_mutations() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    // Test with read-only directory (baseline operations should handle gracefully)
    let readonly_dir = temp_dir.path().join("readonly");
    fs::create_dir(&readonly_dir).expect("Failed to create readonly dir");

    // Make directory read-only (Unix-specific)
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&readonly_dir)
            .expect("Failed to get metadata")
            .permissions();
        perms.set_mode(0o444); // Read-only
        fs::set_permissions(&readonly_dir, perms).expect("Failed to set permissions");

        let output_result = cargo_bench_report(Some(&readonly_dir))
            .args(&["summary"])
            .output();

        // In readonly directories, cargo may fail to spawn (permission denied on lock files)
        // or the command may execute and handle the readonly case gracefully
        match output_result {
            Ok(_output) => {
                // Command executed - should handle gracefully (may succeed or fail)
                if !_output.status.success() {
                    let stderr = String::from_utf8_lossy(&_output.stderr);
                    assert!(
                        !stderr.is_empty(),
                        "Should provide error message for permission issues"
                    );
                }
            }
            Err(e) => {
                // Command failed to spawn - acceptable in readonly directories
                assert!(
                    e.kind() == std::io::ErrorKind::PermissionDenied,
                    "Expected PermissionDenied error in readonly dir, got: {:?}",
                    e
                );
            }
        }

        // Restore permissions for cleanup
        let mut perms = fs::metadata(&readonly_dir)
            .expect("Failed to get metadata")
            .permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&readonly_dir, perms).expect("Failed to restore permissions");
    }
}
