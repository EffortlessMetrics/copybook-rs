//! Integration tests for perf summarize functionality
//!
//! Tests end-to-end behavior of `cargo run -p xtask -- perf --summarize-last`

use std::fs;
use std::path::PathBuf;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn test_summarize_with_synthetic_perf_json() {
    // Create a temporary workspace with synthetic perf.json
    let temp_dir = TempDir::new().unwrap();
    let workspace_root = temp_dir.path();

    // Create scripts/bench directory structure
    let bench_dir = workspace_root.join("scripts/bench");
    fs::create_dir_all(&bench_dir).unwrap();

    // Write synthetic perf.json with good metrics
    let perf_json = r#"{
        "timestamp": "2025-11-14T12:00:00Z",
        "commit": "test123",
        "toolchain": "cargo bench (criterion)",
        "status": "pass",
        "display_mibps": 205.0,
        "comp3_mibps": 58.0,
        "summary": {
            "display_mibps": 205.0,
            "comp3_mibps": 58.0,
            "max_rss_mib": 128
        }
    }"#;
    fs::write(bench_dir.join("perf.json"), perf_json).unwrap();

    // Get path to xtask binary
    let xtask_bin = get_xtask_binary();

    // Run xtask perf --summarize-last from temp workspace
    let output = Command::new(&xtask_bin)
        .args(["perf", "--summarize-last"])
        .current_dir(workspace_root)
        .output()
        .expect("Failed to run xtask");

    // Verify successful execution
    assert!(
        output.status.success(),
        "xtask failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Verify output contains expected metrics
    assert!(stdout.contains("DISPLAY: 205.0 MiB/s"), "stdout: {}", stdout);
    assert!(stdout.contains("COMP-3: 58.0 MiB/s"), "stdout: {}", stdout);
    assert!(stdout.contains("SLO 80 MiB/s"), "stdout: {}", stdout);
    assert!(stdout.contains("SLO 40 MiB/s"), "stdout: {}", stdout);
    assert!(stdout.contains("✓ All SLOs met"), "stdout: {}", stdout);
}

#[test]
fn test_summarize_with_failing_slo() {
    let temp_dir = TempDir::new().unwrap();
    let workspace_root = temp_dir.path();

    let bench_dir = workspace_root.join("scripts/bench");
    fs::create_dir_all(&bench_dir).unwrap();

    // Write perf.json with metrics below SLO
    let perf_json = r#"{
        "display_mibps": 60.0,
        "comp3_mibps": 30.0
    }"#;
    fs::write(bench_dir.join("perf.json"), perf_json).unwrap();

    let xtask_bin = get_xtask_binary();

    let output = Command::new(&xtask_bin)
        .args(["perf", "--summarize-last"])
        .current_dir(workspace_root)
        .output()
        .expect("Failed to run xtask");

    assert!(output.status.success());

    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(stdout.contains("DISPLAY: 60.0 MiB/s"), "stdout: {}", stdout);
    assert!(stdout.contains("COMP-3: 30.0 MiB/s"), "stdout: {}", stdout);
    assert!(stdout.contains("⚠ SLOs not met"), "stdout: {}", stdout);
}

#[test]
fn test_summarize_missing_perf_json() {
    let temp_dir = TempDir::new().unwrap();
    let workspace_root = temp_dir.path();

    // Don't create perf.json - should fail gracefully
    let xtask_bin = get_xtask_binary();

    let output = Command::new(&xtask_bin)
        .args(["perf", "--summarize-last"])
        .current_dir(workspace_root)
        .output()
        .expect("Failed to run xtask");

    // Should fail with helpful error message
    assert!(!output.status.success());

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("No perf.json found") || stderr.contains("Run benchmarks first"),
        "stderr: {}",
        stderr
    );
}

#[test]
fn test_summarize_malformed_json() {
    let temp_dir = TempDir::new().unwrap();
    let workspace_root = temp_dir.path();

    let bench_dir = workspace_root.join("scripts/bench");
    fs::create_dir_all(&bench_dir).unwrap();

    // Write malformed JSON
    let perf_json = r#"{"display_mibps": "not a number"}"#;
    fs::write(bench_dir.join("perf.json"), perf_json).unwrap();

    let xtask_bin = get_xtask_binary();

    let output = Command::new(&xtask_bin)
        .args(["perf", "--summarize-last"])
        .current_dir(workspace_root)
        .output()
        .expect("Failed to run xtask");

    // Should fail on parsing
    assert!(!output.status.success());
}

#[test]
fn test_summarize_nested_summary_structure() {
    // Test that nested summary.{display,comp3}_mibps works
    let temp_dir = TempDir::new().unwrap();
    let workspace_root = temp_dir.path();

    let bench_dir = workspace_root.join("scripts/bench");
    fs::create_dir_all(&bench_dir).unwrap();

    let perf_json = r#"{
        "summary": {
            "display_mibps": 100.0,
            "comp3_mibps": 50.0
        }
    }"#;
    fs::write(bench_dir.join("perf.json"), perf_json).unwrap();

    let xtask_bin = get_xtask_binary();

    let output = Command::new(&xtask_bin)
        .args(["perf", "--summarize-last"])
        .current_dir(workspace_root)
        .output()
        .expect("Failed to run xtask");

    assert!(output.status.success());

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("DISPLAY: 100.0 MiB/s"), "stdout: {}", stdout);
    assert!(stdout.contains("COMP-3: 50.0 MiB/s"), "stdout: {}", stdout);
}

/// Helper to locate xtask binary
fn get_xtask_binary() -> PathBuf {
    // Try debug build first (most common during testing)
    let debug_path = PathBuf::from(env!("CARGO_TARGET_TMPDIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("debug/xtask");

    if debug_path.exists() {
        return debug_path;
    }

    // Fallback: use cargo to build and run
    // This is slower but more reliable
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = manifest_dir.parent().unwrap();
    workspace_root.join("target/debug/xtask")
}
