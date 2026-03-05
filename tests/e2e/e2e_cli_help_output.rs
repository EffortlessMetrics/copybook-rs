// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for CLI help and version output.
//!
//! Validates that `--help` for every subcommand produces well-formed output
//! containing expected flags, and that `--version` emits the version number.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use std::process::Output;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn stdout_str(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

fn stderr_str(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).into_owned()
}

/// Assert that stderr does not contain panic markers.
fn assert_no_panic(stderr: &str) {
    assert!(
        !stderr.contains("panicked at"),
        "CLI panicked! stderr:\n{stderr}"
    );
}

/// Run `copybook <args>` and return the output, asserting success.
fn run_help(args: &[&str]) -> Output {
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .args(args)
        .output()
        .unwrap_or_else(|e| panic!("failed to run copybook {args:?}: {e}"));
    assert_eq!(
        output.status.code(),
        Some(0),
        "expected exit 0 for {args:?}, stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));
    output
}

/// Combine stdout and stderr for help text searching (clap may use either).
fn help_text(output: &Output) -> String {
    format!("{}{}", stdout_str(output), stderr_str(output))
}

// =========================================================================
// 1. `copybook --help` exits 0 and shows usage
// =========================================================================

#[test]
fn help_root_exits_zero_and_shows_usage() {
    let output = run_help(&["--help"]);
    let text = help_text(&output);
    assert!(
        text.contains("Usage") || text.contains("usage") || text.contains("USAGE"),
        "root --help should contain usage text"
    );
}

// =========================================================================
// 2. `copybook parse --help` exits 0
// =========================================================================

#[test]
fn help_parse_subcommand() {
    let output = run_help(&["parse", "--help"]);
    let text = help_text(&output);
    assert!(
        text.contains("parse") || text.contains("Parse"),
        "parse --help should describe parse subcommand"
    );
}

// =========================================================================
// 3. `copybook inspect --help` exits 0
// =========================================================================

#[test]
fn help_inspect_subcommand() {
    let output = run_help(&["inspect", "--help"]);
    let text = help_text(&output);
    assert!(
        text.contains("inspect") || text.contains("Inspect"),
        "inspect --help should describe inspect subcommand"
    );
}

// =========================================================================
// 4. `copybook decode --help` exits 0 and mentions key flags
// =========================================================================

#[test]
fn help_decode_subcommand() {
    let output = run_help(&["decode", "--help"]);
    let text = help_text(&output);
    assert!(
        text.contains("decode") || text.contains("Decode"),
        "decode --help should describe decode subcommand"
    );
}

// =========================================================================
// 5. `copybook encode --help` exits 0
// =========================================================================

#[test]
fn help_encode_subcommand() {
    let output = run_help(&["encode", "--help"]);
    let text = help_text(&output);
    assert!(
        text.contains("encode") || text.contains("Encode"),
        "encode --help should describe encode subcommand"
    );
}

// =========================================================================
// 6. `copybook verify --help` exits 0
// =========================================================================

#[test]
fn help_verify_subcommand() {
    let output = run_help(&["verify", "--help"]);
    let text = help_text(&output);
    assert!(
        text.contains("verify") || text.contains("Verify"),
        "verify --help should describe verify subcommand"
    );
}

// =========================================================================
// 7. `copybook determinism --help` exits 0
// =========================================================================

#[test]
fn help_determinism_subcommand() {
    let output = run_help(&["determinism", "--help"]);
    let text = help_text(&output);
    assert!(
        text.contains("determinism") || text.contains("Determinism"),
        "determinism --help should describe determinism subcommand"
    );
}

// =========================================================================
// 8. `copybook --version` contains a version number (semver-like)
// =========================================================================

#[test]
fn version_output_contains_version_number() {
    let output = run_help(&["--version"]);
    let text = help_text(&output);
    // Expect something like "copybook 0.4.3" or similar semver pattern.
    let has_version = regex::Regex::new(r"\d+\.\d+\.\d+").unwrap().is_match(&text);
    assert!(
        has_version,
        "version output should contain semver number, got: {text}"
    );
}

// =========================================================================
// 9. decode --help mentions --format flag
// =========================================================================

#[test]
fn decode_help_contains_format_flag() {
    let output = run_help(&["decode", "--help"]);
    let text = help_text(&output);
    assert!(
        text.contains("--format"),
        "decode --help should mention --format flag, got:\n{text}"
    );
}

// =========================================================================
// 10. decode --help mentions --codepage flag
// =========================================================================

#[test]
fn decode_help_contains_codepage_flag() {
    let output = run_help(&["decode", "--help"]);
    let text = help_text(&output);
    assert!(
        text.contains("--codepage"),
        "decode --help should mention --codepage flag, got:\n{text}"
    );
}

// =========================================================================
// 11. decode --help mentions --dialect flag
// =========================================================================

#[test]
fn decode_help_contains_dialect_flag() {
    let output = run_help(&["decode", "--help"]);
    let text = help_text(&output);
    assert!(
        text.contains("--dialect"),
        "decode --help should mention --dialect flag, got:\n{text}"
    );
}

// =========================================================================
// 12. decode --help mentions --select flag
// =========================================================================

#[test]
fn decode_help_contains_select_flag() {
    let output = run_help(&["decode", "--help"]);
    let text = help_text(&output);
    assert!(
        text.contains("--select"),
        "decode --help should mention --select flag, got:\n{text}"
    );
}

// =========================================================================
// 13. decode --help mentions --threads flag
// =========================================================================

#[test]
fn decode_help_contains_threads_flag() {
    let output = run_help(&["decode", "--help"]);
    let text = help_text(&output);
    assert!(
        text.contains("--threads"),
        "decode --help should mention --threads flag, got:\n{text}"
    );
}

// =========================================================================
// 14. encode --help mentions --format flag
// =========================================================================

#[test]
fn encode_help_contains_format_flag() {
    let output = run_help(&["encode", "--help"]);
    let text = help_text(&output);
    assert!(
        text.contains("--format"),
        "encode --help should mention --format flag, got:\n{text}"
    );
}

// =========================================================================
// 15. encode --help mentions --codepage flag
// =========================================================================

#[test]
fn encode_help_contains_codepage_flag() {
    let output = run_help(&["encode", "--help"]);
    let text = help_text(&output);
    assert!(
        text.contains("--codepage"),
        "encode --help should mention --codepage flag, got:\n{text}"
    );
}

// =========================================================================
// 16. parse --help mentions --dialect flag
// =========================================================================

#[test]
fn parse_help_contains_dialect_flag() {
    let output = run_help(&["parse", "--help"]);
    let text = help_text(&output);
    assert!(
        text.contains("--dialect"),
        "parse --help should mention --dialect flag, got:\n{text}"
    );
}

// =========================================================================
// 17. root --help lists all subcommands
// =========================================================================

#[test]
fn root_help_lists_all_subcommands() {
    let output = run_help(&["--help"]);
    let text = help_text(&output);
    for subcmd in &[
        "parse",
        "inspect",
        "decode",
        "encode",
        "verify",
        "determinism",
    ] {
        assert!(
            text.contains(subcmd),
            "root --help should list '{subcmd}' subcommand, got:\n{text}"
        );
    }
}

// =========================================================================
// 18. verify --help mentions --format flag
// =========================================================================

#[test]
fn verify_help_contains_format_flag() {
    let output = run_help(&["verify", "--help"]);
    let text = help_text(&output);
    assert!(
        text.contains("--format"),
        "verify --help should mention --format flag, got:\n{text}"
    );
}

// =========================================================================
// 19. version output contains the program name
// =========================================================================

#[test]
fn version_output_contains_program_name() {
    let output = run_help(&["--version"]);
    let text = help_text(&output);
    assert!(
        text.contains("copybook"),
        "version output should contain program name, got: {text}"
    );
}

// =========================================================================
// 20. `-h` short flag works the same as `--help`
// =========================================================================

#[test]
fn short_help_flag_works() {
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .arg("-h")
        .output()
        .expect("run -h");
    assert_eq!(output.status.code(), Some(0), "exit code must be 0");
    let text = help_text(&output);
    assert!(
        text.contains("Usage") || text.contains("usage") || text.contains("USAGE"),
        "-h should produce help output"
    );
}

// =========================================================================
// 21. `-V` short flag works the same as `--version`
// =========================================================================

#[test]
fn short_version_flag_works() {
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .arg("-V")
        .output()
        .expect("run -V");
    assert_eq!(output.status.code(), Some(0), "exit code must be 0");
    let text = help_text(&output);
    let has_version = regex::Regex::new(r"\d+\.\d+\.\d+").unwrap().is_match(&text);
    assert!(has_version, "-V should produce version number, got: {text}");
}
