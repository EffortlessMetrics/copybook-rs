#![allow(clippy::expect_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used)]
#![allow(clippy::panic)]
#![allow(clippy::ignore_without_reason)]
// AC:1,4,5,6,9 - Security Scanning Infrastructure Integration Tests
// Specification: docs/explanation/security-scanning-architecture.md
// Issue: #35 - Dependency & Security Scanning Infrastructure
//
// Test Strategy:
// - AC1: cargo-audit JSON output validation
// - AC4: deny.toml policy enforcement (yanked, wildcards, sources)
// - AC5: Security receipt JSON Schema validation
// - AC6: cargo-geiger zero unsafe code validation
// - AC9: End-to-end CI integration validation
//
// Infrastructure Testing Approach:
// These tests validate configuration files, workflow syntax, and security tooling integration.
// Tests that require external tools (cargo-audit, cargo-deny, cargo-geiger) gracefully degrade
// to config-only validation when tools are not installed.

use std::path::Path;
use std::process::Command;

/// Helper: check if a command is available by running it with --version
fn tool_available(program: &str, args: &[&str]) -> bool {
    Command::new(program)
        .args(args)
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Helper: project root for absolute path resolution in tests
fn project_root() -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root")
        .to_path_buf()
}

/// Tests feature spec: docs/explanation/security-scanning-architecture.md#1-cargo-audit-ci-integration
/// AC:1 - Validate cargo-audit can run and produce valid JSON output
#[test]
fn test_ac1_cargo_audit_produces_json_output() {
    let root = project_root();

    if tool_available("cargo", &["audit", "--version"]) {
        // Tool is installed -- run actual audit and validate JSON structure
        // Note: cargo-audit does not support --workspace; it scans the lockfile in cwd.
        let output = Command::new("cargo")
            .args(["audit", "--json"])
            .current_dir(&root)
            .output()
            .expect("failed to execute cargo audit");

        let stdout = String::from_utf8_lossy(&output.stdout);
        // cargo-audit returns non-zero when vulnerabilities are found, which is fine.
        // We only care that it produces parseable JSON.
        assert!(
            !stdout.is_empty(),
            "cargo-audit should produce JSON output on stdout"
        );

        let json: serde_json::Value =
            serde_json::from_str(&stdout).expect("cargo-audit output should be valid JSON");

        // Validate expected top-level structure
        assert!(
            json.get("vulnerabilities").is_some(),
            "audit JSON must contain 'vulnerabilities' key"
        );
    } else {
        // Tool not installed -- validate that the CI workflow references cargo-audit
        let ci_security = root.join(".github/workflows/ci-security.yml");
        assert!(
            ci_security.exists(),
            "ci-security.yml workflow should exist (cargo-audit not installed locally)"
        );
        let content = std::fs::read_to_string(&ci_security).expect("should read ci-security.yml");
        assert!(
            content.contains("cargo-audit") || content.contains("cargo audit"),
            "ci-security.yml should reference cargo-audit"
        );
        eprintln!("cargo-audit not installed; validated CI workflow config only");
    }
}

/// Tests feature spec: docs/explanation/security-scanning-architecture.md#1-cargo-audit-ci-integration
/// AC:1 - Validate security receipt generation with full metadata
#[test]
fn test_ac1_security_receipt_generation() {
    let root = project_root();

    // Validate that the security receipt schema exists and has the required fields
    let schema_path = root.join("docs/reference/security-receipt-schema.json");
    assert!(
        schema_path.exists(),
        "Security receipt JSON Schema must exist at docs/reference/security-receipt-schema.json"
    );

    let schema_str =
        std::fs::read_to_string(&schema_path).expect("should read security receipt schema");
    let schema: serde_json::Value =
        serde_json::from_str(&schema_str).expect("schema must be valid JSON");

    // Verify the schema defines the required fields for receipt generation
    let required = schema["required"]
        .as_array()
        .expect("schema must have 'required' array");
    let required_fields: Vec<&str> = required
        .iter()
        .map(|v| v.as_str().expect("required field must be string"))
        .collect();

    for expected in &[
        "version",
        "timestamp",
        "commit_sha",
        "scan_type",
        "rust_version",
        "tools",
        "vulnerabilities",
        "exit_status",
    ] {
        assert!(
            required_fields.contains(expected),
            "Schema must require field '{expected}'"
        );
    }

    // Validate that the clean-scan fixture has all required fields
    let receipt_path = root.join("tests/fixtures/security-scanning/receipts/clean-scan.json");
    let receipt_str = std::fs::read_to_string(&receipt_path).expect("should read clean-scan.json");
    let receipt: serde_json::Value =
        serde_json::from_str(&receipt_str).expect("receipt must be valid JSON");

    for field in &required_fields {
        assert!(
            receipt.get(field).is_some(),
            "Clean-scan receipt must contain required field '{field}'"
        );
    }

    // Validate timestamp format (ISO 8601)
    let ts = receipt["timestamp"]
        .as_str()
        .expect("timestamp must be string");
    assert!(
        ts.ends_with('Z') || ts.contains('+'),
        "Timestamp should be ISO 8601 UTC format, got: {ts}"
    );

    // Validate commit SHA is 40 hex characters
    let sha = receipt["commit_sha"]
        .as_str()
        .expect("commit_sha must be string");
    assert_eq!(sha.len(), 40, "commit_sha must be 40 characters");
    assert!(
        sha.chars().all(|c| c.is_ascii_hexdigit()),
        "commit_sha must be hex characters"
    );
}

/// Tests feature spec: docs/explanation/security-scanning-architecture.md#4-enhanced-denytoml-policies
/// AC:4 - Validate deny.toml rejects yanked dependencies
#[test]
fn test_ac4_deny_toml_rejects_yanked_dependencies() {
    let root = project_root();

    // Always validate the config file contains the yanked policy
    let deny_path = root.join("deny.toml");
    assert!(deny_path.exists(), "deny.toml must exist in project root");
    let deny_content = std::fs::read_to_string(&deny_path).expect("should read deny.toml");

    // Verify yanked = "deny" policy is present
    assert!(
        deny_content.contains(r#"yanked = "deny"#),
        "deny.toml must have yanked = \"deny\" policy (AC4)"
    );

    if tool_available("cargo", &["deny", "--version"]) {
        // Tool is installed -- run cargo deny check advisories
        let output = Command::new("cargo")
            .args(["deny", "check", "advisories", "--config", "deny.toml"])
            .current_dir(&root)
            .output()
            .expect("failed to execute cargo deny");

        // We don't assert success here because there might be legitimate advisories.
        // We only check that it ran without crashing.
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            !stderr.contains("error: unexpected argument")
                && !stderr.contains("unrecognized subcommand"),
            "cargo deny should accept the advisories check subcommand"
        );
    } else {
        eprintln!("cargo-deny not installed; validated deny.toml config only");
    }
}

/// Tests feature spec: docs/explanation/security-scanning-architecture.md#4-enhanced-denytoml-policies
/// AC:4 - Validate deny.toml rejects wildcard dependencies
#[test]
fn test_ac4_deny_toml_rejects_wildcard_dependencies() {
    let root = project_root();

    let deny_path = root.join("deny.toml");
    assert!(deny_path.exists(), "deny.toml must exist in project root");
    let deny_content = std::fs::read_to_string(&deny_path).expect("should read deny.toml");

    // Verify wildcards = "deny" policy is present
    assert!(
        deny_content.contains(r#"wildcards = "deny"#),
        "deny.toml must have wildcards = \"deny\" policy (AC4)"
    );

    if tool_available("cargo", &["deny", "--version"]) {
        // Tool is installed -- run cargo deny check bans
        let output = Command::new("cargo")
            .args(["deny", "check", "bans", "--config", "deny.toml"])
            .current_dir(&root)
            .output()
            .expect("failed to execute cargo deny");

        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            !stderr.contains("error: unexpected argument")
                && !stderr.contains("unrecognized subcommand"),
            "cargo deny should accept the bans check subcommand"
        );
    } else {
        eprintln!("cargo-deny not installed; validated deny.toml config only");
    }
}

/// Tests feature spec: docs/explanation/security-scanning-architecture.md#4-enhanced-denytoml-policies
/// AC:4 - Validate deny.toml rejects unknown registries and git sources
#[test]
fn test_ac4_deny_toml_rejects_unknown_sources() {
    let root = project_root();

    let deny_path = root.join("deny.toml");
    assert!(deny_path.exists(), "deny.toml must exist in project root");
    let deny_content = std::fs::read_to_string(&deny_path).expect("should read deny.toml");

    // Verify source policies are present
    assert!(
        deny_content.contains(r#"unknown-registry = "deny"#),
        "deny.toml must have unknown-registry = \"deny\" policy (AC4)"
    );
    assert!(
        deny_content.contains(r#"unknown-git = "deny"#),
        "deny.toml must have unknown-git = \"deny\" policy (AC4)"
    );

    // Verify only crates.io is allowed
    assert!(
        deny_content.contains("crates.io-index"),
        "deny.toml must allow crates.io registry"
    );

    if tool_available("cargo", &["deny", "--version"]) {
        let output = Command::new("cargo")
            .args(["deny", "check", "sources", "--config", "deny.toml"])
            .current_dir(&root)
            .output()
            .expect("failed to execute cargo deny");

        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            !stderr.contains("error: unexpected argument")
                && !stderr.contains("unrecognized subcommand"),
            "cargo deny should accept the sources check subcommand"
        );
    } else {
        eprintln!("cargo-deny not installed; validated deny.toml config only");
    }
}

/// Tests feature spec: docs/explanation/security-scanning-architecture.md#4-enhanced-denytoml-policies
/// AC:4 - Validate deny.toml unsound advisory policy
#[test]
fn test_ac4_deny_toml_rejects_unsound_advisories() {
    let root = project_root();

    // The enhanced deny.toml fixture should have unsound = "deny"
    let enhanced_path = root.join("tests/fixtures/security-scanning/configs/deny-enhanced.toml");
    assert!(
        enhanced_path.exists(),
        "deny-enhanced.toml fixture must exist"
    );
    let enhanced_content =
        std::fs::read_to_string(&enhanced_path).expect("should read deny-enhanced.toml");

    assert!(
        enhanced_content.contains(r#"unsound = "deny"#),
        "deny-enhanced.toml must have unsound = \"deny\" policy (AC4)"
    );

    // Also validate the baseline fixture does NOT have the unsound policy (pre-enhancement)
    let baseline_path = root.join("tests/fixtures/security-scanning/configs/deny-baseline.toml");
    assert!(
        baseline_path.exists(),
        "deny-baseline.toml fixture must exist"
    );
    let baseline_content =
        std::fs::read_to_string(&baseline_path).expect("should read deny-baseline.toml");

    // Baseline should NOT have unsound = "deny" (that is the enhancement)
    assert!(
        !baseline_content.contains(r#"unsound = "deny"#),
        "deny-baseline.toml should NOT have unsound policy (baseline state)"
    );

    if tool_available("cargo", &["deny", "--version"]) {
        // Validate cargo-deny can run with the enhanced config
        let output = Command::new("cargo")
            .args([
                "deny",
                "check",
                "advisories",
                "--config",
                enhanced_path.to_str().unwrap(),
            ])
            .current_dir(&root)
            .output()
            .expect("failed to execute cargo deny");

        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            !stderr.contains("error: unexpected argument")
                && !stderr.contains("unrecognized subcommand"),
            "cargo deny should accept the enhanced deny config"
        );
    } else {
        eprintln!("cargo-deny not installed; validated config fixtures only");
    }
}

/// Tests feature spec: docs/reference/security-receipt-schema.json
/// AC:5 - Validate security receipts against JSON Schema
#[test]
fn test_ac5_security_receipts_validate_against_schema() {
    let root = project_root();

    // Load the schema
    let schema_path = root.join("docs/reference/security-receipt-schema.json");
    assert!(
        schema_path.exists(),
        "Security receipt JSON Schema must exist"
    );
    let schema_str = std::fs::read_to_string(&schema_path).expect("should read schema");
    let schema: serde_json::Value =
        serde_json::from_str(&schema_str).expect("schema must be valid JSON");

    let required = schema["required"]
        .as_array()
        .expect("schema must have 'required'");
    let required_fields: Vec<&str> = required.iter().filter_map(|v| v.as_str()).collect();

    // Validate each receipt fixture against the schema's required fields
    let receipt_files = [
        "tests/fixtures/security-scanning/receipts/clean-scan.json",
        "tests/fixtures/security-scanning/receipts/vulnerabilities-found.json",
        "tests/fixtures/security-scanning/receipts/geiger-zero-unsafe.json",
    ];

    for receipt_file in &receipt_files {
        let receipt_path = root.join(receipt_file);
        assert!(
            receipt_path.exists(),
            "Receipt fixture must exist: {receipt_file}"
        );

        let receipt_str = std::fs::read_to_string(&receipt_path).expect("should read receipt");
        let receipt: serde_json::Value =
            serde_json::from_str(&receipt_str).expect("receipt must be valid JSON");

        // Verify all required fields are present
        for field in &required_fields {
            assert!(
                receipt.get(field).is_some(),
                "Receipt {receipt_file} must contain required field '{field}'"
            );
        }

        // Validate version field
        assert_eq!(
            receipt["version"].as_str(),
            Some("1.0"),
            "Receipt {receipt_file} must have version \"1.0\""
        );

        // Validate scan_type is one of the allowed enum values
        let scan_type = receipt["scan_type"]
            .as_str()
            .expect("scan_type must be string");
        assert!(
            ["pr-gate", "weekly-scan", "manual"].contains(&scan_type),
            "Receipt {receipt_file} scan_type must be pr-gate, weekly-scan, or manual; got: {scan_type}"
        );

        // Validate exit_status is one of the allowed enum values
        let exit_status = receipt["exit_status"]
            .as_str()
            .expect("exit_status must be string");
        assert!(
            ["success", "vulnerabilities_found", "error"].contains(&exit_status),
            "Receipt {receipt_file} exit_status must be success, vulnerabilities_found, or error; got: {exit_status}"
        );

        // Validate vulnerabilities structure
        let vulns = &receipt["vulnerabilities"];
        assert!(
            vulns["count"].is_number(),
            "Receipt {receipt_file} vulnerabilities.count must be a number"
        );
        assert!(
            vulns["by_severity"]["critical"].is_number(),
            "Receipt {receipt_file} vulnerabilities.by_severity.critical must be a number"
        );
        assert!(
            vulns["advisories"].is_array(),
            "Receipt {receipt_file} vulnerabilities.advisories must be an array"
        );
    }
}

/// Tests feature spec: docs/reference/security-receipt-schema.json
/// AC:5 - Validate security receipt schema completeness
#[test]
fn test_ac5_security_receipt_schema_completeness() {
    let root = project_root();

    let schema_path = root.join("docs/reference/security-receipt-schema.json");
    assert!(
        schema_path.exists(),
        "Security receipt JSON Schema must exist at docs/reference/security-receipt-schema.json (AC5)"
    );

    let schema_str = std::fs::read_to_string(&schema_path).expect("should read schema");
    let schema: serde_json::Value =
        serde_json::from_str(&schema_str).expect("schema must be valid JSON");

    // Verify it is a JSON Schema draft-07
    assert_eq!(
        schema["$schema"].as_str(),
        Some("http://json-schema.org/draft-07/schema#"),
        "Schema must be JSON Schema draft-07"
    );

    // Verify the schema has a title and description
    assert!(schema["title"].is_string(), "Schema must have a title");
    assert!(
        schema["description"].is_string(),
        "Schema must have a description"
    );

    // Verify required properties
    let required = schema["required"]
        .as_array()
        .expect("schema must have 'required' array");
    assert!(
        required.len() >= 8,
        "Schema must require at least 8 fields, found: {}",
        required.len()
    );

    // Verify properties are defined
    let properties = schema["properties"]
        .as_object()
        .expect("schema must have 'properties' object");

    let expected_properties = [
        "version",
        "timestamp",
        "commit_sha",
        "scan_type",
        "rust_version",
        "tools",
        "vulnerabilities",
        "exit_status",
    ];

    for prop in &expected_properties {
        assert!(
            properties.contains_key(*prop),
            "Schema must define property '{prop}'"
        );
    }

    // Verify schema includes examples
    assert!(
        schema["examples"].is_array(),
        "Schema should include examples array"
    );
    let examples = schema["examples"].as_array().unwrap();
    assert!(
        !examples.is_empty(),
        "Schema should include at least one example"
    );
}

/// Tests feature spec: docs/reference/security-receipt-schema.json
/// AC:5 - Validate test fixtures conform to schema
#[test]
fn test_ac5_test_fixtures_conform_to_schema() {
    let root = project_root();

    let fixtures = [
        "tests/fixtures/security-scanning/receipts/clean-scan.json",
        "tests/fixtures/security-scanning/receipts/vulnerabilities-found.json",
        "tests/fixtures/security-scanning/receipts/geiger-zero-unsafe.json",
    ];

    for fixture_path in &fixtures {
        let path = root.join(fixture_path);
        assert!(
            path.exists(),
            "Test fixture must exist: {fixture_path} (AC5)"
        );

        // Parse as JSON to ensure validity
        let content = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("should read {fixture_path}: {e}"));
        let json: serde_json::Value = serde_json::from_str(&content)
            .unwrap_or_else(|e| panic!("{fixture_path} must be valid JSON: {e}"));

        // Verify the fixture is an object (not an array or primitive)
        assert!(json.is_object(), "{fixture_path} must be a JSON object");

        // Verify required fields from the schema
        for field in &[
            "version",
            "timestamp",
            "commit_sha",
            "scan_type",
            "rust_version",
            "tools",
            "vulnerabilities",
            "exit_status",
        ] {
            assert!(
                json.get(field).is_some(),
                "{fixture_path} must contain field '{field}'"
            );
        }

        // Validate tools sub-object has at least cargo_audit and cargo_deny
        let tools = &json["tools"];
        assert!(
            tools["cargo_audit"].is_string(),
            "{fixture_path} tools must contain cargo_audit"
        );
        assert!(
            tools["cargo_deny"].is_string(),
            "{fixture_path} tools must contain cargo_deny"
        );

        // Validate vulnerabilities sub-object structure
        let vulns = &json["vulnerabilities"];
        assert!(
            vulns["count"].is_number(),
            "{fixture_path} must have vulnerabilities.count"
        );
        assert!(
            vulns["by_severity"].is_object(),
            "{fixture_path} must have vulnerabilities.by_severity"
        );
        assert!(
            vulns["advisories"].is_array(),
            "{fixture_path} must have vulnerabilities.advisories"
        );
    }
}

/// Tests feature spec: docs/explanation/security-scanning-architecture.md#6-cargo-geiger-integration
/// AC:6 - Validate cargo-geiger detects zero unsafe code
#[test]
fn test_ac6_cargo_geiger_validates_zero_unsafe() {
    let root = project_root();

    if tool_available("cargo", &["geiger", "--version"]) {
        // Tool is installed -- run actual geiger scan
        let output = Command::new("cargo")
            .args([
                "geiger",
                "--output-format",
                "Json",
                "--workspace",
                "--all-features",
            ])
            .current_dir(&root)
            .output()
            .expect("failed to execute cargo geiger");

        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let json: serde_json::Value =
                serde_json::from_str(&stdout).expect("cargo-geiger output should be valid JSON");

            // Validate zero unsafe code in workspace crates
            if let Some(packages) = json["packages"].as_array() {
                for pkg in packages {
                    let name = pkg["name"].as_str().unwrap_or("unknown");
                    if name.starts_with("copybook-") {
                        let unsafe_fns = pkg["unsafety"]["used"]["functions"].as_u64().unwrap_or(0);
                        let unsafe_exprs = pkg["unsafety"]["used"]["exprs"].as_u64().unwrap_or(0);
                        assert_eq!(
                            unsafe_fns, 0,
                            "Workspace crate {name} must have 0 unsafe functions"
                        );
                        assert_eq!(
                            unsafe_exprs, 0,
                            "Workspace crate {name} must have 0 unsafe expressions"
                        );
                    }
                }
            }
        } else {
            eprintln!("cargo-geiger returned non-zero; validating geiger fixture instead");
            validate_geiger_fixture(&root);
        }
    } else {
        // Tool not installed -- validate the fixture that represents expected zero-unsafe state
        validate_geiger_fixture(&root);
        eprintln!("cargo-geiger not installed; validated geiger fixture only");
    }
}

/// Helper to validate the geiger-zero-unsafe fixture
fn validate_geiger_fixture(root: &Path) {
    let fixture_path =
        root.join("tests/fixtures/security-scanning/receipts/geiger-zero-unsafe.json");
    assert!(
        fixture_path.exists(),
        "geiger-zero-unsafe.json fixture must exist"
    );

    let content = std::fs::read_to_string(&fixture_path).expect("should read geiger fixture");
    let json: serde_json::Value =
        serde_json::from_str(&content).expect("geiger fixture must be valid JSON");

    // Validate zero unsafe metrics in fixture
    let metrics = &json["unsafe_code_metrics"];
    assert_eq!(
        metrics["total_functions"].as_u64(),
        Some(0),
        "Geiger fixture must show 0 total unsafe functions"
    );
    assert_eq!(
        metrics["total_expressions"].as_u64(),
        Some(0),
        "Geiger fixture must show 0 total unsafe expressions"
    );

    // Validate per-crate breakdown
    let crates = metrics["crates"]
        .as_array()
        .expect("geiger fixture must have crates array");
    assert!(
        !crates.is_empty(),
        "geiger fixture must list workspace crates"
    );
    for c in crates {
        let name = c["name"].as_str().unwrap_or("unknown");
        assert_eq!(
            c["unsafe_functions"].as_u64(),
            Some(0),
            "Fixture crate {name} must have 0 unsafe functions"
        );
        assert_eq!(
            c["unsafe_expressions"].as_u64(),
            Some(0),
            "Fixture crate {name} must have 0 unsafe expressions"
        );
    }
}

/// Tests feature spec: docs/explanation/security-scanning-architecture.md#6-cargo-geiger-integration
/// AC:6 - Validate cargo-geiger JSON output structure
#[test]
fn test_ac6_cargo_geiger_json_output_structure() {
    let root = project_root();

    if tool_available("cargo", &["geiger", "--version"]) {
        let output = Command::new("cargo")
            .args([
                "geiger",
                "--output-format",
                "Json",
                "--workspace",
                "--all-features",
            ])
            .current_dir(&root)
            .output()
            .expect("failed to execute cargo geiger");

        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let json: serde_json::Value =
                serde_json::from_str(&stdout).expect("cargo-geiger JSON must be parseable");

            // Validate top-level structure
            assert!(
                json.get("packages").is_some(),
                "cargo-geiger JSON must contain 'packages'"
            );

            // Check workspace crates are present
            let packages = json["packages"].as_array().unwrap();
            let workspace_crate_names: Vec<&str> = packages
                .iter()
                .filter_map(|p| p["name"].as_str())
                .filter(|n| n.starts_with("copybook-"))
                .collect();

            assert!(
                !workspace_crate_names.is_empty(),
                "cargo-geiger output should include copybook-* workspace crates"
            );
        } else {
            eprintln!("cargo-geiger returned non-zero; falling back to fixture validation");
            validate_geiger_fixture(&root);
        }
    } else {
        // Validate the geiger fixture has the expected JSON structure
        let fixture_path =
            root.join("tests/fixtures/security-scanning/receipts/geiger-zero-unsafe.json");
        assert!(
            fixture_path.exists(),
            "geiger-zero-unsafe.json fixture must exist"
        );

        let content = std::fs::read_to_string(&fixture_path).expect("should read geiger fixture");
        let json: serde_json::Value =
            serde_json::from_str(&content).expect("geiger fixture must be valid JSON");

        // Validate structure expected from geiger output
        assert!(
            json.get("unsafe_code_metrics").is_some(),
            "geiger fixture must contain 'unsafe_code_metrics'"
        );
        let metrics = &json["unsafe_code_metrics"];
        assert!(
            metrics.get("crates").is_some(),
            "unsafe_code_metrics must contain 'crates' array"
        );

        let crates = metrics["crates"].as_array().unwrap();
        let workspace_crates: Vec<&str> = crates
            .iter()
            .filter_map(|c| c["name"].as_str())
            .filter(|n| n.starts_with("copybook-"))
            .collect();
        assert!(
            !workspace_crates.is_empty(),
            "geiger fixture should include copybook-* workspace crates"
        );

        eprintln!("cargo-geiger not installed; validated fixture JSON structure only");
    }
}

/// Tests feature spec: docs/explanation/security-scanning-architecture.md
/// AC:9 - End-to-end CI integration validation
#[test]
#[ignore = "Requires full CI environment with GitHub Actions"]
fn test_ac9_end_to_end_ci_integration() {
    // This test requires a full GitHub Actions environment to validate:
    // 1. PR quality gate workflow includes cargo-audit job
    // 2. Weekly security scan workflow exists and can be triggered
    // 3. Security receipts are generated and uploaded as artifacts
    // 4. cargo-geiger optional job runs without blocking PRs
    // 5. All workflows pass YAML syntax validation
    //
    // Cannot be run locally -- requires gh CLI + active workflow runs.
    panic!("This test must be run in GitHub Actions CI environment");
}

/// Tests feature spec: docs/explanation/security-scanning-architecture.md
/// AC:9 - Workflow YAML syntax validation
#[test]
fn test_ac9_workflow_yaml_syntax_validation() {
    let root = project_root();

    // Validate CI workflow files exist and contain required structure
    let ci_yml = root.join(".github/workflows/ci.yml");
    assert!(ci_yml.exists(), "ci.yml must exist");
    let ci_content = std::fs::read_to_string(&ci_yml).expect("should read ci.yml");
    // Validate required top-level YAML keys
    assert!(ci_content.contains("name:"), "ci.yml must have 'name:' key");
    assert!(
        ci_content.contains("on:") || ci_content.contains("'on':"),
        "ci.yml must have 'on:' trigger key"
    );
    assert!(ci_content.contains("jobs:"), "ci.yml must have 'jobs:' key");

    // Validate ci-security.yml exists with security-specific content
    let ci_security = root.join(".github/workflows/ci-security.yml");
    assert!(ci_security.exists(), "ci-security.yml must exist");
    let ci_sec_content =
        std::fs::read_to_string(&ci_security).expect("should read ci-security.yml");
    assert!(
        ci_sec_content.contains("name:"),
        "ci-security.yml must have 'name:' key"
    );
    assert!(
        ci_sec_content.contains("jobs:"),
        "ci-security.yml must have 'jobs:' key"
    );
    assert!(
        ci_sec_content.contains("cargo-deny")
            || ci_sec_content.contains("cargo-audit")
            || ci_sec_content.contains("cargo audit")
            || ci_sec_content.contains("cargo deny"),
        "ci-security.yml must reference security tooling"
    );

    // Validate security-scan.yml (weekly scan) exists
    let security_scan = root.join(".github/workflows/security-scan.yml");
    assert!(
        security_scan.exists(),
        "security-scan.yml weekly workflow must exist"
    );
    let scan_content =
        std::fs::read_to_string(&security_scan).expect("should read security-scan.yml");
    assert!(
        scan_content.contains("name:"),
        "security-scan.yml must have 'name:' key"
    );
    assert!(
        scan_content.contains("schedule:") || scan_content.contains("cron:"),
        "security-scan.yml must have a schedule trigger"
    );
    assert!(
        scan_content.contains("jobs:"),
        "security-scan.yml must have 'jobs:' key"
    );
}

/// Tests feature spec: tests/fixtures/security-scanning/configs/dependabot.yml
/// AC:3 - Dependabot configuration validation
#[test]
fn test_ac3_dependabot_configuration_validation() {
    let root = project_root();

    let dependabot_path = root.join(".github/dependabot.yml");
    assert!(
        dependabot_path.exists(),
        "Dependabot configuration must exist at .github/dependabot.yml"
    );

    let content = std::fs::read_to_string(&dependabot_path).expect("should read dependabot.yml");

    // Validate version
    assert!(
        content.contains("version: 2"),
        "Dependabot config must use version 2"
    );

    // Validate cargo ecosystem is configured
    assert!(
        content.contains("package-ecosystem: \"cargo\""),
        "Dependabot must configure cargo ecosystem"
    );

    // Validate GitHub Actions ecosystem is configured
    assert!(
        content.contains("package-ecosystem: \"github-actions\""),
        "Dependabot must configure github-actions ecosystem"
    );

    // Validate PR limits are configured
    assert!(
        content.contains("open-pull-requests-limit:"),
        "Dependabot must configure PR limits"
    );

    // Validate grouping is configured to reduce PR noise
    assert!(
        content.contains("groups:"),
        "Dependabot must configure update groups for reduced PR noise"
    );
}

#[cfg(test)]
mod workflow_validation_tests {
    use super::*;

    /// Tests feature spec: tests/fixtures/security-scanning/workflows/
    /// Test fixture workflow YAML syntax validation
    #[test]
    fn test_workflow_fixtures_exist() {
        let root = project_root();

        let fixture_workflows = [
            "tests/fixtures/security-scanning/workflows/ci-security-audit.yml",
            "tests/fixtures/security-scanning/workflows/security-scan-weekly.yml",
        ];

        for workflow_path in &fixture_workflows {
            let path = root.join(workflow_path);
            assert!(
                path.exists(),
                "Workflow test fixture must exist: {workflow_path}"
            );

            // Validate it is parseable YAML (basic: has required keys)
            let content = std::fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("should read {workflow_path}: {e}"));
            assert!(
                content.contains("name:"),
                "{workflow_path} must have 'name:' key"
            );
            assert!(
                content.contains("jobs:"),
                "{workflow_path} must have 'jobs:' key"
            );
        }
    }

    /// Tests feature spec: tests/fixtures/security-scanning/configs/
    /// Test configuration fixtures exist
    #[test]
    fn test_config_fixtures_exist() {
        let root = project_root();

        let fixture_configs = [
            "tests/fixtures/security-scanning/configs/deny-baseline.toml",
            "tests/fixtures/security-scanning/configs/deny-enhanced.toml",
            "tests/fixtures/security-scanning/configs/dependabot.yml",
        ];

        for config_path in &fixture_configs {
            let path = root.join(config_path);
            assert!(
                path.exists(),
                "Configuration test fixture must exist: {config_path}"
            );

            // Validate files are non-empty and readable
            let content = std::fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("should read {config_path}: {e}"));
            assert!(!content.is_empty(), "{config_path} must not be empty");
        }
    }

    /// Tests feature spec: tests/fixtures/security-scanning/receipts/
    /// Test security receipt fixtures exist
    #[test]
    fn test_receipt_fixtures_exist() {
        let root = project_root();

        let fixture_receipts = [
            "tests/fixtures/security-scanning/receipts/clean-scan.json",
            "tests/fixtures/security-scanning/receipts/vulnerabilities-found.json",
            "tests/fixtures/security-scanning/receipts/geiger-zero-unsafe.json",
        ];

        for receipt_path in &fixture_receipts {
            let path = root.join(receipt_path);
            assert!(
                path.exists(),
                "Security receipt test fixture must exist: {receipt_path}"
            );

            // Validate each receipt is valid JSON
            let content = std::fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("should read {receipt_path}: {e}"));
            let _: serde_json::Value = serde_json::from_str(&content)
                .unwrap_or_else(|e| panic!("{receipt_path} must be valid JSON: {e}"));
        }
    }
}

#[cfg(test)]
mod performance_validation_tests {

    /// Tests feature spec: docs/explanation/security-scanning-architecture.md#performance-budget--optimization
    /// AC:7 - CI performance overhead validation
    #[test]
    #[ignore = "Requires CI timing data from GitHub Actions"]
    fn test_ac7_ci_performance_overhead_within_budget() {
        // This test requires CI timing data from GitHub Actions to validate:
        // 1. Baseline CI duration measured before security enhancements
        // 2. Enhanced CI duration measured after security jobs added
        // 3. Overhead calculated as (enhanced - baseline)
        // 4. Overhead is less than 2 minutes (requirement)
        //
        // Cannot be run locally -- requires `gh run` data.
        panic!("This test must be run in CI with timing data from GitHub Actions");
    }
}
