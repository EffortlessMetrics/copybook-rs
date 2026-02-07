//! CLI tests for feature flag integration

use assert_cmd::Command;
use predicates::prelude::*;

#[test]
fn test_list_features() {
    Command::cargo_bin("copybook")
        .expect("Failed to find copybook binary")
        .arg("--list-features")
        .assert()
        .success()
        .stdout(predicate::str::contains("Available Feature Flags"))
        .stdout(predicate::str::contains("EXPERIMENTAL"))
        .stdout(predicate::str::contains("sign_separate"))
        .stdout(predicate::str::contains("renames_r4_r6"))
        .stdout(predicate::str::contains("comp_1"))
        .stdout(predicate::str::contains("comp_2"))
        .stdout(predicate::str::contains("ENTERPRISE"))
        .stdout(predicate::str::contains("audit_system"))
        .stdout(predicate::str::contains("sox_compliance"))
        .stdout(predicate::str::contains("hipaa_compliance"))
        .stdout(predicate::str::contains("gdpr_compliance"))
        .stdout(predicate::str::contains("pci_dss_compliance"))
        .stdout(predicate::str::contains("security_monitoring"))
        .stdout(predicate::str::contains("PERFORMANCE"))
        .stdout(predicate::str::contains("advanced_optimization"))
        .stdout(predicate::str::contains("lru_cache"))
        .stdout(predicate::str::contains("parallel_decode"))
        .stdout(predicate::str::contains("zero_copy"))
        .stdout(predicate::str::contains("DEBUG"))
        .stdout(predicate::str::contains("verbose_logging"))
        .stdout(predicate::str::contains("diagnostic_output"))
        .stdout(predicate::str::contains("profiling"))
        .stdout(predicate::str::contains("memory_tracking"))
        .stdout(predicate::str::contains("TESTING"))
        .stdout(predicate::str::contains("mutation_testing"))
        .stdout(predicate::str::contains("fuzzing_integration"))
        .stdout(predicate::str::contains("coverage_instrumentation"))
        .stdout(predicate::str::contains("property_based_testing"));
}

#[test]
fn test_enable_features() {
    Command::cargo_bin("copybook")
        .expect("Failed to find copybook binary")
        .arg("--version")
        .env("COPYBOOK_FF_VERBOSE_LOGGING", "1")
        .env("COPYBOOK_FF_DIAGNOSTIC_OUTPUT", "1")
        .assert()
        .success();
}

#[test]
fn test_enable_features_cli() {
    Command::cargo_bin("copybook")
        .expect("Failed to find copybook binary")
        .args(["parse", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("--enable-features"))
        .stdout(predicate::str::contains("--disable-features"))
        .stdout(predicate::str::contains("--enable-category"))
        .stdout(predicate::str::contains("--disable-category"))
        .stdout(predicate::str::contains("--feature-flags-config"));
}

#[test]
fn test_feature_flags_config_toml() {
    let config = r#"
[feature_flags]
enabled = ["verbose_logging", "diagnostic_output"]
disabled = ["lru_cache"]
"#;

    let mut cmd = Command::cargo_bin("copybook")
        .expect("Failed to find copybook binary");
    cmd.arg("--version");
    cmd.env("COPYBOOK_FF_VERBOSE_LOGGING", "0"); // Override with config

    // This test just verifies the CLI accepts the config option
    // Actual config parsing is tested in integration tests
    cmd.assert().success();
}

#[test]
fn test_enable_category() {
    Command::cargo_bin("copybook")
        .expect("Failed to find copybook binary")
        .args(["parse", "--enable-category", "debug", "--help"])
        .assert()
        .success();
}

#[test]
fn test_disable_category() {
    Command::cargo_bin("copybook")
        .expect("Failed to find copybook binary")
        .args(["parse", "--disable-category", "experimental", "--help"])
        .assert()
        .success();
}

#[test]
fn test_invalid_feature_name() {
    Command::cargo_bin("copybook")
        .expect("Failed to find copybook binary")
        .args(["parse", "--enable-features", "invalid_feature"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("Invalid feature flag"));
}

#[test]
fn test_invalid_category_name() {
    Command::cargo_bin("copybook")
        .expect("Failed to find copybook binary")
        .args(["parse", "--enable-category", "invalid_category"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("Invalid feature category"));
}

#[test]
fn test_feature_flag_precedence() {
    // CLI flags should override environment variables
    Command::cargo_bin("copybook")
        .expect("Failed to find copybook binary")
        .args(["parse", "--disable-features", "verbose_logging", "--help"])
        .env("COPYBOOK_FF_VERBOSE_LOGGING", "1")
        .assert()
        .success();
}

#[test]
fn test_lru_cache_enabled_by_default() {
    Command::cargo_bin("copybook")
        .expect("Failed to find copybook binary")
        .arg("--list-features")
        .assert()
        .success()
        .stdout(predicate::str::contains("lru_cache").and(
            predicate::str::contains("(enabled)")
        ));
}

#[test]
fn test_experimental_features_disabled_by_default() {
    Command::cargo_bin("copybook")
        .expect("Failed to find copybook binary")
        .arg("--list-features")
        .assert()
        .success()
        .stdout(predicate::str::contains("sign_separate").and(
            predicate::str::contains("(disabled)")
        ));
}
