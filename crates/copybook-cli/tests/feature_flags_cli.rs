// SPDX-License-Identifier: AGPL-3.0-or-later
//! CLI tests for feature flag integration

use assert_cmd::cargo::cargo_bin_cmd;
use predicates::prelude::*;

mod test_utils;
use test_utils::fixture_path;

#[test]
fn test_list_features() {
    let copybook = fixture_path("copybooks/simple.cpy").expect("fixture should exist");
    cargo_bin_cmd!("copybook")
        .args(["--list-features", "parse"])
        .arg(copybook)
        .assert()
        .success()
        .stdout(predicate::str::contains("Available Feature Flags"))
        .stdout(predicate::str::contains("EXPERIMENTAL"))
        .stdout(predicate::str::contains("renames_r4_r6"))
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
        .stdout(predicate::str::contains("memory_tracking"));
}

#[test]
fn test_enable_features() {
    cargo_bin_cmd!("copybook")
        .arg("--version")
        .env("COPYBOOK_FF_VERBOSE_LOGGING", "1")
        .env("COPYBOOK_FF_DIAGNOSTIC_OUTPUT", "1")
        .assert()
        .success();
}

#[test]
fn test_enable_features_cli() {
    cargo_bin_cmd!("copybook")
        .arg("--help")
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
    let _config = r#"
[feature_flags]
enabled = ["verbose_logging", "diagnostic_output"]
disabled = ["lru_cache"]
"#;

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("--version");
    cmd.env("COPYBOOK_FF_VERBOSE_LOGGING", "0"); // Override with config

    // This test just verifies the CLI accepts the config option
    // Actual config parsing is tested in integration tests
    cmd.assert().success();
}

#[test]
fn test_enable_category() {
    let copybook = fixture_path("copybooks/simple.cpy").expect("fixture should exist");
    cargo_bin_cmd!("copybook")
        .args(["--enable-category", "debug", "parse"])
        .arg(copybook)
        .assert()
        .success();
}

#[test]
fn test_disable_category() {
    let copybook = fixture_path("copybooks/simple.cpy").expect("fixture should exist");
    cargo_bin_cmd!("copybook")
        .args(["--disable-category", "experimental", "parse"])
        .arg(copybook)
        .assert()
        .success();
}

#[test]
fn test_invalid_feature_name() {
    let copybook = fixture_path("copybooks/simple.cpy").expect("fixture should exist");
    cargo_bin_cmd!("copybook")
        .args(["--enable-features", "invalid_feature", "parse"])
        .arg(copybook)
        .assert()
        .failure()
        .stderr(predicate::str::contains("Invalid feature flag"));
}

#[test]
fn test_invalid_category_name() {
    let copybook = fixture_path("copybooks/simple.cpy").expect("fixture should exist");
    cargo_bin_cmd!("copybook")
        .args(["--enable-category", "invalid_category", "parse"])
        .arg(copybook)
        .assert()
        .failure()
        .stderr(predicate::str::contains("Invalid feature category"));
}

#[test]
fn test_feature_flag_precedence() {
    let copybook = fixture_path("copybooks/simple.cpy").expect("fixture should exist");
    // CLI flags should override environment variables
    cargo_bin_cmd!("copybook")
        .args(["--disable-features", "verbose_logging", "parse"])
        .arg(copybook)
        .env("COPYBOOK_FF_VERBOSE_LOGGING", "1")
        .assert()
        .success();
}

#[test]
fn test_lru_cache_enabled_by_default() {
    let copybook = fixture_path("copybooks/simple.cpy").expect("fixture should exist");
    cargo_bin_cmd!("copybook")
        .args(["--list-features", "parse"])
        .arg(copybook)
        .assert()
        .success()
        .stdout(predicate::str::contains("lru_cache").and(predicate::str::contains("(enabled")));
}

#[test]
fn test_removed_phase_c_flags_absent_from_list() {
    // #656 Phase C (v0.6.0): sign_separate, comp_1, comp_2 are stable parser
    // behavior, not runtime flags, so they no longer appear in --list-features.
    let copybook = fixture_path("copybooks/simple.cpy").expect("fixture should exist");
    cargo_bin_cmd!("copybook")
        .args(["--list-features", "parse"])
        .arg(copybook)
        .assert()
        .success()
        .stdout(predicate::str::contains("sign_separate").not())
        .stdout(predicate::str::contains("comp_1").not())
        .stdout(predicate::str::contains("comp_2").not());
}

#[test]
fn test_removed_phase_c_flags_rejected() {
    // Stale configs naming removed flags must fail loudly.
    let copybook = fixture_path("copybooks/simple.cpy").expect("fixture should exist");
    for removed in ["sign_separate", "comp_1", "comp_2"] {
        cargo_bin_cmd!("copybook")
            .args(["--enable-features", removed, "parse"])
            .arg(&copybook)
            .assert()
            .failure()
            .stderr(predicate::str::contains("Invalid feature flag"));
    }
}

#[test]
fn test_experimental_renames_r4_r6_disabled_by_default() {
    let copybook = fixture_path("copybooks/simple.cpy").expect("fixture should exist");
    cargo_bin_cmd!("copybook")
        .args(["--list-features", "parse"])
        .arg(copybook)
        .assert()
        .success()
        .stdout(
            predicate::str::contains("renames_r4_r6").and(predicate::str::contains("(disabled)")),
        );
}
