// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for CLI feature flag management: `--list-features`,
//! `--enable-features`, `--disable-features`, `--enable-category`,
//! `--disable-category`, `--feature-flags-config`, and env-var overrides.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use tempfile::TempDir;

#[allow(deprecated)]
fn cmd() -> Command {
    Command::cargo_bin("copybook").unwrap()
}

/// Write a minimal copybook with a `COMP-1` field to `tmp_dir`, return path.
fn write_comp1_copybook(dir: &TempDir) -> std::path::PathBuf {
    let cpy = dir.path().join("comp1.cpy");
    std::fs::write(&cpy, "       01  REC.\n           05  FLD  COMP-1.\n").unwrap();
    cpy
}

/// Write a minimal SIGN SEPARATE copybook to `tmp_dir`, return path.
fn write_sign_separate_copybook(dir: &TempDir) -> std::path::PathBuf {
    let cpy = dir.path().join("signsep.cpy");
    std::fs::write(
        &cpy,
        "       01  REC.\n           05  AMT  PIC S9(5) SIGN LEADING SEPARATE.\n",
    )
    .unwrap();
    cpy
}

// ── --list-features ─────────────────────────────────────────────────

#[test]
fn list_features_exits_zero() {
    // --list-features must come before the subcommand; parse requires a file
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    cmd()
        .arg("--list-features")
        .args(["parse", cpy.to_str().unwrap()])
        .assert()
        .success();
}

#[test]
fn list_features_shows_categories() {
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    let out = cmd()
        .arg("--list-features")
        .args(["parse", cpy.to_str().unwrap()])
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("EXPERIMENTAL:"), "should list EXPERIMENTAL");
    assert!(stdout.contains("ENTERPRISE:"), "should list ENTERPRISE");
    assert!(stdout.contains("PERFORMANCE:"), "should list PERFORMANCE");
    assert!(stdout.contains("DEBUG:"), "should list DEBUG");
    assert!(stdout.contains("TESTING:"), "should list TESTING");
}

#[test]
fn list_features_shows_default_enabled() {
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    let out = cmd()
        .arg("--list-features")
        .args(["parse", cpy.to_str().unwrap()])
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    // These four are default-enabled (verified in feature_flags_tests.rs)
    assert!(
        stdout.contains("sign_separate") && stdout.contains("enabled"),
        "sign_separate should be enabled by default"
    );
    assert!(
        stdout.contains("comp_1") && stdout.contains("enabled"),
        "comp_1 should be enabled by default"
    );
    assert!(
        stdout.contains("comp_2") && stdout.contains("enabled"),
        "comp_2 should be enabled by default"
    );
    assert!(
        stdout.contains("lru_cache") && stdout.contains("enabled"),
        "lru_cache should be enabled by default"
    );
}

#[test]
fn list_features_shows_default_disabled() {
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    let out = cmd()
        .arg("--list-features")
        .args(["parse", cpy.to_str().unwrap()])
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("verbose_logging") && stdout.contains("disabled"),
        "verbose_logging should be disabled by default"
    );
    assert!(
        stdout.contains("audit_system") && stdout.contains("disabled"),
        "audit_system should be disabled by default"
    );
}

#[test]
fn list_features_shows_env_var_hint() {
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    let out = cmd()
        .arg("--list-features")
        .args(["parse", cpy.to_str().unwrap()])
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("COPYBOOK_FF_"), "should show env var hint");
}

// ── --enable-features / --disable-features ──────────────────────────

#[test]
fn enable_features_flag_accepted() {
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    // Enable a normally-disabled feature; parse should succeed
    cmd()
        .args(["--enable-features", "verbose_logging"])
        .args(["parse", cpy.to_str().unwrap()])
        .assert()
        .success();
}

#[test]
fn disable_features_flag_accepted() {
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    // Disable a normally-enabled feature; parse a copybook that doesn't need it
    cmd()
        .args(["--disable-features", "lru_cache"])
        .args(["parse", cpy.to_str().unwrap()])
        .assert()
        .success();
}

#[test]
fn disable_comp1_rejects_comp1_copybook() {
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    // Disabling comp_1 should cause parse failure on a COMP-1 copybook
    let out = cmd()
        .args(["--disable-features", "comp_1"])
        .args(["parse", cpy.to_str().unwrap()])
        .output()
        .unwrap();
    assert!(
        !out.status.success(),
        "parse should fail when comp_1 is disabled and copybook uses COMP-1"
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("COMP-1") || stderr.contains("comp_1"),
        "error should mention COMP-1 or comp_1, got: {stderr}"
    );
}

#[test]
fn disable_sign_separate_rejects_sign_separate_copybook() {
    let dir = TempDir::new().unwrap();
    let cpy = write_sign_separate_copybook(&dir);
    let out = cmd()
        .args(["--disable-features", "sign_separate"])
        .args(["parse", cpy.to_str().unwrap()])
        .output()
        .unwrap();
    assert!(
        !out.status.success(),
        "parse should fail when sign_separate is disabled"
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("SIGN") || stderr.contains("sign_separate"),
        "error should mention SIGN, got: {stderr}"
    );
}

// ── --enable-category / --disable-category ──────────────────────────

#[test]
fn enable_category_debug_accepted() {
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    cmd()
        .args(["--enable-category", "debug"])
        .args(["parse", cpy.to_str().unwrap()])
        .assert()
        .success();
}

#[test]
fn disable_category_experimental_rejects_comp1() {
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    // COMP-1 is in the experimental category; disabling it should reject the copybook
    let out = cmd()
        .args(["--disable-category", "experimental"])
        .args(["parse", cpy.to_str().unwrap()])
        .output()
        .unwrap();
    assert!(
        !out.status.success(),
        "parse should fail when experimental category is disabled and copybook uses COMP-1"
    );
}

// ── --feature-flags-config ──────────────────────────────────────────

#[test]
fn feature_flags_config_json_accepted() {
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    let config = dir.path().join("flags.json");
    std::fs::write(
        &config,
        r#"{"feature_flags":{"enabled":["verbose_logging"],"disabled":[]}}"#,
    )
    .unwrap();
    cmd()
        .args(["--feature-flags-config", config.to_str().unwrap()])
        .args(["parse", cpy.to_str().unwrap()])
        .assert()
        .success();
}

#[test]
fn feature_flags_config_disables_comp1() {
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    let config = dir.path().join("flags.json");
    std::fs::write(
        &config,
        r#"{"feature_flags":{"enabled":[],"disabled":["comp_1"]}}"#,
    )
    .unwrap();
    let out = cmd()
        .args(["--feature-flags-config", config.to_str().unwrap()])
        .args(["parse", cpy.to_str().unwrap()])
        .output()
        .unwrap();
    assert!(
        !out.status.success(),
        "parse should fail when config disables comp_1"
    );
}

#[test]
fn feature_flags_config_missing_file_fails() {
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    cmd()
        .args(["--feature-flags-config", "/nonexistent/flags.json"])
        .args(["parse", cpy.to_str().unwrap()])
        .assert()
        .failure();
}

// ── Env-var overrides ───────────────────────────────────────────────

#[test]
fn env_var_enables_feature() {
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    // COPYBOOK_FF_VERBOSE_LOGGING=1 should enable verbose_logging
    // Just verify the flag is accepted and doesn't cause failure
    cmd()
        .env("COPYBOOK_FF_VERBOSE_LOGGING", "1")
        .args(["parse", cpy.to_str().unwrap()])
        .assert()
        .success();
}

// ── Interaction: list-features reflects overrides ───────────────────

#[test]
fn list_features_reflects_enable_flag() {
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    let out = cmd()
        .args(["--enable-features", "verbose_logging"])
        .arg("--list-features")
        .args(["parse", cpy.to_str().unwrap()])
        .output()
        .unwrap();
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    // verbose_logging should now show as enabled
    let vl_line = stdout
        .lines()
        .find(|l| l.contains("verbose_logging"))
        .expect("should find verbose_logging line");
    assert!(
        vl_line.contains("enabled"),
        "verbose_logging should be enabled after --enable-features, got: {vl_line}"
    );
}

#[test]
fn list_features_reflects_disable_flag() {
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    let out = cmd()
        .args(["--disable-features", "lru_cache"])
        .arg("--list-features")
        .args(["parse", cpy.to_str().unwrap()])
        .output()
        .unwrap();
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    let lc_line = stdout
        .lines()
        .find(|l| l.contains("lru_cache"))
        .expect("should find lru_cache line");
    assert!(
        lc_line.contains("disabled"),
        "lru_cache should be disabled after --disable-features, got: {lc_line}"
    );
}
