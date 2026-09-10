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
    assert!(
        !stdout.contains("TESTING:"),
        "testing category was removed in 0.6"
    );
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
    // lru_cache is the only default-enabled flag (#656 Phase C: stable
    // language behavior is not flag-gated).
    assert!(
        stdout.contains("lru_cache") && stdout.contains("enabled"),
        "lru_cache should be enabled by default"
    );
}

#[test]
fn list_features_omits_removed_phase_c_flags() {
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    let out = cmd()
        .arg("--list-features")
        .args(["parse", cpy.to_str().unwrap()])
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    // #656 Phase C (v0.6.0): stable language behavior is not flag-gated.
    for removed in ["sign_separate", "comp_1", "comp_2"] {
        assert!(
            !stdout.contains(removed),
            "--list-features should not mention removed flag {removed}"
        );
    }
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
fn removed_phase_c_flag_names_are_rejected() {
    // #656 Phase C (v0.6.0): sign_separate/comp_1/comp_2 no longer exist.
    // Stale CLI usage must fail loudly.
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    for removed in ["comp_1", "comp_2", "sign_separate"] {
        let out = cmd()
            .args(["--disable-features", removed])
            .args(["parse", cpy.to_str().unwrap()])
            .output()
            .unwrap();
        assert!(
            !out.status.success(),
            "removed flag {removed} should be rejected"
        );
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(
            stderr.contains("Invalid feature flag"),
            "removed flag {removed} must report Invalid feature flag, got: {stderr}"
        );
    }
}

#[test]
fn stable_clauses_parse_despite_stale_disable_env() {
    // #656 Phase C: stale COPYBOOK_FF_SIGN_SEPARATE / COPYBOOK_FF_COMP_1=0
    // env vars are no longer recognized and must not affect parsing.
    let dir = TempDir::new().unwrap();
    let cpy = write_sign_separate_copybook(&dir);
    cmd()
        .env("COPYBOOK_FF_SIGN_SEPARATE", "0")
        .args(["parse", cpy.to_str().unwrap()])
        .assert()
        .success();

    let cpy = write_comp1_copybook(&dir);
    cmd()
        .env("COPYBOOK_FF_COMP_1", "0")
        .env("COPYBOOK_FF_COMP_2", "0")
        .args(["parse", cpy.to_str().unwrap()])
        .assert()
        .success();
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
fn disable_category_experimental_still_parses_comp1() {
    // #656 Phase C: COMP-1 is stable parser behavior, not an experimental
    // toggle; disabling the experimental category must not reject it.
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    cmd()
        .args(["--disable-category", "experimental"])
        .args(["parse", cpy.to_str().unwrap()])
        .assert()
        .success();
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
fn feature_flags_config_with_stale_phase_c_names_is_tolerated() {
    // #656 Phase C: config files ignore unknown flag names (pre-existing
    // lenient behavior); the COMP-1 copybook must still parse because
    // COMP-1 is stable behavior, not a toggle.
    let dir = TempDir::new().unwrap();
    let cpy = write_comp1_copybook(&dir);
    let config = dir.path().join("flags.json");
    std::fs::write(
        &config,
        r#"{"feature_flags":{"enabled":[],"disabled":["comp_1"]}}"#,
    )
    .unwrap();
    cmd()
        .args(["--feature-flags-config", config.to_str().unwrap()])
        .args(["parse", cpy.to_str().unwrap()])
        .assert()
        .success();
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
