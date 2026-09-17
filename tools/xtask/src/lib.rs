// SPDX-License-Identifier: AGPL-3.0-or-later
//! xtask library for testing
//!
//! Exposes testable modules

use anyhow::Result;
use std::{fs, path::Path, path::PathBuf, time::SystemTime};

pub mod architecture;
pub mod perf;
pub mod publish;

#[derive(Default, Debug, Clone)]
pub struct Counts {
    pub passed: u64,
    pub failed: u64,
    pub skipped: u64,
}

fn junit_xml_paths() -> [PathBuf; 2] {
    [
        Path::new("target/nextest/junit.xml").to_path_buf(),
        Path::new("target/nextest/ci/junit.xml").to_path_buf(),
    ]
}

/// Return the active nextest junit report location used by docs verification.
///
/// # Errors
///
/// Returns an error if no expected junit report path exists at either location.
#[inline]
pub fn junit_xml_path() -> Result<PathBuf> {
    let candidates = junit_xml_paths();
    let primary = candidates[0].display().to_string();
    let secondary = candidates[1].display().to_string();

    candidates
        .iter()
        .find(|path| path.exists())
        .cloned()
        .ok_or_else(|| {
            anyhow::anyhow!(
                "No junit.xml found (run nextest with junit output in one of {primary} or {secondary})"
            )
        })
}

/// Parse nextest `JUnit` XML and return test counts.
///
/// # Errors
///
/// Returns an error if the `JUnit` XML file is missing or malformed.
#[inline]
pub fn counts() -> Result<Counts> {
    let junit_path = junit_xml_path()?;

    let xml_content = fs::read_to_string(junit_path)?;
    let doc = roxmltree::Document::parse(&xml_content)?;

    let mut c = Counts::default();
    for node in doc.descendants().filter(|n| n.has_tag_name("testsuite")) {
        let tests = attr(node, "tests");
        let failures = attr(node, "failures") + attr(node, "errors");
        let skipped = attr(node, "skipped");

        c.failed += failures;
        c.skipped += skipped;
        c.passed += tests.saturating_sub(failures + skipped);
    }

    Ok(c)
}

fn attr(node: roxmltree::Node<'_, '_>, key: &str) -> u64 {
    node.attribute(key)
        .and_then(|s| s.parse().ok())
        .unwrap_or(0)
}

/// Source roots whose `.rs` files can change the test inventory a receipt
/// claims to prove (#992).
const TEST_SOURCE_ROOTS: [&str; 4] = ["crates", "tests", "tools", "examples"];

/// Newest modification time of any `.rs` file under the test source roots.
///
/// # Errors
///
/// Returns an error when a root cannot be walked or a file time is unreadable.
#[inline]
pub fn newest_test_source_mtime() -> Result<SystemTime> {
    fn visit(dir: &Path, newest: &mut SystemTime) -> Result<()> {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            let file_type = entry.file_type()?;
            if file_type.is_dir() {
                if path.file_name().is_some_and(|name| name != "target") {
                    visit(&path, newest)?;
                }
            } else if file_type.is_file() && path.extension().is_some_and(|ext| ext == "rs") {
                *newest = (*newest).max(fs::metadata(&path)?.modified()?);
            }
        }
        Ok(())
    }

    let mut newest = SystemTime::UNIX_EPOCH;
    for root in TEST_SOURCE_ROOTS {
        let dir = Path::new(root);
        if dir.is_dir() {
            visit(dir, &mut newest)?;
        }
    }
    Ok(newest)
}

/// Describe receipt staleness for `docs sync-tests` output (#992).
///
/// Returns a warning naming the rerun command when the receipt predates
/// the newest test source, and `None` when the receipt is at least as
/// fresh. This reports; it never refuses a sync.
#[must_use]
#[inline]
pub fn stale_receipt_warning(
    receipt_mtime: SystemTime,
    newest_source_mtime: SystemTime,
    rerun_command: &str,
) -> Option<String> {
    if newest_source_mtime > receipt_mtime {
        Some(format!(
            "WARNING: junit receipt predates the newest test source; counts may certify unexecuted tests. Rerun: {rerun_command}"
        ))
    } else {
        None
    }
}

/// Render the gate refusal for a stale receipt (#992).
///
/// Returns the failure report naming the receipt identity and the exact
/// rerun/sync commands when the receipt predates the newest test source,
/// and `None` when the receipt is at least as fresh. Gates must refuse;
/// only `sync-tests` may warn and continue.
#[must_use]
#[inline]
pub fn stale_gate_report(
    receipt_display: &str,
    receipt_sha256: &str,
    receipt_mtime: SystemTime,
    newest_source_mtime: SystemTime,
    rerun_command: &str,
    sync_command: &str,
) -> Option<String> {
    if newest_source_mtime > receipt_mtime {
        Some(format!(
            "test-status evidence is stale: junit receipt {receipt_display} (sha256:{receipt_sha256}) predates the newest test source, so its counts cannot certify the current tests\n\
             regenerate the receipt with:\n\
               {rerun_command}\n\
             then sync the docs with:\n\
               {sync_command}"
        ))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::{newest_test_source_mtime, stale_gate_report, stale_receipt_warning};
    use std::time::{Duration, SystemTime};

    #[test]
    fn stale_receipt_warning_fires_only_for_older_receipts() {
        let receipt = SystemTime::UNIX_EPOCH + Duration::from_secs(100);
        let older_source = SystemTime::UNIX_EPOCH + Duration::from_secs(50);
        let newer_source = SystemTime::UNIX_EPOCH + Duration::from_secs(150);
        assert!(
            stale_receipt_warning(receipt, older_source, "rerun").is_none(),
            "fresh receipt must not warn"
        );
        assert!(
            stale_receipt_warning(receipt, receipt, "rerun").is_none(),
            "same-time receipt must not warn"
        );
        let warning = stale_receipt_warning(receipt, newer_source, "rerun-cmd")
            .expect("stale receipt must warn");
        assert!(warning.contains("rerun-cmd"), "{warning}");
    }

    #[test]
    fn stale_gate_report_refuses_only_older_receipts() {
        let receipt = SystemTime::UNIX_EPOCH + Duration::from_secs(100);
        let older_source = SystemTime::UNIX_EPOCH + Duration::from_secs(50);
        let newer_source = SystemTime::UNIX_EPOCH + Duration::from_secs(150);
        assert!(
            stale_gate_report("junit.xml", "abc", receipt, older_source, "rerun", "sync").is_none(),
            "fresh receipt must pass the gate"
        );
        assert!(
            stale_gate_report("junit.xml", "abc", receipt, receipt, "rerun", "sync").is_none(),
            "same-time receipt must pass the gate"
        );
        let report = stale_gate_report(
            "junit.xml",
            "deadbeef",
            receipt,
            newer_source,
            "rerun-cmd",
            "sync-cmd",
        )
        .expect("stale receipt must fail the gate");
        for needle in ["stale", "junit.xml", "deadbeef", "rerun-cmd", "sync-cmd"] {
            assert!(
                report.contains(needle),
                "report must contain {needle}: {report}"
            );
        }
    }

    #[test]
    fn newest_test_source_mtime_sees_workspace_sources() {
        let newest = newest_test_source_mtime().expect("source walk must succeed in the workspace");
        assert!(
            newest > SystemTime::UNIX_EPOCH,
            "walk must find workspace sources"
        );
        assert!(
            newest <= SystemTime::now() + Duration::from_secs(5),
            "mtime cannot be in the future"
        );
    }
}
