// SPDX-License-Identifier: AGPL-3.0-or-later
//! xtask library for testing
//!
//! Exposes testable modules

use anyhow::Result;
use std::{fs, path::Path, path::PathBuf};

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
