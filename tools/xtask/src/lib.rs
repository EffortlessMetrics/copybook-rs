// SPDX-License-Identifier: AGPL-3.0-or-later
//! xtask library for testing
//!
//! Exposes testable modules

use anyhow::{Result, bail};
use std::{fs, path::Path};

pub mod perf;

#[derive(Default, Debug, Clone)]
pub struct Counts {
    pub passed: u64,
    pub failed: u64,
    pub skipped: u64,
}

pub fn counts() -> Result<Counts> {
    let junit_path = Path::new("target/nextest/junit.xml");
    if !junit_path.exists() {
        bail!("No junit.xml found (run nextest with junit output)");
    }

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
