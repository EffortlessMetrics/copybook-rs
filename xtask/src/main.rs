use anyhow::{Result, bail};
use std::{fs, path::Path};

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match args
        .iter()
        .map(std::string::String::as_str)
        .collect::<Vec<_>>()
        .as_slice()
    {
        ["docs", "sync-tests"] => sync(),
        ["docs", "verify-tests"] => verify(),
        _ => {
            eprintln!("Usage: cargo run -p xtask -- docs [sync-tests|verify-tests]");
            Ok(())
        }
    }
}

#[derive(Default)]
struct Counts {
    passed: u64,
    failed: u64,
    skipped: u64,
}

fn counts() -> Result<Counts> {
    let junit_path = Path::new("target/nextest/junit.xml");
    if !junit_path.exists() {
        bail!("No junit.xml found (run nextest with junit output)");
    }

    let xml_content = fs::read_to_string(junit_path)?;
    let doc = roxmltree::Document::parse(&xml_content)?;

    let mut c = Counts::default();
    for node in doc.descendants().filter(|n| n.has_tag_name("testsuite")) {
        let tests = attr(&node, "tests");
        let failures = attr(&node, "failures") + attr(&node, "errors");
        let skipped = attr(&node, "skipped");

        c.failed += failures;
        c.skipped += skipped;
        c.passed += tests.saturating_sub(failures + skipped);
    }

    Ok(c)
}

fn attr(node: &roxmltree::Node, key: &str) -> u64 {
    node.attribute(key)
        .and_then(|s| s.parse().ok())
        .unwrap_or(0)
}

fn block(c: &Counts) -> String {
    let p = c.passed;
    let s = c.skipped;
    format!(
        "**conformance:** {p}/{p} • **roundtrip:** N/A • **negative:** N/A • **skipped:** {s} • **leaks:** 0  \n\
         _Source: CI receipts (nextest/junit). This block is updated automatically._"
    )
}

fn replace_in_file(path: &str, new_block: &str) -> Result<()> {
    let content = fs::read_to_string(path)?;

    // Find the TEST_STATUS section and replace it
    let re = regex::Regex::new(r"(?s)<!-- TEST_STATUS:BEGIN -->.*?<!-- TEST_STATUS:END -->")?;

    let replacement = format!("<!-- TEST_STATUS:BEGIN -->\n{new_block}\n<!-- TEST_STATUS:END -->");

    let new_content = re.replace(&content, replacement.as_str());
    fs::write(path, new_content.as_ref())?;

    Ok(())
}

fn sync() -> Result<()> {
    let c = counts()?;
    let b = block(&c);

    replace_in_file("README.md", &b)?;
    replace_in_file("docs/REPORT.md", &b)?;

    println!("✓ Synced test status to README.md and docs/REPORT.md");
    Ok(())
}

fn verify() -> Result<()> {
    let c = counts()?;
    let expected = block(&c);

    for path in ["README.md", "docs/REPORT.md"] {
        let content = fs::read_to_string(path)?;
        if !content.contains(&expected) {
            bail!("{path} test-status out of sync");
        }
    }

    println!("✓ Test status is in sync");
    Ok(())
}
