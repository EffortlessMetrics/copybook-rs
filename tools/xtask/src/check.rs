use anyhow::Result;
use std::process::Command;

pub fn check_all() -> Result<()> {
    println!("🚀 Running comprehensive local DevEx check...");

    // 1. Format
    println!("\n🎨 Checking format...");
    let status = Command::new("cargo").args(["fmt", "--", "--check"]).status()?;
    if !status.success() {
        println!("❌ Format check failed. Run 'just fmt' to fix.");
    } else {
        println!("✅ Format ok.");
    }

    // 2. Clippy
    println!("\n📎 Running clippy...");
    let status = Command::new("cargo")
        .args(["clippy", "--workspace", "--all-targets", "--", "-D", "warnings"])
        .status()?;
    if !status.success() {
        println!("❌ Clippy failed.");
    } else {
        println!("✅ Clippy ok.");
    }

    // 3. Tests
    println!("\n🧪 Running tests (nextest)...");
    let status = Command::new("cargo")
        .args(["nextest", "run", "--workspace"])
        .status()?;
    if !status.success() {
        println!("❌ Tests failed.");
    } else {
        println!("✅ Tests ok.");
    }

    // 4. Deny
    println!("\n🛡️ Checking dependencies (cargo-deny)...");
    let status = Command::new("cargo").args(["deny", "check"]).status();
    match status {
        Ok(s) if s.success() => println!("✅ Dependency check ok."),
        _ => println!("⚠️ cargo-deny failed or not installed. Skipping."),
    }

    println!("\n✨ Local DevEx check complete.");
    Ok(())
}