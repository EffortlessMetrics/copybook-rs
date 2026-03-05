use anyhow::{Result, bail};
use std::process::Command;
use std::path::Path;

pub fn build() -> Result<()> {
    println!("📚 Building mdBook documentation...");
    
    let book_dir = Path::new("docs/mdbook");
    if !book_dir.exists() {
        bail!("mdbook directory not found at docs/mdbook");
    }

    // Copy relevant markdown files from root and docs/ to mdbook/src/
    println!("🔄 Syncing markdown files...");
    std::fs::copy("README.md", "docs/mdbook/src/README.md")?;
    std::fs::copy("CONTRIBUTING.md", "docs/mdbook/src/CONTRIBUTING.md")?;
    
    // Attempt to copy other files if they exist
    let _ = std::fs::copy("docs/CLI_REFERENCE.md", "docs/mdbook/src/CLI_REFERENCE.md");
    let _ = std::fs::copy("docs/reference/COBOL_SUPPORT_MATRIX.md", "docs/mdbook/src/SUPPORT_MATRIX.md");
    let _ = std::fs::copy("docs/API_FREEZE.md", "docs/mdbook/src/API_FREEZE.md");

    // Run mdbook build
    let status = Command::new("mdbook")
        .args(["build", "docs/mdbook"])
        .status();

    match status {
        Ok(s) if s.success() => {
            println!("✅ mdBook documentation built at docs/mdbook/book/html");
            Ok(())
        }
        _ => {
            println!("⚠️ mdbook command failed or not installed. Install with 'cargo install mdbook'.");
            println!("Attempting to build using mdbook as a library...");
            
            // This would require more integration, for now just advise installation
            bail!("Failed to build mdBook. Please install mdbook CLI.");
        }
    }
}