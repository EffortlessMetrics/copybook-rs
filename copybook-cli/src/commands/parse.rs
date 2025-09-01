//! Parse command implementation

use copybook_core::parse_copybook;
use crate::utils::atomic_write;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use tracing::info;

pub async fn run(
    copybook: PathBuf,
    output: Option<PathBuf>,
) -> Result<i32, Box<dyn std::error::Error>> {
    info!("Parsing copybook: {:?}", copybook);

    // Read copybook file
    let copybook_text = fs::read_to_string(&copybook)?;

    // Parse copybook
    let schema = parse_copybook(&copybook_text)?;

    // Serialize to JSON
    let json = serde_json::to_string_pretty(&schema)?;

    // Write output
    match output {
        Some(path) => {
            atomic_write(path, |writer| {
                writer.write_all(json.as_bytes())
            })?;
        }
        None => {
            println!("{}", json);
        }
    }

    info!("Parse completed successfully");
    Ok(0)
}
