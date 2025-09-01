//! Parse command implementation

use copybook_core::parse_copybook;
use std::path::PathBuf;
use std::fs;
use tracing::info;

pub async fn run(copybook: PathBuf, output: Option<PathBuf>) -> Result<i32, Box<dyn std::error::Error>> {
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
            fs::write(path, json)?;
        }
        None => {
            println!("{}", json);
        }
    }
    
    info!("Parse completed successfully");
    Ok(0)
}