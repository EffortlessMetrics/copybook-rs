//! Parse command implementation

use crate::utils::{atomic_write, read_file_or_stdin};
use copybook_core::{ParseOptions, parse_copybook_with_options};
use std::path::PathBuf;
use tracing::info;

pub fn run(
    copybook: &PathBuf,
    output: Option<PathBuf>,
    strict: bool,
    strict_comments: bool,
) -> Result<i32, Box<dyn std::error::Error>> {
    info!("Parsing copybook: {:?}", copybook);

    if strict_comments {
        info!("Inline comments (*>) disabled (COBOL-85 compatibility)");
    }

    // Read copybook file or stdin
    let copybook_text = read_file_or_stdin(copybook)?;

    // Parse copybook with options
    let options = ParseOptions {
        strict,
        codepage: "cp037".to_string(),
        emit_filler: false,
        allow_inline_comments: !strict_comments,
    };
    let schema = parse_copybook_with_options(&copybook_text, &options)?;

    // Serialize to JSON
    let json = serde_json::to_string_pretty(&schema)?;

    // Write output
    match output {
        Some(path) => {
            atomic_write(path, |writer| writer.write_all(json.as_bytes()))?;
        }
        None => {
            println!("{json}");
        }
    }

    info!("Parse completed successfully");
    Ok(0)
}
