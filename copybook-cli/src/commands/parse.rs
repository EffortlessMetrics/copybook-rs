// SPDX-License-Identifier: AGPL-3.0-or-later
//! Parse command implementation

use crate::utils::{atomic_write, read_file_or_stdin};
use crate::write_stdout_all;
use copybook_core::{ParseOptions, parse_copybook_with_options};
use copybook_exit_codes::ExitCode;
use std::path::PathBuf;
use tracing::info;

pub fn run(
    copybook: &PathBuf,
    output: Option<PathBuf>,
    strict: bool,
    strict_comments: bool,
    dialect: crate::DialectPreference,
) -> anyhow::Result<ExitCode> {
    info!("Parsing copybook: {:?}", copybook);

    if strict_comments {
        info!("Inline comments (*>) disabled (COBOL-85 compatibility)");
    }

    // Read copybook file or stdin
    let copybook_text = read_file_or_stdin(copybook)?;

    // Parse copybook with options
    let options = ParseOptions {
        strict_comments,
        strict,
        codepage: "cp037".to_string(),
        emit_filler: false,
        allow_inline_comments: !strict_comments,
        dialect: dialect.into(),
    };
    let schema = parse_copybook_with_options(&copybook_text, &options)?;

    // Serialize to JSON
    let json = serde_json::to_string_pretty(&schema)?;

    // Write output
    if let Some(path) = output {
        atomic_write(path, |writer| writer.write_all(json.as_bytes()))?;
    } else {
        let mut json_with_newline = json;
        json_with_newline.push('\n');
        write_stdout_all(json_with_newline.as_bytes())?;
    }

    info!("Parse completed successfully");
    Ok(ExitCode::Ok)
}
