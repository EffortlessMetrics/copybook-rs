//! Support command for feature matrix and copybook validation

use crate::exit_codes::ExitCode;
use std::path::PathBuf;

#[derive(clap::Args)]
pub struct SupportArgs {
    /// Display COBOL support matrix
    #[arg(long)]
    pub matrix: bool,

    /// Check copybook file for unsupported constructs
    #[arg(long)]
    pub check: Option<PathBuf>,
}

pub fn run(args: &SupportArgs) -> anyhow::Result<ExitCode> {
    if args.matrix {
        // TODO: Load from docs/reference/COBOL_SUPPORT_MATRIX.md when available
        println!("# COBOL Support Matrix");
        println!();
        println!("Feature support matrix not yet implemented.");
        println!("See docs/reference/COBOL_SUPPORT_MATRIX.md for current status.");
        return Ok(ExitCode::Ok);
    }

    if let Some(path) = &args.check {
        let _text = std::fs::read_to_string(path)?;
        // TODO: Implement real construct detection using parser
        println!("Copybook validation not yet implemented.");
        println!("Placeholder for feature detection in: {}", path.display());
        return Ok(ExitCode::Ok);
    }

    // No flags provided
    eprintln!("Error: Either --matrix or --check <file> must be specified");
    Ok(ExitCode::Encode)
}
