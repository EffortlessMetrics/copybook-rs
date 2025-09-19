//! Command-line interface for copybook-rs
//!
//! This binary provides a user-friendly CLI for parsing copybooks and
//! converting mainframe data files.

use clap::{Parser, Subcommand};
use copybook_codec::{Codepage, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "copybook")]
#[command(about = "Modern COBOL copybook parser and data converter")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Enable verbose logging
    #[arg(short, long)]
    verbose: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Parse copybook and output schema JSON
    Parse {
        /// Copybook file path
        copybook: PathBuf,
        /// Output file (stdout if not specified)
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Enforce normative validation (ODO bounds/order, REDEFINES ambiguity as errors)
        #[arg(long)]
        strict: bool,
    },
    /// Inspect copybook and show human-readable layout
    Inspect {
        /// Copybook file path
        copybook: PathBuf,
        /// Character encoding
        #[arg(long, default_value = "cp037")]
        codepage: Codepage,
        /// Enforce normative validation (ODO bounds/order, REDEFINES ambiguity as errors)
        #[arg(long)]
        strict: bool,
    },
    /// Decode binary data to JSONL
    Decode {
        /// Copybook file path
        copybook: PathBuf,
        /// Input data file path
        input: PathBuf,
        /// Output JSONL file path
        #[arg(short, long)]
        output: PathBuf,
        /// Record format (explicit, no auto-detection)
        #[arg(long)]
        format: RecordFormat,
        /// Character encoding
        #[arg(long, default_value = "cp037")]
        codepage: Codepage,
        /// JSON number mode
        #[arg(long, default_value = "lossless")]
        json_number: JsonNumberMode,
        /// Enable strict mode (default: false for lenient mode)
        #[arg(long, default_value = "false")]
        strict: bool,
        /// Maximum errors before stopping
        #[arg(long)]
        max_errors: Option<u64>,
        /// Emit FILLER fields
        #[arg(long)]
        emit_filler: bool,
        /// Emit metadata
        #[arg(long)]
        emit_meta: bool,
        /// Raw data capture mode
        #[arg(long, default_value = "off")]
        emit_raw: RawMode,
        /// Unmappable character policy
        #[arg(long, default_value = "error")]
        on_decode_unmappable: UnmappablePolicy,
        /// Number of threads for parallel processing
        #[arg(long, default_value = "1")]
        threads: usize,
    },
    /// Encode JSONL to binary data
    Encode {
        /// Copybook file path
        copybook: PathBuf,
        /// Input JSONL file path
        input: PathBuf,
        /// Output binary file path
        #[arg(short, long)]
        output: PathBuf,
        /// Record format (explicit, no auto-detection)
        #[arg(long)]
        format: RecordFormat,
        /// Character encoding
        #[arg(long, default_value = "cp037")]
        codepage: Codepage,
        /// Use raw data when available
        #[arg(long)]
        use_raw: bool,
        /// Enable BLANK WHEN ZERO encoding
        #[arg(long)]
        bwz_encode: bool,
        /// Enable strict mode (default: false for lenient mode)
        #[arg(long, default_value = "false")]
        strict: bool,
        /// Maximum errors before stopping
        #[arg(long)]
        max_errors: Option<u64>,
        /// Stop on first error (default: true)
        #[arg(long, default_value = "true")]
        fail_fast: bool,
        /// Number of threads for parallel processing
        #[arg(long, default_value = "1")]
        threads: usize,
        /// Coerce non-string JSON numbers to strings before encoding
        #[arg(long)]
        coerce_numbers: bool,
    },
    /// Verify data file structure
    Verify {
        /// Copybook file path
        copybook: PathBuf,
        /// Input data file path
        input: PathBuf,
        /// Verification report output
        #[arg(long)]
        report: Option<PathBuf>,
        /// Record format (explicit, no auto-detection)
        #[arg(long)]
        format: RecordFormat,
        /// Character encoding
        #[arg(long, default_value = "cp037")]
        codepage: Codepage,
        /// Enable strict mode validation
        #[arg(long)]
        strict: bool,
        /// Maximum errors before stopping
        #[arg(long)]
        max_errors: Option<u64>,
        /// Number of sample records to include in report
        #[arg(long, default_value = "5")]
        sample: Option<u32>,
    },
}

fn main() {
    let cli = Cli::parse();

    // Initialize tracing
    let level = if cli.verbose { "debug" } else { "info" };
    tracing_subscriber::fmt()
        .with_env_filter(format!("copybook={level}"))
        .init();

    let result = match cli.command {
        Commands::Parse {
            copybook,
            output,
            strict,
        } => crate::commands::parse::run(&copybook, output, strict),
        Commands::Inspect {
            copybook,
            codepage,
            strict,
        } => crate::commands::inspect::run(&copybook, codepage, strict),
        Commands::Decode {
            copybook,
            input,
            output,
            format,
            codepage,
            json_number,
            strict,
            max_errors,
            emit_filler,
            emit_meta,
            emit_raw,
            on_decode_unmappable,
            threads,
        } => crate::commands::decode::run(
            &copybook,
            &input,
            &output,
            format,
            codepage,
            json_number,
            strict,
            max_errors,
            emit_filler,
            emit_meta,
            emit_raw,
            on_decode_unmappable,
            threads,
        ),
        Commands::Encode {
            copybook,
            input,
            output,
            format,
            codepage,
            use_raw,
            bwz_encode,
            strict,
            max_errors,
            fail_fast,
            threads,
            coerce_numbers,
        } => crate::commands::encode::run(
            &copybook,
            &input,
            &output,
            &crate::commands::encode::EncodeCliOptions {
                format,
                codepage,
                use_raw,
                bwz_encode,
                strict,
                max_errors,
                fail_fast,
                threads,
                coerce_numbers,
            }
        ),
        Commands::Verify {
            copybook,
            input,
            report,
            format,
            codepage,
            strict,
            max_errors,
            sample,
        } => crate::commands::verify::run(&copybook, &input, report, format, codepage, strict, max_errors, sample),
    };

    match result {
        Ok(exit_code) => {
            std::process::exit(exit_code);
        }
        Err(e) => {
            let exit_code = crate::utils::emit_fatal(e.as_ref());
            std::process::exit(exit_code);
        }
    }
}

mod commands {
    pub mod decode;
    pub mod encode;
    pub mod inspect;
    pub mod parse;
    pub mod verify;
    pub mod verify_report;
}

mod utils;
