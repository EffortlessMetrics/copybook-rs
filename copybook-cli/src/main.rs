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
    #[command(
        after_help = "Comments: inline (*>) allowed by default; use --strict-comments to disable."
    )]
    Parse {
        /// Copybook file path
        copybook: PathBuf,
        /// Output file (stdout if not specified)
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Enforce normative validation (ODO bounds/order, REDEFINES ambiguity as errors)
        #[arg(long)]
        strict: bool,
        /// Disable inline comments (*>) - enforce COBOL-85 compatibility
        #[arg(long)]
        strict_comments: bool,
    },
    /// Inspect copybook and show human-readable layout
    #[command(
        after_help = "Comments: inline (*>) allowed by default; use --strict-comments to disable."
    )]
    Inspect {
        /// Copybook file path
        copybook: PathBuf,
        /// Character encoding
        #[arg(long, default_value = "cp037")]
        codepage: Codepage,
        /// Enforce normative validation (ODO bounds/order, REDEFINES ambiguity as errors)
        #[arg(long)]
        strict: bool,
        /// Disable inline comments (*>) - enforce COBOL-85 compatibility
        #[arg(long)]
        strict_comments: bool,
    },
    /// Decode binary data to JSONL
    #[command(
        after_help = "Comments: inline (*>) allowed by default; use --strict-comments to disable."
    )]
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
        /// Stop on first error (default: false)
        #[arg(long, default_value = "false")]
        fail_fast: bool,
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
        /// Disable inline comments (*>) - enforce COBOL-85 compatibility
        #[arg(long)]
        strict_comments: bool,
        /// Preserve zoned decimal encoding format for round-trip fidelity
        #[arg(long)]
        preserve_zoned_encoding: bool,
        /// Preferred zoned encoding format for ambiguous detection
        #[arg(long, default_value = "auto")]
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat,
    },
    /// Encode JSONL to binary data
    #[command(
        after_help = "Comments: inline (*>) allowed by default; use --strict-comments to disable."
    )]
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
        /// Disable inline comments (*>) - enforce COBOL-85 compatibility
        #[arg(long)]
        strict_comments: bool,
        /// Override zoned decimal encoding format (ascii, ebcdic)
        #[arg(long)]
        zoned_encoding_override: Option<copybook_codec::ZonedEncodingFormat>,
    },
    /// Enterprise audit system for regulatory compliance
    #[command(
        after_help = "Enterprise audit capabilities including SOX, HIPAA, GDPR compliance validation, \
                      performance auditing, security monitoring, and data lineage tracking.\n\n\
                      Examples:\n\
                      copybook audit validate --compliance sox,gdpr schema.cpy\n\
                      copybook audit report --include-performance schema.cpy data.bin -o report.json\n\
                      copybook audit lineage source.cpy --source-system mainframe -o lineage.json"
    )]
    Audit {
        #[command(flatten)]
        audit_command: crate::commands::audit::AuditCommand,
    },

    /// Verify data file structure
    #[command(after_help = "\
Exit codes:
  0 = valid data, no errors
  3 = validation errors found
  2 = fatal error (I/O, schema)
Report schema: docs/VERIFY_REPORT.schema.json

Comments: inline (*>) allowed by default; use --strict-comments to disable.")]
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
        /// Disable inline comments (*>) - enforce COBOL-85 compatibility
        #[arg(long)]
        strict_comments: bool,
    },
}

#[allow(clippy::too_many_lines)]
fn main() -> Result<(), Box<dyn std::error::Error>> {
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
            strict_comments,
        } => crate::commands::parse::run(&copybook, output, strict, strict_comments),
        Commands::Inspect {
            copybook,
            codepage,
            strict,
            strict_comments,
        } => crate::commands::inspect::run(&copybook, codepage, strict, strict_comments),
        Commands::Decode {
            copybook,
            input,
            output,
            format,
            codepage,
            json_number,
            strict,
            max_errors,
            fail_fast,
            emit_filler,
            emit_meta,
            emit_raw,
            on_decode_unmappable,
            threads,
            strict_comments,
            preserve_zoned_encoding,
            preferred_zoned_encoding,
        } => crate::commands::decode::run(&crate::commands::decode::DecodeArgs {
            copybook: &copybook,
            input: &input,
            output: &output,
            format,
            codepage,
            json_number,
            strict,
            max_errors,
            fail_fast,
            emit_filler,
            emit_meta,
            emit_raw,
            on_decode_unmappable,
            threads,
            strict_comments,
            preserve_zoned_encoding,
            preferred_zoned_encoding,
        }),
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
            strict_comments,
            zoned_encoding_override,
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
                strict_comments,
                zoned_encoding_override,
            },
        ),
        Commands::Audit { audit_command } => {
            // Run audit command asynchronously
            let runtime = tokio::runtime::Runtime::new()?;
            runtime.block_on(crate::commands::audit::run(audit_command))
        }
        Commands::Verify {
            copybook,
            input,
            report,
            format,
            codepage,
            strict,
            max_errors,
            sample,
            strict_comments,
        } => {
            let opts = crate::commands::verify::VerifyOptions {
                format,
                codepage,
                strict,
                max_errors: u32::try_from(max_errors.unwrap_or(10)).unwrap_or(10),
                sample: sample.unwrap_or(5),
                strict_comments,
            };
            crate::commands::verify::run(&copybook, &input, report, &opts)
        }
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
    pub mod audit;
    pub mod decode;
    pub mod encode;
    pub mod inspect;
    pub mod parse;
    pub mod verify;
    pub mod verify_report;
}

mod utils;
