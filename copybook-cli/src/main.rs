#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
//! Command-line interface for copybook-rs
//!
//! This binary provides a user-friendly CLI for parsing copybooks and
//! converting mainframe data files.

use anyhow::{Error as AnyhowError, anyhow};
use clap::{Parser, Subcommand};
use copybook_codec::{Codepage, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy};
use copybook_core::Error as CoreError;
use std::convert::TryFrom;
use std::path::PathBuf;
use std::process::ExitCode;

#[cfg(feature = "metrics")]
use clap::Args;

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

    #[cfg(feature = "metrics")]
    #[command(flatten)]
    metrics: MetricsOpts,
}

#[cfg(feature = "metrics")]
#[derive(Args, Debug, Clone)]
pub struct MetricsOpts {
    /// Expose Prometheus metrics at this address (e.g. 0.0.0.0:9300)
    #[arg(long)]
    pub metrics_listen: Option<std::net::SocketAddr>,
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
        /// Preserve decoded zoned format; wins over preferred.
        #[arg(long)]
        preserve_zoned_encoding: bool,
        /// Preferred zoned format when not preserved/overridden (e.g., prefer 0xF zero in EBCDIC).
        #[arg(long, value_enum, default_value = "auto")]
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
        /// Force zoned format (ascii|ebcdic), ignoring preserved/preferred.
        #[arg(long, value_enum)]
        zoned_encoding_override: Option<copybook_codec::ZonedEncodingFormat>,
    },
    /// Enterprise audit system for regulatory compliance
    #[cfg(feature = "audit")]
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

fn main() -> ExitCode {
    match run() {
        Ok(code) => code,
        Err(err) => {
            eprintln!("{err}");
            map_error_to_exit_code(&err)
        }
    }
}

#[allow(clippy::too_many_lines)]
fn run() -> anyhow::Result<ExitCode> {
    let cli = Cli::parse();

    #[cfg(feature = "metrics")]
    let _metrics_server = metrics_start_if_requested(&cli.metrics);

    // Initialize tracing
    let level = if cli.verbose { "debug" } else { "info" };
    tracing_subscriber::fmt()
        .with_env_filter(format!("copybook={level}"))
        .init();

    let exit_status = match cli.command {
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
        #[cfg(feature = "audit")]
        Commands::Audit { audit_command } => {
            // Run audit command asynchronously
            let runtime = tokio::runtime::Runtime::new()?;
            runtime
                .block_on(crate::commands::audit::run(audit_command))
                .map_err(|err| anyhow!(err))
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
            let value = max_errors.unwrap_or(10);
            let normalized_max_errors = u32::try_from(value).map_err(|_| {
                anyhow!(
                    "--max-errors must be between 0 and {} (received {value})",
                    u32::MAX
                )
            })?;

            let opts = crate::commands::verify::VerifyOptions {
                format,
                codepage,
                strict,
                max_errors: normalized_max_errors,
                sample: sample.unwrap_or(5),
                strict_comments,
            };
            crate::commands::verify::run(&copybook, &input, report, &opts)
        }
    };

    let status = exit_status?;
    let exit_code = u8::try_from(status).unwrap_or(1);
    Ok(ExitCode::from(exit_code))
}

#[cfg(feature = "metrics")]
fn install_prometheus(
    addr: std::net::SocketAddr,
) -> (
    metrics_exporter_prometheus::PrometheusHandle,
    tokio::task::JoinHandle<()>,
) {
    use metrics_exporter_prometheus::PrometheusBuilder;
    use std::sync::OnceLock;

    static RUNTIME: OnceLock<tokio::runtime::Runtime> = OnceLock::new();

    let builder = PrometheusBuilder::new().with_http_listener(addr);
    let (recorder, exporter) = builder
        .build()
        .expect("failed to build Prometheus exporter");
    let handle = recorder.handle();

    metrics::set_global_recorder(recorder).expect("failed to install Prometheus recorder");

    let runtime = RUNTIME.get_or_init(|| {
        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("failed to build metrics runtime")
    });

    let join_handle = runtime.spawn(async move {
        if let Err(err) = exporter.await {
            tracing::error!(error = ?err, "metrics exporter terminated unexpectedly");
        }
    });

    (handle, join_handle)
}

#[cfg(feature = "metrics")]
fn metrics_start_if_requested(
    opts: &MetricsOpts,
) -> Option<(
    metrics_exporter_prometheus::PrometheusHandle,
    tokio::task::JoinHandle<()>,
)> {
    opts.metrics_listen.map(|addr| {
        let handles = install_prometheus(addr);
        describe_metrics();
        handles
    })
}

#[cfg(feature = "metrics")]
fn describe_metrics() {
    use metrics::{describe_counter, describe_gauge, describe_histogram};

    describe_counter!(
        "copybook_records_total",
        "Records decoded by the copybook CLI"
    );
    describe_counter!("copybook_bytes_total", "Bytes decoded by the copybook CLI");
    describe_counter!(
        "copybook_decode_errors_total",
        "Decode errors grouped by error family"
    );
    describe_histogram!(
        "copybook_decode_seconds",
        "Decode wall time per file (seconds)"
    );
    describe_gauge!(
        "copybook_throughput_mibps",
        "MiB/s throughput for last completed file"
    );
}

fn map_error_to_exit_code(err: &AnyhowError) -> ExitCode {
    if let Some(prefix) = extract_family_prefix(err) {
        match prefix {
            "CBKD" => ExitCode::from(2),
            "CBKE" => ExitCode::from(3),
            "CBKF" => ExitCode::from(4),
            "CBKI" => ExitCode::from(5),
            _ => ExitCode::from(1),
        }
    } else {
        ExitCode::from(1)
    }
}

fn extract_family_prefix(err: &AnyhowError) -> Option<&'static str> {
    if let Some(core) = err.downcast_ref::<CoreError>() {
        return Some(core.family_prefix());
    }
    for cause in err.chain() {
        if let Some(core) = cause.downcast_ref::<CoreError>() {
            return Some(core.family_prefix());
        }
    }
    None
}

mod commands {
    #[cfg(feature = "audit")]
    pub mod audit;
    pub mod decode;
    pub mod encode;
    pub mod inspect;
    pub mod parse;
    pub mod verify;
    pub mod verify_report;
}

mod utils;
