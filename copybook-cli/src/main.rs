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

#[cfg(feature = "metrics")]
use std::net::SocketAddr;

#[cfg(feature = "metrics")]
use std::sync::Once;

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
    pub metrics_listen: Option<SocketAddr>,
    /// Optional delay after run completion so scrapes can observe final metrics
    #[arg(long, default_value_t = 0)]
    pub metrics_grace_ms: u64,
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
    let metrics_opts = cli.metrics.clone();

    #[cfg(feature = "metrics")]
    let metrics_server = metrics_start_if_requested(&metrics_opts)?;

    #[cfg(feature = "metrics")]
    if metrics_server.is_some() {
        describe_metrics_once();
    }

    #[cfg(feature = "metrics")]
    let _metrics_guard = metrics_grace_guard(&metrics_opts);

    let verbose = cli.verbose;
    let command = cli.command;

    // Initialize tracing
    let level = if verbose { "debug" } else { "info" };
    tracing_subscriber::fmt()
        .with_env_filter(format!("copybook={level}"))
        .init();

    let exit_status = match command {
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

    #[cfg(feature = "metrics")]
    if let (Err(err), Some((handle, _))) = (&exit_status, &metrics_server) {
        let records_processed = metrics_records_total(handle);
        bump_error_if_pre_run(err, records_processed);
    }

    let status = exit_status?;
    let exit_code = u8::try_from(status).unwrap_or(1);

    Ok(ExitCode::from(exit_code))
}

#[cfg(feature = "metrics")]
fn install_prometheus(
    addr: SocketAddr,
) -> anyhow::Result<(
    metrics_exporter_prometheus::PrometheusHandle,
    std::thread::JoinHandle<()>,
)> {
    use metrics_exporter_prometheus::PrometheusBuilder;
    use std::sync::mpsc;

    let (handle_tx, handle_rx) = mpsc::channel();
    let join_handle = {
        let pre_runtime_tx = handle_tx.clone();
        std::thread::spawn(move || {
            let runtime = match tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
            {
                Ok(rt) => rt,
                Err(err) => {
                    let _ = pre_runtime_tx
                        .send(Err(anyhow!("failed to build Tokio runtime for metrics exporter: {err}")));
                    return;
                }
            };

            runtime.block_on(async move {
                let builder = PrometheusBuilder::new().with_http_listener(addr);
                let (recorder, exporter) = match builder.build() {
                    Ok(pair) => pair,
                    Err(err) => {
                        let _ = handle_tx
                            .send(Err(anyhow!("failed to build Prometheus exporter: {err}")));
                        return;
                    }
                };

                let handle = recorder.handle();
                if let Err(err) = metrics::set_global_recorder(recorder) {
                    let _ = handle_tx.send(Err(anyhow!(
                        "failed to install Prometheus recorder: {err}"
                    )));
                    return;
                }

                if handle_tx.send(Ok(handle)).is_err() {
                    tracing::warn!(
                        "metrics exporter handle receiver dropped before initialization"
                    );
                    return;
                }

                if let Err(err) = exporter.await {
                    tracing::error!(
                        error = ?err,
                        "metrics exporter terminated unexpectedly"
                    );
                }
            });
        })
    };

    let handle = handle_rx
        .recv()
        .map_err(|err| anyhow!("failed to receive Prometheus handle from exporter thread: {err}"))??;

    Ok((handle, join_handle))
}

#[cfg(feature = "metrics")]
struct MetricsGraceGuard(Option<std::time::Duration>);

#[cfg(feature = "metrics")]
fn metrics_grace_guard(opts: &MetricsOpts) -> MetricsGraceGuard {
    let duration = if opts.metrics_listen.is_some() && opts.metrics_grace_ms > 0 {
        Some(std::time::Duration::from_millis(opts.metrics_grace_ms))
    } else {
        None
    };
    MetricsGraceGuard(duration)
}

#[cfg(feature = "metrics")]
impl Drop for MetricsGraceGuard {
    fn drop(&mut self) {
        if let Some(duration) = self.0.take() {
            std::thread::sleep(duration);
        }
    }
}

#[cfg(feature = "metrics")]
fn metrics_start_if_requested(
    opts: &MetricsOpts,
) -> anyhow::Result<Option<(
    metrics_exporter_prometheus::PrometheusHandle,
    std::thread::JoinHandle<()>,
)>> {
    opts.metrics_listen.map(install_prometheus).transpose()
}

#[cfg(feature = "metrics")]
fn metrics_records_total(handle: &metrics_exporter_prometheus::PrometheusHandle) -> Option<f64> {
    let snapshot = handle.render();
    let mut saw_zero_entry = false;

    for line in snapshot.lines() {
        if line.starts_with('#') {
            continue;
        }
        if line.starts_with("copybook_records_total")
            && let Some(value_str) = line.split_whitespace().last()
            && let Ok(value) = value_str.parse::<f64>()
        {
            if value > 0.0 {
                return Some(value);
            }
            if value == 0.0 {
                saw_zero_entry = true;
            }
        }
    }

    if saw_zero_entry { Some(0.0) } else { None }
}

#[cfg(feature = "metrics")]
fn describe_metrics_once() {
    use metrics::{describe_counter, describe_gauge, describe_histogram};

    static METRICS_ONCE: Once = Once::new();

    METRICS_ONCE.call_once(|| {
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
    });
}

#[cfg(feature = "metrics")]
fn bump_error_if_pre_run(err: &AnyhowError, records_processed: Option<f64>) {
    if records_processed.unwrap_or(0.0) <= f64::EPSILON {
        let family = normalized_family_prefix(err).unwrap_or("CBKI");
        metrics::counter!("copybook_decode_errors_total", "family" => family).increment(1);
    }
}

fn map_error_to_exit_code(err: &AnyhowError) -> ExitCode {
    match normalized_family_prefix(err) {
        Some("CBKD") => ExitCode::from(2),
        Some("CBKE") => ExitCode::from(3),
        Some("CBKF") => ExitCode::from(4),
        Some("CBKI") => ExitCode::from(5),
        _ => ExitCode::from(1),
    }
}

fn normalized_family_prefix(err: &AnyhowError) -> Option<&'static str> {
    match extract_family_prefix(err).as_deref() {
        Some("CBKD") => Some("CBKD"),
        Some("CBKE") => Some("CBKE"),
        Some("CBKF") => Some("CBKF"),
        Some("CBKI") => Some("CBKI"),
        _ => None,
    }
}

fn extract_family_prefix(err: &AnyhowError) -> Option<String> {
    fn walk(error: &(dyn std::error::Error + 'static)) -> Option<String> {
        if let Some(core) = error.downcast_ref::<CoreError>() {
            return Some(core.family_prefix().to_string());
        }
        if let Some(source) = error.source() {
            return walk(source);
        }
        None
    }

    fn parse_prefix_from_str(message: &str) -> Option<String> {
        let token = message.split_whitespace().next()?.trim_end_matches(':');
        if token.len() < 4 || !token.starts_with("CBK") {
            return None;
        }
        Some(token[..4].to_string())
    }

    if let Some(prefix) = walk(err.as_ref()) {
        return Some(prefix);
    }

    if let Some(prefix) = parse_prefix_from_str(&err.to_string()) {
        return Some(prefix);
    }

    for cause in err.chain() {
        if let Some(prefix) = walk(cause) {
            return Some(prefix);
        }
        if let Some(prefix) = parse_prefix_from_str(&cause.to_string()) {
            return Some(prefix);
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

#[cfg(test)]
mod tests {
    use super::*;
    use copybook_core::ErrorCode;

    #[test]
    fn maps_cbkf_family_to_exit_code() {
        let core_error = CoreError::new(
            ErrorCode::CBKF221_RDW_UNDERFLOW,
            "RDW payload underflow detected",
        );
        let io_error = std::io::Error::other(core_error);
        let anyhow_error: AnyhowError = io_error.into();
        assert_eq!(map_error_to_exit_code(&anyhow_error), ExitCode::from(4));
    }
}
