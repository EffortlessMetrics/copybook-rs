#![deny(clippy::unwrap_used, clippy::expect_used)]
//! Command-line interface for copybook-rs
//!
//! This binary provides a user-friendly CLI for parsing copybooks and
//! converting mainframe data files.

use crate::exit_codes::ExitCode;
use anyhow::{Error as AnyhowError, anyhow};
use clap::error::ErrorKind as ClapErrorKind;
use clap::{ColorChoice, Parser, Subcommand, ValueEnum};
use copybook_codec::{Codepage, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy};
use copybook_core::Error as CoreError;
use std::borrow::Cow;
use std::convert::TryFrom;
use std::error::Error as StdError;
use std::io::{self, ErrorKind, IsTerminal, Write};
use std::panic::AssertUnwindSafe;
use std::path::{Path, PathBuf};
use std::process::ExitCode as ProcessExitCode;
use std::sync::OnceLock;
use std::time::{SystemTime, UNIX_EPOCH};
use tracing::Level;
use tracing_subscriber::EnvFilter;

#[cfg(feature = "metrics")]
use clap::Args;

#[cfg(feature = "metrics")]
use std::net::SocketAddr;

#[cfg(feature = "metrics")]
use std::sync::Once;

static INVOCATION_ID: OnceLock<String> = OnceLock::new();

/// Bump when log schema fields change.
pub const LOG_SCHEMA: u8 = 1;

pub mod subcode {
    /// Policy compatibility warning: `--preferred-zoned-encoding` without preservation.
    ///
    /// Reserved ranges:
    /// - `2xx`: deprecations
    /// - `4xx`: policy compatibility and enforcement (reserved for operator-facing guardrails)
    /// - `5xx`: internal escalations / invariants
    pub const POLICY_PREFERRED_WITHOUT_PRESERVE: u16 = 401;
}

fn invocation_id() -> &'static str {
    INVOCATION_ID.get_or_init(|| {
        if let Ok(from_env) = std::env::var("COPYBOOK_INVOCATION_ID") {
            return from_env;
        }
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        format!("pid{}-ts{}", std::process::id(), nanos)
    })
}

#[derive(Parser)]
#[command(name = "copybook", color = ColorChoice::Auto)]
#[command(about = "Modern COBOL copybook parser and data converter")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Enable verbose logging
    #[arg(short, long)]
    verbose: bool,

    /// Enforce policy checks. Precedence: --strict-policy > --no-strict-policy > `COPYBOOK_STRICT_POLICY`.
    #[arg(
        long,
        action = clap::ArgAction::SetTrue,
        conflicts_with = "no_strict_policy"
    )]
    strict_policy: bool,

    /// Disable strict checks for this run, even if `COPYBOOK_STRICT_POLICY=1`.
    #[arg(
        long = "no-strict-policy",
        action = clap::ArgAction::SetTrue,
        conflicts_with = "strict_policy"
    )]
    no_strict_policy: bool,

    #[cfg(feature = "metrics")]
    #[command(flatten)]
    metrics: MetricsOpts,
}

struct BrokenPipeSafeStderr(std::io::Stderr);

impl Write for BrokenPipeSafeStderr {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self.0.write(buf) {
            Ok(written) => Ok(written),
            Err(err) if is_consumer_closed(&err) => Ok(buf.len()),
            Err(err) => Err(err),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self.0.flush() {
            Ok(()) => Ok(()),
            Err(err) if is_consumer_closed(&err) => Ok(()),
            Err(err) => Err(err),
        }
    }
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

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
enum DialectPreference {
    /// Normative dialect - `min_count` is strictly enforced
    #[value(name = "n")]
    N,
    /// Zero-tolerant dialect - `min_count` is ignored
    #[value(name = "0")]
    Zero,
    /// One-tolerant dialect - `min_count` is clamped to 1
    #[value(name = "1")]
    One,
}

impl From<DialectPreference> for copybook_core::dialect::Dialect {
    fn from(value: DialectPreference) -> Self {
        match value {
            DialectPreference::N => Self::Normative,
            DialectPreference::Zero => Self::ZeroTolerant,
            DialectPreference::One => Self::OneTolerant,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
enum ZonedEncodingPreference {
    /// Prefer default zero policy based on target code page.
    #[value(alias = "preferred-zero")]
    Preferred,
    /// Force ASCII zoned encoding format.
    Ascii,
    /// Force EBCDIC zoned encoding format.
    Ebcdic,
    /// Defer to automatic detection when metadata supplies a format.
    Auto,
}

impl From<ZonedEncodingPreference> for copybook_codec::ZonedEncodingFormat {
    fn from(value: ZonedEncodingPreference) -> Self {
        match value {
            ZonedEncodingPreference::Preferred | ZonedEncodingPreference::Auto => Self::Auto,
            ZonedEncodingPreference::Ascii => Self::Ascii,
            ZonedEncodingPreference::Ebcdic => Self::Ebcdic,
        }
    }
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
        /// Dialect for ODO `min_count` interpretation (n=normative, 0=zero-tolerant, 1=one-tolerant)
        #[arg(long, value_enum)]
        dialect: Option<DialectPreference>,
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
        /// Dialect for ODO `min_count` interpretation (n=normative, 0=zero-tolerant, 1=one-tolerant)
        #[arg(long, value_enum)]
        dialect: Option<DialectPreference>,
    },
    /// Decode binary data to JSONL
    #[command(
        after_help = "Comments: inline (*>) allowed by default; use --strict-comments to disable.\n\
Zoned policy: override → preserved → preferred.\n\n\
Field Projection:\n\
  Use --select to include specific fields in output (comma-separated or multiple flags).\n\
  Examples:\n\
    --select \"CUSTOMER-ID,BALANCE\"\n\
    --select CUSTOMER-ID --select BALANCE\n\
  ODO counters and parent groups are automatically included."
    )]
    Decode {
        /// Copybook file path
        copybook: PathBuf,
        /// Input data file path
        input: PathBuf,
        /// Output JSONL file path (use "-" for stdout)
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
        /// Preserve zoned encoding detected during decode; wins over preferred.
        #[arg(long)]
        preserve_zoned_encoding: bool,
        /// Preferred zoned encoding when neither preserved nor overridden.
        /// Example: prefer EBCDIC 'F' zero punch for zero.
        #[arg(long, value_enum, default_value_t = ZonedEncodingPreference::Preferred)]
        preferred_zoned_encoding: ZonedEncodingPreference,
        /// Dialect for ODO `min_count` interpretation (n=normative, 0=zero-tolerant, 1=one-tolerant)
        #[arg(long, value_enum)]
        dialect: Option<DialectPreference>,
        /// Select specific fields to include in output (comma-separated or multiple flags)
        /// Automatically includes ODO counters and parent groups for structure
        #[arg(long, value_name = "FIELD[,FIELD...]")]
        select: Vec<String>,
    },
    /// Encode JSONL to binary data
    #[command(
        after_help = "Comments: inline (*>) allowed by default; use --strict-comments to disable.\n\
Zoned policy: override → preserved → preferred.\n\n\
Field Projection:\n\
  Use --select to validate only specific fields during encoding (comma-separated or multiple flags).\n\
  Examples:\n\
    --select \"CUSTOMER-ID,BALANCE\"\n\
    --select CUSTOMER-ID --select BALANCE\n\
  ODO counters and parent groups are automatically included."
    )]
    Encode {
        /// Copybook file path
        copybook: PathBuf,
        /// Input JSONL file path
        input: PathBuf,
        /// Output binary file path (use "-" for stdout)
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
        /// Force zoned encoding format (ascii|ebcdic), ignoring preserved/preferred.
        #[arg(long, value_enum)]
        zoned_encoding_override: Option<copybook_codec::ZonedEncodingFormat>,
        /// Dialect for ODO `min_count` interpretation (n=normative, 0=zero-tolerant, 1=one-tolerant)
        #[arg(long, value_enum)]
        dialect: Option<DialectPreference>,
        /// Select specific fields to validate during encoding (comma-separated or multiple flags)
        /// Automatically includes ODO counters and parent groups for structure
        #[arg(long, value_name = "FIELD[,FIELD...]")]
        select: Vec<String>,
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

Comments: inline (*>) allowed by default; use --strict-comments to disable.

Field Projection:
  Use --select to validate only specific fields (comma-separated or multiple flags).
  Examples:
    --select \"CUSTOMER-ID,BALANCE\"
    --select CUSTOMER-ID --select BALANCE
  ODO counters and parent groups are automatically included.")]
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
        /// Dialect for ODO `min_count` interpretation (n=normative, 0=zero-tolerant, 1=one-tolerant)
        #[arg(long, value_enum)]
        dialect: Option<DialectPreference>,
        /// Select specific fields to validate (comma-separated or multiple flags)
        /// Automatically includes ODO counters and parent groups for structure
        #[arg(long, value_name = "FIELD[,FIELD...]")]
        select: Vec<String>,
    },
    /// Display COBOL support matrix or check copybook compatibility
    Support {
        #[command(flatten)]
        args: crate::commands::support::SupportArgs,
    },
    /// Determinism validation for encode/decode operations
    #[command(after_help = "\
Exit codes:
  0 = deterministic (hashes match)
  2 = non-deterministic (drift detected)
  3 = codec/usage error (processing failure)

Output formats:
  human = Default human-readable output with diff table
  json  = Structured JSON for CI integration

Comments: inline (*>) allowed by default; use --strict-comments to disable.")]
    Determinism {
        #[command(flatten)]
        command: crate::commands::determinism::DeterminismCommand,
    },
}

fn main() -> ProcessExitCode {
    match std::panic::catch_unwind(AssertUnwindSafe(run)) {
        Ok(Ok(code)) => ProcessExitCode::from(code),
        Ok(Err(err)) => {
            let exit_code = map_error_to_exit_code(&err);
            let stderr_line = format!("{err}\n");
            let _ = write_stderr_all(stderr_line.as_bytes());
            let diagnostics = ExitDiagnostics::new(
                exit_code,
                "copybook CLI terminated with an error",
                "cli_run",
                "", // op_stage will be overridden by emit_exit_diagnostics_stage
                Level::ERROR,
                exit_code.as_i32(),
            )
            .with_io_error(err.downcast_ref::<io::Error>())
            .with_error(Some(err.as_ref()));
            emit_exit_diagnostics_stage(&diagnostics, Stage::Finalize);
            ProcessExitCode::from(exit_code)
        }
        Err(panic_payload) => {
            if panic_caused_by_std_pipe(panic_payload.as_ref()) {
                return ProcessExitCode::from(ExitCode::Ok);
            }
            let panic_msg = extract_panic_message(panic_payload.as_ref());
            let panic_line = format!("panic: {panic_msg}\n");
            let _ = write_stderr_all(panic_line.as_bytes());
            let diagnostics = ExitDiagnostics::new(
                ExitCode::Internal,
                "copybook CLI panicked",
                "panic",
                "", // op_stage will be overridden by emit_exit_diagnostics_stage
                Level::ERROR,
                ExitCode::Internal.as_i32(),
            );
            emit_exit_diagnostics_stage(&diagnostics, Stage::Panic);
            ProcessExitCode::from(ExitCode::Internal)
        }
    }
}

#[allow(clippy::too_many_lines)]
fn run() -> anyhow::Result<ExitCode> {
    #[allow(clippy::panic)]
    if std::env::var("COPYBOOK_TEST_PANIC")
        .map(|v| v == "1")
        .unwrap_or(false)
    {
        panic!("COPYBOOK_TEST_PANIC triggered");
    }

    let cli = match Cli::try_parse() {
        Ok(cli) => cli,
        Err(err) => {
            let kind = err.kind();
            let _ = err.print();
            if matches!(
                kind,
                ClapErrorKind::DisplayHelp | ClapErrorKind::DisplayVersion
            ) {
                let op = if matches!(kind, ClapErrorKind::DisplayVersion) {
                    "version"
                } else {
                    "help"
                };
                let diagnostics = ExitDiagnostics::new(
                    ExitCode::Ok,
                    "completed",
                    op,
                    "", // op_stage will be overridden by emit_exit_diagnostics_stage
                    Level::INFO,
                    0,
                );
                emit_exit_diagnostics_stage(&diagnostics, Stage::Finalize);
                return Ok(ExitCode::Ok);
            }
            let exit_code = ExitCode::Encode;
            let message = err.to_string();
            let diagnostics = ExitDiagnostics::new(
                exit_code,
                &message,
                "cli_parse",
                "", // op_stage will be overridden by emit_exit_diagnostics_stage
                Level::ERROR,
                exit_code.as_i32(),
            )
            .with_error(Some(&err));
            emit_exit_diagnostics_stage(&diagnostics, Stage::Parse);
            return Ok(exit_code);
        }
    };

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

    let strict_policy = effective_strict_policy(&cli);
    let verbose = cli.verbose;
    let command = cli.command;

    // Initialize tracing
    let default_directive = if verbose { "debug" } else { "warn" };
    let env_filter =
        EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new(default_directive));

    let use_colors = std::io::stderr().is_terminal() && std::env::var_os("NO_COLOR").is_none();

    tracing_subscriber::fmt()
        .with_env_filter(env_filter)
        .with_ansi(use_colors)
        .with_writer(|| BrokenPipeSafeStderr(std::io::stderr()))
        .init();

    let help_requested =
        std::env::args_os().any(|arg| arg == "--help" || arg == "-h" || arg == "-?" || arg == "/?");
    let version_requested = std::env::args_os().any(|arg| arg == "--version" || arg == "-V");
    if !(help_requested || version_requested) {
        tracing::info!(
            invocation_id = %invocation_id(),
            version = env!("CARGO_PKG_VERSION"),
            commit = option_env!("GIT_SHA").unwrap_or("unknown"),
            os = std::env::consts::OS,
            arch = std::env::consts::ARCH,
            strict_policy,
            "copybook-cli start"
        );
    }

    let (exit_status, exit_op): (anyhow::Result<ExitCode>, &'static str) = match command {
        Commands::Parse {
            copybook,
            output,
            strict,
            strict_comments,
            dialect,
        } => {
            let effective_dialect = effective_dialect(dialect);
            (
                crate::commands::parse::run(
                    &copybook,
                    output,
                    strict,
                    strict_comments,
                    effective_dialect,
                ),
                "parse",
            )
        }
        Commands::Inspect {
            copybook,
            codepage,
            strict,
            strict_comments,
            dialect,
        } => {
            let effective_dialect = effective_dialect(dialect);
            (
                crate::commands::inspect::run(
                    &copybook,
                    codepage,
                    strict,
                    strict_comments,
                    effective_dialect,
                ),
                "inspect",
            )
        }
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
            preferred_zoned_encoding: preferred_zoned_encoding_cli,
            dialect,
            select,
        } => {
            let effective_dialect = effective_dialect(dialect);
            (
                crate::commands::decode::run(&crate::commands::decode::DecodeArgs {
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
                    preferred_zoned_encoding: preferred_zoned_encoding_cli.into(),
                    strict_policy,
                    dialect: effective_dialect.into(),
                    select: &select,
                }),
                "decode",
            )
        }
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
            dialect,
            select,
        } => {
            let effective_dialect = effective_dialect(dialect);
            (
                crate::commands::encode::run(
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
                        dialect: effective_dialect.into(),
                        select: &select,
                    },
                ),
                "encode",
            )
        }
        #[cfg(feature = "audit")]
        Commands::Audit { audit_command } => {
            // Run audit command asynchronously
            let runtime = tokio::runtime::Runtime::new()?;
            (
                runtime
                    .block_on(crate::commands::audit::run(audit_command))
                    .map_err(|err| anyhow!(err)),
                "audit",
            )
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
            dialect,
            select,
        } => {
            let effective_dialect = effective_dialect(dialect);
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
                dialect: effective_dialect.into(),
                select: &select,
            };
            (
                crate::commands::verify::run(&copybook, &input, report, &opts),
                "verify",
            )
        }
        Commands::Support { args } => (crate::commands::support::run(&args), "support"),
        Commands::Determinism { command } => {
            (crate::commands::determinism::run(&command), "determinism")
        }
    };

    #[cfg(feature = "metrics")]
    if let (Err(err), Some((handle, _))) = (&exit_status, &metrics_server) {
        let records_processed = metrics_records_total(handle);
        bump_error_if_pre_run(err, records_processed);
    }

    let status = exit_status?;

    let diagnostics = if status == ExitCode::Ok {
        ExitDiagnostics::new(
            ExitCode::Ok,
            "completed",
            exit_op,
            "", // op_stage will be overridden by emit_exit_diagnostics_stage
            Level::INFO,
            0,
        )
    } else {
        ExitDiagnostics::new(
            status,
            "command completed with non-zero exit code",
            exit_op,
            "", // op_stage will be overridden by emit_exit_diagnostics_stage
            Level::ERROR,
            status.as_i32(),
        )
    };

    let stage = if status == ExitCode::Ok {
        Stage::Finalize
    } else {
        Stage::Execute
    };
    emit_exit_diagnostics_stage(&diagnostics, stage);

    Ok(status)
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
                    let _ = pre_runtime_tx.send(Err(anyhow!(
                        "failed to build Tokio runtime for metrics exporter: {err}"
                    )));
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
                    let _ = handle_tx
                        .send(Err(anyhow!("failed to install Prometheus recorder: {err}")));
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

    let handle = handle_rx.recv().map_err(|err| {
        anyhow!("failed to receive Prometheus handle from exporter thread: {err}")
    })??;

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
) -> anyhow::Result<
    Option<(
        metrics_exporter_prometheus::PrometheusHandle,
        std::thread::JoinHandle<()>,
    )>,
> {
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
        let dominant = dominant_exit_code(err);
        let family = match dominant {
            ExitCode::Data => ExitCode::Data.tag(),
            ExitCode::Encode => ExitCode::Encode.tag(),
            ExitCode::Format => ExitCode::Format.tag(),
            ExitCode::Internal | ExitCode::Ok | ExitCode::Unknown => ExitCode::Internal.tag(),
        };
        metrics::counter!("copybook_decode_errors_total", "family" => family).increment(1);
    }
}

fn map_error_to_exit_code(err: &AnyhowError) -> ExitCode {
    match dominant_exit_code(err) {
        ExitCode::Ok | ExitCode::Unknown => ExitCode::Internal,
        code => code,
    }
}

fn dominant_exit_code(err: &AnyhowError) -> ExitCode {
    let mut best = ExitCode::Unknown;
    let mut best_precedence = best.precedence();

    for prefix in collect_family_prefixes(err) {
        if let Some(code) = ExitCode::from_family_prefix(&prefix) {
            let precedence = code.precedence();
            if precedence > best_precedence {
                best = code;
                best_precedence = precedence;
            }
        }
    }

    best
}

fn collect_family_prefixes(err: &AnyhowError) -> Vec<String> {
    let mut prefixes = Vec::new();
    for cause in err.chain() {
        if let Some(core) = cause.downcast_ref::<CoreError>() {
            prefixes.push(core.family_prefix().to_string());
        }
        if let Some(prefix) = parse_prefix_from_str(&cause.to_string()) {
            prefixes.push(prefix);
        }
    }
    prefixes
}

fn parse_prefix_from_str(message: &str) -> Option<String> {
    let token = message.split_whitespace().next()?.trim_end_matches(':');
    if token.len() < 4 || !token.starts_with("CBK") {
        return None;
    }
    Some(token[..4].to_string())
}

/// Parameters for exit diagnostics logging.
pub(crate) struct ExitDiagnostics<'a> {
    exit: ExitCode,
    msg: &'a str,
    op: &'a str,
    path: Option<&'a Path>,
    io_error: Option<&'a io::Error>,
    error: Option<&'a (dyn StdError + 'static)>,
    subcode: Option<u16>,
    op_stage: &'a str,
    severity: Level,
    effective_exit: i32,
}

impl<'a> ExitDiagnostics<'a> {
    /// Create a new `ExitDiagnostics` with required parameters.
    pub fn new(
        exit: ExitCode,
        msg: &'a str,
        op: &'a str,
        op_stage: &'a str,
        severity: Level,
        effective_exit: i32,
    ) -> Self {
        Self {
            exit,
            msg,
            op,
            path: None,
            io_error: None,
            error: None,
            subcode: None,
            op_stage,
            severity,
            effective_exit,
        }
    }

    /// Set the path parameter.
    #[must_use]
    pub fn with_path(mut self, path: Option<&'a Path>) -> Self {
        self.path = path;
        self
    }

    /// Set the `io_error` parameter.
    #[must_use]
    pub fn with_io_error(mut self, io_error: Option<&'a io::Error>) -> Self {
        self.io_error = io_error;
        self
    }

    /// Set the error parameter.
    #[must_use]
    pub fn with_error(mut self, error: Option<&'a (dyn StdError + 'static)>) -> Self {
        self.error = error;
        self
    }

    /// Set the subcode parameter.
    #[must_use]
    pub fn with_subcode(mut self, subcode: Option<u16>) -> Self {
        self.subcode = subcode;
        self
    }
}

#[non_exhaustive]
#[derive(Copy, Clone)]
pub(crate) enum Stage {
    Parse,
    Execute,
    Finalize,
    Panic,
}

impl Stage {
    #[inline]
    pub const fn as_str(self) -> &'static str {
        match self {
            Stage::Parse => "parse",
            Stage::Execute => "execute",
            Stage::Finalize => "finalize",
            Stage::Panic => "panic",
        }
    }
}

#[inline]
pub(crate) fn emit_exit_diagnostics_stage(diagnostics: &ExitDiagnostics<'_>, stage: Stage) {
    let ExitDiagnostics {
        exit,
        msg,
        op,
        path,
        io_error,
        error,
        subcode,
        op_stage: _,
        severity,
        effective_exit,
    } = *diagnostics;

    let stage_diagnostics =
        ExitDiagnostics::new(exit, msg, op, stage.as_str(), severity, effective_exit)
            .with_path(path)
            .with_io_error(io_error)
            .with_error(error)
            .with_subcode(subcode);
    emit_exit_diagnostics(&stage_diagnostics);
}

pub(crate) fn emit_exit_diagnostics(diagnostics: &ExitDiagnostics<'_>) {
    let ExitDiagnostics {
        exit,
        msg,
        op,
        path,
        io_error,
        error,
        subcode,
        op_stage,
        severity,
        effective_exit,
    } = *diagnostics;
    let (errno, err_kind) =
        io_error.map_or((None, None), |err| (err.raw_os_error(), Some(err.kind())));
    let subcode_label = subcode.map_or_else(|| "n/a".to_string(), |value| value.to_string());
    let severity_tag = match severity {
        Level::ERROR => "ERROR",
        Level::WARN => "WARN",
        Level::INFO => "INFO",
        Level::DEBUG => "DEBUG",
        Level::TRACE => "TRACE",
    };

    macro_rules! log_diagnostic {
        ($macro:ident) => {
            tracing::$macro!(
                log_schema = LOG_SCHEMA,
                op_stage = %op_stage,
                invocation_id = %invocation_id(),
                severity_tag = %severity_tag,
                code_tag = %exit,
                code = exit.as_i32(),
                family = %exit.family(),
                precedence_rank = exit.precedence_rank(),
                subcode = %subcode_label,
                subcode_numeric = ?subcode,
                effective_exit = effective_exit,
                errno = ?errno,
                err_kind = ?err_kind,
                op = %op,
                path = ?path,
                io_error = ?io_error,
                error = ?error,
                "{msg}"
            )
        };
    }

    match severity {
        Level::ERROR => log_diagnostic!(error),
        Level::WARN => log_diagnostic!(warn),
        Level::INFO => log_diagnostic!(info),
        Level::DEBUG => log_diagnostic!(debug),
        Level::TRACE => log_diagnostic!(trace),
    }
}

fn effective_strict_policy(cli: &Cli) -> bool {
    if cli.strict_policy {
        true
    } else if cli.no_strict_policy {
        false
    } else {
        env_flag("COPYBOOK_STRICT_POLICY")
    }
}

/// Get effective dialect from CLI flag or environment variable
///
/// Precedence: CLI flag > `COPYBOOK_DIALECT` env var > default (Normative)
fn effective_dialect(cli_dialect: Option<DialectPreference>) -> DialectPreference {
    if let Some(dialect) = cli_dialect {
        return dialect;
    }
    if let Ok(env_val) = std::env::var("COPYBOOK_DIALECT") {
        match env_val.trim().to_ascii_lowercase().as_str() {
            "0" => DialectPreference::Zero,
            "1" => DialectPreference::One,
            _ => DialectPreference::N, // Default to normative on invalid value
        }
    } else {
        DialectPreference::N // Default to normative
    }
}

fn env_flag(name: &str) -> bool {
    std::env::var(name).ok().is_some_and(|value| {
        matches!(
            value.trim().to_ascii_lowercase().as_str(),
            "1" | "true" | "yes" | "on"
        )
    })
}

fn panic_caused_by_std_pipe(panic_payload: &dyn std::any::Any) -> bool {
    let message = if let Some(&msg) = panic_payload.downcast_ref::<&str>() {
        msg
    } else if let Some(msg) = panic_payload.downcast_ref::<String>() {
        msg.as_str()
    } else {
        return false;
    };

    let lower = message.to_ascii_lowercase();
    let is_std_stream =
        lower.contains("failed printing to stdout") || lower.contains("failed printing to stderr");
    if !is_std_stream {
        return false;
    }

    let is_broken_pipe = lower.contains("broken pipe")
        || lower.contains("os error 32")
        || lower.contains("error_broken_pipe")
        || lower.contains("error_no_data");
    let is_write_zero = lower.contains("write zero") || lower.contains("writezero");

    is_broken_pipe || is_write_zero
}

fn extract_panic_message(panic_payload: &dyn std::any::Any) -> Cow<'_, str> {
    if let Some(&msg) = panic_payload.downcast_ref::<&str>() {
        return Cow::Borrowed(msg);
    }
    if let Some(msg) = panic_payload.downcast_ref::<String>() {
        return Cow::Borrowed(msg.as_str());
    }
    Cow::Borrowed("unknown panic")
}

#[cfg(feature = "audit")]
pub(crate) fn write_stdout_line(line: &str) -> Result<(), io::Error> {
    let mut buffer = String::with_capacity(line.len() + 1);
    buffer.push_str(line);
    buffer.push('\n');
    write_stdout_all(buffer.as_bytes())
}

#[cfg_attr(not(feature = "audit"), allow(dead_code))] // audit CLI helper retained for enterprise workflows (tracked in ROADMAP phase 6)
pub(crate) fn write_stderr_line(line: &str) -> Result<(), io::Error> {
    let mut buffer = String::with_capacity(line.len() + 1);
    buffer.push_str(line);
    buffer.push('\n');
    write_stderr_all(buffer.as_bytes())
}

pub(crate) fn write_stdout_all(bytes: &[u8]) -> Result<(), io::Error> {
    let mut stdout = io::stdout().lock();
    match stdout.write_all(bytes) {
        Ok(()) => Ok(()),
        Err(err) if is_consumer_closed(&err) => Ok(()),
        Err(err) => Err(err),
    }
}

pub(crate) fn write_stderr_all(bytes: &[u8]) -> Result<(), io::Error> {
    let mut stderr = io::stderr().lock();
    match stderr.write_all(bytes) {
        Ok(()) => Ok(()),
        Err(err) if is_consumer_closed(&err) => Ok(()),
        Err(err) => Err(err),
    }
}

#[inline]
fn is_consumer_closed(err: &io::Error) -> bool {
    matches!(err.kind(), ErrorKind::BrokenPipe | ErrorKind::WriteZero)
        || err.raw_os_error() == Some(109)
        || err.raw_os_error() == Some(232)
}

mod commands {
    #[cfg(feature = "audit")]
    pub mod audit;
    pub mod decode;
    pub mod determinism;
    pub mod encode;
    pub mod inspect;
    pub mod parse;
    pub mod support;
    pub mod verify;
    pub mod verify_report;
}

mod exit_codes;
mod utils;

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use copybook_core::ErrorCode;
    use proptest::prelude::*;

    #[test]
    fn maps_cbkf_family_to_exit_code() {
        let core_error = CoreError::new(
            ErrorCode::CBKF221_RDW_UNDERFLOW,
            "RDW payload underflow detected",
        );
        let io_error = std::io::Error::other(core_error);
        let anyhow_error: AnyhowError = io_error.into();
        assert_eq!(map_error_to_exit_code(&anyhow_error), ExitCode::Format);
    }

    #[test]
    fn selects_highest_precedence_exit_code_from_error_chain() {
        let data_error = CoreError::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, "Record too short");
        let format_error =
            CoreError::new(ErrorCode::CBKF221_RDW_UNDERFLOW, "RDW underflow detected");
        let internal_error =
            CoreError::new(ErrorCode::CBKI001_INVALID_STATE, "Iterator invalid state");

        let chained = AnyhowError::from(data_error)
            .context(format_error.to_string())
            .context(internal_error.to_string());

        assert_eq!(map_error_to_exit_code(&chained), ExitCode::Internal);
    }

    #[test]
    fn exit_code_precedence_is_deterministic() {
        let scenarios = vec![
            (vec![ExitCode::Data, ExitCode::Encode], ExitCode::Encode),
            (vec![ExitCode::Data, ExitCode::Format], ExitCode::Format),
            (
                vec![ExitCode::Format, ExitCode::Internal],
                ExitCode::Internal,
            ),
            (
                vec![ExitCode::Encode, ExitCode::Internal, ExitCode::Data],
                ExitCode::Internal,
            ),
        ];

        for (inputs, expected) in scenarios {
            let err = build_error_stack(&inputs);
            assert_eq!(map_error_to_exit_code(&err), expected);
        }
    }

    proptest! {
        #[test]
        fn exit_code_precedence_respects_permutations(codes in proptest::collection::vec(
            proptest::sample::select(vec![
                ExitCode::Data,
                ExitCode::Encode,
                ExitCode::Format,
                ExitCode::Internal,
            ]),
            1..5
        )) {
            let mut expected = codes[0];
            for code in &codes[1..] {
                if code.precedence() > expected.precedence() {
                    expected = *code;
                }
            }
            for perm in permutations(&codes) {
                let err = build_error_stack(&perm);
                prop_assert_eq!(map_error_to_exit_code(&err), expected);
            }
        }
    }

    fn permutations(codes: &[ExitCode]) -> Vec<Vec<ExitCode>> {
        let mut out = Vec::new();
        let mut current = Vec::with_capacity(codes.len());
        let mut used = vec![false; codes.len()];
        backtrack(codes, &mut used, &mut current, &mut out);
        out
    }

    fn backtrack(
        codes: &[ExitCode],
        used: &mut [bool],
        current: &mut Vec<ExitCode>,
        out: &mut Vec<Vec<ExitCode>>,
    ) {
        if current.len() == codes.len() {
            out.push(current.clone());
            return;
        }
        for (idx, code) in codes.iter().enumerate() {
            if used[idx] {
                continue;
            }
            used[idx] = true;
            current.push(*code);
            backtrack(codes, used, current, out);
            current.pop();
            used[idx] = false;
        }
    }

    fn sample_core_error(code: ExitCode) -> CoreError {
        match code {
            ExitCode::Data => {
                CoreError::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, "Record too short")
            }
            ExitCode::Encode => {
                CoreError::new(ErrorCode::CBKE501_JSON_TYPE_MISMATCH, "JSON type mismatch")
            }
            ExitCode::Format => CoreError::new(ErrorCode::CBKF221_RDW_UNDERFLOW, "RDW underflow"),
            ExitCode::Internal => {
                CoreError::new(ErrorCode::CBKI001_INVALID_STATE, "Iterator invalid state")
            }
            _ => CoreError::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, "Record too short"),
        }
    }

    fn build_error_stack(codes: &[ExitCode]) -> AnyhowError {
        assert!(
            !codes.is_empty(),
            "at least one exit code required for stack"
        );
        let mut err = AnyhowError::from(sample_core_error(codes[0]));
        for code in &codes[1..] {
            err = err.context(sample_core_error(*code).to_string());
        }
        err
    }
}
