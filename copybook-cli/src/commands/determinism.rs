//! Determinism validation command implementation
//!
//! Provides CLI commands for verifying determinism of encode/decode operations
//! using BLAKE3-based hashing and byte-level diff reporting.

use crate::exit_codes::ExitCode;
use crate::utils::read_file_or_stdin;
use anyhow::Context;
use clap::{Args, Subcommand, ValueEnum};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat,
    determinism::{
        DeterminismResult, check_decode_determinism, check_encode_determinism,
        check_round_trip_determinism,
    },
};
use copybook_core::{Schema, parse_copybook};
use std::path::{Path, PathBuf};

/// Determinism validation for encode/decode operations
#[derive(Args, Debug)]
pub struct DeterminismCommand {
    #[command(subcommand)]
    pub mode: DeterminismMode,
}

/// Determinism check modes
#[derive(Subcommand, Debug)]
pub enum DeterminismMode {
    /// Run determinism check for decode (binary → JSON)
    Decode(DecodeDeterminismArgs),

    /// Run determinism check for encode (JSON → binary)
    Encode(EncodeDeterminismArgs),

    /// Run determinism check for full round-trip (binary → JSON → binary → JSON)
    RoundTrip(RoundTripDeterminismArgs),
}

/// Common arguments for all determinism modes
#[derive(Args, Debug)]
pub struct CommonDeterminismArgs {
    /// Copybook schema file
    #[arg(value_name = "COPYBOOK")]
    pub copybook: PathBuf,

    /// Record format (e.g., fixed, rdw)
    #[arg(long, default_value = "fixed")]
    pub format: RecordFormat,

    /// EBCDIC codepage (e.g., cp037)
    #[arg(long, default_value = "cp037")]
    pub codepage: Codepage,

    /// JSON number handling mode
    #[arg(long, value_name = "MODE", default_value = "lossless")]
    pub json_number: JsonNumberMode,

    /// Include metadata in JSON output
    #[arg(long)]
    pub emit_meta: bool,

    /// Output format: human or json
    #[arg(long, value_name = "FORMAT", default_value = "human")]
    pub output: OutputFormat,

    /// Maximum number of byte diffs to report
    #[arg(long, value_name = "N", default_value_t = 100)]
    pub max_diffs: usize,
}

/// Output format for determinism results
#[derive(ValueEnum, Debug, Clone, Copy)]
pub enum OutputFormat {
    /// Human-readable output with symbols and diff table
    Human,
    /// Structured JSON output for CI integration
    Json,
}

/// Arguments for decode determinism check
#[derive(Args, Debug)]
pub struct DecodeDeterminismArgs {
    #[command(flatten)]
    pub common: CommonDeterminismArgs,

    /// Binary data file (single-record or sampled)
    #[arg(value_name = "DATA")]
    pub data: PathBuf,
}

/// Arguments for encode determinism check
#[derive(Args, Debug)]
pub struct EncodeDeterminismArgs {
    #[command(flatten)]
    pub common: CommonDeterminismArgs,

    /// JSON input file (first record used for validation)
    #[arg(value_name = "JSON")]
    pub json: PathBuf,
}

/// Arguments for round-trip determinism check
#[derive(Args, Debug)]
pub struct RoundTripDeterminismArgs {
    #[command(flatten)]
    pub common: CommonDeterminismArgs,

    /// Binary data file
    #[arg(value_name = "DATA")]
    pub data: PathBuf,
}

/// Main entry point for determinism subcommand
pub fn run(cmd: &DeterminismCommand) -> anyhow::Result<ExitCode> {
    match &cmd.mode {
        DeterminismMode::Decode(args) => run_decode(args),
        DeterminismMode::Encode(args) => run_encode(args),
        DeterminismMode::RoundTrip(args) => run_round_trip(args),
    }
}

/// Run decode determinism check
fn run_decode(args: &DecodeDeterminismArgs) -> anyhow::Result<ExitCode> {
    let schema = load_schema(&args.common.copybook)?;
    let decode_opts = build_decode_options(&args.common);

    let data = std::fs::read(&args.data).with_context(|| {
        format!(
            "Failed to read data file for determinism check: {}",
            args.data.display()
        )
    })?;

    let result = check_decode_determinism(&schema, &data, &decode_opts)
        .context("Decode determinism check failed")?;

    handle_result(&result, &args.common)
}

/// Run encode determinism check
fn run_encode(args: &EncodeDeterminismArgs) -> anyhow::Result<ExitCode> {
    let schema = load_schema(&args.common.copybook)?;
    let encode_opts = build_encode_options(&args.common);

    let json_text = std::fs::read_to_string(&args.json).with_context(|| {
        format!(
            "Failed to read JSON input for determinism check: {}",
            args.json.display()
        )
    })?;

    // Parse first line as single JSON record
    let first_line = json_text
        .lines()
        .next()
        .ok_or_else(|| anyhow::anyhow!("JSON input file is empty"))?;

    let value: serde_json::Value =
        serde_json::from_str(first_line).context("Failed to parse JSON input")?;

    let result = check_encode_determinism(&schema, &value, &encode_opts)
        .context("Encode determinism check failed")?;

    handle_result(&result, &args.common)
}

/// Run round-trip determinism check
fn run_round_trip(args: &RoundTripDeterminismArgs) -> anyhow::Result<ExitCode> {
    let schema = load_schema(&args.common.copybook)?;
    let decode_opts = build_decode_options(&args.common);
    let encode_opts = build_encode_options(&args.common);

    let data = std::fs::read(&args.data).with_context(|| {
        format!(
            "Failed to read data file for round-trip determinism check: {}",
            args.data.display()
        )
    })?;

    let result = check_round_trip_determinism(&schema, &data, &decode_opts, &encode_opts)
        .context("Round-trip determinism check failed")?;

    handle_result(&result, &args.common)
}

/// Load and parse copybook schema
fn load_schema(path: &Path) -> anyhow::Result<Schema> {
    let text = read_file_or_stdin(path)?;
    let schema = parse_copybook(&text)
        .with_context(|| format!("Failed to parse copybook: {}", path.display()))?;
    Ok(schema)
}

/// Build `DecodeOptions` from common arguments
fn build_decode_options(common: &CommonDeterminismArgs) -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(common.codepage)
        .with_format(common.format)
        .with_json_number_mode(common.json_number)
        .with_emit_meta(common.emit_meta)
}

/// Build `EncodeOptions` from common arguments
fn build_encode_options(common: &CommonDeterminismArgs) -> EncodeOptions {
    EncodeOptions::new().with_codepage(common.codepage)
}

/// Handle determinism result and format output
fn handle_result(
    result: &DeterminismResult,
    common: &CommonDeterminismArgs,
) -> anyhow::Result<ExitCode> {
    match common.output {
        OutputFormat::Json => {
            let json = serde_json::to_string_pretty(result)
                .context("Failed to serialize determinism result to JSON")?;
            println!("{json}");
        }
        OutputFormat::Human => {
            print_human_result(result, common.max_diffs);
        }
    }

    if result.is_deterministic {
        Ok(ExitCode::Ok)
    } else {
        // Non-deterministic case: return Data exit code (represents validation failure)
        Ok(ExitCode::Data)
    }
}

/// Print human-readable determinism result
fn print_human_result(result: &DeterminismResult, max_diffs: usize) {
    // Header
    println!("Determinism mode: {:?}", result.mode);
    println!("Round 1 hash: {}", truncate_hash(&result.round1_hash));
    println!("Round 2 hash: {}", truncate_hash(&result.round2_hash));

    // Verdict
    if result.is_deterministic {
        println!("\n✅ DETERMINISTIC");
    } else {
        println!("\n❌ NON-DETERMINISTIC");
    }

    // Byte differences (if any)
    if let Some(diffs) = &result.byte_differences {
        let count = diffs.len();
        let shown = diffs.iter().take(max_diffs);

        println!("\nByte differences: {count} total");
        if count > 0 {
            println!("\n  Offset  Round1  Round2");
            println!("  ------  ------  ------");
            for d in shown {
                println!(
                    "  0x{:04X}  0x{:02X}    0x{:02X}",
                    d.offset, d.round1_byte, d.round2_byte
                );
            }

            if count > max_diffs {
                println!("\n  ... {} more differences not shown", count - max_diffs);
            }
        }
    } else {
        println!("\nByte differences: none");
    }
}

/// Truncate BLAKE3 hash for human-readable output (first 16 hex chars)
fn truncate_hash(hash: &str) -> String {
    if hash.len() > 16 {
        format!("{}...", &hash[..16])
    } else {
        hash.to_string()
    }
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn hash_truncation_works() {
        let long_hash = "7a3f9e2b1c4d5e6f7a3f9e2b1c4d5e6f";
        assert_eq!(truncate_hash(long_hash), "7a3f9e2b1c4d5e6f...");

        let short_hash = "7a3f9e2b";
        assert_eq!(truncate_hash(short_hash), "7a3f9e2b");
    }
}
