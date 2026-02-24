// SPDX-License-Identifier: AGPL-3.0-or-later
//! Determinism command execution for copybook CLI workflows.
//!
//! This crate focuses on one responsibility:
//! parsing CLI command arguments and producing formatted determinism output
//! for decode/encode/round-trip checks.

use anyhow::Context;
use clap::{Args, Subcommand, ValueEnum};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat,
    determinism::{
        ByteDiff, DeterminismMode, DeterminismResult, check_decode_determinism,
        check_encode_determinism, check_round_trip_determinism,
    },
};
use copybook_core::{Schema, parse_copybook};
use std::fmt::Write as _;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

pub use copybook_determinism::{DEFAULT_MAX_DIFFS, BLAKE3_HEX_LEN};
pub use copybook_codec::determinism::DeterminismMode as DeterminismCheckMode;

/// Determinism command façade for CLI surface.
#[derive(Args, Debug, Clone)]
pub struct DeterminismCommand {
    #[command(subcommand)]
    pub mode: DeterminismMode,
}

/// Determinism check modes.
#[derive(Subcommand, Debug, Clone)]
pub enum DeterminismMode {
    /// Run determinism check for decode (binary → JSON).
    Decode(DecodeDeterminismArgs),

    /// Run determinism check for encode (JSON → binary).
    Encode(EncodeDeterminismArgs),

    /// Run determinism check for full round-trip (binary → JSON → binary → JSON).
    RoundTrip(RoundTripDeterminismArgs),
}

/// Backward-compatible alias retained for callers importing the command mode by name.
pub type DeterminismModeCommand = DeterminismMode;

/// Shared determinism arguments.
#[derive(Args, Debug, Clone)]
pub struct CommonDeterminismArgs {
    /// Copybook schema file.
    #[arg(value_name = "COPYBOOK")]
    pub copybook: PathBuf,

    /// Record format.
    #[arg(long, default_value = "fixed")]
    pub format: RecordFormat,

    /// EBCDIC codepage.
    #[arg(long, default_value = "cp037")]
    pub codepage: Codepage,

    /// JSON number handling mode.
    #[arg(long, value_name = "MODE", default_value = "lossless")]
    pub json_number: JsonNumberMode,

    /// Include metadata in JSON output.
    #[arg(long)]
    pub emit_meta: bool,

    /// Output format: human or json.
    #[arg(long, value_name = "FORMAT", default_value = "human")]
    pub output: OutputFormat,

    /// Maximum number of byte diffs to report.
    #[arg(long, value_name = "N", default_value_t = DEFAULT_MAX_DIFFS)]
    pub max_diffs: usize,
}

/// Available output rendering modes.
#[derive(ValueEnum, Debug, Clone, Copy)]
pub enum OutputFormat {
    /// Human-readable output with symbols and diff table.
    Human,
    /// Structured JSON output for CI integration.
    Json,
}

/// Decode command arguments.
#[derive(Args, Debug, Clone)]
pub struct DecodeDeterminismArgs {
    #[command(flatten)]
    pub common: CommonDeterminismArgs,

    /// Binary input file (single record or sampled payload).
    #[arg(value_name = "DATA")]
    pub data: PathBuf,
}

/// Encode command arguments.
#[derive(Args, Debug, Clone)]
pub struct EncodeDeterminismArgs {
    #[command(flatten)]
    pub common: CommonDeterminismArgs,

    /// JSON input file (first line is used).
    #[arg(value_name = "JSON")]
    pub json: PathBuf,
}

/// Round-trip command arguments.
#[derive(Args, Debug, Clone)]
pub struct RoundTripDeterminismArgs {
    #[command(flatten)]
    pub common: CommonDeterminismArgs,

    /// Binary input file.
    #[arg(value_name = "DATA")]
    pub data: PathBuf,
}

/// Result of running a determinism command execution.
#[derive(Debug, Clone, Copy)]
pub struct DeterminismRun {
    pub verdict: DeterminismVerdict,
    pub output: String,
}

impl DeterminismRun {
    /// Convert command verdict to CLI exit code semantics.
    #[must_use]
    pub const fn exit_code(&self) -> i32 {
        self.verdict.exit_code()
    }
}

/// Executable verdict from determinism checks.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeterminismVerdict {
    /// Both runs were byte-for-byte identical.
    Deterministic,
    /// A difference was detected and data diverged.
    NonDeterministic,
}

impl DeterminismVerdict {
    const fn from_result(result: &DeterminismResult) -> Self {
        if result.is_deterministic {
            Self::Deterministic
        } else {
            Self::NonDeterministic
        }
    }

    /// Convert verdict to a CLI-style exit code.
    #[must_use]
    pub const fn exit_code(&self) -> i32 {
        match self {
            Self::Deterministic => 0,
            Self::NonDeterministic => 2,
        }
    }
}

/// Execute a determinism subcommand and return output plus verdict.
pub fn run(cmd: &DeterminismCommand) -> anyhow::Result<DeterminismRun> {
    let result = match &cmd.mode {
        DeterminismModeCommand::Decode(args) => run_decode(args),
        DeterminismModeCommand::Encode(args) => run_encode(args),
        DeterminismModeCommand::RoundTrip(args) => run_round_trip(args),
    }?;

    Ok(result)
}

/// Run decode determinism check.
fn run_decode(args: &DecodeDeterminismArgs) -> anyhow::Result<DeterminismRun> {
    let schema = load_schema(&args.common.copybook)?;
    let decode_opts = build_decode_options(&args.common);
    let data = read_bytes_or_stdin(&args.data).with_context(|| {
        format!(
            "Failed to read data file for determinism check: {}",
            args.data.display()
        )
    })?;

    let result = check_decode_determinism(&schema, &data, &decode_opts)
        .context("Decode determinism check failed")?;

    render_result(&result, &args.common)
}

/// Run encode determinism check.
fn run_encode(args: &EncodeDeterminismArgs) -> anyhow::Result<DeterminismRun> {
    let schema = load_schema(&args.common.copybook)?;
    let encode_opts = build_encode_options(&args.common);
    let json_text = read_text_or_stdin(&args.json).with_context(|| {
        format!(
            "Failed to read JSON input for determinism check: {}",
            args.json.display()
        )
    })?;

    let first_line = json_text
        .lines()
        .next()
        .ok_or_else(|| anyhow::anyhow!("JSON input file is empty"))?;
    let value: serde_json::Value = serde_json::from_str(first_line).context("Failed to parse JSON input")?;

    let result = check_encode_determinism(&schema, &value, &encode_opts)
        .context("Encode determinism check failed")?;

    render_result(&result, &args.common)
}

/// Run round-trip determinism check.
fn run_round_trip(args: &RoundTripDeterminismArgs) -> anyhow::Result<DeterminismRun> {
    let schema = load_schema(&args.common.copybook)?;
    let decode_opts = build_decode_options(&args.common);
    let encode_opts = build_encode_options(&args.common);
    let data = read_bytes_or_stdin(&args.data).with_context(|| {
        format!(
            "Failed to read data file for round-trip determinism check: {}",
            args.data.display()
        )
    })?;

    let result = check_round_trip_determinism(&schema, &data, &decode_opts, &encode_opts)
        .context("Round-trip determinism check failed")?;

    render_result(&result, &args.common)
}

/// Common renderer for result + status.
fn render_result(
    result: &DeterminismResult,
    common: &CommonDeterminismArgs,
) -> anyhow::Result<DeterminismRun> {
    let output = match common.output {
        OutputFormat::Json => render_json_result(result),
        OutputFormat::Human => render_human_result(result, common.max_diffs),
    }?;

    Ok(DeterminismRun {
        verdict: DeterminismVerdict::from_result(result),
        output,
    })
}

/// Create JSON formatted output string.
pub fn render_json_result(result: &DeterminismResult) -> anyhow::Result<String> {
    serde_json::to_string_pretty(result).context("Failed to serialize determinism result to JSON")
}

/// Create human-readable output string.
pub fn render_human_result(result: &DeterminismResult, max_diffs: usize) -> String {
    let mut output = String::new();

    writeln!(&mut output, "Determinism mode: {:?}", result.mode).expect("infallible");
    writeln!(&mut output, "Round 1 hash: {}", truncate_hash(&result.round1_hash)).expect("infallible");
    writeln!(&mut output, "Round 2 hash: {}", truncate_hash(&result.round2_hash)).expect("infallible");

    if result.is_deterministic {
        writeln!(&mut output, "\n✅ DETERMINISTIC").expect("infallible");
    } else {
        writeln!(&mut output, "\n❌ NON-DETERMINISTIC").expect("infallible");
    }

    if let Some(diffs) = &result.byte_differences {
        let count = diffs.len();
        let shown = diffs.iter().take(max_diffs);

        writeln!(&mut output, "\nByte differences: {count} total").expect("infallible");
        if count > 0 {
            output.push_str("\n  Offset  Round1  Round2\n");
            output.push_str("  ------  ------  ------\n");
            for diff in shown {
                let _ = writeln!(
                    &mut output,
                    "  0x{:04X}  0x{:02X}    0x{:02X}",
                    diff.offset, diff.round1_byte, diff.round2_byte
                );
            }

            if count > max_diffs {
                let _ = writeln!(
                    &mut output,
                    "\n  ... {} more differences not shown",
                    count - max_diffs
                );
            }
        }
    } else {
        output.push_str("\nByte differences: none");
    }

    output
}

/// Build `DecodeOptions` from shared arguments.
pub fn build_decode_options(common: &CommonDeterminismArgs) -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(common.codepage)
        .with_format(common.format)
        .with_json_number_mode(common.json_number)
        .with_emit_meta(common.emit_meta)
}

/// Build `EncodeOptions` from shared arguments.
pub fn build_encode_options(common: &CommonDeterminismArgs) -> EncodeOptions {
    EncodeOptions::new().with_codepage(common.codepage)
}

/// Load and parse schema from a file or stdin.
pub fn load_schema(path: &Path) -> anyhow::Result<Schema> {
    let text = read_text_or_stdin(path)?;
    let schema = parse_copybook(&text).with_context(|| format!("Failed to parse copybook: {}", path.display()))?;
    Ok(schema)
}

/// Truncate BLAKE3 hash for human output.
pub fn truncate_hash(hash: &str) -> String {
    if hash.len() > 16 {
        format!("{}...", &hash[..16])
    } else {
        hash.to_string()
    }
}

fn read_text_or_stdin(path: &Path) -> anyhow::Result<String> {
    if path.as_os_str() == "-" {
        let mut text = String::new();
        io::stdin().read_to_string(&mut text)?;
        return Ok(text);
    }

    let text = fs::read_to_string(path)?;
    Ok(text)
}

fn read_bytes_or_stdin(path: &Path) -> anyhow::Result<Vec<u8>> {
    if path.as_os_str() == "-" {
        let mut data = Vec::new();
        io::stdin().read_to_end(&mut data)?;
        return Ok(data);
    }

    Ok(fs::read(path)?)
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use copybook_determinism::{ByteDiff, DeterminismMode};
    use proptest::prelude::*;

    #[test]
    fn hash_truncation_works() {
        let long_hash = "7a3f9e2b1c4d5e6f7a3f9e2b1c4d5e6f";
        assert_eq!(truncate_hash(long_hash), "7a3f9e2b1c4d5e6f...");

        let short_hash = "7a3f9e2b";
        assert_eq!(truncate_hash(short_hash), "7a3f9e2b");
    }

    #[test]
    fn human_result_includes_diff_metadata() {
        let result = DeterminismResult {
            mode: DeterminismMode::DecodeOnly,
            round1_hash: "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef12".to_string(),
            round2_hash: "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdeff1".to_string(),
            is_deterministic: false,
            byte_differences: Some(vec![
                ByteDiff {
                    offset: 16,
                    round1_byte: 0xAA,
                    round2_byte: 0x55,
                },
                ByteDiff {
                    offset: 17,
                    round1_byte: 0xBB,
                    round2_byte: 0x44,
                },
            ]),
        };
        let output = render_human_result(&result, 1);
        assert!(output.contains("Determinism mode:"));
        assert!(output.contains("Byte differences: 2 total"));
        assert!(output.contains("0x0010"));
        assert!(output.contains("... 1 more differences not shown"));
    }

    proptest! {
        #[test]
        fn prop_hash_truncation_is_prefix_plus_ellipsis(bytes in prop::collection::vec(any::<u8>(), 0..128)) {
            let mut raw = String::with_capacity(bytes.len());
            for byte in bytes {
                let digit = byte % 16;
                let ch = if digit < 10 {
                    (b'0' + digit) as char
                } else {
                    (b'a' + (digit - 10)) as char
                };
                raw.push(ch);
            }

            let rendered = truncate_hash(&raw);
            if raw.len() <= 16 {
                prop_assert_eq!(rendered, raw);
            } else {
                prop_assert_eq!(&rendered[16..], "...");
                prop_assert_eq!(rendered.len(), 19);
                prop_assert_eq!(&rendered[..16], &raw[..16]);
            }
        }

        #[test]
        fn prop_json_output_is_parseable(
            hash_a in prop::collection::vec(any::<u8>(), 0..64),
            hash_b in prop::collection::vec(any::<u8>(), 0..64),
            deterministic in any::<bool>(),
        ) {
            let make_hash = |bytes: &[u8]| {
                let mut out = String::with_capacity(bytes.len() * 2);
                for byte in bytes {
                    out.push_str(&format!("{byte:02x}"));
                }
                out
            };

            let result = DeterminismResult {
                mode: DeterminismMode::RoundTrip,
                round1_hash: make_hash(&hash_a),
                round2_hash: if deterministic {
                    make_hash(&hash_a)
                } else {
                    make_hash(&hash_b)
                },
                is_deterministic: deterministic,
                byte_differences: if deterministic {
                    None
                } else {
                    Some(vec![ByteDiff {
                        offset: 1,
                        round1_byte: 1,
                        round2_byte: 2,
                    }])
                },
            };
            let json = render_json_result(&result).expect("json output");
            let de = serde_json::from_str::<DeterminismResult>(&json).expect("round-trip decode");
            assert_eq!(de.mode, result.mode);
            assert_eq!(de.is_deterministic, result.is_deterministic);
        }
    }
}
