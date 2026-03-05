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
        DeterminismResult, check_decode_determinism, check_encode_determinism,
        check_round_trip_determinism,
    },
};
use copybook_core::{Schema, parse_copybook};
use std::fmt::Write as _;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

pub use copybook_codec::determinism::DeterminismMode as DeterminismCheckMode;
pub use copybook_determinism::{BLAKE3_HEX_LEN, DEFAULT_MAX_DIFFS};

/// Determinism command façade for CLI surface.
#[derive(Args, Debug, Clone)]
pub struct DeterminismCommand {
    /// The determinism check mode (decode, encode, or round-trip).
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
#[derive(ValueEnum, Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    /// Human-readable output with symbols and diff table.
    Human,
    /// Structured JSON output for CI integration.
    Json,
}

/// Decode command arguments.
#[derive(Args, Debug, Clone)]
pub struct DecodeDeterminismArgs {
    /// Shared determinism arguments.
    #[command(flatten)]
    pub common: CommonDeterminismArgs,

    /// Binary input file (single record or sampled payload).
    #[arg(value_name = "DATA")]
    pub data: PathBuf,
}

/// Encode command arguments.
#[derive(Args, Debug, Clone)]
pub struct EncodeDeterminismArgs {
    /// Shared determinism arguments.
    #[command(flatten)]
    pub common: CommonDeterminismArgs,

    /// JSON input file (first line is used).
    #[arg(value_name = "JSON")]
    pub json: PathBuf,
}

/// Round-trip command arguments.
#[derive(Args, Debug, Clone)]
pub struct RoundTripDeterminismArgs {
    /// Shared determinism arguments.
    #[command(flatten)]
    pub common: CommonDeterminismArgs,

    /// Binary input file.
    #[arg(value_name = "DATA")]
    pub data: PathBuf,
}

/// Result of running a determinism command execution.
#[derive(Debug, Clone)]
pub struct DeterminismRun {
    /// The determinism verdict (pass or fail).
    pub verdict: DeterminismVerdict,
    /// Formatted output text for display.
    pub output: String,
}

impl DeterminismRun {
    /// Convert command verdict to CLI exit code semantics.
    #[inline]
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
    #[inline]
    #[must_use]
    pub const fn exit_code(&self) -> i32 {
        match self {
            Self::Deterministic => 0,
            Self::NonDeterministic => 2,
        }
    }
}

/// Execute a determinism subcommand and return output plus verdict.
///
/// # Errors
///
/// Returns an error if schema loading, data reading, or determinism checks fail.
#[inline]
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
    let value: serde_json::Value =
        serde_json::from_str(first_line).context("Failed to parse JSON input")?;

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
        OutputFormat::Human => Ok(render_human_result(result, common.max_diffs)),
    }?;

    Ok(DeterminismRun {
        verdict: DeterminismVerdict::from_result(result),
        output,
    })
}

/// Create JSON formatted output string.
///
/// # Errors
///
/// Returns an error if JSON serialization fails.
#[inline]
pub fn render_json_result(result: &DeterminismResult) -> anyhow::Result<String> {
    serde_json::to_string_pretty(result).context("Failed to serialize determinism result to JSON")
}

/// Create human-readable output string.
#[inline]
#[must_use]
pub fn render_human_result(result: &DeterminismResult, max_diffs: usize) -> String {
    let mut output = String::new();

    let _ = writeln!(&mut output, "Determinism mode: {:?}", result.mode);
    let _ = writeln!(
        &mut output,
        "Round 1 hash: {}",
        truncate_hash(&result.round1_hash)
    );
    let _ = writeln!(
        &mut output,
        "Round 2 hash: {}",
        truncate_hash(&result.round2_hash)
    );

    if result.is_deterministic {
        let _ = writeln!(&mut output, "\n✅ DETERMINISTIC");
    } else {
        let _ = writeln!(&mut output, "\n❌ NON-DETERMINISTIC");
    }

    if let Some(diffs) = &result.byte_differences {
        let count = diffs.len();
        let shown = diffs.iter().take(max_diffs);

        let _ = writeln!(&mut output, "\nByte differences: {count} total");
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
#[inline]
#[must_use]
pub fn build_decode_options(common: &CommonDeterminismArgs) -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(common.codepage)
        .with_format(common.format)
        .with_json_number_mode(common.json_number)
        .with_emit_meta(common.emit_meta)
}

/// Build `EncodeOptions` from shared arguments.
#[inline]
#[must_use]
pub fn build_encode_options(common: &CommonDeterminismArgs) -> EncodeOptions {
    EncodeOptions::new().with_codepage(common.codepage)
}

/// Load and parse schema from a file or stdin.
///
/// # Errors
///
/// Returns an error if the file cannot be read or parsed.
#[inline]
pub fn load_schema(path: &Path) -> anyhow::Result<Schema> {
    let text = read_text_or_stdin(path)?;
    let schema = parse_copybook(&text)
        .with_context(|| format!("Failed to parse copybook: {}", path.display()))?;
    Ok(schema)
}

/// Truncate BLAKE3 hash for human output.
#[inline]
#[must_use]
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
    use copybook_determinism::ByteDiff;
    use copybook_determinism::DeterminismMode as CodecDeterminismMode;
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
            mode: CodecDeterminismMode::DecodeOnly,
            round1_hash: "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef12"
                .to_string(),
            round2_hash: "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdeff1"
                .to_string(),
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

    #[test]
    fn human_result_deterministic_shows_checkmark() {
        let result = DeterminismResult {
            mode: CodecDeterminismMode::DecodeOnly,
            round1_hash: "a".repeat(64),
            round2_hash: "a".repeat(64),
            is_deterministic: true,
            byte_differences: None,
        };
        let output = render_human_result(&result, 100);
        assert!(output.contains("✅ DETERMINISTIC"));
        assert!(!output.contains("NON-DETERMINISTIC"));
        assert!(output.contains("Byte differences: none"));
    }

    #[test]
    fn human_result_non_deterministic_shows_cross() {
        let result = DeterminismResult {
            mode: CodecDeterminismMode::EncodeOnly,
            round1_hash: "a".repeat(64),
            round2_hash: "b".repeat(64),
            is_deterministic: false,
            byte_differences: Some(vec![ByteDiff {
                offset: 0,
                round1_byte: 0x41,
                round2_byte: 0x42,
            }]),
        };
        let output = render_human_result(&result, 100);
        assert!(output.contains("❌ NON-DETERMINISTIC"));
        assert!(output.contains("Byte differences: 1 total"));
        assert!(output.contains("0x0000"));
        assert!(output.contains("0x41"));
        assert!(output.contains("0x42"));
    }

    #[test]
    fn render_json_deterministic_round_trips_correctly() {
        let result = DeterminismResult {
            mode: CodecDeterminismMode::RoundTrip,
            round1_hash: "c".repeat(64),
            round2_hash: "c".repeat(64),
            is_deterministic: true,
            byte_differences: None,
        };
        let json = render_json_result(&result).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed["is_deterministic"], true);
        assert_eq!(parsed["mode"], "round_trip");
        assert!(parsed.get("byte_differences").is_none());
    }

    #[test]
    fn render_json_non_deterministic_includes_diffs() {
        let result = DeterminismResult {
            mode: CodecDeterminismMode::DecodeOnly,
            round1_hash: "d".repeat(64),
            round2_hash: "e".repeat(64),
            is_deterministic: false,
            byte_differences: Some(vec![ByteDiff {
                offset: 5,
                round1_byte: 0x10,
                round2_byte: 0x20,
            }]),
        };
        let json = render_json_result(&result).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed["is_deterministic"], false);
        let diffs = parsed["byte_differences"].as_array().unwrap();
        assert_eq!(diffs.len(), 1);
        assert_eq!(diffs[0]["offset"], 5);
    }

    #[test]
    fn verdict_exit_codes_are_stable() {
        assert_eq!(DeterminismVerdict::Deterministic.exit_code(), 0);
        assert_eq!(DeterminismVerdict::NonDeterministic.exit_code(), 2);
    }

    #[test]
    fn determinism_run_exit_code_delegates_to_verdict() {
        let run_pass = DeterminismRun {
            verdict: DeterminismVerdict::Deterministic,
            output: String::new(),
        };
        assert_eq!(run_pass.exit_code(), 0);

        let run_fail = DeterminismRun {
            verdict: DeterminismVerdict::NonDeterministic,
            output: String::new(),
        };
        assert_eq!(run_fail.exit_code(), 2);
    }

    #[test]
    fn truncate_hash_exactly_16_chars() {
        let hash = "0123456789abcdef";
        assert_eq!(truncate_hash(hash), "0123456789abcdef");
    }

    #[test]
    fn truncate_hash_empty_string() {
        assert_eq!(truncate_hash(""), "");
    }

    #[test]
    fn human_result_shows_mode_name() {
        for mode in [
            CodecDeterminismMode::DecodeOnly,
            CodecDeterminismMode::EncodeOnly,
            CodecDeterminismMode::RoundTrip,
        ] {
            let result = DeterminismResult {
                mode,
                round1_hash: "f".repeat(64),
                round2_hash: "f".repeat(64),
                is_deterministic: true,
                byte_differences: None,
            };
            let output = render_human_result(&result, 100);
            assert!(output.contains("Determinism mode:"));
            assert!(output.contains("Round 1 hash:"));
            assert!(output.contains("Round 2 hash:"));
        }
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
                mode: CodecDeterminismMode::RoundTrip,
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
