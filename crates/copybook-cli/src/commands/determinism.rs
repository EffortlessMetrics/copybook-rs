#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Determinism command execution for copybook CLI workflows.
//!
//! This module focuses on one responsibility:
//! parsing CLI command arguments and producing formatted determinism output
//! for decode/encode/round-trip checks.

use crate::exit_codes::ExitCode;
use crate::write_stdout_all;
use anyhow::Context;
use clap::{Args, Subcommand, ValueEnum};
use copybook::codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat,
    determinism::{
        DeterminismResult, blake3_hex, check_decode_determinism, check_encode_determinism,
        check_round_trip_determinism,
    },
};
use copybook::core::{FeatureFlags, ParseOptions, Schema, parse_copybook_with_feature_flags};
use std::fmt::Write as _;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

/// Maximum number of byte differences shown by default.
pub const DEFAULT_MAX_DIFFS: usize = 100;

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

/// Convenience alias retained for command wiring and test readability.
pub type DeterminismModeCommand = DeterminismMode;

/// Shared determinism arguments.
///
/// `format`, `codepage`, and `json_number` are optional so a reviewed
/// `--profile` can supply them through the same resolution layers as
/// `decode`/`encode`: explicit flag, then profile, then ambient
/// environment, then product default. A flag that disagrees with the
/// profile is a contradiction, not an override.
#[derive(Args, Debug, Clone)]
pub struct CommonDeterminismArgs {
    /// Copybook schema file.
    #[arg(value_name = "COPYBOOK")]
    pub copybook: PathBuf,

    /// Record format.
    #[arg(long)]
    pub format: Option<RecordFormat>,

    /// EBCDIC codepage.
    #[arg(long)]
    pub codepage: Option<Codepage>,

    /// JSON number handling mode.
    #[arg(long, value_name = "MODE")]
    pub json_number: Option<JsonNumberMode>,

    /// Reviewed interpretation profile (TOML) supplying framing, decode
    /// and encode policy, dialect, and the report identity.
    #[arg(long, value_name = "FILE")]
    pub profile: Option<PathBuf>,

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

/// Profile-resolved inputs for one determinism comparison.
///
/// Built by dispatch through the same resolution the operating commands
/// use, so the comparison runs exactly what `decode`/`encode` would run.
/// `profile_fingerprint` is `None` for direct (profile-less) runs, which
/// the report renders explicitly instead of implying an identity.
#[derive(Debug, Clone)]
pub struct DeterminismInputs {
    /// Effective record format.
    pub format: RecordFormat,
    /// Effective codepage.
    pub codepage: Codepage,
    /// Effective ODO dialect.
    pub dialect: copybook::core::dialect::Dialect,
    /// Effective JSON number mode.
    pub json_number: JsonNumberMode,
    /// Effective decode-side unmappable policy.
    pub decode_unmappable: copybook::codec::UnmappablePolicy,
    /// Effective encode-side unmappable policy.
    pub encode_unmappable: copybook::codec::UnmappablePolicy,
    /// Canonical fingerprint of the bound profile, if any.
    pub profile_fingerprint: Option<String>,
}

/// Available output rendering modes.
///
/// Human-readable output or structured JSON output.
#[derive(ValueEnum, Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    /// Human-readable output with symbols and diff table.
    Human,
    /// Structured JSON output for CI integration.
    Json,
}

/// Profile identity recorded in a determinism report.
///
/// A comparison either runs under a reviewed profile (identified by the
/// profile's canonical SHA-256 fingerprint) or directly from flags and
/// product defaults. The report names which one so a verdict can never be
/// mistaken for a differently-configured run.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ReportProfile {
    /// Direct run: no reviewed profile was bound.
    Direct,
    /// Profile-bound run: canonical fingerprint of the bound profile.
    Profile {
        /// Lowercase hex SHA-256 over the profile's canonical bytes.
        fingerprint: String,
    },
}

/// Identity envelope for one determinism comparison (`--output json`).
///
/// The codec-level [`DeterminismResult`] carries the output hashes and the
/// verdict; this envelope binds them to the comparison kind that produced
/// them, the profile (or explicit direct) identity, the compared input
/// bytes, and the evidence the comparison cannot supply.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub struct DeterminismReport {
    /// Compared operation: `decode`, `encode`, or `round-trip`.
    pub comparison: String,
    /// Profile identity for the run.
    pub profile: ReportProfile,
    /// BLAKE3 hash (lowercase hex) of the compared input bytes.
    pub input_hash: String,
    /// Evidence the comparison cannot supply, stated explicitly.
    pub limitations: Vec<String>,
    /// Codec comparison outcome.
    pub result: DeterminismResult,
}

/// Resolved manifest evidence is not emitted for determinism comparisons:
/// the check binds the profile fingerprint directly instead of routing
/// through a manifest document.
const LIMITATION_NO_MANIFEST: &str = "resolved manifest is not emitted for determinism comparisons; profile identity is the fingerprint above";

/// Determinism compares one record at a time, so no worker scheduling
/// exists: worker count cannot change record ordering or the verdict.
const LIMITATION_SINGLE_RECORD_WORKERS: &str = "single-record comparison performs no worker scheduling; worker count cannot change ordering or verdict";

/// A round-trip check is internal self-consistency (decode then encode then
/// decode again), not an independent external oracle for either direction.
const LIMITATION_INTERNAL_ROUND_TRIP: &str =
    "round-trip is internal self-consistency, not an independent external oracle";

/// Shared arguments for the selected determinism comparison mode.
///
/// Dispatch resolves the profile through these flags, so the comparison
/// runs exactly what `decode`/`encode` would run.
#[inline]
#[must_use]
pub fn common_args(cmd: &DeterminismCommand) -> &CommonDeterminismArgs {
    match &cmd.mode {
        DeterminismModeCommand::Decode(args) => &args.common,
        DeterminismModeCommand::Encode(args) => &args.common,
        DeterminismModeCommand::RoundTrip(args) => &args.common,
    }
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
    #[cfg(test)]
    #[inline]
    #[must_use]
    pub const fn exit_code(&self) -> i32 {
        self.verdict.exit_code()
    }
}

/// Executable verdict from determinism checks.
///
/// The CLI maps deterministic output to exit code 0 and detected drift to 2.
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
    #[cfg(test)]
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
/// `inputs` carries the dispatch-resolved comparison options, so the
/// check runs exactly what the operating commands would run.
///
/// # Errors
///
/// Returns an error if schema loading, data reading, or determinism checks fail.
#[inline]
pub fn run_check(
    cmd: &DeterminismCommand,
    inputs: &DeterminismInputs,
    feature_flags: &FeatureFlags,
) -> anyhow::Result<DeterminismRun> {
    let result = match &cmd.mode {
        DeterminismModeCommand::Decode(args) => run_decode(args, inputs, feature_flags),
        DeterminismModeCommand::Encode(args) => run_encode(args, inputs, feature_flags),
        DeterminismModeCommand::RoundTrip(args) => run_round_trip(args, inputs, feature_flags),
    }?;

    Ok(result)
}

/// Determinism validation for encode/decode operations.
pub fn run(
    cmd: &DeterminismCommand,
    inputs: &DeterminismInputs,
    feature_flags: &FeatureFlags,
) -> anyhow::Result<ExitCode> {
    let result =
        run_check(cmd, inputs, feature_flags).context("Determinism command execution failed")?;
    write_stdout_all(result.output.as_bytes())?;

    let exit_code = match result.verdict {
        DeterminismVerdict::Deterministic => ExitCode::Ok,
        DeterminismVerdict::NonDeterministic => ExitCode::Data,
    };
    Ok(exit_code)
}

/// Run decode determinism check.
fn run_decode(
    args: &DecodeDeterminismArgs,
    inputs: &DeterminismInputs,
    feature_flags: &FeatureFlags,
) -> anyhow::Result<DeterminismRun> {
    let schema = load_schema(&args.common.copybook, inputs, feature_flags)?;
    let decode_opts = build_decode_options(&args.common, inputs);
    let data = read_bytes_or_stdin(&args.data).with_context(|| {
        format!(
            "Failed to read data file for determinism check: {}",
            args.data.display()
        )
    })?;

    let result = check_decode_determinism(&schema, &data, &decode_opts)
        .context("Decode determinism check failed")?;

    render_result(&result, &args.common, "decode", &data, inputs)
}

/// Run encode determinism check.
fn run_encode(
    args: &EncodeDeterminismArgs,
    inputs: &DeterminismInputs,
    feature_flags: &FeatureFlags,
) -> anyhow::Result<DeterminismRun> {
    let schema = load_schema(&args.common.copybook, inputs, feature_flags)?;
    let encode_opts = build_encode_options(inputs);
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

    render_result(
        &result,
        &args.common,
        "encode",
        first_line.as_bytes(),
        inputs,
    )
}

/// Run round-trip determinism check.
fn run_round_trip(
    args: &RoundTripDeterminismArgs,
    inputs: &DeterminismInputs,
    feature_flags: &FeatureFlags,
) -> anyhow::Result<DeterminismRun> {
    let schema = load_schema(&args.common.copybook, inputs, feature_flags)?;
    let decode_opts = build_decode_options(&args.common, inputs);
    let encode_opts = build_encode_options(inputs);
    let data = read_bytes_or_stdin(&args.data).with_context(|| {
        format!(
            "Failed to read data file for round-trip determinism check: {}",
            args.data.display()
        )
    })?;

    let result = check_round_trip_determinism(&schema, &data, &decode_opts, &encode_opts)
        .context("Round-trip determinism check failed")?;

    render_result(&result, &args.common, "round-trip", &data, inputs)
}

/// Common renderer for result + status.
///
/// `comparison` names the compared operation (`decode`, `encode`, or
/// `round-trip`), `data` holds the compared input bytes, and `inputs`
/// carries the dispatch-resolved profile identity.
fn render_result(
    result: &DeterminismResult,
    common: &CommonDeterminismArgs,
    comparison: &str,
    data: &[u8],
    inputs: &DeterminismInputs,
) -> anyhow::Result<DeterminismRun> {
    let output = match common.output {
        OutputFormat::Json => render_json_report(comparison, data, inputs, result),
        OutputFormat::Human => Ok(render_human_report(
            comparison,
            data,
            inputs,
            result,
            common.max_diffs,
        )),
    }?;

    Ok(DeterminismRun {
        verdict: DeterminismVerdict::from_result(result),
        output,
    })
}

/// Profile identity for a report: the bound profile's fingerprint, or an
/// explicit direct marker when no profile was bound.
#[inline]
#[must_use]
pub fn report_profile(inputs: &DeterminismInputs) -> ReportProfile {
    match &inputs.profile_fingerprint {
        Some(fingerprint) => ReportProfile::Profile {
            fingerprint: fingerprint.clone(),
        },
        None => ReportProfile::Direct,
    }
}

/// Evidence the named comparison cannot supply, stated explicitly instead
/// of left for the reader to guess.
#[inline]
#[must_use]
pub fn report_limitations(comparison: &str) -> Vec<String> {
    let mut limitations = vec![
        LIMITATION_NO_MANIFEST.to_string(),
        LIMITATION_SINGLE_RECORD_WORKERS.to_string(),
    ];
    if comparison == "round-trip" {
        limitations.push(LIMITATION_INTERNAL_ROUND_TRIP.to_string());
    }
    limitations
}

/// Create the JSON identity envelope for one comparison.
///
/// # Errors
///
/// Returns an error if JSON serialization fails.
#[inline]
pub fn render_json_report(
    comparison: &str,
    data: &[u8],
    inputs: &DeterminismInputs,
    result: &DeterminismResult,
) -> anyhow::Result<String> {
    let report = DeterminismReport {
        comparison: comparison.to_string(),
        profile: report_profile(inputs),
        input_hash: blake3_hex(data),
        limitations: report_limitations(comparison),
        result: result.clone(),
    };
    serde_json::to_string_pretty(&report).context("Failed to serialize determinism report to JSON")
}

/// Create human-readable output with the comparison identity header and
/// the evidence limitations the comparison cannot supply.
#[inline]
#[must_use]
pub fn render_human_report(
    comparison: &str,
    data: &[u8],
    inputs: &DeterminismInputs,
    result: &DeterminismResult,
    max_diffs: usize,
) -> String {
    let mut output = String::new();
    let _ = writeln!(&mut output, "Comparison: {comparison}");
    match report_profile(inputs) {
        ReportProfile::Direct => {
            let _ = writeln!(&mut output, "Profile: direct (no reviewed profile)");
        }
        ReportProfile::Profile { fingerprint } => {
            let _ = writeln!(&mut output, "Profile: sha256:{fingerprint}");
        }
    }
    let _ = writeln!(&mut output, "Input hash: {}", blake3_hex(data));
    output.push_str(&render_human_result(result, max_diffs));
    if !output.ends_with('\n') {
        output.push('\n');
    }
    output.push_str("\nLimitations:\n");
    for limitation in report_limitations(comparison) {
        let _ = writeln!(&mut output, "  - {limitation}");
    }
    output
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

/// Build `DecodeOptions` from the dispatch-resolved comparison inputs.
///
/// Framing, codepage, JSON numbers, and the decode-side unmappable policy
/// come from the profile resolution in `inputs` (exactly what `decode`
/// would run); `emit_meta` stays a direct presentation-only flag.
#[inline]
#[must_use]
pub fn build_decode_options(
    common: &CommonDeterminismArgs,
    inputs: &DeterminismInputs,
) -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(inputs.codepage)
        .with_format(inputs.format)
        .with_json_number_mode(inputs.json_number)
        .with_emit_meta(common.emit_meta)
        .with_unmappable_policy(inputs.decode_unmappable)
}

/// Build `EncodeOptions` from the dispatch-resolved comparison inputs.
///
/// The write-side unmappable policy comes from the profile resolution in
/// `inputs` (exactly what `encode` would run).
#[inline]
#[must_use]
pub fn build_encode_options(inputs: &DeterminismInputs) -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(inputs.codepage)
        .with_format(inputs.format)
        .with_json_number_mode(inputs.json_number)
        .with_unmappable_policy(inputs.encode_unmappable)
}

/// Load and parse schema from a file or stdin under the resolved dialect.
///
/// # Errors
///
/// Returns an error if the file cannot be read or parsed.
#[inline]
pub fn load_schema(
    path: &Path,
    inputs: &DeterminismInputs,
    feature_flags: &FeatureFlags,
) -> anyhow::Result<Schema> {
    let text = read_text_or_stdin(path)?;
    // #656 Phase D: CLI-resolved flags passed explicitly; no global state.
    let schema = parse_copybook_with_feature_flags(
        &text,
        &ParseOptions::default().with_dialect(inputs.dialect),
        feature_flags,
    )
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
    use copybook::codec::determinism::{ByteDiff, DeterminismMode as CodecDeterminismMode};
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
        let json = render_json_report("round-trip", b"x", &test_inputs(None), &result).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed["result"]["is_deterministic"], true);
        assert_eq!(parsed["result"]["mode"], "round_trip");
        assert!(parsed["result"].get("byte_differences").is_none());
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
        let json = render_json_report("decode", b"x", &test_inputs(None), &result).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed["result"]["is_deterministic"], false);
        let diffs = parsed["result"]["byte_differences"].as_array().unwrap();
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

    fn test_inputs(fingerprint: Option<&str>) -> DeterminismInputs {
        DeterminismInputs {
            format: RecordFormat::Fixed,
            codepage: Codepage::CP037,
            dialect: copybook::core::dialect::Dialect::Normative,
            json_number: JsonNumberMode::Lossless,
            decode_unmappable: copybook::codec::UnmappablePolicy::Error,
            encode_unmappable: copybook::codec::UnmappablePolicy::Replace,
            profile_fingerprint: fingerprint.map(str::to_string),
        }
    }

    fn test_common() -> CommonDeterminismArgs {
        CommonDeterminismArgs {
            copybook: PathBuf::from("test.cpy"),
            // Flags that disagree with `test_inputs` on purpose: the
            // resolved inputs must win so the comparison runs exactly what
            // the operating commands would run.
            format: Some(RecordFormat::RDW),
            codepage: Some(Codepage::CP500),
            json_number: Some(JsonNumberMode::Native),
            profile: None,
            emit_meta: true,
            output: OutputFormat::Human,
            max_diffs: DEFAULT_MAX_DIFFS,
        }
    }

    fn test_result() -> DeterminismResult {
        DeterminismResult {
            mode: CodecDeterminismMode::DecodeOnly,
            round1_hash: "a".repeat(64),
            round2_hash: "a".repeat(64),
            is_deterministic: true,
            byte_differences: None,
        }
    }

    #[test]
    fn decode_options_consume_resolved_inputs_not_flags() {
        let options = build_decode_options(&test_common(), &test_inputs(None));
        assert_eq!(options.format, RecordFormat::Fixed);
        assert_eq!(options.codepage, Codepage::CP037);
        assert_eq!(options.json_number_mode, JsonNumberMode::Lossless);
        assert_eq!(
            options.on_decode_unmappable,
            copybook::codec::UnmappablePolicy::Error
        );
        assert!(options.emit_meta);
    }

    #[test]
    fn encode_options_consume_resolved_inputs() {
        let options = build_encode_options(&test_inputs(None));
        assert_eq!(options.format, RecordFormat::Fixed);
        assert_eq!(options.codepage, Codepage::CP037);
        assert_eq!(options.json_number_mode, JsonNumberMode::Lossless);
        assert_eq!(
            options.on_encode_unmappable,
            copybook::codec::UnmappablePolicy::Replace
        );
    }

    #[test]
    fn json_report_binds_comparison_and_profile_identities() {
        let data = b"input-bytes";
        let json =
            render_json_report("decode", data, &test_inputs(Some("fp")), &test_result()).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed["comparison"], "decode");
        assert_eq!(parsed["profile"]["kind"], "profile");
        assert_eq!(parsed["profile"]["fingerprint"], "fp");
        assert_eq!(parsed["input_hash"], blake3_hex(data));
        assert_eq!(parsed["result"]["is_deterministic"], true);
        let limitations = parsed["limitations"].as_array().unwrap();
        assert_eq!(limitations.len(), 2);
    }

    #[test]
    fn json_report_direct_run_names_direct_identity() {
        let json = render_json_report("encode", b"x", &test_inputs(None), &test_result()).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed["profile"]["kind"], "direct");
        assert!(parsed["profile"].get("fingerprint").is_none());
    }

    #[test]
    fn json_report_round_trip_states_internal_oracle_limitation() {
        let json =
            render_json_report("round-trip", b"x", &test_inputs(None), &test_result()).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
        let limitations = parsed["limitations"].as_array().unwrap();
        assert_eq!(limitations.len(), 3);
        assert!(limitations.iter().any(|limitation| {
            limitation
                .as_str()
                .unwrap_or_default()
                .contains("worker count")
        }));
        assert!(limitations.iter().any(|limitation| {
            limitation
                .as_str()
                .unwrap_or_default()
                .contains("not an independent external oracle")
        }));
    }

    #[test]
    fn human_report_shows_identity_header_and_limitations() {
        let output = render_human_report("encode", b"x", &test_inputs(None), &test_result(), 100);
        assert!(output.contains("Comparison: encode"));
        assert!(output.contains("Profile: direct (no reviewed profile)"));
        assert!(output.contains("Input hash: "));
        assert!(output.contains("Limitations:"));
        assert!(output.contains("worker count cannot change ordering or verdict"));

        let profiled = render_human_report(
            "decode",
            b"x",
            &test_inputs(Some("fp")),
            &test_result(),
            100,
        );
        assert!(profiled.contains("Comparison: decode"));
        assert!(profiled.contains("Profile: sha256:fp"));
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
            let json = render_json_report("round-trip", b"input", &test_inputs(None), &result)
                .expect("json output");
            let parsed: serde_json::Value =
                serde_json::from_str(&json).expect("report parses");
            prop_assert_eq!(parsed["comparison"].as_str(), Some("round-trip"));
            prop_assert_eq!(parsed["result"]["mode"].as_str(), Some("round_trip"));
            prop_assert_eq!(
                parsed["result"]["is_deterministic"].as_bool(),
                Some(result.is_deterministic)
            );
        }
    }
}
