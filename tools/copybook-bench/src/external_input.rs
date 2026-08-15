// SPDX-License-Identifier: AGPL-3.0-or-later
//! Offline manifest validation for future external-input benchmarks.

use std::fmt;
use std::fs;
use std::io::Write;
use std::ops::Range;
use std::path::{Component, Path, PathBuf};
use std::time::{Duration, Instant};

use anyhow::{Context, Result, bail, ensure};
use copybook_codec::{
    Codepage, DecodeOptions, RecordFormat, decode_record_with_scratch, memory::ScratchBuffers,
};
use copybook_core::Schema;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// The only manifest schema version understood by this loader.
pub const EXTERNAL_INPUT_SCHEMA_VERSION: &str = "1.0.0";

/// Record framing declared by an external-input manifest.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum ExternalRecordFormat {
    /// Fixed-length payload records with no framing bytes.
    Fixed,
    /// Four-byte RDW header followed by the payload.
    Rdw,
}

impl From<ExternalRecordFormat> for RecordFormat {
    fn from(value: ExternalRecordFormat) -> Self {
        match value {
            ExternalRecordFormat::Fixed => Self::Fixed,
            ExternalRecordFormat::Rdw => Self::RDW,
        }
    }
}

/// Code pages accepted by the deterministic dataset generator.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum ExternalCodepage {
    /// Seven-bit ASCII.
    #[serde(rename = "ascii")]
    Ascii,
    /// IBM EBCDIC code page 037.
    #[serde(rename = "cp037")]
    Cp037,
    /// IBM EBCDIC code page 273.
    #[serde(rename = "cp273")]
    Cp273,
    /// IBM EBCDIC code page 500.
    #[serde(rename = "cp500")]
    Cp500,
    /// IBM EBCDIC code page 1047.
    #[serde(rename = "cp1047")]
    Cp1047,
    /// IBM EBCDIC code page 1140.
    #[serde(rename = "cp1140")]
    Cp1140,
}

impl From<ExternalCodepage> for Codepage {
    fn from(value: ExternalCodepage) -> Self {
        match value {
            ExternalCodepage::Ascii => Self::ASCII,
            ExternalCodepage::Cp037 => Self::CP037,
            ExternalCodepage::Cp273 => Self::CP273,
            ExternalCodepage::Cp500 => Self::CP500,
            ExternalCodepage::Cp1047 => Self::CP1047,
            ExternalCodepage::Cp1140 => Self::CP1140,
        }
    }
}

/// Workload labels already supported by `scripts/gen_dataset.sh`.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum ExternalWorkload {
    /// Mostly DISPLAY fields.
    DisplayHeavy,
    /// Mostly packed-decimal fields.
    Comp3Heavy,
    /// Mixed DISPLAY, zoned, and packed-decimal fields.
    Mixed,
}

/// Closed metadata contract for one external benchmark dataset.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct ExternalInputManifest {
    /// Manifest schema version.
    pub schema_version: String,
    /// Copybook path relative to the manifest.
    pub copybook: PathBuf,
    /// SHA-256 of the exact UTF-8 bytes passed to the copybook parser.
    #[serde(default)]
    pub copybook_sha256: String,
    /// Dataset path relative to the manifest.
    pub dataset: PathBuf,
    /// Dataset record framing.
    pub record_format: ExternalRecordFormat,
    /// Dataset character code page.
    pub codepage: ExternalCodepage,
    /// Generator workload label.
    pub workload: ExternalWorkload,
    /// Payload bytes in every record.
    pub record_length: usize,
    /// Number of records in the dataset.
    pub record_count: usize,
    /// Lowercase SHA-256 digest of the complete dataset file.
    pub dataset_sha256: String,
}

/// Manifest artifact whose declared integrity failed validation.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IntegrityArtifact {
    /// Copybook source bytes.
    Copybook,
    /// Dataset bytes.
    Dataset,
}

impl fmt::Display for IntegrityArtifact {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Copybook => formatter.write_str("copybook"),
            Self::Dataset => formatter.write_str("dataset"),
        }
    }
}

/// Typed manifest-integrity failure for missing, malformed, or mismatched digests.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ManifestIntegrityError {
    /// Artifact whose integrity declaration failed.
    pub artifact: IntegrityArtifact,
    /// Stable human-readable failure detail.
    pub detail: String,
}

impl fmt::Display for ManifestIntegrityError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "{} manifest integrity error: {}",
            self.artifact, self.detail
        )
    }
}

impl std::error::Error for ManifestIntegrityError {}

/// Input artifact that an external-input preflight output must not alias.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PreflightInputArtifact {
    /// Manifest JSON input.
    Manifest,
    /// Copybook source input.
    Copybook,
    /// Physical dataset input.
    Dataset,
}

impl fmt::Display for PreflightInputArtifact {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Manifest => formatter.write_str("manifest"),
            Self::Copybook => formatter.write_str("copybook"),
            Self::Dataset => formatter.write_str("dataset"),
        }
    }
}

/// Typed rejection for a preflight output path that aliases an input.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PreflightOutputAliasError {
    /// Input artifact aliased by the requested output.
    pub input: PreflightInputArtifact,
}

impl fmt::Display for PreflightOutputAliasError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "preflight output path must not alias the {} input",
            self.input
        )
    }
}

impl std::error::Error for PreflightOutputAliasError {}

/// Fully read and structurally validated external input.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ValidatedExternalInput {
    /// Parsed manifest.
    pub manifest: ExternalInputManifest,
    /// Parsed copybook source.
    pub copybook_source: String,
    /// Complete dataset bytes.
    pub dataset: Vec<u8>,
    /// Payload-only byte ranges within `dataset`, in record order.
    pub payload_ranges: Vec<Range<usize>>,
}

struct LoadedExternalInput {
    validated: ValidatedExternalInput,
    schema: Schema,
    manifest_sha256: String,
}

/// Prepared state for the opt-in local external-input Criterion target.
///
/// This is public only because Cargo compiles benchmark targets as separate
/// crates; `copybook-bench` itself is unpublished.
pub struct ExternalInputDecodeBenchmark {
    loaded: LoadedExternalInput,
    options: DecodeOptions,
    scratch: ScratchBuffers,
    payload_bytes: usize,
    benchmark_id: String,
}

impl ExternalInputDecodeBenchmark {
    /// Sum of validated payload bytes processed by one decode pass.
    #[must_use]
    pub const fn payload_bytes(&self) -> usize {
        self.payload_bytes
    }

    /// Stable Criterion identity for this validated manifest and dataset.
    #[must_use]
    pub fn benchmark_id(&self) -> &str {
        &self.benchmark_id
    }

    /// Decode every validated payload range exactly once.
    ///
    /// # Errors
    ///
    /// Returns contextual errors for an invalid range, decode failure, or
    /// checked record-count overflow.
    pub fn decode_pass(&mut self) -> Result<usize> {
        decode_loaded_pass(&self.loaded, &self.options, &mut self.scratch)
    }

    /// Time complete decode passes, returning no duration if any pass fails.
    ///
    /// # Errors
    ///
    /// Returns the first contextual decode failure. Callers must not record a
    /// Criterion sample unless this method returns `Ok`.
    pub fn measure_decode_iterations(&mut self, iterations: u64) -> Result<Duration> {
        let start = Instant::now();
        for iteration in 0..iterations {
            let _decoded = self
                .decode_pass()
                .with_context(|| format!("external-input decode iteration {iteration} failed"))?;
        }
        Ok(start.elapsed())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct ExternalInputPreflight {
    manifest_sha256: String,
    copybook_sha256: String,
    dataset_sha256: String,
    record_format: ExternalRecordFormat,
    codepage: ExternalCodepage,
    workload: ExternalWorkload,
    decoded_records: usize,
    physical_bytes: usize,
    payload_bytes: usize,
    framing_bytes: usize,
    payload_ranges: Vec<Range<usize>>,
}

/// Schema version emitted by the external-input preflight publisher.
pub const EXTERNAL_INPUT_PREFLIGHT_REPORT_VERSION: &str = "1.0.0";

/// One decoded payload range in the physical dataset.
#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct ExternalInputPayloadRange {
    /// Inclusive physical byte offset.
    pub start: usize,
    /// Exclusive physical byte offset.
    pub end: usize,
}

/// Deterministic decode telemetry for one external-input manifest.
#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct ExternalInputPreflightReport {
    /// Closed report schema version.
    pub schema_version: String,
    /// Completed decode state; always `decoded` for a published report.
    pub status: String,
    /// Commit whose checked-in manifest and tool produced this report.
    pub commit: String,
    /// SHA-256 of the exact manifest bytes.
    pub manifest_sha256: String,
    /// SHA-256 of the exact copybook bytes.
    pub copybook_sha256: String,
    /// SHA-256 of the exact physical dataset bytes.
    pub dataset_sha256: String,
    /// Record framing selected by the manifest.
    pub record_format: ExternalRecordFormat,
    /// Character code page selected by the manifest.
    pub codepage: ExternalCodepage,
    /// Workload label selected by the manifest.
    pub workload: ExternalWorkload,
    /// Number of records successfully decoded.
    pub decoded_records: usize,
    /// Complete physical dataset byte count.
    pub physical_bytes: usize,
    /// Sum of decoded payload byte counts.
    pub payload_bytes: usize,
    /// Physical framing byte count.
    pub framing_bytes: usize,
    /// Exact payload ranges decoded in record order.
    pub payload_ranges: Vec<ExternalInputPayloadRange>,
}

impl ValidatedExternalInput {
    /// Build decode options matching the manifest without running a benchmark.
    #[must_use]
    pub fn decode_options(&self) -> DecodeOptions {
        DecodeOptions::new()
            .with_format(self.manifest.record_format.into())
            .with_codepage(self.manifest.codepage.into())
    }
}

/// Load and validate an external-input manifest and its local files.
///
/// # Errors
///
/// Returns an error for unreadable or unsafe paths, malformed metadata,
/// copybook layout disagreement, integrity mismatch, or invalid record framing.
pub fn load_external_input(manifest_path: &Path) -> Result<ValidatedExternalInput> {
    Ok(load_external_input_bundle(manifest_path, None)?.validated)
}

/// Prepare one validated external-input dataset for local Criterion decoding.
///
/// # Errors
///
/// Returns an error when manifest loading or validation fails, when the
/// payload-byte total overflows, or when the initial decode witness fails.
pub fn prepare_external_input_decode_benchmark(
    manifest_path: &Path,
) -> Result<ExternalInputDecodeBenchmark> {
    let loaded = load_external_input_bundle(manifest_path, None)?;
    let options = loaded.validated.decode_options();
    let payload_bytes = checked_payload_bytes(&loaded)?;
    let benchmark_id = benchmark_identity(&loaded)?;
    let mut benchmark = ExternalInputDecodeBenchmark {
        loaded,
        options,
        scratch: ScratchBuffers::new(),
        payload_bytes,
        benchmark_id,
    };
    let decoded_records = benchmark.decode_pass()?;
    ensure!(
        decoded_records == benchmark.loaded.validated.payload_ranges.len(),
        "external-input initial decode count does not match validated payload ranges"
    );
    Ok(benchmark)
}

fn benchmark_identity(loaded: &LoadedExternalInput) -> Result<String> {
    let manifest = &loaded.validated.manifest;
    let format = match manifest.record_format {
        ExternalRecordFormat::Fixed => "fixed",
        ExternalRecordFormat::Rdw => "rdw",
    };
    let codepage = match manifest.codepage {
        ExternalCodepage::Ascii => "ascii",
        ExternalCodepage::Cp037 => "cp037",
        ExternalCodepage::Cp273 => "cp273",
        ExternalCodepage::Cp500 => "cp500",
        ExternalCodepage::Cp1047 => "cp1047",
        ExternalCodepage::Cp1140 => "cp1140",
    };
    let workload = match manifest.workload {
        ExternalWorkload::DisplayHeavy => "display-heavy",
        ExternalWorkload::Comp3Heavy => "comp3-heavy",
        ExternalWorkload::Mixed => "mixed",
    };
    let digest = loaded
        .manifest_sha256
        .get(..12)
        .context("validated manifest SHA-256 is shorter than 12 hexadecimal characters")?;
    Ok(format!(
        "{format}-{codepage}-{workload}-l{}-n{}-{digest}",
        manifest.record_length, manifest.record_count
    ))
}

fn checked_payload_bytes(loaded: &LoadedExternalInput) -> Result<usize> {
    loaded
        .validated
        .payload_ranges
        .iter()
        .try_fold(0_usize, |total, range| {
            let payload = loaded
                .validated
                .dataset
                .get(range.clone())
                .with_context(|| {
                    format!(
                        "payload range {}..{} is outside the validated dataset",
                        range.start, range.end
                    )
                })?;
            total
                .checked_add(payload.len())
                .context("external-input payload byte total overflows usize")
        })
}

fn decode_loaded_pass(
    loaded: &LoadedExternalInput,
    options: &DecodeOptions,
    scratch: &mut ScratchBuffers,
) -> Result<usize> {
    let mut decoded_records = 0_usize;
    for (record_index, range) in loaded.validated.payload_ranges.iter().enumerate() {
        let payload = loaded
            .validated
            .dataset
            .get(range.clone())
            .with_context(|| {
                format!(
                    "record {record_index} payload range {}..{} is outside the validated dataset",
                    range.start, range.end
                )
            })?;
        let _decoded = decode_record_with_scratch(&loaded.schema, payload, options, scratch)
            .with_context(|| {
                format!(
                    "failed to decode record {record_index} payload range {}..{}",
                    range.start, range.end
                )
            })?;
        decoded_records = decoded_records
            .checked_add(1)
            .context("external-input decoded record count overflows usize")?;
    }
    Ok(decoded_records)
}

fn load_external_input_bundle(
    manifest_path: &Path,
    stale_output: Option<&Path>,
) -> Result<LoadedExternalInput> {
    if let Some(output_path) = stale_output {
        reject_output_alias(output_path, manifest_path, PreflightInputArtifact::Manifest)?;
    }
    reject_symlink_or_non_file(manifest_path, "manifest")?;
    let manifest_bytes = fs::read(manifest_path)
        .with_context(|| format!("failed to read manifest {}", manifest_path.display()))?;
    let manifest: ExternalInputManifest = serde_json::from_slice(&manifest_bytes)
        .with_context(|| format!("failed to parse manifest {}", manifest_path.display()))?;
    let manifest_sha256 = format!("{:x}", Sha256::digest(&manifest_bytes));

    let base = manifest_path
        .parent()
        .filter(|path| !path.as_os_str().is_empty())
        .unwrap_or(Path::new("."));
    if let Some(output_path) = stale_output {
        reject_output_alias(
            output_path,
            &base.join(&manifest.copybook),
            PreflightInputArtifact::Copybook,
        )?;
        reject_output_alias(
            output_path,
            &base.join(&manifest.dataset),
            PreflightInputArtifact::Dataset,
        )?;
        remove_output_if_present(output_path)?;
    }

    ensure!(
        manifest.schema_version == EXTERNAL_INPUT_SCHEMA_VERSION,
        "unsupported external-input schema version {}; expected {EXTERNAL_INPUT_SCHEMA_VERSION}",
        manifest.schema_version
    );
    ensure!(manifest.record_length > 0, "record_length must be positive");
    ensure!(
        u16::try_from(manifest.record_length).is_ok(),
        "record_length must not exceed 65535 bytes"
    );
    ensure!(manifest.record_count > 0, "record_count must be positive");
    validate_declared_sha256(
        IntegrityArtifact::Copybook,
        "copybook_sha256",
        &manifest.copybook_sha256,
    )?;
    validate_declared_sha256(
        IntegrityArtifact::Dataset,
        "dataset_sha256",
        &manifest.dataset_sha256,
    )?;

    let copybook_path = resolve_local_file(base, &manifest.copybook, "copybook")?;
    let dataset_path = resolve_local_file(base, &manifest.dataset, "dataset")?;
    let copybook_source = fs::read_to_string(&copybook_path)
        .with_context(|| format!("failed to read copybook {}", copybook_path.display()))?;
    let actual_copybook_sha256 = format!("{:x}", Sha256::digest(copybook_source.as_bytes()));
    validate_digest_match(
        IntegrityArtifact::Copybook,
        &manifest.copybook_sha256,
        &actual_copybook_sha256,
    )?;
    let schema = copybook_core::parse_copybook(&copybook_source)
        .with_context(|| format!("failed to parse copybook {}", copybook_path.display()))?;
    let schema_length = schema
        .lrecl_fixed
        .context("copybook does not define a fixed maximum record length")?;
    let schema_length =
        usize::try_from(schema_length).context("copybook record length does not fit usize")?;
    ensure!(
        schema_length == manifest.record_length,
        "manifest record_length {} does not match copybook LRECL {schema_length}",
        manifest.record_length
    );

    let dataset = fs::read(&dataset_path)
        .with_context(|| format!("failed to read dataset {}", dataset_path.display()))?;
    let actual_sha256 = format!("{:x}", Sha256::digest(&dataset));
    validate_digest_match(
        IntegrityArtifact::Dataset,
        &manifest.dataset_sha256,
        &actual_sha256,
    )?;
    let payload_ranges = validate_framing(&manifest, &dataset)?;

    Ok(LoadedExternalInput {
        validated: ValidatedExternalInput {
            manifest,
            copybook_source,
            dataset,
            payload_ranges,
        },
        schema,
        manifest_sha256,
    })
}

fn run_external_input_preflight(
    manifest_path: &Path,
    stale_output: Option<&Path>,
) -> Result<ExternalInputPreflight> {
    let loaded = load_external_input_bundle(manifest_path, stale_output)?;
    let options = loaded.validated.decode_options();
    let mut scratch = ScratchBuffers::new();
    let payload_bytes = checked_payload_bytes(&loaded)?;
    let decoded_records = decode_loaded_pass(&loaded, &options, &mut scratch)?;

    let physical_bytes = loaded.validated.dataset.len();
    let framing_bytes = physical_bytes
        .checked_sub(payload_bytes)
        .context("external-input payload bytes exceed physical dataset bytes")?;
    Ok(ExternalInputPreflight {
        manifest_sha256: loaded.manifest_sha256,
        copybook_sha256: loaded.validated.manifest.copybook_sha256.clone(),
        dataset_sha256: loaded.validated.manifest.dataset_sha256.clone(),
        record_format: loaded.validated.manifest.record_format,
        codepage: loaded.validated.manifest.codepage,
        workload: loaded.validated.manifest.workload,
        decoded_records,
        physical_bytes,
        payload_bytes,
        framing_bytes,
        payload_ranges: loaded.validated.payload_ranges,
    })
}

/// Decode one validated external input and atomically publish deterministic telemetry.
///
/// # Errors
///
/// Returns an error for an invalid commit identity, manifest validation or decode
/// failure, report serialization failure, or output filesystem failure.
/// Once a readable manifest establishes that the output is distinct from all
/// three inputs, a pre-existing output is removed before validation and decode.
/// Missing, unreadable, or malformed manifests leave the unverifiable output
/// untouched and return an error; the nonzero CLI exit is authoritative. Input
/// aliases are always rejected without mutation.
pub fn publish_external_input_preflight(
    manifest_path: &Path,
    output_path: &Path,
    commit: &str,
) -> Result<ExternalInputPreflightReport> {
    let output_lock = PreflightOutputLock::acquire(output_path)?;
    let preflight = run_external_input_preflight(manifest_path, Some(output_path))?;
    validate_commit(commit)?;
    let report = ExternalInputPreflightReport {
        schema_version: EXTERNAL_INPUT_PREFLIGHT_REPORT_VERSION.to_string(),
        status: "decoded".to_string(),
        commit: commit.to_string(),
        manifest_sha256: preflight.manifest_sha256,
        copybook_sha256: preflight.copybook_sha256,
        dataset_sha256: preflight.dataset_sha256,
        record_format: preflight.record_format,
        codepage: preflight.codepage,
        workload: preflight.workload,
        decoded_records: preflight.decoded_records,
        physical_bytes: preflight.physical_bytes,
        payload_bytes: preflight.payload_bytes,
        framing_bytes: preflight.framing_bytes,
        payload_ranges: preflight
            .payload_ranges
            .into_iter()
            .map(|range| ExternalInputPayloadRange {
                start: range.start,
                end: range.end,
            })
            .collect(),
    };
    let bytes = serde_json::to_vec_pretty(&report)
        .context("failed to serialize external-input preflight report")?;
    write_report_atomically(&output_lock, output_path, &bytes)?;
    Ok(report)
}

fn validate_commit(commit: &str) -> Result<()> {
    ensure!(
        commit.len() == 40
            && commit
                .bytes()
                .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte)),
        "commit must be 40 lowercase hexadecimal characters"
    );
    Ok(())
}

fn reject_output_alias(
    output_path: &Path,
    input_path: &Path,
    input: PreflightInputArtifact,
) -> Result<()> {
    let output_resolved = comparable_path(output_path).with_context(|| {
        format!(
            "failed to resolve preflight output path {} before alias validation",
            output_path.display()
        )
    })?;
    let input_resolved = comparable_path(input_path).with_context(|| {
        format!(
            "failed to resolve preflight {input} input path {} before alias validation",
            input_path.display()
        )
    })?;
    if output_resolved == input_resolved {
        return Err(PreflightOutputAliasError { input }.into());
    }
    Ok(())
}

fn comparable_path(path: &Path) -> Result<PathBuf> {
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()
            .context("failed to resolve current directory for preflight path comparison")?
            .join(path)
    };

    match fs::canonicalize(&absolute) {
        Ok(canonical) => Ok(canonical),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            let file_name = absolute
                .file_name()
                .context("preflight comparison path must name a file")?;
            let parent = absolute
                .parent()
                .context("preflight comparison path must have a parent")?;
            let canonical_parent = fs::canonicalize(parent).with_context(|| {
                format!(
                    "failed to resolve parent {} for preflight path comparison",
                    parent.display()
                )
            })?;
            Ok(canonical_parent.join(file_name))
        }
        Err(error) => Err(error).with_context(|| {
            format!(
                "failed to resolve {} for preflight path comparison",
                absolute.display()
            )
        }),
    }
}

struct PreflightOutputLock {
    path: PathBuf,
}

impl PreflightOutputLock {
    fn acquire(output_path: &Path) -> Result<Self> {
        let parent = output_path
            .parent()
            .filter(|path| !path.as_os_str().is_empty())
            .unwrap_or(Path::new("."));
        ensure!(
            parent.is_dir(),
            "preflight output directory does not exist: {}",
            parent.display()
        );
        let file_name = output_path
            .file_name()
            .context("preflight output path must name a file")?;
        let path = parent.join(format!(".{}.lock", file_name.to_string_lossy()));
        fs::create_dir(&path).with_context(|| {
            format!(
                "failed to acquire exclusive preflight output lock {}",
                path.display()
            )
        })?;
        Ok(Self { path })
    }
}

impl Drop for PreflightOutputLock {
    fn drop(&mut self) {
        let _cleanup = fs::remove_dir(&self.path);
    }
}

fn write_report_atomically(
    _output_lock: &PreflightOutputLock,
    output_path: &Path,
    bytes: &[u8],
) -> Result<()> {
    let parent = output_path
        .parent()
        .filter(|path| !path.as_os_str().is_empty())
        .unwrap_or(Path::new("."));
    ensure!(
        parent.is_dir(),
        "preflight output directory does not exist: {}",
        parent.display()
    );
    let file_name = output_path
        .file_name()
        .context("preflight output path must name a file")?;
    let prefix = format!(".{}.", file_name.to_string_lossy());
    let mut temporary = tempfile::Builder::new()
        .prefix(&prefix)
        .suffix(".tmp")
        .tempfile_in(parent)
        .with_context(|| {
            format!(
                "failed to create exclusive temporary preflight report in {}",
                parent.display()
            )
        })?;
    temporary.write_all(bytes).with_context(|| {
        format!(
            "failed to write temporary preflight report for {}",
            output_path.display()
        )
    })?;
    temporary.flush().with_context(|| {
        format!(
            "failed to flush temporary preflight report for {}",
            output_path.display()
        )
    })?;
    temporary.as_file().sync_all().with_context(|| {
        format!(
            "failed to sync temporary preflight report for {}",
            output_path.display()
        )
    })?;
    temporary.persist(output_path).with_context(|| {
        format!(
            "failed to atomically publish preflight report {}",
            output_path.display()
        )
    })?;
    Ok(())
}

fn remove_output_if_present(path: &Path) -> Result<()> {
    match fs::remove_file(path) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error)
            .with_context(|| format!("failed to remove stale preflight output {}", path.display())),
    }
}

fn validate_declared_sha256(artifact: IntegrityArtifact, field: &str, value: &str) -> Result<()> {
    if value.is_empty() {
        return Err(ManifestIntegrityError {
            artifact,
            detail: format!("missing required {field}"),
        }
        .into());
    }
    if value.len() != 64
        || !value
            .bytes()
            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
    {
        return Err(ManifestIntegrityError {
            artifact,
            detail: format!("{field} must be 64 lowercase hexadecimal characters"),
        }
        .into());
    }
    Ok(())
}

fn validate_digest_match(artifact: IntegrityArtifact, expected: &str, actual: &str) -> Result<()> {
    if expected != actual {
        return Err(ManifestIntegrityError {
            artifact,
            detail: format!("SHA-256 mismatch: expected {expected}, got {actual}"),
        }
        .into());
    }
    Ok(())
}

fn reject_symlink_or_non_file(path: &Path, label: &str) -> Result<()> {
    let metadata = fs::symlink_metadata(path)
        .with_context(|| format!("failed to inspect {label} {}", path.display()))?;
    ensure!(
        !metadata.file_type().is_symlink(),
        "{label} must not be a symlink: {}",
        path.display()
    );
    ensure!(
        metadata.is_file(),
        "{label} is not a regular file: {}",
        path.display()
    );
    Ok(())
}

fn resolve_local_file(base: &Path, relative: &Path, label: &str) -> Result<PathBuf> {
    ensure!(
        !relative.as_os_str().is_empty(),
        "{label} path must not be empty"
    );
    let canonical_base = fs::canonicalize(base)
        .with_context(|| format!("failed to resolve manifest directory {}", base.display()))?;
    let mut candidate = canonical_base.clone();
    for component in relative.components() {
        let Component::Normal(segment) = component else {
            bail!(
                "{label} path must be relative and must not traverse directories: {}",
                relative.display()
            );
        };
        candidate.push(segment);
        let metadata = fs::symlink_metadata(&candidate)
            .with_context(|| format!("failed to inspect {label} {}", candidate.display()))?;
        ensure!(
            !metadata.file_type().is_symlink(),
            "{label} path must not contain symlinks: {}",
            relative.display()
        );
    }
    reject_symlink_or_non_file(&candidate, label)?;
    let canonical_candidate = fs::canonicalize(&candidate)
        .with_context(|| format!("failed to resolve {label} {}", candidate.display()))?;
    ensure!(
        canonical_candidate.starts_with(&canonical_base),
        "{label} resolves outside the manifest directory: {}",
        relative.display()
    );
    Ok(canonical_candidate)
}

fn validate_framing(manifest: &ExternalInputManifest, dataset: &[u8]) -> Result<Vec<Range<usize>>> {
    match manifest.record_format {
        ExternalRecordFormat::Fixed => validate_fixed_framing(manifest, dataset),
        ExternalRecordFormat::Rdw => validate_rdw_framing(manifest, dataset),
    }
}

fn validate_fixed_framing(
    manifest: &ExternalInputManifest,
    dataset: &[u8],
) -> Result<Vec<Range<usize>>> {
    let expected_bytes = manifest
        .record_length
        .checked_mul(manifest.record_count)
        .context("fixed dataset length overflows usize")?;
    ensure!(
        dataset.len() == expected_bytes,
        "fixed dataset length {} does not match {expected_bytes} bytes for {} records",
        dataset.len(),
        manifest.record_count
    );
    Ok((0..manifest.record_count)
        .map(|index| {
            let start = index * manifest.record_length;
            start..start + manifest.record_length
        })
        .collect())
}

fn validate_rdw_framing(
    manifest: &ExternalInputManifest,
    dataset: &[u8],
) -> Result<Vec<Range<usize>>> {
    let mut offset = 0_usize;
    // The manifest is hostile input. Grow only as validated records are
    // discovered instead of reserving from its untrusted record_count.
    let mut payload_ranges = Vec::new();
    while offset < dataset.len() {
        let header_end = offset
            .checked_add(4)
            .context("RDW header offset overflows usize")?;
        let header = dataset
            .get(offset..header_end)
            .context("truncated RDW header in external dataset")?;
        let length_bytes: [u8; 2] = header
            .get(..2)
            .context("RDW header does not contain a length field")?
            .try_into()
            .context("RDW length field is not two bytes")?;
        let declared = usize::from(u16::from_be_bytes(length_bytes));
        ensure!(
            declared == manifest.record_length,
            "RDW payload length {declared} does not match manifest record_length {}",
            manifest.record_length
        );
        let payload_end = header_end
            .checked_add(declared)
            .context("RDW payload offset overflows usize")?;
        ensure!(
            payload_end <= dataset.len(),
            "truncated RDW payload in external dataset"
        );
        payload_ranges.push(header_end..payload_end);
        offset = payload_end;
    }
    ensure!(
        payload_ranges.len() == manifest.record_count,
        "RDW dataset contains {} records; manifest declares {}",
        payload_ranges.len(),
        manifest.record_count
    );
    Ok(payload_ranges)
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::{Path, PathBuf};

    use anyhow::{Context, Result, ensure};
    use serde_json::{Map, Value, json};
    use sha2::{Digest, Sha256};
    use tempfile::TempDir;

    use super::{
        ExternalCodepage, ExternalRecordFormat, ExternalWorkload, IntegrityArtifact,
        ManifestIntegrityError, PreflightInputArtifact, PreflightOutputAliasError,
        PreflightOutputLock, load_external_input, prepare_external_input_decode_benchmark,
        publish_external_input_preflight, run_external_input_preflight, write_report_atomically,
    };

    #[cfg(unix)]
    use super::comparable_path;

    fn fixtures() -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR")).join("test_fixtures/external_input")
    }

    fn copy_fixture(name: &str) -> Result<(TempDir, PathBuf)> {
        let temp = tempfile::tempdir()?;
        for entry in fs::read_dir(fixtures())? {
            let entry = entry?;
            if entry.file_type()?.is_file() {
                fs::copy(entry.path(), temp.path().join(entry.file_name()))?;
            }
        }
        let manifest = temp.path().join(name);
        Ok((temp, manifest))
    }

    fn edit_manifest(path: &Path, update: impl FnOnce(&mut Map<String, Value>)) -> Result<()> {
        let mut value: Value = serde_json::from_slice(&fs::read(path)?)?;
        let root = value
            .as_object_mut()
            .context("fixture manifest root is not an object")?;
        update(root);
        fs::write(path, serde_json::to_vec_pretty(&value)?)?;
        Ok(())
    }

    #[derive(Clone, Copy)]
    enum TomlMultilineString {
        Basic,
        Literal,
    }

    fn line_is_toml_structure(line: &str, multiline: &mut Option<TomlMultilineString>) -> bool {
        let bytes = line.as_bytes();
        let mut cursor = 0;
        let mut basic_string = false;
        let mut literal_string = false;
        let mut escaped = false;
        let mut touched_multiline = multiline.is_some();

        while cursor < bytes.len() {
            if let Some(delimiter) = *multiline {
                let marker = match delimiter {
                    TomlMultilineString::Basic => b"\"\"\"".as_slice(),
                    TomlMultilineString::Literal => b"'''".as_slice(),
                };
                if bytes
                    .get(cursor..)
                    .is_some_and(|tail| tail.starts_with(marker))
                    && (!matches!(delimiter, TomlMultilineString::Basic) || !escaped)
                {
                    *multiline = None;
                    cursor += marker.len();
                    escaped = false;
                    continue;
                }
                escaped = matches!(delimiter, TomlMultilineString::Basic)
                    && bytes.get(cursor) == Some(&b'\\')
                    && !escaped;
                cursor += 1;
                continue;
            }

            if !basic_string
                && !literal_string
                && bytes
                    .get(cursor..)
                    .is_some_and(|tail| tail.starts_with(b"\"\"\""))
            {
                *multiline = Some(TomlMultilineString::Basic);
                touched_multiline = true;
                cursor += 3;
                continue;
            }
            if !basic_string
                && !literal_string
                && bytes
                    .get(cursor..)
                    .is_some_and(|tail| tail.starts_with(b"'''"))
            {
                *multiline = Some(TomlMultilineString::Literal);
                touched_multiline = true;
                cursor += 3;
                continue;
            }

            match bytes.get(cursor) {
                Some(b'#') if !basic_string && !literal_string => break,
                Some(b'\"') if !literal_string && !escaped => basic_string = !basic_string,
                Some(b'\'') if !basic_string => literal_string = !literal_string,
                _ => {}
            }
            escaped = basic_string && bytes.get(cursor) == Some(&b'\\') && !escaped;
            cursor += 1;
        }

        !touched_multiline
    }

    fn has_adjacent_toml_lines(source: &str, expected: &[&str]) -> bool {
        let Some((first, tail)) = expected.split_first() else {
            return false;
        };
        let mut multiline = None;
        let mut matched_tail: Option<&[&str]> = None;
        for line in source.lines() {
            if !line_is_toml_structure(line, &mut multiline) {
                matched_tail = None;
                continue;
            }
            if let Some(expected_tail) = matched_tail.as_mut() {
                if expected_tail
                    .first()
                    .is_some_and(|expected| line == *expected)
                {
                    *expected_tail = expected_tail.get(1..).unwrap_or_default();
                    if expected_tail.is_empty() {
                        return true;
                    }
                    continue;
                }
                matched_tail = None;
            }
            if line == *first {
                if tail.is_empty() {
                    return true;
                }
                matched_tail = Some(tail);
            }
        }
        false
    }

    fn cargo_manifest_admits_external_input_benchmark(source: &str) -> bool {
        let mut multiline = None;
        let mut in_features = false;
        let mut feature_declared = false;
        for line in source.lines() {
            if !line_is_toml_structure(line, &mut multiline) {
                continue;
            }
            if line.starts_with('[') {
                in_features = line == "[features]";
            } else if in_features && line == "external-input = []" {
                feature_declared = true;
            }
        }
        feature_declared
            && has_adjacent_toml_lines(
                source,
                &[
                    "[[bench]]",
                    "name = \"external_input_decode\"",
                    "harness = false",
                    "required-features = [\"external-input\"]",
                ],
            )
    }

    #[test]
    fn external_input_accepts_fixed_rdw_ascii_cp037_matrix() -> Result<()> {
        let cases = [
            (
                "fixed-ascii.json",
                ExternalRecordFormat::Fixed,
                ExternalCodepage::Ascii,
                0..5,
            ),
            (
                "fixed-cp037.json",
                ExternalRecordFormat::Fixed,
                ExternalCodepage::Cp037,
                0..5,
            ),
            (
                "rdw-ascii.json",
                ExternalRecordFormat::Rdw,
                ExternalCodepage::Ascii,
                4..9,
            ),
            (
                "rdw-cp037.json",
                ExternalRecordFormat::Rdw,
                ExternalCodepage::Cp037,
                4..9,
            ),
        ];
        for (name, format, codepage, expected_range) in cases {
            let loaded = load_external_input(&fixtures().join(name))?;
            ensure!(loaded.manifest.record_format == format);
            ensure!(loaded.manifest.codepage == codepage);
            ensure!(loaded.payload_ranges == [expected_range]);
            let options = loaded.decode_options();
            ensure!(options.format == format.into());
            ensure!(options.codepage == codepage.into());
        }
        Ok(())
    }

    #[test]
    fn external_input_repeat_load_is_deterministic() -> Result<()> {
        let path = fixtures().join("rdw-cp037.json");
        ensure!(load_external_input(&path)? == load_external_input(&path)?);
        Ok(())
    }

    #[test]
    fn external_input_preflight_decodes_fixture_matrix_with_exact_telemetry() -> Result<()> {
        let cases = [
            (
                "fixed-ascii.json",
                ExternalRecordFormat::Fixed,
                ExternalCodepage::Ascii,
                ExternalWorkload::DisplayHeavy,
                5,
                0,
                0..5,
            ),
            (
                "fixed-cp037.json",
                ExternalRecordFormat::Fixed,
                ExternalCodepage::Cp037,
                ExternalWorkload::DisplayHeavy,
                5,
                0,
                0..5,
            ),
            (
                "rdw-ascii.json",
                ExternalRecordFormat::Rdw,
                ExternalCodepage::Ascii,
                ExternalWorkload::DisplayHeavy,
                9,
                4,
                4..9,
            ),
            (
                "rdw-cp037.json",
                ExternalRecordFormat::Rdw,
                ExternalCodepage::Cp037,
                ExternalWorkload::DisplayHeavy,
                9,
                4,
                4..9,
            ),
        ];
        for (name, format, codepage, workload, physical, framing, expected_range) in cases {
            let path = fixtures().join(name);
            let telemetry = run_external_input_preflight(&path, None)?;
            let expected_identity = format!("{:x}", Sha256::digest(fs::read(&path)?));
            ensure!(telemetry.manifest_sha256 == expected_identity);
            ensure!(telemetry.record_format == format);
            ensure!(telemetry.codepage == codepage);
            ensure!(telemetry.workload == workload);
            ensure!(telemetry.decoded_records == 1);
            ensure!(telemetry.physical_bytes == physical);
            ensure!(telemetry.payload_bytes == 5);
            ensure!(telemetry.framing_bytes == framing);
            ensure!(telemetry.payload_ranges == [expected_range]);
        }
        Ok(())
    }

    #[test]
    fn external_input_preflight_is_deterministic() -> Result<()> {
        let path = fixtures().join("rdw-cp037.json");
        ensure!(
            run_external_input_preflight(&path, None)?
                == run_external_input_preflight(&path, None)?
        );
        Ok(())
    }

    #[test]
    fn external_input_benchmark_decodes_all_four_manifests() -> Result<()> {
        let mut identities = Vec::new();
        for name in [
            "fixed-ascii.json",
            "fixed-cp037.json",
            "rdw-ascii.json",
            "rdw-cp037.json",
        ] {
            let mut benchmark = prepare_external_input_decode_benchmark(&fixtures().join(name))?;
            ensure!(benchmark.payload_bytes() == 5);
            ensure!(benchmark.benchmark_id().len() <= 80);
            identities.push(benchmark.benchmark_id().to_string());
            ensure!(benchmark.decode_pass()? == 1);
            ensure!(benchmark.decode_pass()? == 1);
        }
        let mut distinct = identities.clone();
        distinct.sort_unstable();
        distinct.dedup();
        ensure!(distinct.len() == identities.len());
        let repeat = prepare_external_input_decode_benchmark(&fixtures().join("fixed-ascii.json"))?;
        ensure!(repeat.benchmark_id() == identities[0]);
        Ok(())
    }

    #[test]
    fn external_input_benchmark_target_is_explicitly_feature_gated() -> Result<()> {
        let crate_root = Path::new(env!("CARGO_MANIFEST_DIR"));
        let cargo_toml = fs::read_to_string(crate_root.join("Cargo.toml"))?;
        ensure!(cargo_manifest_admits_external_input_benchmark(&cargo_toml));

        let target = fs::read_to_string(crate_root.join("benches/external_input_decode.rs"))?;
        ensure!(target.contains("COPYBOOK_EXTERNAL_INPUT_MANIFEST"));
        ensure!(target.contains("must name one external-input manifest"));
        Ok(())
    }

    #[test]
    fn external_input_benchmark_admission_is_crlf_safe_and_fail_closed() -> Result<()> {
        let valid = "[package]\nname = \"copybook-bench\"\n\n[[bench]]\nname = \"external_input_decode\"\nharness = false\nrequired-features = [\"external-input\"]\n\n[features]\ndefault = []\nexternal-input = []\n";
        ensure!(cargo_manifest_admits_external_input_benchmark(valid));
        ensure!(cargo_manifest_admits_external_input_benchmark(
            &valid.replace('\n', "\r\n")
        ));

        for delimiter in ["\"\"\"", "'''"] {
            let fake = format!(
                "[package]\nname = \"copybook-bench\"\n\n[package.metadata]\ntext = {delimiter}\n[features]\nexternal-input = []\n\n[[bench]]\nname = \"external_input_decode\"\nharness = false\nrequired-features = [\"external-input\"]\n{delimiter}\n"
            );
            ensure!(!cargo_manifest_admits_external_input_benchmark(&fake));
        }

        for rejected in [
            valid.replace("external-input = []\n", ""),
            valid.replace("[features]\n", "[package.metadata]\n"),
            valid.replace("required-features = [\"external-input\"]\n", ""),
            valid.replace(
                "required-features = [\"external-input\"]",
                "required-features = [\"diagnostics\"]",
            ),
            valid.replace(
                "harness = false\nrequired-features = [\"external-input\"]",
                "required-features = [\"external-input\"]\nharness = false",
            ),
            valid.replace(
                "harness = false\nrequired-features = [\"external-input\"]",
                "harness = false\n\n[[bench]]\nname = \"detached\"\nharness = false\nrequired-features = [\"external-input\"]",
            ),
            valid.replace(
                "name = \"external_input_decode\"",
                "name = \"decode_performance\"",
            ),
        ] {
            ensure!(!cargo_manifest_admits_external_input_benchmark(&rejected));
        }
        Ok(())
    }

    #[test]
    fn external_input_benchmark_rejects_decode_invalid_payload() -> Result<()> {
        let (temp, manifest) = copy_fixture("fixed-ascii.json")?;
        let numeric_copybook = b"       01 RECORD PIC 9(5).\n";
        fs::write(temp.path().join("simple.cpy"), numeric_copybook)?;
        edit_manifest(&manifest, |root| {
            root.insert(
                "copybook_sha256".to_string(),
                json!(format!("{:x}", Sha256::digest(numeric_copybook))),
            );
        })?;
        let error = prepare_external_input_decode_benchmark(&manifest)
            .err()
            .context("decode-invalid input unexpectedly prepared a benchmark")?;
        ensure!(
            error
                .to_string()
                .contains("failed to decode record 0 payload range 0..5")
        );
        Ok(())
    }

    #[test]
    fn external_input_benchmark_returns_no_timing_after_decode_failure() -> Result<()> {
        let (temp, manifest) = copy_fixture("fixed-ascii.json")?;
        let numeric_copybook = b"       01 RECORD PIC 9(5).\n";
        let valid_dataset = b"12345";
        fs::write(temp.path().join("simple.cpy"), numeric_copybook)?;
        fs::write(temp.path().join("fixed-ascii.bin"), valid_dataset)?;
        edit_manifest(&manifest, |root| {
            root.insert(
                "copybook_sha256".to_string(),
                json!(format!("{:x}", Sha256::digest(numeric_copybook))),
            );
            root.insert(
                "dataset_sha256".to_string(),
                json!(format!("{:x}", Sha256::digest(valid_dataset))),
            );
        })?;
        let mut benchmark = prepare_external_input_decode_benchmark(&manifest)?;
        benchmark.loaded.validated.dataset.copy_from_slice(b"ABCDE");
        let error = benchmark
            .measure_decode_iterations(2)
            .err()
            .context("decode failure unexpectedly returned a timing sample")?;
        ensure!(
            error
                .to_string()
                .contains("external-input decode iteration 0 failed")
        );
        Ok(())
    }

    #[test]
    fn external_input_preflight_propagates_decode_failure_after_valid_load() -> Result<()> {
        let (temp, manifest) = copy_fixture("fixed-ascii.json")?;
        let numeric_copybook = b"       01 RECORD PIC 9(5).\n";
        let copybook_path = temp.path().join("simple.cpy");
        fs::write(&copybook_path, numeric_copybook)?;
        edit_manifest(&manifest, |root| {
            root.insert(
                "copybook_sha256".to_string(),
                json!(format!("{:x}", Sha256::digest(numeric_copybook))),
            );
        })?;

        let validated = load_external_input(&manifest)?;
        ensure!(validated.manifest.record_length == 5);
        let error = run_external_input_preflight(&manifest, None)
            .err()
            .context("invalid numeric payload unexpectedly passed decode preflight")?;
        let message = error.to_string();
        ensure!(message.contains("failed to decode record 0 payload range 0..5"));

        let output = temp.path().join("preflight.json");
        fs::write(&output, b"stale-success")?;
        let publish = publish_external_input_preflight(
            &manifest,
            &output,
            "0123456789abcdef0123456789abcdef01234567",
        )
        .err()
        .context("invalid numeric payload unexpectedly published decode telemetry")?;
        ensure!(
            publish
                .to_string()
                .contains("failed to decode record 0 payload range 0..5")
        );
        ensure!(!output.exists());
        Ok(())
    }

    #[test]
    fn external_input_preflight_rejects_typed_input_aliases_without_removal() -> Result<()> {
        let cases = [
            ("fixed-ascii.json", PreflightInputArtifact::Manifest),
            ("simple.cpy", PreflightInputArtifact::Copybook),
            ("fixed-ascii.bin", PreflightInputArtifact::Dataset),
        ];
        for (output_name, expected_input) in cases {
            let (temp, manifest) = copy_fixture("fixed-ascii.json")?;
            let output = temp.path().join(output_name);
            let before = fs::read(&output)?;
            let error = publish_external_input_preflight(
                &manifest,
                &output,
                "0123456789abcdef0123456789abcdef01234567",
            )
            .err()
            .context("aliased preflight output unexpectedly succeeded")?;
            let typed = error
                .downcast_ref::<PreflightOutputAliasError>()
                .context("alias rejection did not retain its typed error")?;
            ensure!(typed.input == expected_input);
            ensure!(fs::read(&output)? == before);
        }
        Ok(())
    }

    #[cfg(unix)]
    #[test]
    fn external_input_preflight_resolves_symlink_parent_components_before_aliasing() -> Result<()> {
        use std::os::unix::fs::symlink;

        let cases = [
            ("fixed-ascii.json", PreflightInputArtifact::Manifest),
            ("simple.cpy", PreflightInputArtifact::Copybook),
            ("fixed-ascii.bin", PreflightInputArtifact::Dataset),
        ];
        for (output_name, expected_input) in cases {
            let (temp, manifest) = copy_fixture("fixed-ascii.json")?;
            let nested = temp.path().join("nested/child");
            fs::create_dir_all(&nested)?;
            let linked_child = temp.path().join("linked-child");
            symlink(&nested, &linked_child)?;
            let output = linked_child.join("../..").join(output_name);
            let input_path = temp.path().join(output_name);
            let before = fs::read(&input_path)?;

            let error = publish_external_input_preflight(
                &manifest,
                &output,
                "0123456789abcdef0123456789abcdef01234567",
            )
            .err()
            .context("symlink-plus-parent alias unexpectedly published telemetry")?;
            let typed = error
                .downcast_ref::<PreflightOutputAliasError>()
                .context("symlink-plus-parent alias did not retain its typed error")?;
            ensure!(typed.input == expected_input);
            ensure!(fs::read(&input_path)? == before);
        }

        let (temp, manifest) = copy_fixture("fixed-ascii.json")?;
        let nested = temp.path().join("nested/child");
        fs::create_dir_all(&nested)?;
        let linked_child = temp.path().join("linked-child");
        symlink(&nested, &linked_child)?;
        let output = linked_child.join("../..").join("distinct-report.json");
        publish_external_input_preflight(
            &manifest,
            &output,
            "0123456789abcdef0123456789abcdef01234567",
        )?;
        ensure!(temp.path().join("distinct-report.json").is_file());
        Ok(())
    }

    #[test]
    fn external_input_preflight_removes_distinct_stale_output_on_commit_failure() -> Result<()> {
        let (temp, manifest) = copy_fixture("fixed-ascii.json")?;
        let output = temp.path().join("preflight.json");
        fs::write(&output, b"stale-success")?;
        let error = publish_external_input_preflight(&manifest, &output, "invalid")
            .err()
            .context("invalid commit unexpectedly published preflight telemetry")?;
        ensure!(error.to_string().contains("40 lowercase hexadecimal"));
        ensure!(!output.exists());
        Ok(())
    }

    #[cfg(unix)]
    #[test]
    fn external_input_preflight_preserves_lexical_target_when_parent_is_unresolved() -> Result<()> {
        let (temp, manifest) = copy_fixture("fixed-ascii.json")?;
        let stale = temp.path().join("stale-report.json");
        fs::write(&stale, b"stale-success")?;
        let unresolved = temp.path().join("missing-parent/../stale-report.json");

        let resolution = comparable_path(&unresolved)
            .err()
            .context("unresolved comparison path unexpectedly resolved")?;
        ensure!(
            format!("{resolution:#}").contains("failed to resolve parent"),
            "unresolved comparison path lost its resolution context: {resolution:#}"
        );

        let error = publish_external_input_preflight(
            &manifest,
            &unresolved,
            "0123456789abcdef0123456789abcdef01234567",
        )
        .err()
        .context("unresolved output parent unexpectedly published telemetry")?;
        ensure!(error.downcast_ref::<PreflightOutputAliasError>().is_none());
        ensure!(fs::read(&stale)? == b"stale-success");
        Ok(())
    }

    #[test]
    fn external_input_report_publish_lock_prevents_concurrent_clobber() -> Result<()> {
        let temp = tempfile::tempdir()?;
        let output = temp.path().join("report.json");
        let first = PreflightOutputLock::acquire(&output)?;
        let second = PreflightOutputLock::acquire(&output)
            .err()
            .context("concurrent preflight output lock unexpectedly succeeded")?;
        ensure!(
            second
                .to_string()
                .contains("exclusive preflight output lock")
        );
        write_report_atomically(&first, &output, b"first")?;
        ensure!(fs::read(&output)? == b"first");
        drop(first);
        let next = PreflightOutputLock::acquire(&output)?;
        write_report_atomically(&next, &output, b"second")?;
        ensure!(fs::read(&output)? == b"second");
        Ok(())
    }

    #[cfg(unix)]
    #[test]
    fn external_input_report_publish_ignores_predictable_temp_symlink() -> Result<()> {
        use std::os::unix::fs::symlink;

        let temp = tempfile::tempdir()?;
        let output = temp.path().join("report.json");
        let victim = temp.path().join("victim.json");
        let predictable = temp.path().join(".report.json.tmp");
        fs::write(&victim, b"victim")?;
        symlink(&victim, &predictable)?;
        let output_lock = PreflightOutputLock::acquire(&output)?;
        write_report_atomically(&output_lock, &output, b"report")?;
        ensure!(fs::read(&output)? == b"report");
        ensure!(fs::read(&victim)? == b"victim");
        ensure!(fs::symlink_metadata(&predictable)?.file_type().is_symlink());
        Ok(())
    }

    #[test]
    fn external_input_json_schema_matches_closed_parser_values() -> Result<()> {
        let schema_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../schemas/external-input-manifest.json");
        let schema: Value = serde_json::from_slice(&fs::read(schema_path)?)?;
        ensure!(schema.pointer("/additionalProperties") == Some(&json!(false)));
        ensure!(schema.pointer("/properties/schema_version/const") == Some(&json!("1.0.0")));
        let required = schema
            .pointer("/required")
            .and_then(Value::as_array)
            .context("schema required list is missing")?;
        ensure!(required.contains(&json!("copybook_sha256")));
        ensure!(
            schema.pointer("/properties/copybook_sha256/pattern") == Some(&json!("^[a-f0-9]{64}$"))
        );
        ensure!(schema.pointer("/properties/record_format/enum") == Some(&json!(["fixed", "rdw"])));
        ensure!(
            schema.pointer("/properties/codepage/enum")
                == Some(&json!([
                    "ascii", "cp037", "cp273", "cp500", "cp1047", "cp1140"
                ]))
        );
        ensure!(
            schema.pointer("/properties/workload/enum")
                == Some(&json!(["display-heavy", "comp3-heavy", "mixed"]))
        );
        Ok(())
    }

    #[test]
    fn external_input_rejects_missing_and_malformed_manifest() -> Result<()> {
        let missing = load_external_input(&fixtures().join("missing.json"))
            .err()
            .context("missing manifest unexpectedly loaded")?;
        ensure!(missing.to_string().contains("failed to inspect manifest"));

        let temp = tempfile::tempdir()?;
        let malformed = temp.path().join("malformed.json");
        fs::write(&malformed, b"{not-json")?;
        let error = load_external_input(&malformed)
            .err()
            .context("malformed manifest unexpectedly loaded")?;
        ensure!(error.to_string().contains("failed to parse manifest"));

        let (_temp, manifest) = copy_fixture("fixed-ascii.json")?;
        edit_manifest(&manifest, |root| {
            root.insert("dataset".to_string(), json!("missing.bin"));
        })?;
        let error = load_external_input(&manifest)
            .err()
            .context("missing dataset unexpectedly loaded")?;
        ensure!(error.to_string().contains("failed to inspect dataset"));
        Ok(())
    }

    #[test]
    fn external_input_rejects_version_unknown_field_and_lrecl_mismatch() -> Result<()> {
        for (field, value, expected) in [
            (
                "schema_version",
                json!("2.0.0"),
                "unsupported external-input schema version",
            ),
            ("record_length", json!(6), "does not match copybook LRECL"),
        ] {
            let (_temp, manifest) = copy_fixture("fixed-ascii.json")?;
            edit_manifest(&manifest, |root| {
                root.insert(field.to_string(), value);
            })?;
            let error = load_external_input(&manifest)
                .err()
                .context("invalid manifest unexpectedly loaded")?;
            ensure!(error.to_string().contains(expected));
        }

        let (_temp, manifest) = copy_fixture("fixed-ascii.json")?;
        edit_manifest(&manifest, |root| {
            root.insert("unknown".to_string(), json!(true));
        })?;
        let error = load_external_input(&manifest)
            .err()
            .context("unknown field unexpectedly accepted")?;
        ensure!(error.to_string().contains("failed to parse manifest"));

        for (field, value) in [
            ("record_format", json!("variable")),
            ("codepage", json!("cp999")),
            ("workload", json!("unknown")),
        ] {
            let (_temp, manifest) = copy_fixture("fixed-ascii.json")?;
            edit_manifest(&manifest, |root| {
                root.insert(field.to_string(), value);
            })?;
            let error = load_external_input(&manifest)
                .err()
                .context("unknown closed-enum value unexpectedly loaded")?;
            ensure!(error.to_string().contains("failed to parse manifest"));
        }
        Ok(())
    }

    #[test]
    fn external_input_requires_typed_copybook_integrity() -> Result<()> {
        let canonical_copybook = fs::read(fixtures().join("simple.cpy"))?;
        ensure!(canonical_copybook.ends_with(b"\n"));
        ensure!(!canonical_copybook.contains(&b'\r'));
        load_external_input(&fixtures().join("fixed-ascii.json"))?;

        let (_temp, manifest) = copy_fixture("fixed-ascii.json")?;
        edit_manifest(&manifest, |root| {
            root.remove("copybook_sha256");
        })?;
        let missing = load_external_input(&manifest)
            .err()
            .context("manifest without copybook digest unexpectedly loaded")?;
        let missing = missing
            .downcast_ref::<ManifestIntegrityError>()
            .context("missing copybook digest did not return typed integrity error")?;
        ensure!(missing.artifact == IntegrityArtifact::Copybook);
        ensure!(missing.detail.contains("missing required copybook_sha256"));

        for malformed in ["abc".to_string(), "A".repeat(64), "g".repeat(64)] {
            let (_temp, manifest) = copy_fixture("fixed-ascii.json")?;
            edit_manifest(&manifest, |root| {
                root.insert("copybook_sha256".to_string(), json!(malformed));
            })?;
            let error = load_external_input(&manifest)
                .err()
                .context("malformed copybook digest unexpectedly loaded")?;
            let integrity = error
                .downcast_ref::<ManifestIntegrityError>()
                .context("malformed copybook digest did not return typed integrity error")?;
            ensure!(integrity.artifact == IntegrityArtifact::Copybook);
            ensure!(
                integrity
                    .detail
                    .contains("64 lowercase hexadecimal characters")
            );
        }

        let (temp, manifest) = copy_fixture("fixed-ascii.json")?;
        let lf_copybook = fs::read_to_string(temp.path().join("simple.cpy"))?;
        ensure!(!lf_copybook.contains('\r'));
        fs::write(
            temp.path().join("simple.cpy"),
            lf_copybook.replace('\n', "\r\n"),
        )?;
        let crlf = load_external_input(&manifest)
            .err()
            .context("CRLF copybook unexpectedly matched the LF fingerprint")?;
        let crlf = crlf
            .downcast_ref::<ManifestIntegrityError>()
            .context("CRLF copybook mismatch did not return typed integrity error")?;
        ensure!(crlf.artifact == IntegrityArtifact::Copybook);
        ensure!(crlf.detail.contains("SHA-256 mismatch"));

        let (temp, manifest) = copy_fixture("fixed-ascii.json")?;
        let altered = "       01 RECORD. 05 LEFT-FIELD PIC X(2). 05 RIGHT-FIELD PIC X(3).\n";
        let altered_schema = copybook_core::parse_copybook(altered)?;
        ensure!(altered_schema.lrecl_fixed == Some(5));
        fs::write(temp.path().join("simple.cpy"), altered)?;
        let mismatch = load_external_input(&manifest)
            .err()
            .context("same-LRECL altered copybook unexpectedly loaded")?;
        let mismatch = mismatch
            .downcast_ref::<ManifestIntegrityError>()
            .context("copybook mismatch did not return typed integrity error")?;
        ensure!(mismatch.artifact == IntegrityArtifact::Copybook);
        ensure!(mismatch.detail.contains("SHA-256 mismatch"));
        Ok(())
    }

    #[test]
    fn external_input_rejects_digest_count_and_truncation_mismatch() -> Result<()> {
        let (_temp, manifest) = copy_fixture("fixed-ascii.json")?;
        edit_manifest(&manifest, |root| {
            root.insert("dataset_sha256".to_string(), json!("0".repeat(64)));
        })?;
        let error = load_external_input(&manifest)
            .err()
            .context("digest mismatch unexpectedly loaded")?;
        ensure!(error.to_string().contains("SHA-256 mismatch"));

        let (_temp, manifest) = copy_fixture("fixed-ascii.json")?;
        edit_manifest(&manifest, |root| {
            root.insert("record_count".to_string(), json!(2));
        })?;
        let error = load_external_input(&manifest)
            .err()
            .context("count mismatch unexpectedly loaded")?;
        ensure!(error.to_string().contains("fixed dataset length"));

        let (temp, manifest) = copy_fixture("rdw-ascii.json")?;
        let dataset = temp.path().join("rdw-ascii.bin");
        let mut bytes = fs::read(&dataset)?;
        bytes.truncate(bytes.len().saturating_sub(1));
        fs::write(&dataset, &bytes)?;
        edit_manifest(&manifest, |root| {
            root.insert(
                "dataset_sha256".to_string(),
                json!(format!("{:x}", sha2::Sha256::digest(&bytes))),
            );
        })?;
        let error = load_external_input(&manifest)
            .err()
            .context("truncated RDW unexpectedly loaded")?;
        ensure!(error.to_string().contains("truncated RDW payload"));

        let (temp, manifest) = copy_fixture("rdw-ascii.json")?;
        let dataset = temp.path().join("rdw-ascii.bin");
        let mut bytes = fs::read(&dataset)?;
        bytes
            .get_mut(..2)
            .context("RDW fixture does not contain a length field")?
            .copy_from_slice(&4_u16.to_be_bytes());
        fs::write(&dataset, &bytes)?;
        edit_manifest(&manifest, |root| {
            root.insert(
                "dataset_sha256".to_string(),
                json!(format!("{:x}", sha2::Sha256::digest(&bytes))),
            );
        })?;
        let error = load_external_input(&manifest)
            .err()
            .context("heterogeneous RDW length unexpectedly loaded")?;
        ensure!(
            error
                .to_string()
                .contains("does not match manifest record_length")
        );
        Ok(())
    }

    #[test]
    fn external_input_rejects_huge_rdw_count_without_manifest_sized_allocation() -> Result<()> {
        let (_temp, manifest) = copy_fixture("rdw-ascii.json")?;
        edit_manifest(&manifest, |root| {
            root.insert("record_count".to_string(), json!(usize::MAX));
        })?;

        let error = load_external_input(&manifest)
            .err()
            .context("huge hostile RDW record count unexpectedly loaded")?;

        ensure!(error.to_string().contains("RDW dataset contains 1 records"));
        ensure!(error.to_string().contains(&usize::MAX.to_string()));
        Ok(())
    }

    #[test]
    fn external_input_rejects_non_file_and_traversing_paths() -> Result<()> {
        let (temp, manifest) = copy_fixture("fixed-ascii.json")?;
        fs::create_dir(temp.path().join("dataset-dir"))?;
        edit_manifest(&manifest, |root| {
            root.insert("dataset".to_string(), json!("dataset-dir"));
        })?;
        let error = load_external_input(&manifest)
            .err()
            .context("directory dataset unexpectedly loaded")?;
        ensure!(error.to_string().contains("not a regular file"));

        let (_temp, manifest) = copy_fixture("fixed-ascii.json")?;
        edit_manifest(&manifest, |root| {
            root.insert("dataset".to_string(), json!("../outside.bin"));
        })?;
        let error = load_external_input(&manifest)
            .err()
            .context("traversing dataset unexpectedly loaded")?;
        ensure!(error.to_string().contains("must not traverse"));
        Ok(())
    }

    #[cfg(unix)]
    #[test]
    fn external_input_rejects_symlink_components() -> Result<()> {
        use std::os::unix::fs::symlink;

        let (temp, manifest) = copy_fixture("fixed-ascii.json")?;
        let linked = temp.path().join("linked.bin");
        symlink(temp.path().join("fixed-ascii.bin"), &linked)?;
        edit_manifest(&manifest, |root| {
            root.insert("dataset".to_string(), json!("linked.bin"));
        })?;
        let error = load_external_input(&manifest)
            .err()
            .context("symlink dataset unexpectedly loaded")?;
        ensure!(error.to_string().contains("must not contain symlinks"));
        Ok(())
    }
}
