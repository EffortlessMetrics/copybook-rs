// SPDX-License-Identifier: AGPL-3.0-or-later
//! Offline manifest validation for future external-input benchmarks.

use std::fmt;
use std::fs;
use std::ops::Range;
use std::path::{Component, Path, PathBuf};

use anyhow::{Context, Result, bail, ensure};
use copybook_codec::{Codepage, DecodeOptions, RecordFormat};
use serde::Deserialize;
use sha2::{Digest, Sha256};

/// The only manifest schema version understood by this loader.
pub const EXTERNAL_INPUT_SCHEMA_VERSION: &str = "1.0.0";

/// Record framing declared by an external-input manifest.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
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
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
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
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
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
    reject_symlink_or_non_file(manifest_path, "manifest")?;
    let manifest_bytes = fs::read(manifest_path)
        .with_context(|| format!("failed to read manifest {}", manifest_path.display()))?;
    let manifest: ExternalInputManifest = serde_json::from_slice(&manifest_bytes)
        .with_context(|| format!("failed to parse manifest {}", manifest_path.display()))?;

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

    let base = manifest_path
        .parent()
        .filter(|path| !path.as_os_str().is_empty())
        .unwrap_or(Path::new("."));
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

    Ok(ValidatedExternalInput {
        manifest,
        copybook_source,
        dataset,
        payload_ranges,
    })
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
    use sha2::Digest;
    use tempfile::TempDir;

    use super::{
        ExternalCodepage, ExternalRecordFormat, IntegrityArtifact, ManifestIntegrityError,
        load_external_input,
    };

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
