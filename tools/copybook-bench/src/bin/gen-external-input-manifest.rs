// SPDX-License-Identifier: AGPL-3.0-or-later
//! Generate a schema-valid manifest for an existing external-input dataset.

use std::collections::BTreeMap;
use std::env;
use std::ffi::OsString;
use std::fs;
use std::path::{Component, Path, PathBuf};

use anyhow::{Context, Result, bail, ensure};
use serde_json::{Value, json};
use sha2::{Digest, Sha256};

const USAGE: &str = "Usage: gen-external-input-manifest --copybook PATH --dataset PATH \\
  --format fixed|rdw --codepage ascii|cp037|cp273|cp500|cp1047|cp1140 \\
  --workload display-heavy|comp3-heavy|mixed --record-length BYTES --output PATH";

fn main() -> Result<()> {
    let arguments = Arguments::parse(env::args_os().skip(1))?;
    let output = arguments.output;
    let manifest_dir = output
        .parent()
        .filter(|path| !path.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."))
        .canonicalize()
        .with_context(|| {
            format!(
                "failed to resolve manifest directory for {}",
                output.display()
            )
        })?;
    let output_name = output
        .file_name()
        .filter(|name| !name.is_empty())
        .context("output path must name a file")?;
    ensure!(
        !matches!(output_name.to_str(), Some("." | "..")),
        "output path must name a file"
    );
    let output_path = manifest_dir.join(output_name);
    let copybook = read_input(&arguments.copybook, &manifest_dir, "copybook")?;
    let dataset = read_input(&arguments.dataset, &manifest_dir, "dataset")?;
    ensure!(!copybook.bytes.is_empty(), "copybook must not be empty");
    ensure!(!dataset.bytes.is_empty(), "dataset must not be empty");
    if output_path.exists() {
        let metadata = fs::symlink_metadata(&output_path)
            .with_context(|| format!("failed to inspect output {}", output_path.display()))?;
        ensure!(
            !metadata.file_type().is_symlink(),
            "output must not be a symlink"
        );
        let existing_output = output_path
            .canonicalize()
            .with_context(|| format!("failed to resolve output {}", output_path.display()))?;
        ensure!(
            existing_output != copybook.path && existing_output != dataset.path,
            "output must not overwrite the copybook or dataset"
        );
    }

    let record_count = match arguments.format.as_str() {
        "fixed" => validate_fixed(&dataset.bytes, arguments.record_length)?,
        "rdw" => validate_rdw(&dataset.bytes, arguments.record_length)?,
        format => bail!("unsupported record format '{format}'"),
    };
    let mut manifest = BTreeMap::<String, Value>::new();
    manifest.insert("codepage".to_owned(), json!(arguments.codepage));
    manifest.insert(
        "copybook".to_owned(),
        json!(relative_name(&copybook.path, &manifest_dir, "copybook")?),
    );
    manifest.insert(
        "copybook_sha256".to_owned(),
        json!(sha256_hex(&copybook.bytes)),
    );
    manifest.insert(
        "dataset".to_owned(),
        json!(relative_name(&dataset.path, &manifest_dir, "dataset")?),
    );
    manifest.insert(
        "dataset_sha256".to_owned(),
        json!(sha256_hex(&dataset.bytes)),
    );
    manifest.insert("record_count".to_owned(), json!(record_count));
    manifest.insert("record_format".to_owned(), json!(arguments.format));
    manifest.insert("record_length".to_owned(), json!(arguments.record_length));
    manifest.insert("schema_version".to_owned(), json!("1.0.0"));
    manifest.insert("workload".to_owned(), json!(arguments.workload));

    let encoded = serde_json::to_vec_pretty(&manifest).context("failed to encode manifest")?;
    fs::write(&output_path, encoded)
        .with_context(|| format!("failed to write manifest {}", output_path.display()))?;
    Ok(())
}

struct Arguments {
    copybook: PathBuf,
    dataset: PathBuf,
    format: String,
    codepage: String,
    workload: String,
    record_length: usize,
    output: PathBuf,
}

impl Arguments {
    fn parse<I>(mut values: I) -> Result<Self>
    where
        I: Iterator<Item = OsString>,
    {
        let mut parsed = BTreeMap::new();
        while let Some(raw_flag) = values.next() {
            let flag = argument_text(raw_flag, "flag")?;
            ensure!(
                flag.starts_with("--"),
                "unexpected argument '{flag}'\n\n{USAGE}"
            );
            let value = argument_text(
                values
                    .next()
                    .with_context(|| format!("missing value for {flag}\n\n{USAGE}"))?,
                "argument value",
            )?;
            ensure!(
                !value.starts_with("--"),
                "missing value for {flag}\n\n{USAGE}"
            );
            ensure!(
                parsed.insert(flag.clone(), value).is_none(),
                "duplicate argument {flag}"
            );
        }
        let take = |flag: &str| {
            parsed
                .get(flag)
                .cloned()
                .with_context(|| format!("missing {flag}\n\n{USAGE}"))
        };
        let copybook = PathBuf::from(take("--copybook")?);
        let dataset = PathBuf::from(take("--dataset")?);
        let format = take("--format")?;
        let codepage = take("--codepage")?;
        ensure!(
            matches!(
                codepage.as_str(),
                "ascii" | "cp037" | "cp273" | "cp500" | "cp1047" | "cp1140"
            ),
            "unsupported codepage '{codepage}'"
        );
        let workload = take("--workload")?;
        ensure!(
            matches!(workload.as_str(), "display-heavy" | "comp3-heavy" | "mixed"),
            "unsupported workload '{workload}'"
        );
        let record_length = take("--record-length")?
            .parse::<usize>()
            .context("record length must be a positive integer")?;
        ensure!(
            (1..=usize::from(u16::MAX)).contains(&record_length),
            "record length must be between 1 and 65535"
        );
        let output = PathBuf::from(take("--output")?);
        ensure!(parsed.len() == 7, "unknown argument supplied\n\n{USAGE}");
        Ok(Self {
            copybook,
            dataset,
            format,
            codepage,
            workload,
            record_length,
            output,
        })
    }
}

fn argument_text(value: OsString, label: &str) -> Result<String> {
    value
        .into_string()
        .map_err(|_| anyhow::anyhow!("{label} must be valid UTF-8"))
}

struct InputFile {
    path: PathBuf,
    bytes: Vec<u8>,
}

fn read_input(path: &Path, manifest_dir: &Path, label: &str) -> Result<InputFile> {
    let path = if path.is_absolute() {
        path.to_path_buf()
    } else {
        manifest_dir.join(path)
    };
    let canonical = path
        .canonicalize()
        .with_context(|| format!("failed to resolve {label} {}", path.display()))?;
    ensure!(
        canonical.starts_with(manifest_dir),
        "{label} must be inside the manifest directory"
    );
    let bytes = fs::read(&canonical).with_context(|| format!("failed to read {label}"))?;
    Ok(InputFile {
        path: canonical,
        bytes,
    })
}

fn relative_name(path: &Path, manifest_dir: &Path, label: &str) -> Result<String> {
    let relative = path
        .strip_prefix(manifest_dir)
        .with_context(|| format!("{label} is outside the manifest directory"))?;
    ensure!(
        !relative.as_os_str().is_empty(),
        "{label} path must not be empty"
    );
    ensure!(
        relative
            .components()
            .all(|component| matches!(component, Component::Normal(_))),
        "{label} path must not contain traversal components"
    );
    let mut parts = Vec::new();
    for component in relative.components() {
        let Component::Normal(part) = component else {
            bail!("{label} path must not contain traversal components");
        };
        parts.push(
            part.to_str()
                .with_context(|| format!("{label} path is not valid UTF-8"))?,
        );
    }
    Ok(parts.join("/"))
}

fn validate_fixed(dataset: &[u8], record_length: usize) -> Result<usize> {
    ensure!(
        dataset.len().is_multiple_of(record_length),
        "fixed dataset length {} is not divisible by record length {record_length}",
        dataset.len()
    );
    let count = dataset.len() / record_length;
    ensure!(count > 0, "dataset must contain at least one record");
    Ok(count)
}

fn validate_rdw(dataset: &[u8], record_length: usize) -> Result<usize> {
    let mut offset = 0;
    let mut count = 0;
    while offset < dataset.len() {
        let header_end = offset
            .checked_add(4)
            .context("RDW header offset overflow")?;
        let header = dataset
            .get(offset..header_end)
            .context("dataset contains a truncated RDW header")?;
        let declared = usize::from(u16::from_be_bytes([header[0], header[1]]));
        ensure!(
            declared == record_length,
            "RDW payload length {declared} does not match record length {record_length}"
        );
        let payload_end = header_end
            .checked_add(declared)
            .context("RDW payload offset overflow")?;
        ensure!(
            payload_end <= dataset.len(),
            "dataset contains a truncated RDW payload"
        );
        offset = payload_end;
        count += 1;
    }
    ensure!(count > 0, "dataset must contain at least one record");
    Ok(count)
}

fn sha256_hex(bytes: &[u8]) -> String {
    format!("{:x}", Sha256::digest(bytes))
}

#[cfg(test)]
mod tests {
    #[cfg(unix)]
    use super::argument_text;
    #[cfg(unix)]
    use std::ffi::OsString;

    #[cfg(unix)]
    #[test]
    fn encoding_non_utf8_argument_is_rejected() -> anyhow::Result<()> {
        use std::os::unix::ffi::OsStringExt;

        let value = OsString::from_vec(vec![0xff]);
        anyhow::ensure!(argument_text(value, "path").is_err());
        Ok(())
    }
}
