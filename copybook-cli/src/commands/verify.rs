//! Verify command implementation

//! Verify command implementation

use crate::utils::{atomic_write, determine_exit_code};
use copybook_codec::{Codepage, DecodeOptions, RecordFormat, iter_records_from_file};
use copybook_core::{Error, ErrorCode, parse_copybook};
use serde_json::json;
use std::fs;
use std::path::PathBuf;
use tracing::info;

/// Verify a data file against a copybook schema
///
/// Returns an exit code following the normative specification:
/// - `0` when the file is valid (warnings allowed)
/// - `1` when any record-level errors are detected
pub fn run(
    copybook: &PathBuf,
    input: &PathBuf,
    report: Option<PathBuf>,
    format: RecordFormat,
    codepage: Codepage,
) -> Result<i32, Box<dyn std::error::Error>> {
    info!("Verifying data file: {:?}", input);

    // Read and parse copybook
    let copybook_text = fs::read_to_string(copybook)?;
    let schema = parse_copybook(&copybook_text)?;

    // Configure decode options
    let options = DecodeOptions::default()
        .with_format(format)
        .with_codepage(codepage);

    // Collect errors
    let mut errors: Vec<Error> = Vec::new();

    // Basic file-level validation for fixed records
    if format == RecordFormat::Fixed {
        if let Some(lrecl) = schema.lrecl_fixed {
            let file_len = fs::metadata(input)?.len();
            if file_len % u64::from(lrecl) != 0 {
                let record_index = file_len / u64::from(lrecl) + 1;
                errors.push(
                    Error::new(
                        ErrorCode::CBKD301_RECORD_TOO_SHORT,
                        "File length is not a multiple of record length",
                    )
                    .with_record(record_index),
                );
            }
        }
    }

    // Iterate through records collecting errors
    let mut iter = iter_records_from_file(input, &schema, &options)?;

    while let Some(result) = iter.next() {
        if let Err(e) = result {
            let idx = iter.current_record_index();
            errors.push(e.with_record(idx));
        }
    }

    let warnings: Vec<Error> = Vec::new();

    // Print summary
    println!("=== Verification Summary ===");
    println!("Records processed: {}", iter.current_record_index());
    println!("Errors: {}", errors.len());
    println!("Warnings: {}", warnings.len());

    // Generate optional JSON report
    if let Some(report_path) = report {
        let errors_json: Vec<_> = errors
            .iter()
            .map(|e| {
                let mut obj = json!({
                    "code": format!("{}", e.code),
                    "message": e.message,
                });
                if let Some(ctx) = &e.context {
                    let mut ctx_map = serde_json::Map::new();
                    if let Some(r) = ctx.record_index {
                        ctx_map.insert("record".into(), json!(r));
                    }
                    if let Some(f) = &ctx.field_path {
                        ctx_map.insert("field".into(), json!(f));
                    }
                    if let Some(b) = ctx.byte_offset {
                        ctx_map.insert("byte".into(), json!(b));
                    }
                    if let Some(l) = ctx.line_number {
                        ctx_map.insert("line".into(), json!(l));
                    }
                    if let Some(d) = &ctx.details {
                        ctx_map.insert("details".into(), json!(d));
                    }
                    if let Some(obj_map) = obj.as_object_mut() {
                        obj_map.insert("context".into(), serde_json::Value::Object(ctx_map));
                    }
                }
                obj
            })
            .collect();

        let report_json = json!({
            "file": input,
            "format": format,
            "codepage": codepage,
            "errors": errors_json,
            "warnings": Vec::<serde_json::Value>::new(),
        });

        let content = serde_json::to_string_pretty(&report_json)?;
        atomic_write(report_path, |writer| writer.write_all(content.as_bytes()))?;
    }

    let exit_code = determine_exit_code(false, !errors.is_empty());
    info!("Verify completed: {} errors", errors.len());
    Ok(exit_code)
}

#[cfg(test)]
mod tests {
    use super::*;
    use copybook_codec::{EncodeOptions, encode_record};
    use serde_json::json;
    use tempfile::tempdir;

    fn sample_copybook() -> &'static str {
        "01 RECORD.\n   05 FIELD-A PIC X(5).\n   05 FIELD-B PIC X(5)."
    }

    #[test]
    fn verify_succeeds_on_valid_file() {
        let dir = tempdir().unwrap();
        let copybook_path = dir.path().join("test.cpy");
        fs::write(&copybook_path, sample_copybook()).unwrap();

        let schema = parse_copybook(sample_copybook()).unwrap();
        let json_record = json!({ "FIELD-A": "HELLO", "FIELD-B": "WORLD" });
        let enc_opts = EncodeOptions::default()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let data = encode_record(&schema, &json_record, &enc_opts).unwrap();
        let data_path = dir.path().join("data.bin");
        fs::write(&data_path, &data).unwrap();

        let report_path = dir.path().join("report.json");
        let exit = run(
            &copybook_path,
            &data_path,
            Some(report_path.clone()),
            RecordFormat::Fixed,
            Codepage::ASCII,
        )
        .unwrap();

        assert_eq!(exit, 0);
        let report: serde_json::Value =
            serde_json::from_str(&fs::read_to_string(report_path).unwrap()).unwrap();
        assert!(report["errors"].as_array().unwrap().is_empty());
    }

    #[test]
    fn verify_reports_error_on_short_record() {
        let dir = tempdir().unwrap();
        let copybook_path = dir.path().join("test.cpy");
        fs::write(&copybook_path, sample_copybook()).unwrap();

        // Create data shorter than schema's LRECL (10 bytes)
        let data_path = dir.path().join("data.bin");
        fs::write(&data_path, b"SHORT").unwrap();
        assert_eq!(fs::metadata(&data_path).unwrap().len(), 5);

        let report_path = dir.path().join("report.json");
        let exit = run(
            &copybook_path,
            &data_path,
            Some(report_path.clone()),
            RecordFormat::Fixed,
            Codepage::ASCII,
        )
        .unwrap();

        assert_eq!(exit, 1);
        let report: serde_json::Value =
            serde_json::from_str(&fs::read_to_string(report_path).unwrap()).unwrap();
        assert!(report["errors"].as_array().unwrap().len() > 0);
    }
}
