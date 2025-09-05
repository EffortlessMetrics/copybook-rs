//! Verify command implementation

use crate::utils::{atomic_write, determine_exit_code};
use copybook_codec::{Codepage, DecodeOptions, RecordFormat, iter_records_from_file};
use copybook_core::{Error, ErrorCode, parse_copybook};
use std::{fs, path::PathBuf};
use tracing::info;

pub fn run(
    copybook: &PathBuf,
    input: &PathBuf,
    report: Option<PathBuf>,
    format: RecordFormat,
    codepage: Codepage,
) -> Result<i32, Box<dyn std::error::Error>> {
    info!("Verifying data file: {:?}", input);

    // Read copybook file
    let copybook_text = fs::read_to_string(copybook)?;

    // Parse copybook
    let schema = parse_copybook(&copybook_text)?;

    // Configure decode options
    let options = DecodeOptions::new()
        .with_format(format)
        .with_codepage(codepage);

    // Iterate through records, collecting errors
    let mut errors: Vec<Error> = Vec::new();
    let mut records: u64 = 0;

    if format == RecordFormat::RDW && schema.lrecl_fixed.is_some() {
        errors.push(Error::new(
            ErrorCode::CBKR221_RDW_UNDERFLOW,
            "RDW format specified but schema uses fixed-length records",
        ));
    } else {
        let mut iter = iter_records_from_file(input, &schema, &options)?;
        while let Some(result) = iter.next() {
            records += 1;
            if let Err(e) = result {
                errors.push(e.with_record(records));
            }
        }
    }

    println!("=== Verify Summary ===");
    println!("Records processed: {}", records);
    println!("Records with errors: {}", errors.len());
    println!("Warnings: 0");

    if let Some(report_path) = report {
        let errors_json: Vec<_> = errors
            .iter()
            .map(|e| {
                let mut obj = serde_json::json!({
                    "code": format!("{}", e.code),
                    "message": e.message,
                });
                if let Some(ctx) = &e.context {
                    let mut ctx_map = serde_json::Map::new();
                    if let Some(r) = ctx.record_index {
                        ctx_map.insert("record".to_string(), serde_json::json!(r));
                    }
                    if let Some(f) = &ctx.field_path {
                        ctx_map.insert("field".to_string(), serde_json::json!(f));
                    }
                    if let Some(b) = ctx.byte_offset {
                        ctx_map.insert("byte".to_string(), serde_json::json!(b));
                    }
                    if let Some(l) = ctx.line_number {
                        ctx_map.insert("line".to_string(), serde_json::json!(l));
                    }
                    if let Some(d) = &ctx.details {
                        ctx_map.insert("details".to_string(), serde_json::json!(d));
                    }
                    if let Some(obj_map) = obj.as_object_mut() {
                        obj_map.insert("context".to_string(), serde_json::Value::Object(ctx_map));
                    }
                }
                obj
            })
            .collect();

        let report_json = serde_json::json!({
            "file": input,
            "format": format,
            "codepage": codepage,
            "records": records,
            "errors": errors_json,
            "warnings": Vec::<serde_json::Value>::new(),
        });

        let report_content = serde_json::to_vec_pretty(&report_json)?;
        atomic_write(report_path, |writer| {
            std::io::Write::write_all(writer, &report_content)
        })?;
    }

    info!("Verify completed successfully");
    let exit_code = determine_exit_code(false, !errors.is_empty());
    Ok(exit_code)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn test_verify_success() {
        let dir = tempdir().unwrap();
        let copybook_path = dir.path().join("schema.cpy");
        let data_path = dir.path().join("data.bin");

        fs::write(&copybook_path, "01 RECORD.\n   05 FIELD1 PIC X(3).\n").unwrap();
        fs::write(&data_path, b"ABC").unwrap();

        let code = run(
            &copybook_path,
            &data_path,
            None,
            RecordFormat::Fixed,
            Codepage::ASCII,
        )
        .unwrap();
        assert_eq!(code, 0);
    }

    #[test]
    fn test_verify_rdw_mismatch_errors() {
        let dir = tempdir().unwrap();
        let copybook_path = dir.path().join("schema.cpy");
        let data_path = dir.path().join("data.bin");

        fs::write(&copybook_path, "01 RECORD.\n   05 FIELD1 PIC X(3).\n").unwrap();
        // RDW header claims length 7 (header + 3 bytes) but only 1 byte payload provided
        fs::write(&data_path, [0x00, 0x07, 0x00, 0x00, b'A']).unwrap();

        let code = run(
            &copybook_path,
            &data_path,
            None,
            RecordFormat::RDW,
            Codepage::ASCII,
        )
        .unwrap();
        assert_eq!(code, 1);
    }
}
