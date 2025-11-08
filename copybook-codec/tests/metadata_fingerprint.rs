#![allow(clippy::cast_precision_loss)]

use anyhow::{Context, Result};
use copybook_core::{Field, FieldKind, Schema};
use serde_json::Value;

type TestResult = Result<()>;

struct DummyWriter {
    json_buffer: String,
}

impl DummyWriter {
    fn new() -> Self {
        Self {
            json_buffer: String::new(),
        }
    }

    fn write_record_metadata(
        &mut self,
        schema_fingerprint: &str,
        byte_offset: u64,
        record_length: usize,
        first_field: &mut bool,
    ) {
        if !*first_field {
            self.json_buffer.push(',');
        }
        *first_field = false;

        self.json_buffer.push_str("\"schema_fingerprint\":");
        self.write_json_string_to_buffer(schema_fingerprint);

        self.json_buffer.push_str(",\"offset\":");
        self.write_json_number_to_buffer(byte_offset as f64);

        self.json_buffer.push_str(",\"length\":");
        self.write_json_number_to_buffer(record_length as f64);
    }

    fn write_json_string_to_buffer(&mut self, s: &str) {
        self.json_buffer.push('"');
        for c in s.chars() {
            match c {
                '"' => self.json_buffer.push_str("\\\""),
                '\\' => self.json_buffer.push_str("\\\\"),
                '\n' => self.json_buffer.push_str("\\n"),
                '\r' => self.json_buffer.push_str("\\r"),
                '\t' => self.json_buffer.push_str("\\t"),
                c if c.is_control() => {
                    use std::fmt::Write;
                    let _ = write!(self.json_buffer, "\\u{:04x}", c as u32);
                }
                c => self.json_buffer.push(c),
            }
        }
        self.json_buffer.push('"');
    }

    fn write_json_number_to_buffer(&mut self, num: f64) {
        use std::fmt::Write;
        let _ = write!(self.json_buffer, "{num}");
    }
}

#[test]
fn test_streaming_metadata_fingerprint_matches_schema() -> TestResult {
    let field = Field {
        path: "ROOT.A".to_string(),
        name: "A".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 1 },
        offset: 0,
        len: 1,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    };
    let schema = Schema::from_fields(vec![field]);

    let mut writer = DummyWriter::new();
    let mut first = true;
    writer.write_record_metadata(schema.fingerprint.as_str(), 0, 0, &mut first);

    let json_str = format!("{{{}}}", writer.json_buffer);
    let value: Value = serde_json::from_str(&json_str).context("parsing metadata JSON")?;

    let fingerprint = value
        .get("schema_fingerprint")
        .and_then(Value::as_str)
        .context("missing schema_fingerprint field")?;
    assert_eq!(fingerprint, schema.fingerprint);

    Ok(())
}
