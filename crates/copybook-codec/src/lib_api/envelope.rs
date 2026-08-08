// SPDX-License-Identifier: AGPL-3.0-or-later
//! JSON envelope construction for decoded records.

use crate::JSON_SCHEMA_VERSION;
use crate::options::{DecodeOptions, ZonedEncodingFormat};
use copybook_core::Schema;
use serde_json::Value;

pub(super) struct RecordMetadata {
    pub(super) length: usize,
    pub(super) offset: Option<u64>,
}

/// Recursively flatten hierarchical fields into a target map so that leaf
/// field names are accessible at the root level for backward compatibility.
fn flatten_fields_into(
    source: &serde_json::Map<String, Value>,
    target: &mut serde_json::Map<String, Value>,
) {
    for (key, value) in source {
        if let Value::Object(nested) = value {
            // Recurse into group objects to flatten their children
            flatten_fields_into(nested, target);
        } else {
            target.insert(key.clone(), value.clone());
        }
    }
}

/// Build a standard JSON envelope for a decoded COBOL record.
///
/// Wraps the decoded fields with metadata like schema version, record index,
/// and codepage. Optionally includes extended metadata if `options.emit_meta` is true.
pub(super) fn build_json_envelope(
    fields: serde_json::Map<String, Value>,
    schema: &Schema,
    options: &DecodeOptions,
    record_index: u64,
    record_metadata: &RecordMetadata,
    raw_b64: Option<String>,
    encoding_metadata: Vec<(String, ZonedEncodingFormat)>,
) -> Value {
    let mut root = serde_json::Map::new();

    root.insert(
        String::from("schema"),
        Value::String(JSON_SCHEMA_VERSION.into()),
    );
    root.insert(
        String::from("record_index"),
        Value::Number(serde_json::Number::from(record_index)),
    );

    let codepage = options.codepage.to_string();
    root.insert(String::from("codepage"), Value::String(codepage));

    flatten_fields_into(&fields, &mut root);
    root.insert(String::from("fields"), Value::Object(fields));

    if options.emit_meta {
        if !schema.fingerprint.is_empty() {
            root.insert(
                String::from("schema_fingerprint"),
                Value::String(schema.fingerprint.clone()),
            );
            root.insert(
                String::from("__schema_id"),
                Value::String(schema.fingerprint.clone()),
            );
        }
        root.insert(
            String::from("length"),
            Value::Number(serde_json::Number::from(record_metadata.length)),
        );
        root.insert(
            String::from("__record_index"),
            Value::Number(serde_json::Number::from(record_index)),
        );
        root.insert(
            String::from("__length"),
            Value::Number(serde_json::Number::from(record_metadata.length)),
        );
        if let Some(offset) = record_metadata.offset {
            root.insert(
                String::from("offset"),
                Value::Number(serde_json::Number::from(offset)),
            );
        }
    }

    if let Some(raw) = raw_b64 {
        root.insert(String::from("raw_b64"), Value::String(raw.clone()));
        root.insert(String::from("__raw_b64"), Value::String(raw));
    }

    if options.preserve_zoned_encoding && !encoding_metadata.is_empty() {
        let mut meta_map = serde_json::Map::new();
        for (field_name, format) in encoding_metadata {
            let format_text = format.to_string();
            meta_map.insert(field_name, Value::String(format_text));
        }
        root.insert(String::from("_encoding_metadata"), Value::Object(meta_map));
    }

    Value::Object(root)
}
