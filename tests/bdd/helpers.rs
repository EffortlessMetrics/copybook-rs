use copybook_codec::{
    encode_jsonl_to_file, Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode,
    RecordFormat,
};
use copybook_core::Error;
use serde_json::{Map, Value};
use std::io::Cursor;

use crate::world::CopybookWorld;

pub(crate) fn default_ascii_decode_options() -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(Codepage::ASCII)
        .with_format(RecordFormat::Fixed)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_filler(false)
        .with_emit_meta(true)
        .with_emit_raw(RawMode::Off)
        .with_strict_mode(false)
        .with_max_errors(None)
        .with_unmappable_policy(copybook_codec::UnmappablePolicy::Error)
        .with_threads(1)
        .with_preserve_zoned_encoding(false)
        .with_preferred_zoned_encoding(copybook_codec::ZonedEncodingFormat::Auto)
}

pub(crate) fn default_ascii_encode_options() -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(Codepage::ASCII)
        .with_format(RecordFormat::Fixed)
        .with_use_raw(false)
        .with_bwz_encode(false)
        .with_strict_mode(false)
        .with_max_errors(None)
        .with_threads(1)
        .with_coerce_numbers(true)
        .with_zoned_encoding_override(None)
}

pub(crate) fn build_binary_for_all_leaf_fields(world: &mut CopybookWorld) -> Vec<u8> {
    if !world.ensure_schema_and_return() {
        return vec![];
    }
    let schema = world.schema();

    // Use the fixed record length if available, otherwise compute from fields
    let record_len = schema.lrecl_fixed.unwrap_or_else(|| {
        schema
            .all_fields()
            .iter()
            .map(|f| if f.children.is_empty() { f.len } else { 0 })
            .max()
            .unwrap_or(0)
    }) as usize;

    // Create binary data filled with ASCII spaces
    vec![b' '; record_len.max(1)]
}

pub(crate) fn encode_from_payload(
    payload: &Map<String, Value>,
    world: &mut CopybookWorld,
) -> Result<Vec<u8>, Error> {
    if !world.ensure_schema_and_return() {
        return Err(world.error.clone().expect("Parse error should be set"));
    }
    world.ensure_encode_options();

    let payload_text = serde_json::to_string(&Value::Object(payload.clone())).unwrap();
    let mut output = Vec::new();

    encode_jsonl_to_file(
        world.schema(),
        Cursor::new(payload_text.as_bytes()),
        &mut output,
        world
            .encode_options
            .as_ref()
            .expect("Encode options not set"),
    )?;

    Ok(output)
}

pub(crate) fn parse_binary_literal(data: &str) -> Vec<u8> {
    let mut result = Vec::new();
    let bytes = data.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if i + 3 < bytes.len() && bytes[i] == b'\\' && bytes[i + 1] == b'x' {
            // Parse \xHH hex escape
            let hi = bytes[i + 2];
            let lo = bytes[i + 3];
            if let (Some(h), Some(l)) = (hex_digit(hi), hex_digit(lo)) {
                result.push(h << 4 | l);
                i += 4;
                continue;
            }
        }
        result.push(bytes[i]);
        i += 1;
    }
    result
}

pub(crate) fn hex_digit(b: u8) -> Option<u8> {
    match b {
        b'0'..=b'9' => Some(b - b'0'),
        b'a'..=b'f' => Some(b - b'a' + 10),
        b'A'..=b'F' => Some(b - b'A' + 10),
        _ => None,
    }
}

pub(crate) fn does_path_match_counter(occurs: &copybook_core::Occurs, counter: &str) -> bool {
    match occurs {
        copybook_core::Occurs::ODO { counter_path, .. } => {
            counter_path.eq_ignore_ascii_case(counter) || counter_path.ends_with(counter)
        }
        copybook_core::Occurs::Fixed { .. } => false,
    }
}

pub(crate) fn assert_field_level_depth(field: &copybook_core::Field) {
    for child in &field.children {
        assert!(
            child.level > field.level,
            "Expected child field '{}' level {} to be greater than parent '{}' level {}",
            child.name,
            child.level,
            field.name,
            field.level
        );
        assert_field_level_depth(child);
    }
}

pub(crate) fn json_value_for_field<'a>(record: &'a Value, field_name: &str) -> Option<&'a Value> {
    let obj = record.as_object()?;
    obj.get(field_name)
        .or_else(|| obj.get(&field_name.to_ascii_uppercase()))
        .or_else(|| obj.get(&field_name.to_ascii_lowercase()))
}

pub(crate) fn field_value_as_string(value: &Value) -> Option<String> {
    match value {
        Value::String(value) => Some(value.clone()),
        Value::Number(value) => Some(value.to_string()),
        Value::Bool(value) => Some(value.to_string()),
        Value::Null => Some(String::new()),
        _ => None,
    }
}

pub(crate) fn collect_string_values(value: &Value) -> Vec<String> {
    let mut result = Vec::new();
    match value {
        Value::Object(object) => {
            for v in object.values() {
                result.extend(collect_string_values(v));
            }
        }
        Value::Array(values) => {
            for v in values {
                result.extend(collect_string_values(v));
            }
        }
        Value::String(s) => result.push(s.clone()),
        Value::Number(number) => result.push(number.to_string()),
        Value::Bool(b) => result.push(b.to_string()),
        Value::Null => {}
    }
    result
}
