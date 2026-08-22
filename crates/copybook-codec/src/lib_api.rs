// SPDX-License-Identifier: AGPL-3.0-or-later
//! Core decode / encode API for COBOL binary data.
//!
//! | Function | Direction | Scope |
//! |----------|-----------|-------|
//! | [`decode_record`] | Binary → JSON | Single record |
//! | [`encode_record`] | JSON → Binary | Single record |
//! | [`decode_file_to_jsonl`] | Binary → JSONL | Whole file |
//! | [`encode_jsonl_to_file`] | JSONL → Binary | Whole file |
#![allow(clippy::missing_inline_in_public_items)]

use crate::options::{DecodeOptions, EncodeOptions, RecordFormat, ZonedEncodingFormat};
use crate::zoned_overpunch::ZeroSignPolicy;
use base64::Engine;
use copybook_core::{Error, ErrorCode, Result, Schema};
use serde_json::Value;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::io::{BufRead, BufReader, Read, Write};
use std::sync::Arc;
use tracing::info;

mod envelope;
mod run_summary;
mod telemetry;
mod warnings;

use envelope::{RecordMetadata, build_json_envelope};
pub use run_summary::{MAX_CAPTURED_FAILURES, RecordFailure, RunSummary};
pub use warnings::increment_warning_counter;
use warnings::{reset_warning_counter, warning_count};

const MAX_WORKERS: usize = 64;

#[derive(Clone, Copy)]
enum RawCapture {
    Record,
    RecordRdw,
}

impl RawCapture {
    const fn as_str(self) -> &'static str {
        match self {
            Self::Record => "record",
            Self::RecordRdw => "record+rdw",
        }
    }
}

struct RawRecord {
    b64: String,
    capture: RawCapture,
}

fn parse_raw_rdw_frame(frame: &[u8]) -> Result<(u16, &[u8])> {
    let (raw_header, raw_payload) = frame.split_at_checked(4).ok_or_else(|| {
        Error::new(
            ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
            format!(
                "Raw RDW record is {} bytes; expected at least a 4-byte header",
                frame.len()
            ),
        )
    })?;
    let header_bytes: [u8; 4] = raw_header.try_into().map_err(|_| {
        Error::new(
            ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
            "Raw RDW record does not contain a complete 4-byte header",
        )
    })?;
    let header = copybook_rdw::RdwHeader::from_bytes(header_bytes);
    let declared_payload_len = usize::from(header.length());
    if declared_payload_len != raw_payload.len() {
        return Err(Error::new(
            ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
            format!(
                "Raw RDW header declares {declared_payload_len} payload bytes, but {} bytes follow",
                raw_payload.len()
            ),
        ));
    }
    Ok((header.reserved(), raw_payload))
}

fn validate_captured_raw_rdw(frame: &[u8], expected_payload: &[u8]) -> Result<()> {
    let (_, raw_payload) = parse_raw_rdw_frame(frame)?;
    if raw_payload != expected_payload {
        return Err(Error::new(
            ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
            "Raw RDW payload does not match the decoded record payload",
        ));
    }
    Ok(())
}

fn captured_raw_record(
    data: &[u8],
    supplied_raw: Option<&[u8]>,
    mode: crate::options::RawMode,
) -> Result<Option<RawRecord>> {
    let (bytes, capture) = match mode {
        crate::options::RawMode::Off | crate::options::RawMode::Field => return Ok(None),
        crate::options::RawMode::Record => (data, RawCapture::Record),
        crate::options::RawMode::RecordRDW => {
            let frame = supplied_raw.ok_or_else(|| {
                Error::new(
                    ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
                    "RawMode::RecordRDW requires an RDW header plus payload",
                )
            })?;
            validate_captured_raw_rdw(frame, data)?;
            (frame, RawCapture::RecordRdw)
        }
    };
    Ok(Some(RawRecord {
        b64: base64::engine::general_purpose::STANDARD.encode(bytes),
        capture,
    }))
}

/// Decode one fixed-size COBOL record into the public JSON envelope.
///
/// This uses the supplied schema and decode options, returning the same
/// envelope shape as the streaming decode APIs for a single record.
///
/// # Errors
/// Returns an error if `data` cannot be decoded according to `schema` and
/// `options`, including field conversion errors, invalid record lengths, or
/// unsupported encoding combinations.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_record(schema: &Schema, data: &[u8], options: &DecodeOptions) -> Result<Value> {
    decode_record_with_raw_data(schema, data, options, None, 0)
}

/// High-performance decode using reusable scratch buffers
///
/// This optimized version reuses memory buffers across calls to minimize allocations,
/// providing significant performance improvements for high-throughput scenarios.
///
/// # Arguments
///
/// * `schema` - The parsed copybook schema
/// * `data` - The binary record data
/// * `options` - Decoding options
/// * `scratch` - Reusable scratch buffers for optimization
///
/// # Examples
///
/// ```
/// use copybook_core::parse_copybook;
/// use copybook_codec::{decode_record_with_scratch, DecodeOptions};
/// use copybook_codec::runtime::ScratchBuffers;
/// use copybook_codec::options::{Codepage, RecordFormat};
///
/// let schema = parse_copybook("01 FLD PIC X(5).").unwrap();
/// let options = DecodeOptions::new()
///     .with_codepage(Codepage::ASCII)
///     .with_format(RecordFormat::Fixed);
/// let mut scratch = ScratchBuffers::new();
///
/// // Decode multiple records reusing the same scratch buffers
/// for record_data in [b"AAAAA", b"BBBBB", b"CCCCC"] {
///     let json = decode_record_with_scratch(&schema, record_data, &options, &mut scratch).unwrap();
///     assert!(json["fields"]["FLD"].is_string());
/// }
/// ```
///
/// # Errors
/// Returns an error if the data cannot be decoded according to the schema.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_record_with_scratch(
    schema: &Schema,
    data: &[u8],
    options: &DecodeOptions,
    scratch: &mut crate::memory::ScratchBuffers,
) -> Result<Value> {
    decode_record_with_scratch_and_raw(schema, data, options, None, 0, None, scratch)
}

/// Decode a record with optional raw data and scratch buffers for maximum performance
fn decode_record_with_scratch_and_raw(
    schema: &Schema,
    data: &[u8],
    options: &DecodeOptions,
    raw_data: Option<&[u8]>,
    record_index: u64,
    record_offset: Option<u64>,
    scratch: &mut crate::memory::ScratchBuffers,
) -> Result<Value> {
    use serde_json::Map;

    let mut fields_map = Map::new();
    let mut encoding_acc = Vec::new();
    let record_raw = captured_raw_record(data, raw_data, options.emit_raw)?;

    process_fields_recursive_with_scratch(
        &schema.fields,
        data,
        &mut fields_map,
        options,
        scratch,
        record_index,
        &mut encoding_acc,
    )?;

    Ok(build_json_envelope(
        fields_map,
        schema,
        options,
        record_index,
        &RecordMetadata {
            length: data.len(),
            offset: record_offset,
        },
        record_raw,
        encoding_acc,
    ))
}

/// Decode a record with optional raw data for RDW format
///
/// # Errors
/// Returns an error if field decoding fails or the raw payload is inconsistent with the schema.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_record_with_raw_data(
    schema: &Schema,
    data: &[u8],
    options: &DecodeOptions,
    raw_data_with_header: Option<&[u8]>,
    record_index: u64,
) -> Result<Value> {
    decode_record_with_raw_data_at_offset(
        schema,
        data,
        options,
        raw_data_with_header,
        record_index,
        None,
    )
}

fn decode_record_with_raw_data_at_offset(
    schema: &Schema,
    data: &[u8],
    options: &DecodeOptions,
    raw_data_with_header: Option<&[u8]>,
    record_index: u64,
    record_offset: Option<u64>,
) -> Result<Value> {
    use serde_json::Map;

    // Validate whole-record framing before field decoding so a malformed
    // RecordRDW capture cannot be masked by an unrelated field error.
    let record_raw = captured_raw_record(data, raw_data_with_header, options.emit_raw)?;

    let mut fields_map = Map::new();
    let mut scratch_buffers: Option<crate::memory::ScratchBuffers> = None;
    let mut encoding_acc = Vec::new();

    process_fields_recursive(
        &schema.fields,
        data,
        &mut fields_map,
        options,
        &mut scratch_buffers,
        record_index,
        &mut encoding_acc,
    )?;

    Ok(build_json_envelope(
        fields_map,
        schema,
        options,
        record_index,
        &RecordMetadata {
            length: data.len(),
            offset: record_offset,
        },
        record_raw,
        encoding_acc,
    ))
}

/// Recursively process schema fields to decode record data into a JSON map.
///
/// Iterates through the schema hierarchy, handling groups, scalars, and
/// conditional logic (ODO, REDEFINES).
fn process_fields_recursive(
    fields: &[copybook_core::Field],
    data: &[u8],
    json_obj: &mut serde_json::Map<String, Value>,
    options: &DecodeOptions,
    scratch_buffers: &mut Option<crate::memory::ScratchBuffers>,
    record_index: u64,
    encoding_acc: &mut Vec<(String, ZonedEncodingFormat)>,
) -> Result<()> {
    use copybook_core::FieldKind;

    let total_fields = fields.len();
    let mut deferred_group_views = Vec::new();

    for (field_index, field) in fields.iter().enumerate() {
        match (&field.kind, &field.occurs) {
            (_, Some(occurs)) => {
                process_array_field(
                    field,
                    occurs,
                    data,
                    json_obj,
                    options,
                    fields,
                    scratch_buffers,
                    record_index,
                    encoding_acc,
                )?;
            }
            (FieldKind::Group, None) if field.level > 1 => {
                let mut group_obj = serde_json::Map::new();
                let metadata_start = encoding_acc.len();
                process_fields_recursive(
                    &field.children,
                    data,
                    &mut group_obj,
                    options,
                    scratch_buffers,
                    record_index,
                    encoding_acc,
                )?;
                if is_scalar_target_group_redefine(field, fields) {
                    let group_value = Value::Object(group_obj);
                    if let Value::Object(group_fields) = &group_value {
                        insert_decoded_group_fields(
                            json_obj,
                            group_fields,
                            &mut encoding_acc[metadata_start..],
                        );
                    }
                    deferred_group_views.push((field.name.clone(), group_value));
                } else if field.redefines_of.is_none() {
                    insert_decoded_field(json_obj, &field.name, Value::Object(group_obj));
                }
            }
            (FieldKind::Group, None) => {
                process_fields_recursive(
                    &field.children,
                    data,
                    json_obj,
                    options,
                    scratch_buffers,
                    record_index,
                    encoding_acc,
                )?;
            }
            _ => {
                process_scalar_field_standard(
                    field,
                    field_index,
                    total_fields,
                    data,
                    json_obj,
                    options,
                    scratch_buffers,
                    record_index,
                    encoding_acc,
                )?;
            }
        }
    }

    for (name, value) in deferred_group_views {
        insert_decoded_field(json_obj, &name, value);
    }

    Ok(())
}

/// Insert a decoded field without overwriting an earlier colliding view.
///
/// Flattened REDEFINES children share the enclosing JSON map with their
/// siblings. Preserve every view in traversal order using the repository's
/// deterministic duplicate-name convention.
fn insert_decoded_field(json_obj: &mut serde_json::Map<String, Value>, name: &str, value: Value) {
    let _ = insert_decoded_field_with_key(json_obj, name, value);
}

/// Flatten a decoded group while keeping field raw sidecars paired with their
/// emitted collision key.
fn insert_decoded_group_fields(
    json_obj: &mut serde_json::Map<String, Value>,
    group_fields: &serde_json::Map<String, Value>,
    encoding_metadata: &mut [(String, ZonedEncodingFormat)],
) {
    let mut emitted_keys = Vec::new();
    for (name, value) in group_fields {
        if let Some(field_name) = name.strip_suffix("_raw_b64")
            && let Some((_, emitted_key)) = emitted_keys
                .iter()
                .rev()
                .find(|(original, _)| original == field_name)
        {
            json_obj.insert(format!("{emitted_key}_raw_b64"), value.clone());
            continue;
        }

        let emitted_key = insert_decoded_field_with_key(json_obj, name, value.clone())
            .unwrap_or_else(|| name.clone());
        if let Some((metadata_name, _)) = encoding_metadata
            .iter_mut()
            .find(|(metadata_name, _)| metadata_name == name)
        {
            metadata_name.clone_from(&emitted_key);
        }
        if !name.ends_with("_raw_b64") {
            emitted_keys.push((name.clone(), emitted_key));
        }
    }
}

/// Insert a decoded field and return the key actually emitted into the map.
fn insert_decoded_field_with_key(
    json_obj: &mut serde_json::Map<String, Value>,
    name: &str,
    value: Value,
) -> Option<String> {
    // FILLER output and encode handling retain their existing overwrite
    // contract until the dedicated raw-sidecar/filler follow-up.
    if name.eq_ignore_ascii_case("FILLER") || name.starts_with("_filler_") {
        json_obj.insert(name.to_owned(), value);
        return None;
    }

    match json_obj.entry(name.to_owned()) {
        serde_json::map::Entry::Vacant(entry) => {
            entry.insert(value);
            return None;
        }
        serde_json::map::Entry::Occupied(_) => {}
    }

    let (base_name, has_duplicate_suffix) = duplicate_name_base(name);
    let mut candidate = if has_duplicate_suffix && json_obj.contains_key(base_name) {
        base_name.to_owned()
    } else {
        name.to_owned()
    };
    let mut suffix = 2;
    while json_obj.contains_key(&candidate) {
        candidate = format!("{base_name}__dup{suffix}");
        suffix += 1;
    }
    json_obj.insert(candidate.clone(), value);
    Some(candidate)
}

/// Return the unsuffixed schema name for a conventional `__dupN` key.
fn duplicate_name_base(name: &str) -> (&str, bool) {
    let Some((base, suffix)) = name.rsplit_once("__dup") else {
        return (name, false);
    };
    let Ok(number) = suffix.parse::<usize>() else {
        return (name, false);
    };
    if base.is_empty() || number < 2 {
        return (name, false);
    }
    (base, true)
}

/// Optimized field processing with scratch buffers for COMP-3 performance
/// CRITICAL PERFORMANCE OPTIMIZATION - reduces string allocations by 90%+
fn process_fields_recursive_with_scratch(
    fields: &[copybook_core::Field],
    data: &[u8],
    json_obj: &mut serde_json::Map<String, Value>,
    options: &DecodeOptions,
    scratch: &mut crate::memory::ScratchBuffers,
    record_index: u64,
    encoding_acc: &mut Vec<(String, ZonedEncodingFormat)>,
) -> Result<()> {
    use copybook_core::FieldKind;

    let mut deferred_group_views = Vec::new();

    for field in fields {
        if is_filler_field(field) && !options.emit_filler {
            continue;
        }

        match (&field.kind, &field.occurs) {
            (_, Some(occurs)) => {
                process_array_field_with_scratch(
                    field,
                    occurs,
                    data,
                    json_obj,
                    options,
                    fields,
                    scratch,
                    record_index,
                    encoding_acc,
                )?;
            }
            (FieldKind::Group, None) if field.level > 1 => {
                let mut group_obj = serde_json::Map::new();
                let metadata_start = encoding_acc.len();
                process_fields_recursive_with_scratch(
                    &field.children,
                    data,
                    &mut group_obj,
                    options,
                    scratch,
                    record_index,
                    encoding_acc,
                )?;
                if is_scalar_target_group_redefine(field, fields) {
                    let group_value = Value::Object(group_obj);
                    if let Value::Object(group_fields) = &group_value {
                        insert_decoded_group_fields(
                            json_obj,
                            group_fields,
                            &mut encoding_acc[metadata_start..],
                        );
                    }
                    deferred_group_views.push((field.name.clone(), group_value));
                } else if field.redefines_of.is_none() {
                    insert_decoded_field(json_obj, &field.name, Value::Object(group_obj));
                }
            }
            (FieldKind::Group, None) => {
                process_fields_recursive_with_scratch(
                    &field.children,
                    data,
                    json_obj,
                    options,
                    scratch,
                    record_index,
                    encoding_acc,
                )?;
            }
            _ => {
                process_scalar_field_with_scratch(
                    field,
                    data,
                    json_obj,
                    options,
                    scratch,
                    record_index,
                    encoding_acc,
                )?;
            }
        }
    }

    for (name, value) in deferred_group_views {
        insert_decoded_field(json_obj, &name, value);
    }

    Ok(())
}

/// Process a single scalar field using the standard (non-scratch) decode path.
///
/// # Arguments
/// * `field` - The scalar field metadata
/// * `field_index` - Index of the current field in its parent group
/// * `total_fields` - Total number of sibling fields
/// * `data` - The raw record data bytes
/// * `json_obj` - The JSON map to populate
/// * `options` - Decoding configuration
#[inline]
#[allow(clippy::too_many_arguments)]
fn process_scalar_field_standard(
    field: &copybook_core::Field,
    field_index: usize,
    total_fields: usize,
    data: &[u8],
    json_obj: &mut serde_json::Map<String, Value>,
    options: &DecodeOptions,
    scratch_buffers: &mut Option<crate::memory::ScratchBuffers>,
    record_index: u64,
    encoding_acc: &mut Vec<(String, ZonedEncodingFormat)>,
) -> Result<()> {
    // Special handling for RENAMES fields - they use resolved metadata, not field offset/len
    if matches!(field.kind, copybook_core::FieldKind::Renames { .. }) {
        let Some(resolved) = &field.resolved_renames else {
            return Err(Error::new(
                ErrorCode::CBKD101_INVALID_FIELD_TYPE,
                format!(
                    "RENAMES field '{name}' has no resolved metadata",
                    name = field.name
                ),
            ));
        };

        let alias_start = resolved.offset as usize;
        let alias_end = alias_start + resolved.length as usize;

        if alias_end > data.len() {
            return Err(Error::new(
                ErrorCode::CBKD301_RECORD_TOO_SHORT,
                format!(
                    "RENAMES field '{name}' at offset {offset} with length {length} exceeds data length {data_len}",
                    name = field.name,
                    offset = resolved.offset,
                    length = resolved.length,
                    data_len = data.len()
                ),
            ));
        }

        let alias_data = &data[alias_start..alias_end];
        let text = crate::charset::ebcdic_to_utf8(
            alias_data,
            options.codepage,
            options.on_decode_unmappable,
        )?;
        insert_decoded_field(json_obj, &field.name, Value::String(text));
        return Ok(());
    }

    let field_start = field.offset as usize;
    let mut field_end = field_start + field.len as usize;

    if options.format == RecordFormat::RDW
        && field_index + 1 == total_fields
        && matches!(field.kind, copybook_core::FieldKind::Alphanum { .. })
        && data.len() > field_end
    {
        field_end = data.len();
    }

    if field_start > data.len() {
        return Err(Error::new(
            ErrorCode::CBKD301_RECORD_TOO_SHORT,
            format!(
                "Field '{name}' starts beyond record boundary",
                name = field.name
            ),
        ));
    }

    field_end = field_end.min(data.len());

    if field_start >= field_end {
        return Ok(());
    }

    let field_data = &data[field_start..field_end];
    let value = decode_scalar_field_value_standard(field, field_data, options, scratch_buffers)
        .map_err(|error| add_zoned_overflow_context(error, field, record_index))?;

    let emitted_key = insert_decoded_field_with_key(json_obj, &field.name, value);

    // Metadata must follow the key emitted by collision-aware insertion.
    if options.preserve_zoned_encoding {
        let metadata_key = emitted_key.as_deref().unwrap_or(&field.name);
        collect_zoned_encoding_info(field, metadata_key, field_data, options, encoding_acc);
    }

    // Emit field-level raw bytes when RawMode::Field is active
    if matches!(options.emit_raw, crate::options::RawMode::Field) {
        let raw_key = emitted_key.map_or_else(
            || format!("{}_raw_b64", field.name),
            |key| format!("{key}_raw_b64"),
        );
        let raw_b64 = base64::engine::general_purpose::STANDARD.encode(field_data);
        json_obj.insert(raw_key, Value::String(raw_b64));
    }

    Ok(())
}

/// Process a single scalar field using optimized scratch buffers.
///
/// This path is optimized for high-throughput processing and minimizes allocations.
#[inline]
fn process_scalar_field_with_scratch(
    field: &copybook_core::Field,
    data: &[u8],
    json_obj: &mut serde_json::Map<String, Value>,
    options: &DecodeOptions,
    scratch: &mut crate::memory::ScratchBuffers,
    record_index: u64,
    encoding_acc: &mut Vec<(String, ZonedEncodingFormat)>,
) -> Result<()> {
    // Special handling for RENAMES fields - they use resolved metadata, not field offset/len
    if matches!(field.kind, copybook_core::FieldKind::Renames { .. }) {
        let Some(resolved) = &field.resolved_renames else {
            return Err(Error::new(
                ErrorCode::CBKD101_INVALID_FIELD_TYPE,
                format!(
                    "RENAMES field '{name}' has no resolved metadata",
                    name = field.name
                ),
            ));
        };

        let alias_start = resolved.offset as usize;
        let alias_end = alias_start + resolved.length as usize;

        if alias_end > data.len() {
            return Err(Error::new(
                ErrorCode::CBKD301_RECORD_TOO_SHORT,
                format!(
                    "RENAMES field '{name}' at offset {offset} with length {length} exceeds data length {data_len}",
                    name = field.name,
                    offset = resolved.offset,
                    length = resolved.length,
                    data_len = data.len()
                ),
            ));
        }

        let alias_data = &data[alias_start..alias_end];
        let text = crate::charset::ebcdic_to_utf8(
            alias_data,
            options.codepage,
            options.on_decode_unmappable,
        )?;
        insert_decoded_field(json_obj, &field.name, Value::String(text));
        return Ok(());
    }

    let field_start = field.offset as usize;
    let mut field_end = field_start + field.len as usize;

    if field_start > data.len() {
        return Err(Error::new(
            ErrorCode::CBKD301_RECORD_TOO_SHORT,
            format!(
                "Field '{name}' starts beyond record boundary",
                name = field.name
            ),
        ));
    }

    if options.format == RecordFormat::RDW {
        field_end = field_end.min(data.len());
    }

    if field_start >= field_end {
        return Ok(());
    }

    if field_end > data.len() {
        return Err(Error::new(
            ErrorCode::CBKD301_RECORD_TOO_SHORT,
            format!(
                "Field '{name}' at offset {offset} with length {length} exceeds data length {data_len}",
                name = field.name,
                offset = field.offset,
                length = field.len,
                data_len = data.len()
            ),
        ));
    }

    let field_data = &data[field_start..field_end];
    let value = decode_scalar_field_value_with_scratch(field, field_data, options, scratch)
        .map_err(|error| add_zoned_overflow_context(error, field, record_index))?;

    let emitted_key = insert_decoded_field_with_key(json_obj, &field.name, value);

    // Metadata must follow the key emitted by collision-aware insertion.
    if options.preserve_zoned_encoding {
        let metadata_key = emitted_key.as_deref().unwrap_or(&field.name);
        collect_zoned_encoding_info(field, metadata_key, field_data, options, encoding_acc);
    }

    // Emit field-level raw bytes when RawMode::Field is active
    if matches!(options.emit_raw, crate::options::RawMode::Field) {
        let raw_key = emitted_key.map_or_else(
            || format!("{}_raw_b64", field.name),
            |key| format!("{key}_raw_b64"),
        );
        let raw_b64 = base64::engine::general_purpose::STANDARD.encode(field_data);
        json_obj.insert(raw_key, Value::String(raw_b64));
    }

    Ok(())
}

#[inline]
fn add_zoned_overflow_context(
    error: Error,
    field: &copybook_core::Field,
    record_index: u64,
) -> Error {
    if error.code == ErrorCode::CBKD410_ZONED_OVERFLOW {
        error
            .with_record(record_index)
            .with_field(field.path.clone())
            .with_offset(u64::from(field.offset))
    } else {
        error
    }
}

/// Process an array field (with OCCURS clause)
#[allow(clippy::too_many_arguments)]
fn process_array_field(
    field: &copybook_core::Field,
    occurs: &copybook_core::Occurs,
    data: &[u8],
    json_obj: &mut serde_json::Map<String, Value>,
    options: &DecodeOptions,
    all_fields: &[copybook_core::Field],
    scratch_buffers: &mut Option<crate::memory::ScratchBuffers>,
    record_index: u64,
    encoding_acc: &mut Vec<(String, ZonedEncodingFormat)>,
) -> Result<()> {
    use copybook_core::{FieldKind, Occurs};

    let count = match occurs {
        Occurs::Fixed { count } => *count,
        Occurs::ODO {
            min,
            max,
            counter_path,
        } => {
            // Find the counter field and get its value
            let scratch = scratch_buffers.get_or_insert_with(crate::memory::ScratchBuffers::new);
            let counter_value = find_and_read_counter_field(
                counter_path,
                all_fields,
                data,
                options,
                scratch,
                record_index,
            )?;

            let counter_field = find_field_by_path(all_fields, counter_path)?;
            let validation_context = crate::odo_redefines::OdoValidationContext {
                field_path: field.path.clone(),
                counter_path: counter_path.clone(),
                record_index,
                byte_offset: u64::from(counter_field.offset),
            };
            let validation = crate::odo_redefines::validate_odo_decode(
                counter_value,
                *min,
                *max,
                &validation_context,
                options,
            )?;

            if let Some(warning) = validation.warning {
                tracing::warn!("{}", warning);
                increment_warning_counter();
            }

            validation.actual_count
        }
    };

    let element_size = field.len as usize;
    let array_start = field.offset as usize;
    let total_array_size = element_size * count as usize;
    let array_end = array_start + total_array_size;

    // Check if we have enough data for all array elements
    if array_end > data.len() {
        return Err(Error::new(
            ErrorCode::CBKD301_RECORD_TOO_SHORT,
            format!(
                "Array '{}' requires {} bytes but only {} bytes available",
                field.name,
                total_array_size,
                data.len().saturating_sub(array_start)
            ),
        ));
    }

    // Process array elements
    let mut array_values = Vec::new();
    for i in 0..count {
        let element_start = array_start + (i as usize * element_size);
        let element_end = element_start + element_size;

        let element_value = match &field.kind {
            FieldKind::Group => {
                // For group fields, create a modified field with adjusted offsets for this element
                let mut element_obj = serde_json::Map::new();
                let element_base_offset = u32::try_from(element_start).map_err(|_| {
                    Error::new(
                        ErrorCode::CBKD301_RECORD_TOO_SHORT,
                        format!("Array element offset {element_start} exceeds supported range"),
                    )
                })?;
                let adjusted_children =
                    adjust_field_offsets(&field.children, element_base_offset, field.offset);
                process_fields_recursive(
                    &adjusted_children,
                    data,
                    &mut element_obj,
                    options,
                    scratch_buffers,
                    record_index,
                    encoding_acc,
                )?;
                Value::Object(element_obj)
            }
            FieldKind::Condition { values } => condition_value(values, "CONDITION_ARRAY"),
            _ => {
                let element_data = &data[element_start..element_end];
                let val = decode_scalar_field_value_standard(
                    field,
                    element_data,
                    options,
                    scratch_buffers,
                )
                .map_err(|error| add_zoned_overflow_context(error, field, record_index))?;
                if options.preserve_zoned_encoding {
                    collect_array_zoned_encoding_info(field, element_data, options, encoding_acc);
                }
                val
            }
        };

        array_values.push(element_value);
    }

    insert_decoded_field(json_obj, &field.name, Value::Array(array_values));
    Ok(())
}

/// Process an array field with scratch buffers for COMP-3 optimization
#[allow(clippy::too_many_arguments)]
fn process_array_field_with_scratch(
    field: &copybook_core::Field,
    occurs: &copybook_core::Occurs,
    data: &[u8],
    json_obj: &mut serde_json::Map<String, Value>,
    options: &DecodeOptions,
    all_fields: &[copybook_core::Field],
    scratch: &mut crate::memory::ScratchBuffers,
    record_index: u64,
    encoding_acc: &mut Vec<(String, ZonedEncodingFormat)>,
) -> Result<()> {
    use copybook_core::{FieldKind, Occurs};
    use serde_json::Value;

    let count = match occurs {
        Occurs::Fixed { count } => *count,
        Occurs::ODO {
            min,
            max,
            counter_path,
        } => {
            // Find the counter field and get its value
            let counter_value = find_and_read_counter_field(
                counter_path,
                all_fields,
                data,
                options,
                scratch,
                record_index,
            )?;

            let counter_field = find_field_by_path(all_fields, counter_path)?;
            let validation_context = crate::odo_redefines::OdoValidationContext {
                field_path: field.path.clone(),
                counter_path: counter_path.clone(),
                record_index,
                byte_offset: u64::from(counter_field.offset),
            };
            let validation = crate::odo_redefines::validate_odo_decode(
                counter_value,
                *min,
                *max,
                &validation_context,
                options,
            )?;

            if let Some(warning) = validation.warning {
                tracing::warn!("{}", warning);
                increment_warning_counter();
            }

            validation.actual_count
        }
    };

    let element_size = field.len as usize;
    let array_start = field.offset as usize;
    let total_array_size = element_size * count as usize;
    let array_end = array_start + total_array_size;

    if array_end > data.len() {
        return Err(Error::new(
            ErrorCode::CBKD301_RECORD_TOO_SHORT,
            format!(
                "Array field '{}' with {} elements at offset {} requires {} bytes but record has {}",
                field.name,
                count,
                array_start,
                total_array_size,
                data.len() - array_start
            ),
        ));
    }

    let mut array_values = Vec::new();

    for i in 0..count {
        let element_offset = array_start + (i as usize * element_size);
        let element_data = &data[element_offset..element_offset + element_size];

        let element_value = match &field.kind {
            FieldKind::Group => {
                // For group arrays, each element should be an object with child fields
                let mut group_obj = serde_json::Map::new();

                // Rebase every child to this element, matching the standard
                // traversal so scratch and non-scratch decoding see the same
                // bytes for each repeated group element.
                let element_offset_u32 = u32::try_from(element_offset).map_err(|_| {
                    Error::new(
                        ErrorCode::CBKD301_RECORD_TOO_SHORT,
                        format!("Array element offset {element_offset} exceeds supported range"),
                    )
                })?;
                let adjusted_children =
                    adjust_field_offsets(&field.children, element_offset_u32, field.offset);

                process_fields_recursive_with_scratch(
                    &adjusted_children,
                    data,
                    &mut group_obj,
                    options,
                    scratch,
                    record_index,
                    encoding_acc,
                )?;
                Value::Object(group_obj)
            }
            FieldKind::Condition { values } => condition_value(values, "CONDITION_ARRAY"),
            _ => {
                let val =
                    decode_scalar_field_value_with_scratch(field, element_data, options, scratch)
                        .map_err(|error| add_zoned_overflow_context(error, field, record_index))?;
                if options.preserve_zoned_encoding {
                    collect_array_zoned_encoding_info(field, element_data, options, encoding_acc);
                }
                val
            }
        };

        array_values.push(element_value);
    }

    insert_decoded_field(json_obj, &field.name, Value::Array(array_values));
    Ok(())
}

/// Find and read the value of a counter field for ODO arrays
fn find_and_read_counter_field(
    counter_path: &str,
    all_fields: &[copybook_core::Field],
    data: &[u8],
    options: &DecodeOptions,
    scratch: &mut crate::memory::ScratchBuffers,
    record_index: u64,
) -> Result<u32> {
    // Find the counter field by path
    let counter_field = find_field_by_path(all_fields, counter_path)?;

    // Read the counter field value
    let field_start = counter_field.offset as usize;
    let field_end = field_start + counter_field.len as usize;

    if field_end > data.len() {
        return Err(Error::new(
            ErrorCode::CBKD301_RECORD_TOO_SHORT,
            format!("Counter field '{counter_path}' extends beyond record"),
        ));
    }

    let field_data = &data[field_start..field_end];

    // Decode the counter value based on its type
    match &counter_field.kind {
        copybook_core::FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
            sign_separate,
        } => {
            let count = if let Some(sign_sep) = sign_separate {
                let decimal = crate::numeric::decode_zoned_decimal_sign_separate(
                    field_data,
                    *digits,
                    *scale,
                    sign_sep,
                    options.codepage,
                )?;
                decimal_counter_to_u32(&decimal, counter_path)?
            } else {
                let decimal_str = crate::numeric::decode_zoned_decimal_to_string_with_scratch(
                    field_data,
                    *digits,
                    *scale,
                    *signed,
                    options.codepage,
                    counter_field.blank_when_zero,
                    scratch,
                )
                .map_err(|error| add_zoned_overflow_context(error, counter_field, record_index))?;
                decimal_str.parse::<u32>().map_err(|_| {
                    Error::new(
                        ErrorCode::CBKS121_COUNTER_NOT_FOUND,
                        format!("ODO counter '{counter_path}' has invalid value: {decimal_str}"),
                    )
                })?
            };

            Ok(count)
        }
        copybook_core::FieldKind::BinaryInt { bits, signed } => {
            let int_value = crate::numeric::decode_binary_int(field_data, *bits, *signed)?;
            if int_value < 0 {
                return Err(Error::new(
                    ErrorCode::CBKS121_COUNTER_NOT_FOUND,
                    format!("ODO counter '{counter_path}' has negative value: {int_value}"),
                ));
            }
            Ok(u32::try_from(int_value).map_err(|_| {
                Error::new(
                    ErrorCode::CBKS121_COUNTER_NOT_FOUND,
                    format!("ODO counter '{counter_path}' exceeds supported range: {int_value}"),
                )
            })?)
        }
        copybook_core::FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => {
            let decimal_str = crate::numeric::decode_packed_decimal_to_string_with_scratch(
                field_data, *digits, *scale, *signed, scratch,
            )?;
            let count = decimal_str.parse::<u32>().map_err(|_| {
                Error::new(
                    ErrorCode::CBKS121_COUNTER_NOT_FOUND,
                    format!("ODO counter '{counter_path}' has invalid value: {decimal_str}"),
                )
            })?;
            Ok(count)
        }
        _ => Err(Error::new(
            ErrorCode::CBKS121_COUNTER_NOT_FOUND,
            format!("ODO counter '{counter_path}' has unsupported type"),
        )),
    }
}

/// Find a field by its path in the field hierarchy
fn find_field_by_path<'a>(
    fields: &'a [copybook_core::Field],
    path: &str,
) -> Result<&'a copybook_core::Field> {
    for field in fields {
        if field.path == path || field.name == path {
            return Ok(field);
        }
        // Search in children recursively
        if let Ok(found) = find_field_by_path(&field.children, path) {
            return Ok(found);
        }
    }

    Err(Error::new(
        ErrorCode::CBKS121_COUNTER_NOT_FOUND,
        format!("ODO counter field '{path}' not found"),
    ))
}

/// Adjust field offsets for array element processing
/// Adjust field offsets for array element processing.
///
/// Recalculates field offsets relative to a base offset (e.g., when processing
/// an OCCURS group element).
fn adjust_field_offsets(
    fields: &[copybook_core::Field],
    base_offset: u32,
    source_base_offset: u32,
) -> Vec<copybook_core::Field> {
    fields
        .iter()
        .map(|field| {
            let mut adjusted_field = field.clone();
            let relative_offset = field.offset.saturating_sub(source_base_offset);
            adjusted_field.offset = base_offset.saturating_add(relative_offset);
            if !adjusted_field.children.is_empty() {
                adjusted_field.children =
                    adjust_field_offsets(&adjusted_field.children, base_offset, source_base_offset);
            }
            adjusted_field
        })
        .collect()
}

/// Check if a field is a FILLER field (should usually be omitted from JSON).
#[inline]
fn is_filler_field(field: &copybook_core::Field) -> bool {
    field.name.eq_ignore_ascii_case("FILLER") || field.name.starts_with("_filler_")
}

/// Collect zoned encoding format info for a field when preservation is enabled.
///
/// Detects the encoding format (ASCII vs EBCDIC) from the raw field data
/// and pushes it to the accumulator for later emission as `_encoding_metadata`.
#[inline]
fn collect_zoned_encoding_info(
    field: &copybook_core::Field,
    emitted_key: &str,
    field_data: &[u8],
    options: &DecodeOptions,
    encoding_acc: &mut Vec<(String, ZonedEncodingFormat)>,
) {
    if let copybook_core::FieldKind::ZonedDecimal { digits, signed, .. } = &field.kind
        && let Ok((_, Some(info))) = crate::numeric::decode_zoned_decimal_with_encoding(
            field_data,
            *digits,
            0, // scale doesn't affect encoding detection
            *signed,
            options.codepage,
            field.blank_when_zero,
            true,
        )
        && !info.has_mixed_encoding
    {
        encoding_acc.push((emitted_key.to_owned(), info.detected_format));
    }
}

/// Convert a numeric string to a JSON value respecting [`JsonNumberMode`].
///
/// In `Lossless` mode the decimal string is returned as-is (`Value::String`).
/// In `Native` mode the string is parsed to the narrowest JSON number type
/// (i64 → u64 → f64), falling back to string if parsing fails.
fn numeric_string_to_value(s: String, options: &DecodeOptions) -> Value {
    use crate::options::JsonNumberMode;
    match options.json_number_mode {
        JsonNumberMode::Lossless => Value::String(s),
        JsonNumberMode::Native => {
            // Try integer first (no decimal point and no exponent)
            if !s.contains('.') && !s.contains('e') && !s.contains('E') {
                if let Ok(n) = s.parse::<i64>() {
                    return Value::Number(serde_json::Number::from(n));
                }
                if let Ok(n) = s.parse::<u64>() {
                    return Value::Number(serde_json::Number::from(n));
                }
            }
            // Fall back to f64
            if let Ok(f) = s.parse::<f64>()
                && let Some(n) = serde_json::Number::from_f64(f)
            {
                return Value::Number(n);
            }
            // Unparseable → keep as string
            Value::String(s)
        }
    }
}

/// Decode a scalar field value from raw data (standard path)
#[allow(clippy::too_many_lines)]
fn decode_scalar_field_value_standard(
    field: &copybook_core::Field,
    field_data: &[u8],
    options: &DecodeOptions,
    scratch_buffers: &mut Option<crate::memory::ScratchBuffers>,
) -> Result<Value> {
    use copybook_core::FieldKind;

    match &field.kind {
        FieldKind::Alphanum { .. } => {
            let text = crate::charset::ebcdic_to_utf8(
                field_data,
                options.codepage,
                options.on_decode_unmappable,
            )?;
            Ok(Value::String(text))
        }
        FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
            sign_separate,
        } => {
            if let Some(sign_sep) = sign_separate {
                let decimal = crate::numeric::decode_zoned_decimal_sign_separate(
                    field_data,
                    *digits,
                    *scale,
                    sign_sep,
                    options.codepage,
                )?;
                Ok(zoned_decimal_to_json_value(
                    &decimal,
                    *digits,
                    *scale,
                    field.blank_when_zero,
                    options,
                ))
            } else if options.preserve_zoned_encoding {
                // Use encoding-aware decoding for round-trip preservation
                let (decimal, _encoding_info) = crate::numeric::decode_zoned_decimal_with_encoding(
                    field_data,
                    *digits,
                    *scale,
                    *signed,
                    options.codepage,
                    field.blank_when_zero,
                    true, // preserve_encoding = true
                )?;

                // Encoding info is collected by collect_zoned_encoding_info() at the caller level
                // and emitted as _encoding_metadata in the JSON envelope
                Ok(zoned_decimal_to_json_value(
                    &decimal,
                    *digits,
                    *scale,
                    field.blank_when_zero,
                    options,
                ))
            } else {
                // Use standard decoding
                let decimal = crate::numeric::decode_zoned_decimal(
                    field_data,
                    *digits,
                    *scale,
                    *signed,
                    options.codepage,
                    field.blank_when_zero,
                )?;
                Ok(zoned_decimal_to_json_value(
                    &decimal,
                    *digits,
                    *scale,
                    field.blank_when_zero,
                    options,
                ))
            }
        }
        FieldKind::BinaryInt { bits, signed } => {
            let int_value = crate::numeric::decode_binary_int(field_data, *bits, *signed)?;
            let scratch = scratch_buffers.get_or_insert_with(crate::memory::ScratchBuffers::new);
            let formatted =
                crate::numeric::format_binary_int_to_string_with_scratch(int_value, scratch);
            Ok(numeric_string_to_value(formatted, options))
        }
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => {
            let scratch = scratch_buffers.get_or_insert_with(crate::memory::ScratchBuffers::new);
            let decimal_str = crate::numeric::decode_packed_decimal_to_string_with_scratch(
                field_data, *digits, *scale, *signed, scratch,
            )?;
            Ok(numeric_string_to_value(decimal_str, options))
        }
        FieldKind::Group => {
            // Group fields should not be processed as scalars
            Err(Error::new(
                ErrorCode::CBKD101_INVALID_FIELD_TYPE,
                format!(
                    "Cannot process group field '{name}' as scalar",
                    name = field.name
                ),
            ))
        }
        FieldKind::Condition { values } => {
            // Level-88 fields are condition names, not data scalars
            // Return structured representation for API consistency
            Ok(condition_value(values, "CONDITION"))
        }
        FieldKind::Renames { .. } => {
            // Slice-2: Decode RENAMES fields using resolved metadata
            let Some(resolved) = &field.resolved_renames else {
                return Err(Error::new(
                    ErrorCode::CBKD101_INVALID_FIELD_TYPE,
                    format!(
                        "RENAMES field '{name}' has no resolved metadata",
                        name = field.name
                    ),
                ));
            };
            // Extract the aliased byte range
            let alias_start = resolved.offset as usize;
            let alias_end = alias_start + resolved.length as usize;

            if alias_end > field_data.len() {
                return Err(Error::new(
                    ErrorCode::CBKD301_RECORD_TOO_SHORT,
                    format!(
                        "RENAMES field '{name}' at offset {offset} with length {length} exceeds data length {data_len}",
                        name = field.name,
                        offset = resolved.offset,
                        length = resolved.length,
                        data_len = field_data.len()
                    ),
                ));
            }

            // For scalar RENAMES (single member), decode that field
            // For group RENAMES (multiple members), this shouldn't be called - should be handled elsewhere
            if resolved.members.len() == 1 {
                // Single field alias - extract and decode
                let alias_data = &field_data[alias_start..alias_end];
                // Return as raw string for now - proper field decoding would require schema traversal
                let text = crate::charset::ebcdic_to_utf8(
                    alias_data,
                    options.codepage,
                    options.on_decode_unmappable,
                )?;
                return Ok(Value::String(text));
            }
            // Multi-field alias treated as alphanum for scalar context
            let alias_data = &field_data[alias_start..alias_end];
            let text = crate::charset::ebcdic_to_utf8(
                alias_data,
                options.codepage,
                options.on_decode_unmappable,
            )?;
            Ok(Value::String(text))
        }
        FieldKind::EditedNumeric {
            pic_string, scale, ..
        } => {
            // Phase E2: Decode edited PIC fields
            let raw_str = crate::charset::ebcdic_to_utf8(
                field_data,
                options.codepage,
                options.on_decode_unmappable,
            )?;

            // Tokenize the PIC pattern
            let pattern = crate::edited_pic::tokenize_edited_pic(pic_string)?;

            // Decode the edited numeric value
            let numeric_value = crate::edited_pic::decode_edited_numeric(
                &raw_str,
                &pattern,
                *scale,
                field.blank_when_zero,
            )?;

            // Return respecting json_number_mode
            Ok(numeric_string_to_value(
                numeric_value.to_decimal_string(),
                options,
            ))
        }
        FieldKind::FloatSingle => {
            let value =
                crate::numeric::decode_float_single_with_format(field_data, options.float_format)?;
            if value.is_nan() || value.is_infinite() {
                Ok(Value::Null)
            } else {
                Ok(Value::Number(
                    serde_json::Number::from_f64(f64::from(value))
                        .unwrap_or_else(|| serde_json::Number::from(0)),
                ))
            }
        }
        FieldKind::FloatDouble => {
            let value =
                crate::numeric::decode_float_double_with_format(field_data, options.float_format)?;
            if value.is_nan() || value.is_infinite() {
                Ok(Value::Null)
            } else {
                Ok(Value::Number(
                    serde_json::Number::from_f64(value)
                        .unwrap_or_else(|| serde_json::Number::from(0)),
                ))
            }
        }
    }
}

/// Identify group views that redefine a scalar field.
///
/// These views are emitted both as flattened child fields and as a named group
/// object. Group-over-group redefines retain their existing skip behavior.
fn is_scalar_target_group_redefine(
    field: &copybook_core::Field,
    sibling_fields: &[copybook_core::Field],
) -> bool {
    let Some(target_path) = field.redefines_of.as_deref() else {
        return false;
    };

    matches!(field.kind, copybook_core::FieldKind::Group)
        && find_field_by_path(sibling_fields, target_path)
            .is_ok_and(|target| !matches!(target.kind, copybook_core::FieldKind::Group))
}

/// Decode a scalar field value using shared scratch buffers
#[allow(clippy::too_many_lines)]
fn decode_scalar_field_value_with_scratch(
    field: &copybook_core::Field,
    field_data: &[u8],
    options: &DecodeOptions,
    scratch: &mut crate::memory::ScratchBuffers,
) -> Result<Value> {
    use copybook_core::FieldKind;

    match &field.kind {
        FieldKind::Alphanum { .. } => {
            let text = crate::charset::ebcdic_to_utf8(
                field_data,
                options.codepage,
                options.on_decode_unmappable,
            )?;
            Ok(Value::String(text))
        }
        FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
            sign_separate,
        } => {
            if let Some(sign_sep) = sign_separate {
                let decimal = crate::numeric::decode_zoned_decimal_sign_separate(
                    field_data,
                    *digits,
                    *scale,
                    sign_sep,
                    options.codepage,
                )?;
                Ok(zoned_decimal_to_json_value(
                    &decimal,
                    *digits,
                    *scale,
                    field.blank_when_zero,
                    options,
                ))
            } else {
                let decimal_str = crate::numeric::decode_zoned_decimal_to_string_with_scratch(
                    field_data,
                    *digits,
                    *scale,
                    *signed,
                    options.codepage,
                    field.blank_when_zero,
                    scratch,
                )?;
                Ok(numeric_string_to_value(decimal_str, options))
            }
        }
        FieldKind::BinaryInt { bits, signed } => {
            let int_value = crate::numeric::decode_binary_int(field_data, *bits, *signed)?;
            let formatted =
                crate::numeric::format_binary_int_to_string_with_scratch(int_value, scratch);
            Ok(numeric_string_to_value(formatted, options))
        }
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => {
            let decimal_str = crate::numeric::decode_packed_decimal_to_string_with_scratch(
                field_data, *digits, *scale, *signed, scratch,
            )?;
            Ok(numeric_string_to_value(decimal_str, options))
        }
        FieldKind::Group => Err(Error::new(
            ErrorCode::CBKD101_INVALID_FIELD_TYPE,
            format!(
                "Cannot process group field '{name}' as scalar",
                name = field.name
            ),
        )),
        FieldKind::Condition { values } => Ok(condition_value(values, "CONDITION")),
        FieldKind::Renames { .. } => {
            // Slice-2: Decode RENAMES fields using resolved metadata (with scratch buffers)
            let Some(resolved) = &field.resolved_renames else {
                return Err(Error::new(
                    ErrorCode::CBKD101_INVALID_FIELD_TYPE,
                    format!(
                        "RENAMES field '{name}' has no resolved metadata",
                        name = field.name
                    ),
                ));
            };
            // Extract the aliased byte range
            let alias_start = resolved.offset as usize;
            let alias_end = alias_start + resolved.length as usize;

            if alias_end > field_data.len() {
                return Err(Error::new(
                    ErrorCode::CBKD301_RECORD_TOO_SHORT,
                    format!(
                        "RENAMES field '{name}' at offset {offset} with length {length} exceeds data length {data_len}",
                        name = field.name,
                        offset = resolved.offset,
                        length = resolved.length,
                        data_len = field_data.len()
                    ),
                ));
            }

            // For scalar RENAMES, decode the aliased range as alphanum
            let alias_data = &field_data[alias_start..alias_end];
            let text = crate::charset::ebcdic_to_utf8(
                alias_data,
                options.codepage,
                options.on_decode_unmappable,
            )?;
            Ok(Value::String(text))
        }
        FieldKind::EditedNumeric {
            pic_string, scale, ..
        } => {
            // Phase E2: Decode edited PIC fields
            let raw_str = crate::charset::ebcdic_to_utf8(
                field_data,
                options.codepage,
                options.on_decode_unmappable,
            )?;

            // Tokenize the PIC pattern
            let pattern = crate::edited_pic::tokenize_edited_pic(pic_string)?;

            // Decode the edited numeric value
            let numeric_value = crate::edited_pic::decode_edited_numeric(
                &raw_str,
                &pattern,
                *scale,
                field.blank_when_zero,
            )?;

            // Return respecting json_number_mode (scratch path)
            Ok(numeric_string_to_value(
                numeric_value.to_decimal_string(),
                options,
            ))
        }
        FieldKind::FloatSingle => {
            let value =
                crate::numeric::decode_float_single_with_format(field_data, options.float_format)?;
            if value.is_nan() || value.is_infinite() {
                Ok(Value::Null)
            } else {
                Ok(Value::Number(
                    serde_json::Number::from_f64(f64::from(value))
                        .unwrap_or_else(|| serde_json::Number::from(0)),
                ))
            }
        }
        FieldKind::FloatDouble => {
            let value =
                crate::numeric::decode_float_double_with_format(field_data, options.float_format)?;
            if value.is_nan() || value.is_infinite() {
                Ok(Value::Null)
            } else {
                Ok(Value::Number(
                    serde_json::Number::from_f64(value)
                        .unwrap_or_else(|| serde_json::Number::from(0)),
                ))
            }
        }
    }
}

/// Build a JSON value for a Level-88 condition.
///
/// Returns a boolean if there's a single value, or an array if there are multiple.
#[inline]
fn condition_value(values: &[String], prefix: &str) -> Value {
    if values.is_empty() {
        Value::String(prefix.to_owned())
    } else {
        Value::String(format!("{prefix}({})", values.join("|")))
    }
}

/// Encode JSON data to binary using the provided schema
///
/// # Arguments
///
/// * `schema` - The parsed copybook schema
/// * `json` - The JSON data to encode
/// * `options` - Encoding options
///
/// # Examples
///
/// ```
/// use copybook_core::parse_copybook;
/// use copybook_codec::{encode_record, EncodeOptions};
/// use copybook_codec::options::{Codepage, RecordFormat};
/// use serde_json::json;
///
/// let schema = parse_copybook("01 FLD PIC X(5).").unwrap();
/// let json = json!({"fields": {"FLD": "HELLO"}});
/// let options = EncodeOptions::new()
///     .with_codepage(Codepage::ASCII)
///     .with_format(RecordFormat::Fixed);
/// let binary = encode_record(&schema, &json, &options).unwrap();
/// assert_eq!(&binary[..5], b"HELLO");
/// ```
///
/// # Errors
/// Returns an error if the JSON data cannot be encoded according to the schema.
/// RDW raw replay also returns `CBKF102_RECORD_LENGTH_INVALID` when the decoded
/// raw value is shorter than its four-byte header or a changed payload exceeds
/// the format's `u16` payload-length field.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn encode_record(schema: &Schema, json: &Value, options: &EncodeOptions) -> Result<Vec<u8>> {
    let root_obj = json.as_object().ok_or_else(|| {
        Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            "Expected JSON object for record envelope",
        )
    })?;
    let encoding_metadata = root_obj
        .get("_encoding_metadata")
        .and_then(Value::as_object);
    let fields_value = if let Some(fields_val) = root_obj.get("fields") {
        fields_val.as_object().ok_or_else(|| {
            Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                "`fields` must be a JSON object",
            )
        })?;
        fields_val
    } else {
        json
    };

    if let Some(raw_replay) =
        encode_raw_replay(root_obj, fields_value, schema, encoding_metadata, options)?
    {
        return Ok(raw_replay);
    }

    // No raw data or not using raw - encode from fields
    validate_lib_api_redefines_encoding(schema, fields_value, options)?;
    validate_lib_api_odo_encoding(schema, fields_value, options)?;

    match options.format {
        RecordFormat::Fixed => {
            let payload = encode_fields_to_bytes(schema, fields_value, encoding_metadata, options)?;
            Ok(payload)
        }
        RecordFormat::RDW => {
            let payload = encode_fields_to_bytes(schema, fields_value, encoding_metadata, options)?;

            // Create RDW record
            let rdw_record = crate::record::RDWRecord::try_new(payload)?;
            let mut result = Vec::new();
            result.extend_from_slice(&rdw_record.header);
            result.extend_from_slice(&rdw_record.payload);
            Ok(result)
        }
    }
}

fn parse_raw_capture(root: &serde_json::Map<String, Value>) -> Result<Option<RawCapture>> {
    match root.get("raw_capture") {
        None => Ok(None),
        Some(Value::String(value)) if value == "record" => Ok(Some(RawCapture::Record)),
        Some(Value::String(value)) if value == "record+rdw" => Ok(Some(RawCapture::RecordRdw)),
        Some(value) => Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            format!("Invalid raw_capture {value}; expected 'record' or 'record+rdw'"),
        )),
    }
}

fn encode_raw_replay(
    root: &serde_json::Map<String, Value>,
    fields: &Value,
    schema: &Schema,
    encoding_metadata: Option<&serde_json::Map<String, Value>>,
    options: &EncodeOptions,
) -> Result<Option<Vec<u8>>> {
    if !options.use_raw {
        return Ok(None);
    }
    let Some(raw_str) = root
        .get("raw_b64")
        .or_else(|| root.get("__raw_b64"))
        .and_then(Value::as_str)
    else {
        return Ok(None);
    };
    let capture = parse_raw_capture(root)?;
    let raw_data = base64::engine::general_purpose::STANDARD
        .decode(raw_str)
        .map_err(|error| {
            Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!("Invalid base64 in raw_b64: {error}"),
            )
        })?;

    match options.format {
        RecordFormat::Fixed => encode_fixed_raw_replay(raw_data, capture),
        RecordFormat::RDW => encode_rdw_raw_replay(
            raw_data,
            capture,
            fields,
            schema,
            encoding_metadata,
            options,
        ),
    }
    .map(Some)
}

fn encode_fixed_raw_replay(raw_data: Vec<u8>, capture: Option<RawCapture>) -> Result<Vec<u8>> {
    if matches!(capture, Some(RawCapture::RecordRdw)) {
        return Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            "raw_capture 'record+rdw' conflicts with fixed record format",
        ));
    }
    Ok(raw_data)
}

fn encode_rdw_raw_replay(
    raw_data: Vec<u8>,
    capture: Option<RawCapture>,
    fields: &Value,
    schema: &Schema,
    encoding_metadata: Option<&serde_json::Map<String, Value>>,
    options: &EncodeOptions,
) -> Result<Vec<u8>> {
    if matches!(capture, Some(RawCapture::Record)) {
        return Ok(crate::record::RDWRecord::try_with_reserved(raw_data, 0)?.as_bytes());
    }

    // Validate framing before field encoding so a malformed raw frame cannot
    // be masked by an unrelated field error.
    let (reserved, raw_payload) = parse_raw_rdw_frame(&raw_data)?;
    let field_payload = encode_fields_to_bytes(schema, fields, encoding_metadata, options)?;
    if field_payload == raw_payload {
        return Ok(raw_data);
    }
    Ok(crate::record::RDWRecord::try_with_reserved(field_payload, reserved)?.as_bytes())
}

/// Validate REDEFINES encoding constraints for direct `lib_api` encoding.
fn validate_lib_api_redefines_encoding(
    schema: &Schema,
    json_value: &Value,
    options: &EncodeOptions,
) -> Result<()> {
    let redefines_context = crate::odo_redefines::build_redefines_context(schema, json_value);

    for (cluster_path, non_null_views) in &redefines_context.cluster_views {
        let field_path = non_null_views
            .first()
            .cloned()
            .unwrap_or_else(|| cluster_path.clone());

        let byte_offset = non_null_views
            .iter()
            .find_map(|view| schema.find_field(view).map(|field| u64::from(field.offset)))
            .or_else(|| {
                schema
                    .find_field(cluster_path)
                    .map(|field| u64::from(field.offset))
            })
            .unwrap_or(0);

        crate::odo_redefines::validate_redefines_encoding(
            &redefines_context,
            cluster_path,
            &field_path,
            json_value,
            options.use_raw,
            0,
            byte_offset,
        )?;
    }

    Ok(())
}

/// Validate ODO encoding constraints for direct `lib_api` encoding.
fn validate_lib_api_odo_encoding(
    schema: &Schema,
    json_value: &Value,
    options: &EncodeOptions,
) -> Result<()> {
    let Some(tail_odo) = &schema.tail_odo else {
        return Ok(());
    };

    let fields_value = if let Some(fields_value) = json_value.get("fields") {
        fields_value
    } else {
        json_value
    };

    let has_wrapper = json_value.get("fields").is_some();
    if !fields_value.is_object() {
        if has_wrapper {
            return Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                "`fields` must be a JSON object",
            ));
        }
        return Ok(());
    }

    let array_field =
        crate::odo_redefines::find_field_by_path_or_unique_name(schema, &tail_odo.array_path)
            .ok_or_else(|| {
                Error::new(
                    ErrorCode::CBKS121_COUNTER_NOT_FOUND,
                    format!(
                        "ODO array field '{}' not found in schema",
                        tail_odo.array_path
                    ),
                )
                .with_context(
                    crate::odo_redefines::create_comprehensive_error_context(
                        0,
                        &tail_odo.array_path,
                        0,
                        None,
                    ),
                )
            })?;

    let counter_field =
        crate::odo_redefines::find_field_by_path_or_unique_name(schema, &tail_odo.counter_path)
            .ok_or_else(|| {
                crate::odo_redefines::handle_missing_counter_field(
                    &tail_odo.counter_path,
                    &tail_odo.array_path,
                    schema,
                    0,
                    0,
                )
            })?;

    if let Some(array) = json_lookup_array(fields_value, &array_field.path)
        .or_else(|| json_lookup_array(fields_value, &tail_odo.array_path))
    {
        let Some(counter_json_value) = json_lookup_value(fields_value, &counter_field.path)
            .or_else(|| json_lookup_value(fields_value, &tail_odo.counter_path))
        else {
            return Err(crate::odo_redefines::handle_missing_counter_field(
                &counter_field.path,
                &array_field.path,
                schema,
                0,
                u64::from(counter_field.offset),
            ));
        };

        // The counter field is encoded independently as a scalar, and the array
        // is written using its own length. If the two disagree, whichever value
        // wins at encode time makes the other one wrong on decode - silently
        // dropping array elements or leaving a stale counter. Reject the
        // inconsistent input instead of guessing which side is authoritative.
        if let Some(counter_count) = json_counter_value_as_usize(counter_json_value)
            && counter_count != array.len()
        {
            return Err(Error::new(
                ErrorCode::CBKE521_ARRAY_LEN_OOB,
                format!(
                    "ODO counter '{}' value ({counter_count}) does not match array '{}' length ({})",
                    counter_field.path,
                    array_field.path,
                    array.len()
                ),
            )
            .with_context(crate::odo_redefines::create_comprehensive_error_context(
                0,
                &array_field.path,
                u64::from(array_field.offset),
                Some(format!(
                    "counter_field={}, counter_value={counter_count}, array_length={}",
                    counter_field.path,
                    array.len()
                )),
            )));
        }

        let context = crate::odo_redefines::OdoValidationContext {
            field_path: array_field.path.clone(),
            counter_path: counter_field.path.clone(),
            record_index: 0,
            byte_offset: u64::from(array_field.offset),
        };

        crate::odo_redefines::validate_odo_encode(
            array.len(),
            tail_odo.min_count,
            tail_odo.max_count,
            &context,
            options,
        )?;
    }

    Ok(())
}

/// Parse a JSON counter field value (string or number) into an element count.
fn json_counter_value_as_usize(value: &Value) -> Option<usize> {
    match value {
        Value::Number(n) => n.as_u64().and_then(|v| usize::try_from(v).ok()),
        Value::String(s) => s.trim().parse::<usize>().ok(),
        _ => None,
    }
}

fn json_lookup_value<'a>(value: &'a Value, field_path: &str) -> Option<&'a Value> {
    json_lookup_exact_value(value, field_path).or_else(|| {
        let (_, path_without_root) = field_path.split_once('.')?;
        json_lookup_exact_value(value, path_without_root)
    })
}

fn json_lookup_exact_value<'a>(value: &'a Value, field_path: &str) -> Option<&'a Value> {
    let mut current = value;
    for segment in field_path.split('.') {
        current = current.as_object()?.get(segment)?;
    }
    Some(current)
}

fn json_lookup_array<'a>(value: &'a Value, field_path: &str) -> Option<&'a Vec<Value>> {
    let leaf = field_path.split('.').next_back().unwrap_or("");
    match json_lookup_value(value, field_path) {
        Some(Value::Array(array)) => Some(array),
        _ => {
            if let Value::Object(obj) = value {
                obj.get(leaf).and_then(|candidate| candidate.as_array())
            } else {
                None
            }
        }
    }
}

/// Helper function to encode JSON fields to binary payload
fn encode_fields_to_bytes(
    schema: &Schema,
    json: &Value,
    encoding_metadata: Option<&serde_json::Map<String, Value>>,
    options: &EncodeOptions,
) -> Result<Vec<u8>> {
    let maximum_record_length = schema.lrecl_fixed.unwrap_or_else(|| {
        // For variable length, estimate based on schema
        schema.fields.iter().map(|f| f.len).sum::<u32>()
    }) as usize;
    let record_length = if options.format == RecordFormat::RDW {
        rdw_record_length_for_json(schema, json).unwrap_or(maximum_record_length)
    } else {
        maximum_record_length
    };

    let mut buffer = vec![0u8; record_length];

    if let Some(obj) = json.as_object() {
        encode_fields_recursive(
            &schema.fields,
            obj,
            encoding_metadata,
            "",
            &mut buffer,
            0,
            options,
        )?;
    }

    Ok(buffer)
}

/// Return the payload length required by one JSON record for RDW encoding.
///
/// `Schema::lrecl_fixed` is the maximum allocation for an ODO layout. RDW
/// records are variable-length, so retaining that allocation would silently
/// add zero-filled occurrences during a decode → encode round-trip. Preserve
/// any fixed storage after a nested ODO group when calculating the length.
fn rdw_record_length_for_json(schema: &Schema, json: &Value) -> Option<usize> {
    let array_field = schema
        .all_fields()
        .into_iter()
        .find(|field| matches!(field.occurs, Some(copybook_core::Occurs::ODO { .. })))?;
    let Some(copybook_core::Occurs::ODO { max, .. }) = array_field.occurs else {
        return None;
    };
    let field_offset = usize::try_from(array_field.offset).ok()?;
    let field_length = usize::try_from(array_field.len).ok()?;
    let maximum_array_end = array_field
        .offset
        .checked_add(array_field.len.checked_mul(max)?)?;
    let maximum_array_end = usize::try_from(maximum_array_end).ok()?;
    let schema_length = usize::try_from(schema.lrecl_fixed?).ok()?;
    let trailing_length = schema_length.checked_sub(maximum_array_end)?;
    let array = json_lookup_array(json, &array_field.path)
        .or_else(|| json_lookup_array(json, &array_field.name))?;
    let count = array.len();
    let array_bytes = field_length.checked_mul(count)?;
    field_offset
        .checked_add(array_bytes)?
        .checked_add(trailing_length)
}

/// Recursively encode fields into the buffer
fn encode_fields_recursive(
    fields: &[copybook_core::Field],
    json_obj: &serde_json::Map<String, Value>,
    encoding_metadata: Option<&serde_json::Map<String, Value>>,
    path_prefix: &str,
    buffer: &mut [u8],
    offset: usize,
    options: &EncodeOptions,
) -> Result<usize> {
    let mut current_offset = offset;
    let mut name_occurrences = HashMap::new();

    for field in fields {
        let occurrence = name_occurrences
            .entry(field.name.as_str())
            .and_modify(|count| *count += 1)
            .or_insert(0);
        let json_field_name = emitted_field_name(json_obj, &field.name, *occurrence);
        let field_path = if path_prefix.is_empty() {
            field.name.clone()
        } else {
            format!("{path_prefix}.{}", field.name)
        };

        let field_names = FieldNames {
            path: &field_path,
            json: &json_field_name,
        };
        current_offset = encode_single_field(
            field,
            &field_names,
            json_obj,
            encoding_metadata,
            buffer,
            current_offset,
            options,
        )?;
    }

    Ok(current_offset)
}

#[inline]
fn collect_array_zoned_encoding_info(
    field: &copybook_core::Field,
    field_data: &[u8],
    options: &DecodeOptions,
    encoding_acc: &mut Vec<(String, ZonedEncodingFormat)>,
) {
    collect_zoned_encoding_info(field, &field.name, field_data, options, encoding_acc);
}

struct FieldNames<'a> {
    path: &'a str,
    json: &'a str,
}

fn emitted_field_name(
    json_obj: &serde_json::Map<String, Value>,
    field_name: &str,
    occurrence: usize,
) -> String {
    let candidate = if occurrence == 0 {
        field_name.to_owned()
    } else {
        format!("{field_name}__dup{}", occurrence + 1)
    };
    if json_obj.contains_key(&candidate) {
        candidate
    } else {
        field_name.to_owned()
    }
}

/// Encode a single field (scalar or group) into the output byte buffer.
///
/// Orchestrates the encoding of various COBOL data types by delegating to
/// specialized encoding functions.
#[inline]
#[allow(clippy::too_many_lines)]
fn encode_single_field(
    field: &copybook_core::Field,
    field_names: &FieldNames<'_>,
    json_obj: &serde_json::Map<String, Value>,
    encoding_metadata: Option<&serde_json::Map<String, Value>>,
    buffer: &mut [u8],
    current_offset: usize,
    options: &EncodeOptions,
) -> Result<usize> {
    use copybook_core::FieldKind;

    if let Some(occurs) = &field.occurs {
        return encode_occurs_field(
            field,
            occurs,
            field_names,
            json_obj,
            encoding_metadata,
            buffer,
            current_offset,
            options,
        );
    }

    match &field.kind {
        FieldKind::Group => encode_group_field(
            field,
            field_names,
            json_obj,
            encoding_metadata,
            buffer,
            current_offset,
            options,
        ),
        FieldKind::Alphanum { .. } => {
            encode_alphanum_field(field, json_obj, buffer, current_offset, options)
        }
        FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
            sign_separate,
        } => {
            if let Some(sign_sep) = sign_separate {
                let field_len = field.len as usize;
                if let Some(text) = json_obj.get(field_names.json).and_then(|v| v.as_str()) {
                    crate::numeric::encode_zoned_decimal_sign_separate(
                        text,
                        *digits,
                        *scale,
                        sign_sep,
                        options.codepage,
                        &mut buffer[current_offset..current_offset + field_len],
                    )?;
                }
                Ok(current_offset + field_len)
            } else {
                encode_zoned_decimal_field(
                    field,
                    field_names.path,
                    field_names.json,
                    json_obj,
                    encoding_metadata,
                    buffer,
                    current_offset,
                    options,
                    DecimalSpec {
                        digits: *digits,
                        scale: *scale,
                        signed: *signed,
                    },
                )
            }
        }
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => encode_packed_decimal_field(
            field,
            field_names.path,
            json_obj,
            buffer,
            current_offset,
            options,
            DecimalSpec {
                digits: *digits,
                scale: *scale,
                signed: *signed,
            },
        ),
        FieldKind::BinaryInt { bits, signed } => encode_binary_int_field(
            field,
            field_names.path,
            json_obj,
            buffer,
            current_offset,
            options,
            BinarySpec {
                bits: *bits,
                signed: *signed,
            },
        ),
        FieldKind::Condition { .. } => Ok(current_offset),
        FieldKind::Renames { .. } => {
            // RENAMES fields are aliases with no storage of their own.
            // The actual bytes are written by the storage fields (members).
            // Skip encoding for RENAMES - the aliased fields handle it.
            Ok(current_offset)
        }
        FieldKind::EditedNumeric {
            pic_string, scale, ..
        } => {
            // Phase E3.1: Encode edited PIC fields
            if let Some(text) = encodable_numeric_text(
                json_obj,
                field,
                &field.name,
                "an edited numeric string",
                options.coerce_numbers,
            )? {
                // Tokenize the PIC pattern
                let pattern = crate::edited_pic::tokenize_edited_pic(pic_string)?;

                // Encode the edited numeric value
                let encoded = crate::edited_pic::encode_edited_numeric(
                    &text,
                    &pattern,
                    *scale,
                    field.blank_when_zero,
                )?;

                // Convert to EBCDIC and write to buffer
                let bytes = crate::charset::utf8_to_ebcdic(&encoded, options.codepage)?;
                let field_len = field.len as usize;
                let copy_len = bytes.len().min(field_len);

                if current_offset + field_len <= buffer.len() {
                    buffer[current_offset..current_offset + copy_len]
                        .copy_from_slice(&bytes[..copy_len]);
                    // Pad with codepage-aware space (0x40 for EBCDIC, 0x20 for ASCII)
                    let space = crate::charset::space_byte(options.codepage);
                    buffer[current_offset + copy_len..current_offset + field_len].fill(space);
                }
            }
            Ok(current_offset + field.len as usize)
        }
        FieldKind::FloatSingle => {
            let field_len = field.len as usize;
            if let Some(val) = json_obj.get(&field.name) {
                let f = match val {
                    Value::Number(n) => {
                        let f64_val = n.as_f64().unwrap_or(0.0);
                        // Check for f64->f32 overflow
                        if f64_val.is_finite()
                            && (f64_val > f64::from(f32::MAX) || f64_val < f64::from(f32::MIN))
                        {
                            return Err(Error::new(
                                ErrorCode::CBKE531_FLOAT_ENCODE_OVERFLOW,
                                format!("Value overflow for COMP-1 field '{}'", field.name),
                            ));
                        }
                        // Overflow already checked above, truncation is intentional
                        #[allow(clippy::cast_possible_truncation)]
                        {
                            f64_val as f32
                        }
                    }
                    Value::String(s) => s.parse::<f32>().map_err(|e| {
                        Error::new(
                            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                            format!(
                                "Cannot parse '{}' as f32 for field '{}': {}",
                                s, field.name, e
                            ),
                        )
                    })?,
                    Value::Null => f32::NAN,
                    _ => {
                        return Err(Error::new(
                            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                            format!("Expected number for COMP-1 field '{}'", field.name),
                        ));
                    }
                };
                if current_offset + field_len <= buffer.len() {
                    crate::numeric::encode_float_single_with_format(
                        f,
                        &mut buffer[current_offset..current_offset + field_len],
                        options.float_format,
                    )?;
                }
            }
            Ok(current_offset + field_len)
        }
        FieldKind::FloatDouble => {
            let field_len = field.len as usize;
            if let Some(val) = json_obj.get(&field.name) {
                let f = match val {
                    Value::Number(n) => n.as_f64().unwrap_or(0.0),
                    Value::String(s) => s.parse::<f64>().map_err(|e| {
                        Error::new(
                            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                            format!(
                                "Cannot parse '{}' as f64 for field '{}': {}",
                                s, field.name, e
                            ),
                        )
                    })?,
                    Value::Null => f64::NAN,
                    _ => {
                        return Err(Error::new(
                            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                            format!("Expected number for COMP-2 field '{}'", field.name),
                        ));
                    }
                };
                if current_offset + field_len <= buffer.len() {
                    crate::numeric::encode_float_double_with_format(
                        f,
                        &mut buffer[current_offset..current_offset + field_len],
                        options.float_format,
                    )?;
                }
            }
            Ok(current_offset + field_len)
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn encode_occurs_field(
    field: &copybook_core::Field,
    occurs: &copybook_core::Occurs,
    field_names: &FieldNames<'_>,
    json_obj: &serde_json::Map<String, Value>,
    encoding_metadata: Option<&serde_json::Map<String, Value>>,
    buffer: &mut [u8],
    current_offset: usize,
    options: &EncodeOptions,
) -> Result<usize> {
    let max_count = occurs_max_count(occurs);
    let element_len = field.len as usize;
    let allocation_len = element_len
        .checked_mul(max_count as usize)
        .ok_or_else(|| Error::new(ErrorCode::CBKS141_RECORD_TOO_LARGE, "OCCURS size overflow"))?;

    let Some(array) = json_obj.get(field_names.json).and_then(Value::as_array) else {
        return Ok(current_offset + allocation_len);
    };

    validate_occurs_array_len(array.len(), occurs, field)?;

    for (index, element) in array.iter().enumerate() {
        let element_offset = current_offset + index * element_len;
        encode_occurs_element(
            field,
            field_names,
            element,
            encoding_metadata,
            buffer,
            element_offset,
            options,
        )?;
    }

    Ok(current_offset + allocation_len)
}

fn occurs_max_count(occurs: &copybook_core::Occurs) -> u32 {
    match occurs {
        copybook_core::Occurs::Fixed { count } => *count,
        copybook_core::Occurs::ODO { max, .. } => *max,
    }
}

fn validate_occurs_array_len(
    actual_len: usize,
    occurs: &copybook_core::Occurs,
    field: &copybook_core::Field,
) -> Result<()> {
    match occurs {
        copybook_core::Occurs::Fixed { count } if actual_len != *count as usize => Err(Error::new(
            ErrorCode::CBKE521_ARRAY_LEN_OOB,
            format!(
                "Array length {} doesn't match fixed OCCURS count {} for field '{}'",
                actual_len, count, field.path
            ),
        )
        .with_field(field.path.clone())),
        copybook_core::Occurs::ODO { max, .. } if actual_len > *max as usize => Err(Error::new(
            ErrorCode::CBKE521_ARRAY_LEN_OOB,
            format!(
                "Array length {} exceeds ODO max {} for field '{}'",
                actual_len, max, field.path
            ),
        )
        .with_field(field.path.clone())),
        copybook_core::Occurs::Fixed { .. } | copybook_core::Occurs::ODO { .. } => Ok(()),
    }
}

fn encode_occurs_element(
    field: &copybook_core::Field,
    field_names: &FieldNames<'_>,
    element: &Value,
    encoding_metadata: Option<&serde_json::Map<String, Value>>,
    buffer: &mut [u8],
    element_offset: usize,
    options: &EncodeOptions,
) -> Result<()> {
    use copybook_core::FieldKind;

    let mut element_field = field.clone();
    element_field.occurs = None;

    if let FieldKind::Group = &field.kind {
        let element_obj = element.as_object().ok_or_else(|| {
            Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!("Expected object element for OCCURS group '{}'", field.path),
            )
            .with_field(field.path.clone())
        })?;
        encode_fields_recursive(
            &element_field.children,
            element_obj,
            encoding_metadata,
            field_names.path,
            buffer,
            element_offset,
            options,
        )?;
    } else {
        let mut element_obj = serde_json::Map::new();
        element_obj.insert(field.name.clone(), element.clone());
        encode_single_field(
            &element_field,
            field_names,
            &element_obj,
            encoding_metadata,
            buffer,
            element_offset,
            options,
        )?;
    }

    Ok(())
}

/// Recursively encode a group field and its children.
#[inline]
fn encode_group_field(
    field: &copybook_core::Field,
    field_names: &FieldNames<'_>,
    json_obj: &serde_json::Map<String, Value>,
    encoding_metadata: Option<&serde_json::Map<String, Value>>,
    buffer: &mut [u8],
    current_offset: usize,
    options: &EncodeOptions,
) -> Result<usize> {
    if let Some(sub_obj) = json_obj.get(field_names.json).and_then(|v| v.as_object()) {
        encode_fields_recursive(
            &field.children,
            sub_obj,
            encoding_metadata,
            field_names.path,
            buffer,
            current_offset,
            options,
        )
    } else {
        encode_fields_recursive(
            &field.children,
            json_obj,
            encoding_metadata,
            field_names.path,
            buffer,
            current_offset,
            options,
        )
    }
}

/// Encode an alphanumeric (PIC X) field.
#[inline]
fn encode_alphanum_field(
    field: &copybook_core::Field,
    json_obj: &serde_json::Map<String, Value>,
    buffer: &mut [u8],
    current_offset: usize,
    options: &EncodeOptions,
) -> Result<usize> {
    let field_len = field.len as usize;

    if let Some(text) = json_obj.get(&field.name).and_then(|value| value.as_str()) {
        // Validate encoded byte length doesn't exceed field capacity.
        let bytes = crate::charset::utf8_to_ebcdic(text, options.codepage)?;
        if bytes.len() > field_len {
            return Err(Error::new(
                ErrorCode::CBKE515_STRING_LENGTH_VIOLATION,
                format!(
                    "Encoded byte length {} exceeds field capacity {} for alphanumeric field {}",
                    bytes.len(),
                    field_len,
                    field.path
                ),
            )
            .with_field(field.path.clone()));
        }

        let copy_len = bytes.len();

        if current_offset + field_len <= buffer.len() {
            buffer[current_offset..current_offset + copy_len].copy_from_slice(&bytes);
            // Pad with codepage-aware space (0x40 for EBCDIC, 0x20 for ASCII)
            let space = crate::charset::space_byte(options.codepage);
            buffer[current_offset + copy_len..current_offset + field_len].fill(space);
        }
    }

    Ok(current_offset + field_len)
}

#[derive(Copy, Clone)]
struct DecimalSpec {
    digits: u16,
    scale: i16,
    signed: bool,
}

fn resolve_preserved_zoned_format(
    metadata: &serde_json::Map<String, Value>,
    field_path: &str,
    field_name: &str,
) -> Option<ZonedEncodingFormat> {
    let candidates = [field_path, field_name];
    for key in candidates {
        if let Some(format) = metadata
            .get(key)
            .and_then(parse_zoned_encoding_metadata_value)
        {
            return Some(format);
        }
    }
    None
}

fn parse_zoned_encoding_metadata_value(value: &Value) -> Option<ZonedEncodingFormat> {
    match value {
        Value::String(s) => parse_zoned_encoding_format_str(s),
        Value::Object(map) => map
            .get("zoned_encoding")
            .and_then(Value::as_str)
            .and_then(parse_zoned_encoding_format_str),
        _ => None,
    }
}

fn parse_zoned_encoding_format_str(value: &str) -> Option<ZonedEncodingFormat> {
    match value.trim().to_ascii_lowercase().as_str() {
        "ascii" => Some(ZonedEncodingFormat::Ascii),
        "ebcdic" => Some(ZonedEncodingFormat::Ebcdic),
        "auto" => Some(ZonedEncodingFormat::Auto),
        _ => None,
    }
}

/// Extract a string representation from a JSON value for encoding numeric fields.
///
/// When `coerce_numbers` is true, `Value::Number` inputs are converted to their
/// string representation (e.g., `42` → `"42"`, `123.45` → `"123.45"`).
/// When false, only `Value::String` inputs are accepted.
fn coerce_to_str(value: &Value, coerce: bool) -> Option<String> {
    match value {
        Value::String(s) => Some(s.clone()),
        Value::Number(n) if coerce => Some(n.to_string()),
        _ => None,
    }
}

/// Name the JSON type of `value` the way a user would recognize it in their input.
fn json_type_name(value: &Value) -> &'static str {
    match value {
        Value::Null => "null",
        Value::Bool(_) => "boolean",
        Value::Number(_) => "number",
        Value::String(_) => "string",
        Value::Array(_) => "array",
        Value::Object(_) => "object",
    }
}

/// Look up a numeric field's text value, rejecting a present-but-unusable JSON type.
///
/// Returns `Ok(None)` when the field is absent or `null`, which leaves the field at
/// its default bytes. When the field *is* present but carries a type this encoder
/// cannot use, this returns [`ErrorCode::CBKE501_JSON_TYPE_MISMATCH`] rather than
/// silently writing zeros over the value the caller supplied.
fn encodable_numeric_text(
    json_obj: &serde_json::Map<String, Value>,
    field: &copybook_core::Field,
    json_field_name: &str,
    expected: &str,
    coerce_numbers: bool,
) -> Result<Option<String>> {
    let Some(value) = json_obj.get(json_field_name) else {
        return Ok(None);
    };
    if value.is_null() {
        return Ok(None);
    }
    if let Some(text) = coerce_to_str(value, coerce_numbers) {
        return Ok(Some(text));
    }

    let hint = if value.is_number() {
        " (pass --coerce-numbers to accept JSON numbers here)"
    } else {
        ""
    };
    Err(Error::new(
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
        format!(
            "Field '{}' expects {expected}, found {}{hint}",
            field.name,
            json_type_name(value),
        ),
    ))
}

#[inline]
#[allow(clippy::too_many_arguments)]
fn encode_zoned_decimal_field(
    field: &copybook_core::Field,
    field_path: &str,
    json_field_name: &str,
    json_obj: &serde_json::Map<String, Value>,
    encoding_metadata: Option<&serde_json::Map<String, Value>>,
    buffer: &mut [u8],
    current_offset: usize,
    options: &EncodeOptions,
    spec: DecimalSpec,
) -> Result<usize> {
    let field_len = field.len as usize;

    if let Some(text) = encodable_numeric_text(
        json_obj,
        field,
        json_field_name,
        "a zoned decimal string",
        options.coerce_numbers,
    )? {
        // Check BWZ policy: when field has BLANK WHEN ZERO and bwz_encode is enabled,
        // zero values are encoded as spaces instead of numeric zeros.
        if field.blank_when_zero && options.bwz_encode {
            let encoded = crate::numeric::encode_zoned_decimal_with_bwz(
                &text,
                spec.digits,
                spec.scale,
                spec.signed,
                options.codepage,
                options.bwz_encode,
            )?;
            if current_offset + field_len <= buffer.len() && encoded.len() == field_len {
                buffer[current_offset..current_offset + field_len].copy_from_slice(&encoded);
            }
            return Ok(current_offset + field_len);
        }

        let preserved_format = encoding_metadata
            .and_then(|meta| resolve_preserved_zoned_format(meta, field_path, json_field_name));
        let resolved_format = options
            .zoned_encoding_override
            .or(preserved_format)
            .unwrap_or(options.preferred_zoned_encoding);
        // Resolve Auto format and determine zero policy in single match (no unreachable arms)
        let (effective_format, zero_policy) = match resolved_format {
            ZonedEncodingFormat::Ascii => (ZonedEncodingFormat::Ascii, ZeroSignPolicy::Positive),
            ZonedEncodingFormat::Ebcdic => (ZonedEncodingFormat::Ebcdic, ZeroSignPolicy::Preferred),
            ZonedEncodingFormat::Auto => {
                if options.codepage.is_ascii() {
                    (ZonedEncodingFormat::Ascii, ZeroSignPolicy::Positive)
                } else {
                    (ZonedEncodingFormat::Ebcdic, ZeroSignPolicy::Preferred)
                }
            }
        };

        let encoded = crate::numeric::encode_zoned_decimal_with_format_and_policy(
            &text,
            spec.digits,
            spec.scale,
            spec.signed,
            options.codepage,
            Some(effective_format),
            zero_policy,
        )?;

        if current_offset + field_len <= buffer.len() && encoded.len() == field_len {
            buffer[current_offset..current_offset + field_len].copy_from_slice(&encoded);
        }
    }

    Ok(current_offset + field_len)
}

#[inline]
fn encode_packed_decimal_field(
    field: &copybook_core::Field,
    _field_path: &str,
    json_obj: &serde_json::Map<String, Value>,
    buffer: &mut [u8],
    current_offset: usize,
    options: &EncodeOptions,
    spec: DecimalSpec,
) -> Result<usize> {
    let field_len = field.len as usize;

    if let Some(text) = encodable_numeric_text(
        json_obj,
        field,
        &field.name,
        "a packed decimal string",
        options.coerce_numbers,
    )? {
        let encoded =
            crate::numeric::encode_packed_decimal(&text, spec.digits, spec.scale, spec.signed)?;
        if current_offset + field_len <= buffer.len() && encoded.len() == field_len {
            buffer[current_offset..current_offset + field_len].copy_from_slice(&encoded);
        }
    }

    Ok(current_offset + field_len)
}

#[derive(Copy, Clone)]
struct BinarySpec {
    bits: u16,
    signed: bool,
}

#[inline]
fn encode_binary_int_field(
    field: &copybook_core::Field,
    _field_path: &str,
    json_obj: &serde_json::Map<String, Value>,
    buffer: &mut [u8],
    current_offset: usize,
    options: &EncodeOptions,
    spec: BinarySpec,
) -> Result<usize> {
    let field_len = field.len as usize;

    let value = match json_obj.get(&field.name) {
        None => None,
        Some(v) if v.is_null() => None,
        Some(v) => Some(v),
    };

    if let Some(value) = value {
        // Direct numeric path (always available for Value::Number in i64 range).
        let num = if let Some(n) = value.as_i64() {
            n
        } else {
            let text = coerce_to_str(value, options.coerce_numbers).ok_or_else(|| {
                Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    format!(
                        "Field '{}' expects an integer, found {}",
                        field.name,
                        json_type_name(value),
                    ),
                )
            })?;
            text.parse::<i64>().map_err(|e| {
                Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    format!(
                        "Field '{}' expects an integer, found '{text}': {e}",
                        field.name,
                    ),
                )
            })?
        };
        let encoded = crate::numeric::encode_binary_int(num, spec.bits, spec.signed)?;
        if current_offset + field_len <= buffer.len() && encoded.len() == field_len {
            buffer[current_offset..current_offset + field_len].copy_from_slice(&encoded);
        }
    }

    Ok(current_offset + field_len)
}

/// Decode a file to JSONL format
///
/// Reads records from `input` using the configured [`RecordFormat`]
///
/// When `options.threads` is greater than one, records are decoded through a
/// bounded worker pool and emitted in input order. A zero thread setting uses
/// one worker for a safe single-threaded fallback; requests above the safe
/// worker limit are capped.
///
/// # Examples
///
/// ```
/// use copybook_core::parse_copybook;
/// use copybook_codec::{decode_file_to_jsonl, DecodeOptions};
/// use copybook_codec::options::{Codepage, RecordFormat};
///
/// let schema = parse_copybook("01 FLD PIC X(5).").unwrap();
/// let input: &[u8] = b"HELLOWORLD";  // Two 5-byte records
/// let mut output = Vec::new();
/// let options = DecodeOptions::new()
///     .with_codepage(Codepage::ASCII)
///     .with_format(RecordFormat::Fixed);
/// let summary = decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
/// assert_eq!(summary.records_processed, 2);
/// ```
///
/// # Errors
/// Returns an error if the input cannot be read, decoded, or written.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_file_to_jsonl(
    schema: &Schema,
    input: impl Read,
    mut output: impl Write,
    options: &DecodeOptions,
) -> Result<RunSummary> {
    let start_time = std::time::Instant::now();
    let mut summary = RunSummary::with_threads(effective_worker_count(options.threads));
    summary.set_schema_fingerprint(schema.fingerprint.clone());

    reset_warning_counter();

    match options.format {
        RecordFormat::Fixed => {
            process_fixed_records(schema, input, &mut output, options, &mut summary)?;
        }
        RecordFormat::RDW => {
            process_rdw_records(schema, input, &mut output, options, &mut summary)?;
        }
    }

    let elapsed_ms = start_time.elapsed().as_millis();
    summary.processing_time_ms = u64::try_from(elapsed_ms).unwrap_or(u64::MAX);
    summary.calculate_throughput();
    summary.warnings = warning_count();
    telemetry::record_completion(
        summary.processing_time_seconds(),
        summary.throughput_mbps,
        options,
    );
    info!(
        target: "copybook::decode",
        records_processed = summary.records_processed,
        records_with_errors = summary.records_with_errors,
        warnings = summary.warnings,
        bytes_processed = summary.bytes_processed,
        elapsed_ms = summary.processing_time_ms,
        throughput_mibps = summary.throughput_mbps,
        schema_fingerprint = %summary.schema_fingerprint,
        codepage = %options.codepage,
        format = ?options.format,
        strict_mode = options.strict_mode,
        raw_mode = ?options.emit_raw,
    );

    Ok(summary)
}

fn process_fixed_records<R: Read, W: Write>(
    schema: &Schema,
    reader: R,
    output: &mut W,
    options: &DecodeOptions,
    summary: &mut RunSummary,
) -> Result<()> {
    if options.threads > 1 {
        return process_fixed_records_parallel(schema, reader, output, options, summary);
    }

    let mut reader = crate::file::fixed::reader(reader, schema)?;
    let mut scratch = crate::memory::ScratchBuffers::new();
    let mut record_index = 0u64;
    let mut record_offset = 0u64;

    while let Some(record_data) = reader.read_record()? {
        record_index += 1;
        let current_offset = record_offset;
        record_offset = record_offset.saturating_add(record_data.len() as u64);
        crate::file::fixed::validate_record_length(
            schema,
            reader.lrecl(),
            reader.record_count(),
            &record_data,
        )?;
        summary.bytes_processed += record_data.len() as u64;
        telemetry::record_read(record_data.len(), options);

        let raw_data_for_decode = match options.emit_raw {
            crate::options::RawMode::Record => Some(record_data.clone()),
            _ => None,
        };

        match decode_record_with_scratch_and_raw(
            schema,
            &record_data,
            options,
            raw_data_for_decode.as_deref(),
            record_index,
            Some(current_offset),
            &mut scratch,
        ) {
            Ok(json_value) => {
                write_json_record(output, &json_value)?;
                summary.records_processed += 1;
            }
            Err(error) => {
                summary.note_failure(record_index, &error);
                telemetry::record_error(error.family_prefix());
                if options.strict_mode {
                    return Err(error);
                }
            }
        }
    }

    Ok(())
}

struct DecodeWork {
    payload: Vec<u8>,
    raw_data: Option<Vec<u8>>,
    record_index: u64,
    record_offset: u64,
}

struct DecodeOutcome {
    result: Result<Value>,
    warnings: u64,
    /// Carried back from the work item so a failure can name its record.
    record_index: u64,
}

fn effective_worker_count(requested: usize) -> usize {
    requested.clamp(1, MAX_WORKERS)
}

fn decode_worker_pool(
    schema: &Schema,
    options: &DecodeOptions,
) -> crate::memory::WorkerPool<DecodeWork, DecodeOutcome> {
    let workers = effective_worker_count(options.threads);
    let channel_capacity = workers.saturating_mul(4).max(1);
    let max_window_size = workers.saturating_mul(2).max(1);
    let schema = Arc::new(schema.clone());
    let options = Arc::new(options.clone());

    crate::memory::WorkerPool::new(
        workers,
        channel_capacity,
        max_window_size,
        move |work: DecodeWork, scratch: &mut crate::memory::ScratchBuffers| {
            let warning_count_before = warning_count();
            let result = decode_record_with_scratch_and_raw(
                &schema,
                &work.payload,
                &options,
                work.raw_data.as_deref(),
                work.record_index,
                Some(work.record_offset),
                scratch,
            );
            let warnings = warning_count().saturating_sub(warning_count_before);
            DecodeOutcome {
                result,
                warnings,
                record_index: work.record_index,
            }
        },
    )
}

fn process_decode_batch<W: Write>(
    pool: &mut crate::memory::WorkerPool<DecodeWork, DecodeOutcome>,
    batch_len: usize,
    output: &mut W,
    options: &DecodeOptions,
    summary: &mut RunSummary,
) -> Result<()> {
    let mut first_error = None;

    for _ in 0..batch_len {
        let outcome = pool
            .recv_ordered()
            .map_err(|error| Error::new(ErrorCode::CBKI001_INVALID_STATE, error.to_string()))?
            .ok_or_else(|| {
                Error::new(
                    ErrorCode::CBKI001_INVALID_STATE,
                    "decode worker pool ended before the submitted batch completed",
                )
            })?;

        for _ in 0..outcome.warnings {
            increment_warning_counter();
        }

        if first_error.is_some() {
            continue;
        }

        let record_index = outcome.record_index;
        match outcome.result {
            Ok(json_value) => {
                write_json_record(output, &json_value)?;
                summary.records_processed += 1;
            }
            Err(error) => {
                summary.note_failure(record_index, &error);
                telemetry::record_error(error.family_prefix());
                if options.strict_mode {
                    first_error = Some(error);
                }
            }
        }
    }

    first_error.map_or(Ok(()), Err)
}

fn process_fixed_records_parallel<R: Read, W: Write>(
    schema: &Schema,
    reader: R,
    output: &mut W,
    options: &DecodeOptions,
    summary: &mut RunSummary,
) -> Result<()> {
    let mut reader = crate::file::fixed::reader(reader, schema)?;
    let workers = effective_worker_count(options.threads);
    let batch_capacity = workers.saturating_mul(4).max(1);
    let mut pool = decode_worker_pool(schema, options);
    let mut record_index = 0_u64;
    let mut record_offset = 0_u64;
    let mut batch_len = 0_usize;

    loop {
        let record = match reader.read_record() {
            Ok(record) => record,
            Err(error) => {
                let pending_result = if batch_len > 0 {
                    process_decode_batch(&mut pool, batch_len, output, options, summary)
                } else {
                    Ok(())
                };
                let _ = pool.shutdown();
                pending_result?;
                return Err(error);
            }
        };
        let Some(record_data) = record else { break };

        record_index += 1;
        let current_offset = record_offset;
        record_offset = record_offset.saturating_add(record_data.len() as u64);
        crate::file::fixed::validate_record_length(
            schema,
            reader.lrecl(),
            reader.record_count(),
            &record_data,
        )?;
        summary.bytes_processed += record_data.len() as u64;
        telemetry::record_read(record_data.len(), options);
        let raw_data = match options.emit_raw {
            crate::options::RawMode::Record => Some(record_data.clone()),
            _ => None,
        };

        if let Err(error) = pool.submit(DecodeWork {
            payload: record_data,
            raw_data,
            record_index,
            record_offset: current_offset,
        }) {
            let pending_result = if batch_len > 0 {
                process_decode_batch(&mut pool, batch_len, output, options, summary)
            } else {
                Ok(())
            };
            let _ = pool.shutdown();
            pending_result?;
            return Err(Error::new(
                ErrorCode::CBKI001_INVALID_STATE,
                error.to_string(),
            ));
        }
        batch_len += 1;

        if batch_len == batch_capacity {
            let result = process_decode_batch(&mut pool, batch_len, output, options, summary);
            batch_len = 0;
            if let Err(error) = result {
                let _ = pool.shutdown();
                return Err(error);
            }
        }
    }

    if batch_len > 0 {
        let result = process_decode_batch(&mut pool, batch_len, output, options, summary);
        if let Err(error) = result {
            let _ = pool.shutdown();
            return Err(error);
        }
    }

    pool.shutdown().map_err(|error| {
        Error::new(
            ErrorCode::CBKI001_INVALID_STATE,
            format!("decode worker pool shutdown failed: {error}"),
        )
    })
}

fn process_rdw_records<R: Read, W: Write>(
    schema: &Schema,
    reader: R,
    output: &mut W,
    options: &DecodeOptions,
    summary: &mut RunSummary,
) -> Result<()> {
    if options.threads > 1 {
        return process_rdw_records_parallel(schema, reader, output, options, summary);
    }

    let mut reader = crate::record::RDWRecordReader::new(reader, options.strict_mode);
    let mut scratch = crate::memory::ScratchBuffers::new();
    let mut record_index = 0u64;
    let mut record_offset = 0u64;

    while let Some(rdw_record) = reader.read_record()? {
        record_index += 1;
        let record_bytes = rdw_record.header.len() + rdw_record.payload.len();
        let current_offset = record_offset;
        record_offset = record_offset.saturating_add(record_bytes as u64);
        summary.bytes_processed += record_bytes as u64;
        telemetry::record_read(record_bytes, options);
        if rdw_record.reserved() != 0 {
            increment_warning_counter();
        }

        // The fixed-length underflow guard only applies to genuinely
        // fixed-length schemas. For variable-length records driven by a tail
        // OCCURS DEPENDING ON, `lrecl_fixed` holds the *maximum* allocation
        // (base prefix + max occurrences), so a valid record shorter than that
        // maximum must not be rejected here. The ODO-aware record decoder below
        // resolves the actual length from the counter and still raises
        // `CBKD301_RECORD_TOO_SHORT` when the payload is genuinely too short.
        if let Some(schema_lrecl) = schema.lrecl_fixed
            && schema.tail_odo.is_none()
            && rdw_record.payload.len() < schema_lrecl as usize
        {
            let error = rdw_underflow_error(schema_lrecl, rdw_record.payload.len());

            summary.note_failure(record_index, &error);
            telemetry::record_error(error.family_prefix());
            if options.strict_mode {
                return Err(error);
            }
            continue;
        }

        let full_raw_data = rdw_raw_data(&rdw_record, options.emit_raw);

        match decode_record_with_scratch_and_raw(
            schema,
            &rdw_record.payload,
            options,
            full_raw_data.as_deref(),
            record_index,
            Some(current_offset),
            &mut scratch,
        ) {
            Ok(json_value) => {
                write_json_record(output, &json_value)?;
                summary.records_processed += 1;
            }
            Err(error) => {
                summary.note_failure(record_index, &error);
                telemetry::record_error(error.family_prefix());
                if options.strict_mode {
                    return Err(error);
                }
            }
        }
    }

    Ok(())
}

fn rdw_underflow_error(schema_lrecl: u32, payload_len: usize) -> Error {
    Error::new(
        ErrorCode::CBKF221_RDW_UNDERFLOW,
        format!("RDW payload too short: {payload_len} bytes, schema requires {schema_lrecl} bytes"),
    )
}

fn rdw_raw_data(
    record: &crate::record::RDWRecord,
    raw_mode: crate::options::RawMode,
) -> Option<Vec<u8>> {
    match raw_mode {
        crate::options::RawMode::RecordRDW => {
            let mut full_data = Vec::with_capacity(record.header.len() + record.payload.len());
            full_data.extend_from_slice(&record.header);
            full_data.extend_from_slice(&record.payload);
            Some(full_data)
        }
        crate::options::RawMode::Record => Some(record.payload.clone()),
        _ => None,
    }
}

fn process_rdw_records_parallel<R: Read, W: Write>(
    schema: &Schema,
    reader: R,
    output: &mut W,
    options: &DecodeOptions,
    summary: &mut RunSummary,
) -> Result<()> {
    let mut reader = crate::record::RDWRecordReader::new(reader, options.strict_mode);
    let workers = effective_worker_count(options.threads);
    let batch_capacity = workers.saturating_mul(4).max(1);
    let mut pool = decode_worker_pool(schema, options);
    let mut record_index = 0_u64;
    let mut record_offset = 0_u64;
    let mut batch_len = 0_usize;

    loop {
        let rdw_record = match reader.read_record() {
            Ok(record) => record,
            Err(error) => {
                let pending_result = if batch_len > 0 {
                    process_decode_batch(&mut pool, batch_len, output, options, summary)
                } else {
                    Ok(())
                };
                let _ = pool.shutdown();
                pending_result?;
                return Err(error);
            }
        };
        let Some(rdw_record) = rdw_record else { break };

        record_index += 1;
        let record_bytes = rdw_record.header.len() + rdw_record.payload.len();
        let current_offset = record_offset;
        record_offset = record_offset.saturating_add(record_bytes as u64);
        summary.bytes_processed += record_bytes as u64;
        telemetry::record_read(record_bytes, options);
        if rdw_record.reserved() != 0 {
            increment_warning_counter();
        }

        if let Some(schema_lrecl) = schema.lrecl_fixed
            && schema.tail_odo.is_none()
            && rdw_record.payload.len() < schema_lrecl as usize
        {
            let error = rdw_underflow_error(schema_lrecl, rdw_record.payload.len());

            summary.note_failure(record_index, &error);
            telemetry::record_error(error.family_prefix());
            if options.strict_mode {
                let pending_result = if batch_len > 0 {
                    process_decode_batch(&mut pool, batch_len, output, options, summary)
                } else {
                    Ok(())
                };
                let _ = pool.shutdown();
                pending_result?;
                return Err(error);
            }
            continue;
        }

        let full_raw_data = rdw_raw_data(&rdw_record, options.emit_raw);

        if let Err(error) = pool.submit(DecodeWork {
            payload: rdw_record.payload,
            raw_data: full_raw_data,
            record_index,
            record_offset: current_offset,
        }) {
            let pending_result = if batch_len > 0 {
                process_decode_batch(&mut pool, batch_len, output, options, summary)
            } else {
                Ok(())
            };
            let _ = pool.shutdown();
            pending_result?;
            return Err(Error::new(
                ErrorCode::CBKI001_INVALID_STATE,
                error.to_string(),
            ));
        }
        batch_len += 1;

        if batch_len == batch_capacity {
            let result = process_decode_batch(&mut pool, batch_len, output, options, summary);
            batch_len = 0;
            if let Err(error) = result {
                let _ = pool.shutdown();
                return Err(error);
            }
        }
    }

    if batch_len > 0 {
        let result = process_decode_batch(&mut pool, batch_len, output, options, summary);
        if let Err(error) = result {
            let _ = pool.shutdown();
            return Err(error);
        }
    }

    pool.shutdown().map_err(|error| {
        Error::new(
            ErrorCode::CBKI001_INVALID_STATE,
            format!("decode worker pool shutdown failed: {error}"),
        )
    })
}

#[inline]
fn write_json_record<W: Write>(output: &mut W, value: &Value) -> Result<()> {
    if let Err(e) = serde_json::to_writer(&mut *output, value) {
        let error = Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string());
        telemetry::record_error(error.family_prefix());
        return Err(error);
    }

    if let Err(e) = writeln!(output) {
        let error = Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string());
        telemetry::record_error(error.family_prefix());
        return Err(error);
    }

    Ok(())
}

fn encode_worker_pool(
    schema: &Schema,
    options: &EncodeOptions,
) -> crate::memory::WorkerPool<Value, Result<Vec<u8>>> {
    let workers = effective_worker_count(options.threads);
    let channel_capacity = workers.saturating_mul(4).max(1);
    let max_window_size = workers.saturating_mul(2).max(1);
    let schema = Arc::new(schema.clone());
    let options = Arc::new(options.clone());

    crate::memory::WorkerPool::new(
        workers,
        channel_capacity,
        max_window_size,
        move |json_value: Value, _scratch: &mut crate::memory::ScratchBuffers| {
            encode_record(&schema, &json_value, &options)
        },
    )
}

fn process_encode_batch<W: Write>(
    pool: &mut crate::memory::WorkerPool<Value, Result<Vec<u8>>>,
    batch_len: usize,
    records_before_batch: u64,
    output: &mut W,
    options: &EncodeOptions,
    summary: &mut RunSummary,
) -> Result<bool> {
    let mut stop_after_error = false;

    for position in 0..batch_len {
        let result = pool
            .recv_ordered()
            .map_err(|error| Error::new(ErrorCode::CBKI001_INVALID_STATE, error.to_string()))?
            .ok_or_else(|| {
                Error::new(
                    ErrorCode::CBKI001_INVALID_STATE,
                    "encode worker pool ended before the submitted batch completed",
                )
            })?;

        if stop_after_error {
            continue;
        }

        match result {
            Ok(binary_data) => {
                output.write_all(&binary_data).map_err(|error| {
                    Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, error.to_string())
                })?;
                summary.bytes_processed += binary_data.len() as u64;
                summary.records_processed += 1;
            }
            Err(error) => {
                // `position` is batch-relative; report the record's absolute index.
                summary.note_failure(records_before_batch + position as u64 + 1, &error);
                telemetry::record_error(error.family_prefix());
                if options.strict_mode {
                    stop_after_error = true;
                }
            }
        }
    }

    Ok(stop_after_error)
}

fn shutdown_encode_pool(pool: crate::memory::WorkerPool<Value, Result<Vec<u8>>>) -> Result<()> {
    pool.shutdown().map_err(|error| {
        Error::new(
            ErrorCode::CBKI001_INVALID_STATE,
            format!("encode worker pool shutdown failed: {error}"),
        )
    })
}

fn finish_encode_input_error<W: Write>(
    pool: crate::memory::WorkerPool<Value, Result<Vec<u8>>>,
    batch_len: usize,
    records_before_batch: u64,
    output: &mut W,
    options: &EncodeOptions,
    summary: &mut RunSummary,
    error: Error,
) -> Result<u64> {
    let mut pool = pool;
    let pending_result = if batch_len > 0 {
        process_encode_batch(
            &mut pool,
            batch_len,
            records_before_batch,
            output,
            options,
            summary,
        )
    } else {
        Ok(false)
    };
    let shutdown_result = shutdown_encode_pool(pool);
    let pending_stop = pending_result?;
    shutdown_result?;
    if pending_stop {
        Ok(summary.records_processed)
    } else {
        Err(error)
    }
}

fn process_encode_jsonl_parallel<R: BufRead, W: Write>(
    schema: &Schema,
    reader: R,
    output: &mut W,
    options: &EncodeOptions,
    summary: &mut RunSummary,
) -> Result<u64> {
    let workers = effective_worker_count(options.threads);
    let batch_capacity = workers.saturating_mul(4).max(1);
    let mut pool = encode_worker_pool(schema, options);
    let mut records_seen = 0_u64;
    let mut records_before_batch = 0_u64;
    let mut batch_len = 0_usize;

    for line in reader.lines() {
        let line = match line {
            Ok(line) => line,
            Err(error) => {
                return finish_encode_input_error(
                    pool,
                    batch_len,
                    records_before_batch,
                    output,
                    options,
                    summary,
                    Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, error.to_string()),
                );
            }
        };

        if line.trim().is_empty() {
            continue;
        }

        let json_value: Value = match serde_json::from_str(&line) {
            Ok(json_value) => json_value,
            Err(error) => {
                return finish_encode_input_error(
                    pool,
                    batch_len,
                    records_before_batch,
                    output,
                    options,
                    summary,
                    Error::new(ErrorCode::CBKE501_JSON_TYPE_MISMATCH, error.to_string()),
                );
            }
        };

        records_seen += 1;
        if let Err(error) = pool.submit(json_value) {
            return finish_encode_input_error(
                pool,
                batch_len,
                records_before_batch,
                output,
                options,
                summary,
                Error::new(ErrorCode::CBKI001_INVALID_STATE, error.to_string()),
            );
        }
        batch_len += 1;

        if batch_len == batch_capacity {
            let batch_result = process_encode_batch(
                &mut pool,
                batch_len,
                records_before_batch,
                output,
                options,
                summary,
            );
            let stop = match batch_result {
                Ok(stop) => stop,
                Err(error) => {
                    let _ = shutdown_encode_pool(pool);
                    return Err(error);
                }
            };
            batch_len = 0;
            records_before_batch = records_seen;
            if stop {
                shutdown_encode_pool(pool)?;
                return Ok(summary.records_processed);
            }
        }
    }

    if batch_len > 0 {
        let batch_result = process_encode_batch(
            &mut pool,
            batch_len,
            records_before_batch,
            output,
            options,
            summary,
        );
        let stop = match batch_result {
            Ok(stop) => stop,
            Err(error) => {
                let _ = shutdown_encode_pool(pool);
                return Err(error);
            }
        };
        if stop {
            shutdown_encode_pool(pool)?;
            return Ok(summary.records_processed);
        }
    }

    shutdown_encode_pool(pool)?;
    Ok(summary.records_processed)
}

/// Encode JSONL to binary file
///
/// # Arguments
///
/// * `schema` - The parsed copybook schema
/// * `input` - Input stream to read JSONL from
/// * `output` - Output stream to write binary to
/// * `options` - Encoding options
///
/// When `options.threads` is greater than one, records are encoded through a
/// bounded worker pool and written in input order. Requested worker counts are
/// capped at the repository's safe limit.
///
/// # Examples
///
/// ```
/// use copybook_core::parse_copybook;
/// use copybook_codec::{encode_jsonl_to_file, EncodeOptions};
/// use copybook_codec::options::{Codepage, RecordFormat};
///
/// let schema = parse_copybook("01 FLD PIC X(5).").unwrap();
/// let jsonl = br#"{"fields":{"FLD":"HELLO"}}
/// {"fields":{"FLD":"WORLD"}}
/// "#;
/// let mut output = Vec::new();
/// let options = EncodeOptions::new()
///     .with_codepage(Codepage::ASCII)
///     .with_format(RecordFormat::Fixed);
/// let summary = encode_jsonl_to_file(&schema, &jsonl[..], &mut output, &options).unwrap();
/// assert_eq!(summary.records_processed, 2);
/// assert_eq!(&output[..5], b"HELLO");
/// assert_eq!(&output[5..10], b"WORLD");
/// ```
///
/// # Errors
/// Returns an error if the JSONL cannot be encoded or written.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn encode_jsonl_to_file(
    schema: &Schema,
    input: impl Read,
    mut output: impl Write,
    options: &EncodeOptions,
) -> Result<RunSummary> {
    let start_time = std::time::Instant::now();
    let mut summary = RunSummary::with_threads(effective_worker_count(options.threads));
    summary.set_schema_fingerprint(schema.fingerprint.clone());

    let reader = BufReader::new(input);
    let record_count = if options.threads > 1 {
        process_encode_jsonl_parallel(schema, reader, &mut output, options, &mut summary)?
    } else {
        let mut records_seen = 0u64;
        let mut records_processed = 0u64;

        for line in reader.lines() {
            let line =
                line.map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string()))?;

            if line.trim().is_empty() {
                continue;
            }

            records_seen += 1;

            // Parse JSON
            let json_value: Value = serde_json::from_str(&line)
                .map_err(|e| Error::new(ErrorCode::CBKE501_JSON_TYPE_MISMATCH, e.to_string()))?;

            // Encode to binary
            match encode_record(schema, &json_value, options) {
                Ok(binary_data) => {
                    output.write_all(&binary_data).map_err(|e| {
                        Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string())
                    })?;
                    summary.bytes_processed += binary_data.len() as u64;
                    records_processed += 1;
                }
                Err(error) => {
                    summary.note_failure(records_seen, &error);
                    telemetry::record_error(error.family_prefix());
                    if options.strict_mode {
                        break;
                    }
                }
            }
        }

        records_processed
    };

    summary.records_processed = record_count;
    let elapsed_ms = start_time.elapsed().as_millis();
    summary.processing_time_ms = u64::try_from(elapsed_ms).unwrap_or(u64::MAX);
    summary.calculate_throughput();

    Ok(summary)
}

/// Helper function to format zoned decimal with proper digit padding
fn format_zoned_decimal_with_digits(
    decimal: &crate::numeric::SmallDecimal,
    digits: u16,
    blank_when_zero: bool,
) -> String {
    use std::fmt::Write;

    // For blank-when-zero fields, use natural formatting (no leading zeros)
    if blank_when_zero {
        return decimal.to_string();
    }

    // For any zero values in signed fields or when to_string() gives normalized result,
    // prefer the normalized "0" over padded format
    if decimal.value == 0 {
        let natural_format = decimal.to_string();
        if natural_format == "0" {
            return "0".to_string();
        }
    }

    // For regular fields, use padding to maintain field width consistency
    let mut result = String::new();
    let value = decimal.value;
    let negative = decimal.negative && value != 0;

    if negative {
        result.push('-');
    }

    // For integer scale, pad with leading zeros to maintain field width
    if decimal.scale <= 0 {
        let scaled_value = if decimal.scale < 0 {
            let exponent = u32::from(decimal.scale.unsigned_abs());
            value * 10_i64.pow(exponent)
        } else {
            value
        };
        if write!(result, "{:0width$}", scaled_value, width = digits as usize).is_err() {
            // Writing to a String should not fail
            result.push('0');
        }
    } else {
        // This shouldn't happen for integer zoned decimals, but handle it
        result.push_str(&decimal.to_string());
    }

    result
}

#[inline]
fn small_decimal_to_string(decimal: &crate::numeric::SmallDecimal) -> String {
    decimal.to_string()
}

fn zoned_decimal_to_json_value(
    decimal: &crate::numeric::SmallDecimal,
    digits: u16,
    scale: i16,
    blank_when_zero: bool,
    options: &DecodeOptions,
) -> Value {
    let formatted = if scale == 0 {
        format_zoned_decimal_with_digits(decimal, digits, blank_when_zero)
    } else {
        small_decimal_to_string(decimal)
    };
    numeric_string_to_value(formatted, options)
}

#[inline]
fn decimal_counter_to_u32(
    decimal: &crate::numeric::SmallDecimal,
    counter_path: &str,
) -> Result<u32> {
    let text = small_decimal_to_string(decimal);
    text.parse::<u32>().map_err(|_| {
        Error::new(
            ErrorCode::CBKS121_COUNTER_NOT_FOUND,
            format!("ODO counter '{counter_path}' has invalid value: {text}"),
        )
    })
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use crate::Codepage;
    use crate::iterator::RecordIterator;
    use copybook_core::{Error, ErrorCode, Result, parse_copybook};
    use std::io::Cursor;

    #[test]
    fn test_decode_record() -> Result<()> {
        let copybook_text = r"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        ";

        let schema = parse_copybook(copybook_text)?;
        let options = DecodeOptions {
            codepage: Codepage::ASCII, // Fix: Use ASCII for ASCII test data
            ..DecodeOptions::default()
        };
        let data = b"001ALICE";

        let result = decode_record(&schema, data, &options)?;
        assert!(result.is_object());
        let object = result.as_object().ok_or_else(|| {
            Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "decoded record should be an object".to_string(),
            )
        })?;
        assert!(object.len() > 1);
        Ok(())
    }

    #[test]
    fn test_encode_record() -> Result<()> {
        let copybook_text = r"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        ";

        let schema = parse_copybook(copybook_text)?;
        let options = EncodeOptions::default();

        let mut json_obj = serde_json::Map::new();
        json_obj.insert("ID".into(), Value::String("123".into()));
        json_obj.insert("NAME".into(), Value::String("HELLO".into()));
        let json = Value::Object(json_obj);

        let result = encode_record(&schema, &json, &options)?;
        assert!(!result.is_empty());
        // The result should be a properly encoded binary record
        // For this basic test, just verify it's the expected length
        assert_eq!(result.len(), 8); // 3 digits for ID + 5 chars for NAME
        Ok(())
    }

    #[test]
    fn rdw_odo_length_preserves_storage_after_nested_group() -> Result<()> {
        let copybook_text = r"
            01 RECORD.
               05 HEADER PIC X(1).
               05 WRAP.
                  10 CNT PIC 9(3).
                  10 ITEMS OCCURS 1 TO 5 DEPENDING ON CNT PIC X(4).
               05 TRAILER PIC X(2).
        ";
        let schema = parse_copybook(copybook_text)?;
        let json = serde_json::json!({
            "HEADER": "H",
            "WRAP": {"CNT": "002", "ITEMS": ["ABCD", "WXYZ"]},
            "TRAILER": "TT"
        });

        assert_eq!(rdw_record_length_for_json(&schema, &json), Some(14));
        Ok(())
    }

    #[test]
    fn test_record_iterator() -> Result<()> {
        let copybook_text = r"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        ";

        let schema = parse_copybook(copybook_text)?;
        let options = DecodeOptions::default();

        // Create test data
        let test_data = vec![0u8; 16]; // Two 8-byte records
        let cursor = Cursor::new(test_data);

        let iterator = RecordIterator::new(cursor, &schema, &options)?;
        assert_eq!(iterator.current_record_index(), 0);
        assert!(!iterator.is_eof());
        Ok(())
    }

    #[test]
    fn test_decode_file_to_jsonl() -> Result<()> {
        let copybook_text = r"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        ";

        let schema = parse_copybook(copybook_text)?;
        let options = DecodeOptions {
            codepage: Codepage::ASCII, // Fix: Use ASCII for ASCII test data
            ..DecodeOptions::default()
        };

        // Create test input with valid ASCII digits and characters
        let input_data = b"001ALICE002BOBBY".to_vec(); // Two 8-byte records with valid data
        let input = Cursor::new(input_data);

        // Create output buffer
        let mut output = Vec::new();

        let summary = decode_file_to_jsonl(&schema, input, &mut output, &options)?;
        assert!(summary.records_processed > 0);
        assert!(!output.is_empty());
        Ok(())
    }

    #[test]
    fn test_encode_jsonl_to_file() -> Result<()> {
        let copybook_text = r"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        ";

        let schema = parse_copybook(copybook_text)?;
        let options = EncodeOptions::default();

        // Create test JSONL input
        let jsonl_data = "{\"__status\":\"test\"}\n{\"__status\":\"test2\"}";
        let input = Cursor::new(jsonl_data.as_bytes());

        // Create output buffer
        let mut output = Vec::new();

        let summary = encode_jsonl_to_file(&schema, input, &mut output, &options)?;
        assert_eq!(summary.records_processed, 2);
        assert!(!output.is_empty());
        Ok(())
    }
}
