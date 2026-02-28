// SPDX-License-Identifier: AGPL-3.0-or-later
//! Core library API implementation for task 9.1
#![allow(clippy::missing_inline_in_public_items)]
//!
//! This module provides the main library functions required by R11:
//! - `parse_copybook` (already exists in copybook-core)
//! - `decode_record`
//! - `encode_record`
//! - `decode_file_to_jsonl`
//! - `encode_jsonl_to_file`
//! - `RecordIterator` (for programmatic access)

use crate::JSON_SCHEMA_VERSION;
use crate::options::{DecodeOptions, EncodeOptions, RawMode, RecordFormat, ZonedEncodingFormat};
use crate::zoned_overpunch::ZeroSignPolicy;
use base64::Engine;
use copybook_core::{Error, ErrorCode, Result, Schema};
use serde_json::Value;
use std::cell::RefCell;
use std::convert::TryFrom;
use std::fmt;
use std::io::{BufRead, BufReader, Read, Write};
use tracing::info;

/// Build a standard JSON envelope for a decoded COBOL record.
///
/// Wraps the decoded fields with metadata like schema version, record index,
/// and codepage. Optionally includes extended metadata if `options.emit_meta` is true.
fn build_json_envelope(
    fields: serde_json::Map<String, Value>,
    schema: &Schema,
    options: &DecodeOptions,
    record_index: u64,
    record_length: usize,
    raw_b64: Option<String>,
    encoding_metadata: Vec<(String, ZonedEncodingFormat)>,
) -> Value {
    let mut root = serde_json::Map::with_capacity(
        fields.len()
            + 8
            + if raw_b64.is_some() { 1 } else { 0 }
            + if options.preserve_zoned_encoding {
                1
            } else {
                0
            },
    );
    root.insert(
        "schema".to_string(),
        Value::String(JSON_SCHEMA_VERSION.to_string()),
    );
    root.insert(
        "record_index".to_string(),
        Value::Number(serde_json::Number::from(record_index)),
    );
    root.insert(
        "codepage".to_string(),
        Value::String(options.codepage.to_string()),
    );

    let flat_fields = fields.clone();
    root.insert("fields".to_string(), Value::Object(fields));
    for (key, value) in flat_fields {
        root.insert(key, value);
    }

    if options.emit_meta {
        if !schema.fingerprint.is_empty() {
            root.insert(
                "schema_fingerprint".to_string(),
                Value::String(schema.fingerprint.clone()),
            );
            root.insert(
                "__schema_id".to_string(),
                Value::String(schema.fingerprint.clone()),
            );
        }
        root.insert(
            "length".to_string(),
            Value::Number(serde_json::Number::from(record_length)),
        );
        root.insert(
            "__record_index".to_string(),
            Value::Number(serde_json::Number::from(record_index)),
        );
        root.insert(
            "__length".to_string(),
            Value::Number(serde_json::Number::from(record_length)),
        );
    }

    if let Some(raw) = raw_b64 {
        root.insert("raw_b64".to_string(), Value::String(raw.clone()));
        root.insert("__raw_b64".to_string(), Value::String(raw));
    }

    // Emit zoned encoding metadata when preserve_zoned_encoding is enabled
    if options.preserve_zoned_encoding && !encoding_metadata.is_empty() {
        let mut meta_map = serde_json::Map::new();
        for (field_name, format) in encoding_metadata {
            meta_map.insert(field_name, Value::String(format.to_string()));
        }
        root.insert("_encoding_metadata".to_string(), Value::Object(meta_map));
    }

    Value::Object(root)
}

thread_local! {
    static WARNING_COUNTER: RefCell<u64> = const { RefCell::new(0) };
}

#[cfg(feature = "metrics")]
mod telemetry {
    use crate::options::{Codepage, DecodeOptions, RecordFormat, ZonedEncodingFormat};
    use metrics::{counter, gauge, histogram};

    #[inline]
    pub fn record_read(bytes: usize, options: &DecodeOptions) {
        let format_label = format_label(options.format);
        let codepage_label = codepage_label(options.codepage);
        let zero_policy_label = zero_policy_label(options);

        counter!(
            "copybook_records_total",
            "format" => format_label,
            "codepage" => codepage_label,
            "zero_policy" => zero_policy_label
        )
        .increment(1);
        counter!(
            "copybook_bytes_total",
            "format" => format_label,
            "codepage" => codepage_label,
            "zero_policy" => zero_policy_label
        )
        .increment(bytes as u64);
    }

    #[inline]
    pub fn record_error(family: &'static str) {
        counter!("copybook_decode_errors_total", "family" => family).increment(1);
    }

    #[inline]
    pub fn record_completion(
        duration_seconds: f64,
        throughput_mibps: f64,
        options: &DecodeOptions,
    ) {
        let format_label = format_label(options.format);
        let codepage_label = codepage_label(options.codepage);

        if duration_seconds.is_finite() && duration_seconds >= 0.0 {
            histogram!(
                "copybook_decode_seconds",
                "format" => format_label,
                "codepage" => codepage_label
            )
            .record(duration_seconds);
        }

        if throughput_mibps.is_finite() {
            gauge!(
                "copybook_throughput_mibps",
                "format" => format_label,
                "codepage" => codepage_label
            )
            .set(throughput_mibps);
        }
    }

    #[inline]
    fn zero_policy_label(options: &DecodeOptions) -> &'static str {
        if options.preserve_zoned_encoding {
            "preserved"
        } else if options.preferred_zoned_encoding != ZonedEncodingFormat::Auto {
            "override"
        } else {
            "preferred"
        }
    }

    #[inline]
    fn format_label(format: RecordFormat) -> &'static str {
        match format {
            RecordFormat::Fixed => "fixed",
            RecordFormat::RDW => "rdw",
        }
    }

    #[inline]
    fn codepage_label(codepage: Codepage) -> &'static str {
        match codepage {
            Codepage::ASCII => "ascii",
            Codepage::CP037 => "cp037",
            Codepage::CP273 => "cp273",
            Codepage::CP500 => "cp500",
            Codepage::CP1047 => "cp1047",
            Codepage::CP1140 => "cp1140",
        }
    }
}

#[cfg(not(feature = "metrics"))]
mod telemetry {
    use crate::options::DecodeOptions;

    #[inline]
    pub fn record_read(_bytes: usize, _options: &DecodeOptions) {}

    #[inline]
    pub fn record_error(_family: &'static str) {}

    #[inline]
    pub fn record_completion(
        _duration_seconds: f64,
        _throughput_mibps: f64,
        _options: &DecodeOptions,
    ) {
    }
}

/// Summary of processing run with comprehensive statistics
#[derive(Debug, Default, Clone, PartialEq)]
pub struct RunSummary {
    /// Total records processed successfully
    pub records_processed: u64,
    /// Number of records with errors
    pub records_with_errors: u64,
    /// Number of warnings generated
    pub warnings: u64,
    /// Processing time in milliseconds
    pub processing_time_ms: u64,
    /// Total bytes processed
    pub bytes_processed: u64,
    /// Schema fingerprint used for processing
    pub schema_fingerprint: String,
    /// Lineage identifier derived from schema fingerprint
    pub lineage_id: String,
    /// Processing throughput in MB/s
    pub throughput_mbps: f64,
    /// Peak memory usage in bytes (if available)
    pub peak_memory_bytes: Option<u64>,
    /// Number of threads used for processing
    pub threads_used: usize,
}

impl RunSummary {
    /// Create a new run summary with default values
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new run summary with specified thread count
    #[must_use]
    pub fn with_threads(threads: usize) -> Self {
        Self {
            threads_used: threads,
            ..Self::default()
        }
    }

    /// Calculate throughput based on bytes and time
    #[allow(clippy::cast_precision_loss)]
    pub fn calculate_throughput(&mut self) {
        if self.processing_time_ms > 0 {
            let seconds = self.processing_time_ms as f64 / 1000.0;
            let megabytes = self.bytes_processed as f64 / (1024.0 * 1024.0);
            self.throughput_mbps = megabytes / seconds;
        }
    }

    /// Check if processing had any errors
    #[must_use]
    pub const fn has_errors(&self) -> bool {
        self.records_with_errors > 0
    }

    /// Check if processing had any warnings
    #[must_use]
    pub const fn has_warnings(&self) -> bool {
        self.warnings > 0
    }

    /// Check if processing was successful (no errors)
    #[must_use]
    pub const fn is_successful(&self) -> bool {
        !self.has_errors()
    }

    /// Get the total number of records attempted (processed + errors)
    #[must_use]
    pub const fn total_records(&self) -> u64 {
        self.records_processed + self.records_with_errors
    }

    /// Get the success rate as a percentage (0.0 to 100.0)
    #[must_use]
    #[allow(clippy::cast_precision_loss)]
    pub fn success_rate(&self) -> f64 {
        let total = self.total_records();
        if total == 0 {
            100.0
        } else {
            (self.records_processed as f64 / total as f64) * 100.0
        }
    }

    /// Get the error rate as a percentage (0.0 to 100.0)
    #[must_use]
    pub fn error_rate(&self) -> f64 {
        100.0 - self.success_rate()
    }

    /// Get processing time in seconds
    #[must_use]
    #[allow(clippy::cast_precision_loss)]
    pub fn processing_time_seconds(&self) -> f64 {
        self.processing_time_ms as f64 / 1000.0
    }

    /// Get bytes processed in megabytes
    #[must_use]
    #[allow(clippy::cast_precision_loss)]
    pub fn bytes_processed_mb(&self) -> f64 {
        self.bytes_processed as f64 / (1024.0 * 1024.0)
    }

    /// Set the schema fingerprint
    pub fn set_schema_fingerprint(&mut self, fingerprint: String) {
        self.schema_fingerprint = fingerprint;
    }

    /// Set the lineage identifier
    pub fn set_lineage_id(&mut self, lineage_id: String) {
        self.lineage_id = lineage_id;
    }

    /// Set the peak memory usage
    pub fn set_peak_memory_bytes(&mut self, bytes: u64) {
        self.peak_memory_bytes = Some(bytes);
    }

    /// Get the lineage identifier
    #[must_use]
    pub const fn lineage_id(&self) -> &str {
        &self.lineage_id
    }
}

impl fmt::Display for RunSummary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Processing Summary:")?;
        writeln!(f, "  Records processed: {}", self.records_processed)?;
        writeln!(f, "  Records with errors: {}", self.records_with_errors)?;
        writeln!(f, "  Warnings: {}", self.warnings)?;
        writeln!(f, "  Success rate: {:.1}%", self.success_rate())?;
        writeln!(
            f,
            "  Processing time: {:.2}s",
            self.processing_time_seconds()
        )?;
        writeln!(f, "  Bytes processed: {:.2} MB", self.bytes_processed_mb())?;
        writeln!(f, "  Throughput: {:.2} MB/s", self.throughput_mbps)?;
        writeln!(f, "  Threads used: {}", self.threads_used)?;
        if let Some(peak_memory) = self.peak_memory_bytes {
            #[allow(clippy::cast_precision_loss)]
            let peak_mb = peak_memory as f64 / (1024.0 * 1024.0);
            writeln!(f, "  Peak memory: {peak_mb:.2} MB")?;
        }
        if !self.schema_fingerprint.is_empty() {
            writeln!(f, "  Schema fingerprint: {}", self.schema_fingerprint)?;
        }
        if !self.lineage_id.is_empty() {
            writeln!(f, "  Lineage ID: {}", self.lineage_id)?;
        }
        Ok(())
    }
}

#[cfg(feature = "audit")]
#[inline]
fn derive_lineage_id(fingerprint: &str) -> String {
    copybook_core::audit::derive_lineage_id_from_fingerprint(fingerprint)
}

#[cfg(not(feature = "audit"))]
#[inline]
fn derive_lineage_id(fingerprint: &str) -> String {
    if fingerprint.is_empty() {
        String::new()
    } else {
        format!("lineage-{fingerprint}")
    }
}

/// Decode a single record from binary data to JSON
///
/// # Arguments
///
/// * `schema` - The parsed copybook schema
/// * `data` - The binary record data
/// * `options` - Decoding options
///
/// # Errors
/// Returns an error if the data cannot be decoded according to the schema.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_record(schema: &Schema, data: &[u8], options: &DecodeOptions) -> Result<Value> {
    let mut scratch = crate::memory::ScratchBuffers::new();
    decode_record_with_scratch_and_raw(schema, data, options, None, 0, &mut scratch)
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
    decode_record_with_scratch_and_raw(schema, data, options, None, 0, scratch)
}

/// Decode a record with optional raw data and scratch buffers for maximum performance
fn decode_record_with_scratch_and_raw(
    schema: &Schema,
    data: &[u8],
    options: &DecodeOptions,
    raw_data: Option<Vec<u8>>,
    record_index: u64,
    scratch: &mut crate::memory::ScratchBuffers,
) -> Result<Value> {
    use serde_json::Map;

    let mut fields_map = Map::with_capacity(schema.fields.len());
    let mut record_raw = None;
    let mut encoding_acc = if options.preserve_zoned_encoding {
        Vec::with_capacity(schema.fields.len())
    } else {
        Vec::new()
    };

    if let Some(raw_bytes) = raw_data.filter(|_| {
        matches!(
            options.emit_raw,
            crate::options::RawMode::Record | crate::options::RawMode::Field | crate::options::RawMode::RecordRDW
        )
    }) {
        record_raw = Some(base64::engine::general_purpose::STANDARD.encode(raw_bytes));
    }

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
        data.len(),
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
    use crate::memory::ScratchBuffers;
    let mut scratch = ScratchBuffers::new();

    let raw_data = match options.emit_raw {
        RawMode::Off => None,
        RawMode::Record | RawMode::Field => Some(data.to_vec()),
        RawMode::RecordRDW => Some(raw_data_with_header.unwrap_or(data).to_vec()),
    };

    decode_record_with_scratch_and_raw(
        schema,
        data,
        options,
        raw_data,
        record_index,
        &mut scratch,
    )
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

    for (field_index, field) in fields.iter().enumerate() {
        if is_filler_field(field) && !options.emit_filler {
            continue;
        }

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
                    encoding_acc,
                )?;
            }
        }
    }

    Ok(())
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
                    encoding_acc,
                )?;
            }
        }
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
        json_obj.insert(field.name.clone(), Value::String(text));
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
    let value = decode_scalar_field_value_standard(field, field_data, options, scratch_buffers)?;

    // Collect zoned encoding metadata when preservation is enabled
    if options.preserve_zoned_encoding {
        collect_zoned_encoding_info(field, field_data, options, encoding_acc);
    }

    json_obj.insert(field.name.clone(), value);
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
        json_obj.insert(field.name.clone(), Value::String(text));
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
    let value = decode_scalar_field_value_with_scratch(field, field_data, options, scratch)?;

    // Collect zoned encoding metadata when preservation is enabled
    if options.preserve_zoned_encoding {
        collect_zoned_encoding_info(field, field_data, options, encoding_acc);
    }

    json_obj.insert(field.name.clone(), value);
    Ok(())
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
            let counter_value =
                find_and_read_counter_field(counter_path, all_fields, data, options)?;

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
                let adjusted_children = adjust_field_offsets(&field.children, element_base_offset);
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
                )?;
                if options.preserve_zoned_encoding {
                    collect_zoned_encoding_info(field, element_data, options, encoding_acc);
                }
                val
            }
        };

        array_values.push(element_value);
    }

    json_obj.insert(field.name.clone(), Value::Array(array_values));
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
            let counter_value =
                find_and_read_counter_field(counter_path, all_fields, data, options)?;

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

                // Create a temporary field for processing group element
                let element_offset_u32 = u32::try_from(element_offset).map_err(|_| {
                    Error::new(
                        ErrorCode::CBKD301_RECORD_TOO_SHORT,
                        format!("Array element offset {element_offset} exceeds supported range"),
                    )
                })?;

                let mut element_field = field.clone();
                element_field.offset = element_offset_u32;
                element_field.occurs = None; // Remove OCCURS for individual element

                process_fields_recursive_with_scratch(
                    &element_field.children,
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
                    decode_scalar_field_value_with_scratch(field, element_data, options, scratch)?;
                if options.preserve_zoned_encoding {
                    collect_zoned_encoding_info(field, element_data, options, encoding_acc);
                }
                val
            }
        };

        array_values.push(element_value);
    }

    json_obj.insert(field.name.clone(), Value::Array(array_values));
    Ok(())
}

/// Find and read the value of a counter field for ODO arrays
fn find_and_read_counter_field(
    counter_path: &str,
    all_fields: &[copybook_core::Field],
    data: &[u8],
    options: &DecodeOptions,
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
            let mut scratch = crate::memory::ScratchBuffers::new();
            let decimal_str = if let Some(sign_sep) = sign_separate {
                // Use SIGN SEPARATE decoding
                crate::numeric::decode_zoned_decimal_sign_separate(
                    field_data,
                    *digits,
                    *scale,
                    sign_sep,
                    options.codepage,
                )?
                .to_string()
            } else {
                // Use standard zoned decimal decoding
                crate::numeric::decode_zoned_decimal_to_string_with_scratch(
                    field_data,
                    *digits,
                    *scale,
                    *signed,
                    options.codepage,
                    counter_field.blank_when_zero,
                    &mut scratch,
                )?
            };

            let count = decimal_str.parse::<u32>().map_err(|_| {
                Error::new(
                    ErrorCode::CBKS121_COUNTER_NOT_FOUND,
                    format!("ODO counter '{counter_path}' has invalid value: {decimal_str}"),
                )
            })?;

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
            // OPTIMIZATION: Use scratch buffer for ODO counter decoding
            let mut scratch = crate::memory::ScratchBuffers::new();
            let decimal_str = crate::numeric::decode_packed_decimal_to_string_with_scratch(
                field_data,
                *digits,
                *scale,
                *signed,
                &mut scratch,
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
) -> Vec<copybook_core::Field> {
    fields
        .iter()
        .map(|field| {
            let mut adjusted_field = field.clone();
            adjusted_field.offset = base_offset;
            if !adjusted_field.children.is_empty() {
                adjusted_field.children =
                    adjust_field_offsets(&adjusted_field.children, base_offset);
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
        encoding_acc.push((field.name.clone(), info.detected_format));
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
                let formatted = if *scale == 0 {
                    format_zoned_decimal_with_digits(&decimal, *digits, field.blank_when_zero)
                } else {
                    decimal.to_string()
                };
                Ok(Value::String(formatted))
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

                let formatted = if *scale == 0 {
                    format_zoned_decimal_with_digits(&decimal, *digits, field.blank_when_zero)
                } else {
                    decimal.to_string()
                };

                // Encoding info is collected by collect_zoned_encoding_info() at the caller level
                // and emitted as _encoding_metadata in the JSON envelope
                Ok(Value::String(formatted))
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
                let formatted = if *scale == 0 {
                    format_zoned_decimal_with_digits(&decimal, *digits, field.blank_when_zero)
                } else {
                    decimal.to_string()
                };
                Ok(Value::String(formatted))
            }
        }
        FieldKind::BinaryInt { bits, signed } => {
            let int_value = crate::numeric::decode_binary_int(field_data, *bits, *signed)?;
            // tripwire: hot-path allocation guard should catch this
            let _bad = Value::String(int_value.to_string());
            let scratch = scratch_buffers.get_or_insert_with(crate::memory::ScratchBuffers::new);
            let formatted =
                crate::numeric::format_binary_int_to_string_with_scratch(int_value, scratch);
            Ok(Value::String(formatted))
        }
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => {
            // Wave-3 regression posture: keep PACKED on the scratch-backed decoder path.
            let scratch = scratch_buffers
                .get_or_insert_with(crate::memory::ScratchBuffers::new);
            let decimal_str = crate::numeric::decode_packed_decimal_to_string_with_scratch(
                field_data,
                *digits,
                *scale,
                *signed,
                scratch,
            )?;
            Ok(Value::String(decimal_str))
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

            // Return as string (consistent with other numeric types)
            Ok(Value::String(numeric_value.to_decimal_string()))
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
                Ok(Value::String(decimal.to_string()))
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
                Ok(Value::String(decimal_str))
            }
        }
        FieldKind::BinaryInt { bits, signed } => {
            let int_value = crate::numeric::decode_binary_int(field_data, *bits, *signed)?;
            let formatted =
                crate::numeric::format_binary_int_to_string_with_scratch(int_value, scratch);
            Ok(Value::String(formatted))
        }
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => {
            let decimal_str = crate::numeric::decode_packed_decimal_to_string_with_scratch(
                field_data, *digits, *scale, *signed, scratch,
            )?;
            Ok(Value::String(decimal_str))
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

            // Return as string (consistent with other numeric types)
            Ok(Value::String(numeric_value.to_decimal_string()))
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
/// # Errors
/// Returns an error if the JSON data cannot be encoded according to the schema.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn encode_record(schema: &Schema, json: &Value, options: &EncodeOptions) -> Result<Vec<u8>> {
    let root_obj = json.as_object().ok_or_else(|| {
        Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            "Expected JSON object for record envelope",
        )
    })?;
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

    // Check if we should use raw data
    if options.use_raw
        && let Some(raw_b64_value) = root_obj
            .get("raw_b64")
            .or_else(|| root_obj.get("__raw_b64"))
        && let Some(raw_str) = raw_b64_value.as_str()
    {
        // Decode base64 raw data
        let raw_data = base64::engine::general_purpose::STANDARD
            .decode(raw_str)
            .map_err(|e| {
                Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    format!("Invalid base64 in raw_b64: {e}"),
                )
            })?;

        match options.format {
            RecordFormat::RDW => {
                // For RDW, we need to validate/recompute length if payload changed
                if raw_data.len() >= 4 {
                    let mut rdw_record = raw_data.clone();

                    // Extract the payload portion (everything after 4-byte header)
                    let payload = &rdw_record[4..];

                    // Check if we need to recompute length based on field changes
                    let mut should_recompute = false;

                    // Encode the fields to see if payload changed
                    let field_payload = encode_fields_to_bytes(schema, fields_value, options)?;
                    if field_payload != payload {
                        should_recompute = true;
                    }

                    if should_recompute {
                        // Recompute length header
                        let capped_len = field_payload.len().min(u16::MAX as usize);
                        let new_length = u16::try_from(capped_len).unwrap_or(u16::MAX);
                        let length_bytes = new_length.to_be_bytes();
                        rdw_record[0] = length_bytes[0];
                        rdw_record[1] = length_bytes[1];
                        // Preserve reserved bytes [2] and [3]

                        // Replace payload
                        rdw_record.splice(4.., field_payload);
                    }

                    return Ok(rdw_record);
                }
            }
            RecordFormat::Fixed => {
                return Ok(raw_data);
            }
        }
    }

    // No raw data or not using raw - encode from fields
    validate_lib_api_redefines_encoding(schema, fields_value, options)?;
    validate_lib_api_odo_encoding(schema, fields_value, options)?;

    match options.format {
        RecordFormat::Fixed => {
            let payload = encode_fields_to_bytes(schema, fields_value, options)?;
            Ok(payload)
        }
        RecordFormat::RDW => {
            let payload = encode_fields_to_bytes(schema, fields_value, options)?;

            // Create RDW record
            let rdw_record = crate::record::RDWRecord::try_new(payload)?;
            let mut result = Vec::new();
            result.extend_from_slice(&rdw_record.header);
            result.extend_from_slice(&rdw_record.payload);
            Ok(result)
        }
    }
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

    if let Some(array) = json_lookup_array(fields_value, &tail_odo.array_path) {
        let array_field = schema.find_field(&tail_odo.array_path).ok_or_else(|| {
            Error::new(
                ErrorCode::CBKS121_COUNTER_NOT_FOUND,
                format!(
                    "ODO array field '{}' not found in schema",
                    tail_odo.array_path
                ),
            )
            .with_context(crate::odo_redefines::create_comprehensive_error_context(
                0,
                &tail_odo.array_path,
                0,
                None,
            ))
        })?;

        let counter_field = schema.find_field(&tail_odo.counter_path).ok_or_else(|| {
            crate::odo_redefines::handle_missing_counter_field(
                &tail_odo.counter_path,
                &tail_odo.array_path,
                schema,
                0,
                0,
            )
        })?;

        if json_lookup_value(fields_value, &tail_odo.counter_path).is_none() {
            return Err(crate::odo_redefines::handle_missing_counter_field(
                &tail_odo.counter_path,
                &tail_odo.array_path,
                schema,
                0,
                u64::from(counter_field.offset),
            ));
        }

        let context = crate::odo_redefines::OdoValidationContext {
            field_path: tail_odo.array_path.clone(),
            counter_path: tail_odo.counter_path.clone(),
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

fn json_lookup_value<'a>(value: &'a Value, field_path: &str) -> Option<&'a Value> {
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
    options: &EncodeOptions,
) -> Result<Vec<u8>> {
    let record_length = schema.lrecl_fixed.unwrap_or_else(|| {
        // For variable length, estimate based on schema
        schema.fields.iter().map(|f| f.len).sum::<u32>()
    }) as usize;

    let mut buffer = vec![0u8; record_length];

    if let Some(obj) = json.as_object() {
        let encoding_metadata = obj
            .get("_encoding_metadata")
            .and_then(|value| value.as_object());
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

    for field in fields {
        let field_path = if path_prefix.is_empty() {
            field.name.clone()
        } else {
            format!("{path_prefix}.{}", field.name)
        };

        current_offset = encode_single_field(
            field,
            &field_path,
            json_obj,
            encoding_metadata,
            buffer,
            current_offset,
            options,
        )?;
    }

    Ok(current_offset)
}

/// Encode a single field (scalar or group) into the output byte buffer.
///
/// Orchestrates the encoding of various COBOL data types by delegating to
/// specialized encoding functions.
#[inline]
#[allow(clippy::too_many_lines)]
fn encode_single_field(
    field: &copybook_core::Field,
    field_path: &str,
    json_obj: &serde_json::Map<String, Value>,
    encoding_metadata: Option<&serde_json::Map<String, Value>>,
    buffer: &mut [u8],
    current_offset: usize,
    options: &EncodeOptions,
) -> Result<usize> {
    use copybook_core::FieldKind;

    match &field.kind {
        FieldKind::Group => encode_group_field(
            field,
            field_path,
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
                if let Some(text) = json_obj.get(&field.name).and_then(|v| v.as_str()) {
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
                    field_path,
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
            field_path,
            json_obj,
            buffer,
            current_offset,
            DecimalSpec {
                digits: *digits,
                scale: *scale,
                signed: *signed,
            },
        ),
        FieldKind::BinaryInt { bits, signed } => encode_binary_int_field(
            field,
            field_path,
            json_obj,
            buffer,
            current_offset,
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
            if let Some(text) = json_obj.get(&field.name).and_then(|value| value.as_str()) {
                // Tokenize the PIC pattern
                let pattern = crate::edited_pic::tokenize_edited_pic(pic_string)?;

                // Encode the edited numeric value
                let encoded = crate::edited_pic::encode_edited_numeric(
                    text,
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

/// Recursively encode a group field and its children.
#[inline]
fn encode_group_field(
    field: &copybook_core::Field,
    field_path: &str,
    json_obj: &serde_json::Map<String, Value>,
    encoding_metadata: Option<&serde_json::Map<String, Value>>,
    buffer: &mut [u8],
    current_offset: usize,
    options: &EncodeOptions,
) -> Result<usize> {
    if let Some(sub_obj) = json_obj.get(&field.name).and_then(|v| v.as_object()) {
        encode_fields_recursive(
            &field.children,
            sub_obj,
            encoding_metadata,
            field_path,
            buffer,
            current_offset,
            options,
        )
    } else {
        encode_fields_recursive(
            &field.children,
            json_obj,
            encoding_metadata,
            field_path,
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
        // Validate string length doesn't exceed field capacity
        if text.len() > field_len {
            return Err(Error::new(
                ErrorCode::CBKE515_STRING_LENGTH_VIOLATION,
                format!(
                    "String length {} exceeds field capacity {} for alphanumeric field {}",
                    text.len(),
                    field_len,
                    field.path
                ),
            )
            .with_field(field.path.clone()));
        }

        let bytes = crate::charset::utf8_to_ebcdic(text, options.codepage)?;
        let copy_len = bytes.len().min(field_len);

        if current_offset + field_len <= buffer.len() {
            buffer[current_offset..current_offset + copy_len].copy_from_slice(&bytes[..copy_len]);
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

#[inline]
#[allow(clippy::too_many_arguments)]
fn encode_zoned_decimal_field(
    field: &copybook_core::Field,
    field_path: &str,
    json_obj: &serde_json::Map<String, Value>,
    encoding_metadata: Option<&serde_json::Map<String, Value>>,
    buffer: &mut [u8],
    current_offset: usize,
    options: &EncodeOptions,
    spec: DecimalSpec,
) -> Result<usize> {
    let field_len = field.len as usize;

    if let Some(text) = json_obj.get(&field.name).and_then(|value| value.as_str()) {
        let preserved_format = encoding_metadata
            .and_then(|meta| resolve_preserved_zoned_format(meta, field_path, &field.name));
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
            text,
            spec.digits,
            spec.scale,
            spec.signed,
            options.codepage,
            Some(effective_format),
            zero_policy,
        )?;

        debug_assert!(
            current_offset + field_len <= buffer.len(),
            "buffer too small for zoned-decimal field {}",
            field.name
        );
        debug_assert_eq!(
            encoded.len(),
            field_len,
            "encoded zoned-decimal size mismatch for field {}: expected {}, got {}",
            field.name,
            field_len,
            encoded.len()
        );
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
    spec: DecimalSpec,
) -> Result<usize> {
    let field_len = field.len as usize;

    if let Some(text) = json_obj.get(&field.name).and_then(|value| value.as_str()) {
        let encoded =
            crate::numeric::encode_packed_decimal(text, spec.digits, spec.scale, spec.signed)?;
        debug_assert!(
            current_offset + field_len <= buffer.len(),
            "buffer too small for packed-decimal field {}",
            field.name
        );
        debug_assert_eq!(
            encoded.len(),
            field_len,
            "encoded packed-decimal size mismatch for field {}: expected {}, got {}",
            field.name,
            field_len,
            encoded.len()
        );
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
    spec: BinarySpec,
) -> Result<usize> {
    let field_len = field.len as usize;

    if let Some(num) = json_obj
        .get(&field.name)
        .and_then(|value| value.as_str())
        .and_then(|text| text.parse::<i64>().ok())
    {
        let encoded = crate::numeric::encode_binary_int(num, spec.bits, spec.signed)?;
        debug_assert!(
            current_offset + field_len <= buffer.len(),
            "buffer too small for binary integer field {}",
            field.name
        );
        debug_assert_eq!(
            encoded.len(),
            field_len,
            "encoded binary integer size mismatch for field {}: expected {}, got {}",
            field.name,
            field_len,
            encoded.len()
        );
        if current_offset + field_len <= buffer.len() && encoded.len() == field_len {
            buffer[current_offset..current_offset + field_len].copy_from_slice(&encoded);
        }
    }

    Ok(current_offset + field_len)
}

/// Decode a file to JSONL format
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
    let mut summary = RunSummary::new();
    summary.set_schema_fingerprint(schema.fingerprint.clone());
    summary.set_lineage_id(derive_lineage_id(&schema.fingerprint));

    WARNING_COUNTER.with(|counter| {
        *counter.borrow_mut() = 0;
    });

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
    summary.warnings = WARNING_COUNTER.with(|counter| *counter.borrow());
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
    let mut reader = crate::record::FixedRecordReader::new(reader, schema.lrecl_fixed)?;
    let mut scratch = crate::memory::ScratchBuffers::new();
    let mut record_index = 0u64;

    while let Some(record_data) = reader.read_record()? {
        record_index += 1;
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
            raw_data_for_decode,
            record_index,
            &mut scratch,
        ) {
            Ok(json_value) => {
                write_json_record(output, &json_value)?;
                summary.records_processed += 1;
            }
            Err(error) => {
                summary.records_with_errors += 1;
                let family = error.family_prefix();
                telemetry::record_error(family);
                if options.strict_mode {
                    return Err(error);
                }
            }
        }
    }

    Ok(())
}

fn process_rdw_records<R: Read, W: Write>(
    schema: &Schema,
    reader: R,
    output: &mut W,
    options: &DecodeOptions,
    summary: &mut RunSummary,
) -> Result<()> {
    let mut reader = crate::record::RDWRecordReader::new(reader, options.strict_mode);
    let mut scratch = crate::memory::ScratchBuffers::new();
    let mut record_index = 0u64;

    while let Some(rdw_record) = reader.read_record()? {
        record_index += 1;
        summary.bytes_processed += rdw_record.payload.len() as u64;
        telemetry::record_read(rdw_record.payload.len(), options);

        if let Some(schema_lrecl) = schema.lrecl_fixed
            && rdw_record.payload.len() < schema_lrecl as usize
        {
            let error = Error::new(
                ErrorCode::CBKF221_RDW_UNDERFLOW,
                format!(
                    "RDW payload too short: {} bytes, schema requires {} bytes",
                    rdw_record.payload.len(),
                    schema_lrecl
                ),
            );

            summary.records_with_errors += 1;
            let family = error.family_prefix();
            telemetry::record_error(family);
            if options.strict_mode {
                return Err(error);
            }
            continue;
        }

        let full_raw_data = match options.emit_raw {
            crate::options::RawMode::RecordRDW => {
                let mut full_data =
                    Vec::with_capacity(rdw_record.header.len() + rdw_record.payload.len());
                full_data.extend_from_slice(&rdw_record.header);
                full_data.extend_from_slice(&rdw_record.payload);
                Some(full_data)
            }
            crate::options::RawMode::Record => Some(rdw_record.payload.clone()),
            _ => None,
        };

        match decode_record_with_scratch_and_raw(
            schema,
            &rdw_record.payload,
            options,
            full_raw_data,
            record_index,
            &mut scratch,
        ) {
            Ok(json_value) => {
                write_json_record(output, &json_value)?;
                summary.records_processed += 1;
            }
            Err(error) => {
                summary.records_with_errors += 1;
                let family = error.family_prefix();
                telemetry::record_error(family);
                if options.strict_mode {
                    return Err(error);
                }
            }
        }
    }

    Ok(())
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

/// Increment warning counter (thread-local)
pub fn increment_warning_counter() {
    WARNING_COUNTER.with(|counter| {
        *counter.borrow_mut() += 1;
    });
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
    let mut summary = RunSummary::new();
    summary.set_schema_fingerprint(schema.fingerprint.clone());
    summary.set_lineage_id(derive_lineage_id(&schema.fingerprint));

    let reader = BufReader::new(input);
    let mut record_count = 0u64;

    for line in reader.lines() {
        let line =
            line.map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string()))?;

        if line.trim().is_empty() {
            continue;
        }

        record_count += 1;

        // Parse JSON
        let json_value: Value = serde_json::from_str(&line)
            .map_err(|e| Error::new(ErrorCode::CBKE501_JSON_TYPE_MISMATCH, e.to_string()))?;

        // Encode to binary
        if let Ok(binary_data) = encode_record(schema, &json_value, options) {
            output
                .write_all(&binary_data)
                .map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string()))?;
            summary.bytes_processed += binary_data.len() as u64;
        } else {
            summary.records_with_errors += 1;
            if options.strict_mode {
                break;
            }
        }
    }

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
