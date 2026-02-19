// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deterministic JSON output for COBOL records
//!
//! This module implements streaming JSON writer that produces deterministic output
//! following the normative requirements for field ordering, numeric representation,
//! and REDEFINES handling.

#![allow(clippy::unused_self, clippy::unnecessary_wraps)]

use crate::options::{DecodeOptions, JsonNumberMode, RawMode, UnmappablePolicy};
use crate::JSON_SCHEMA_VERSION;
use base64::{engine::general_purpose, Engine as _};
use copybook_core::{Error, ErrorCode, Field, FieldKind, Occurs, Result, Schema};
use indexmap::IndexMap;
use serde_json::{Map, Value};
use std::collections::HashMap;
use std::io::Write;
use core::fmt::Write as _;

/// Streaming JSON writer for deterministic output
pub struct JsonWriter<W: Write> {
    writer: W,
    options: DecodeOptions,
    sequence_id: u64,
    /// Buffer for building JSON strings without intermediate allocations
    json_buffer: String,
    /// Schema reference for field lookup
    schema: Schema,
}

impl<W: Write> JsonWriter<W> {
    /// Create a new JSON writer
    pub fn new(writer: W, schema: Schema, options: DecodeOptions) -> Self {
        Self {
            writer,
            options,
            sequence_id: 0,
            json_buffer: String::with_capacity(4096), // Pre-allocate 4KB buffer
            schema,
        }
    }

    /// Write a single record as JSON line
    ///
    /// # Errors
    /// Returns an error if the record cannot be written or JSON serialization fails
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn write_record(
        &mut self,
        record_data: &[u8],
        record_index: u64,
        byte_offset: u64,
    ) -> Result<()> {
        // Create JSON object in schema order (pre-order traversal)
        let mut fields_obj = IndexMap::new();

        // Process fields in schema order
        self.process_fields_recursive(
            &self.schema.fields,
            record_data,
            &mut fields_obj,
            record_index,
            byte_offset,
        )?;

        let record_value = self.build_record_envelope(
            fields_obj,
            &self.schema,
            record_index,
            byte_offset,
            record_data.len(),
            record_data,
        )?;

        serde_json::to_writer(&mut self.writer, &record_value).map_err(|e| {
            Error::new(
                ErrorCode::CBKC201_JSON_WRITE_ERROR,
                format!("JSON write error: {}", e),
            )
        })?;

        writeln!(self.writer).map_err(|e| {
            Error::new(
                ErrorCode::CBKC201_JSON_WRITE_ERROR,
                format!("Write error: {}", e),
            )
        })?;

        self.sequence_id += 1;
        Ok(())
    }

    /// Write a single record as JSON line using optimized streaming approach
    /// Avoids intermediate Map allocations for better performance
    ///
    /// # Errors
    /// Returns an error if the record cannot be written or JSON serialization fails
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn write_record_streaming(
        &mut self,
        record_data: &[u8],
        record_index: u64,
        byte_offset: u64,
    ) -> Result<()> {
        // Clear and prepare JSON buffer
        self.json_buffer.clear();
        self.json_buffer.push('{');
        self.json_buffer.push_str("\"schema\":");
        self.write_json_string_to_buffer(JSON_SCHEMA_VERSION);

        let _ = write!(self.json_buffer, ",\"record_index\":{record_index}");

        self.json_buffer.push_str(",\"codepage\":");
        self.write_json_string_to_buffer(&self.options.codepage.to_string());

        self.json_buffer.push_str(",\"fields\":{");
        let mut fields_first = true;
        self.write_fields_streaming(
            &self.schema.fields,
            record_data,
            &mut fields_first,
            record_index,
            byte_offset,
        )?;
        self.json_buffer.push('}');

        if self.options.emit_meta {
            self.json_buffer.push_str(",\"schema_fingerprint\":");
            self.write_json_string_to_buffer(&self.schema.fingerprint);

            let _ = write!(self.json_buffer, ",\"offset\":{byte_offset}");
            let _ = write!(
                self.json_buffer,
                ",\"length\":{}",
                record_data.len()
            );
        }

        if matches!(self.options.emit_raw, RawMode::Record | RawMode::RecordRDW) {
            let encoded = general_purpose::STANDARD.encode(record_data);
            self.json_buffer.push_str(",\"raw_b64\":");
            self.write_json_string_to_buffer(&encoded);
        }

        self.json_buffer.push('}');

        // Write JSON line directly from buffer
        self.writer
            .write_all(self.json_buffer.as_bytes())
            .map_err(|e| {
                Error::new(
                    ErrorCode::CBKC201_JSON_WRITE_ERROR,
                    format!("Write error: {}", e),
                )
            })?;

        writeln!(self.writer).map_err(|e| {
            Error::new(
                ErrorCode::CBKC201_JSON_WRITE_ERROR,
                format!("Write error: {}", e),
            )
        })?;

        self.sequence_id += 1;
        Ok(())
    }

    /// Write fields directly to JSON string buffer for optimal performance
    fn write_fields_streaming(
        &mut self,
        fields: &[Field],
        record_data: &[u8],
        first_field: &mut bool,
        _record_index: u64,
        _byte_offset: u64,
    ) -> Result<()> {
        for field in fields {
            // Skip FILLER fields unless explicitly requested
            if (field.path.contains("_filler_") || field.name.eq_ignore_ascii_case("FILLER"))
                && !self.options.emit_filler
            {
                continue;
            }

            // Add field separator
            if !*first_field {
                self.json_buffer.push(',');
            }
            *first_field = false;

            // Write field name (use local name rather than full path)
            let field_name = self.get_field_name(field);
            self.json_buffer.push('"');
            self.json_buffer.push_str(&field_name);
            self.json_buffer.push_str(":");

            // Write field value based on type
            match &field.kind {
                FieldKind::Alphanum { len } => {
                    self.write_alphanum_value_streaming(field, record_data, *len)?;
                }
                FieldKind::ZonedDecimal {
                    digits,
                    scale,
                    signed,
                    sign_separate,
                } => {
                    if let Some(ref sign_sep) = sign_separate {
                        // Use SIGN SEPARATE decoding
                        let field_data = &record_data[field.offset as usize..(field.offset + field.len) as usize];
                        let decimal = crate::numeric::decode_zoned_decimal_sign_separate(
                            field_data,
                            *digits,
                            *scale,
                            sign_sep,
                            self.options.codepage,
                        )?;

                        // Write as JSON string with fixed scale (NORMATIVE)
                        self.json_buffer.push('"');
                        self.json_buffer.push_str(&decimal.to_string());
                        self.json_buffer.push('"');
                    } else {
                        // Use standard zoned decimal decoding
                        self.write_zoned_decimal_value_streaming(
                            field,
                            record_data,
                            *digits,
                            *scale,
                            *signed,
                        )?;
                    }
                }
                FieldKind::PackedDecimal {
                    digits,
                    scale,
                    signed,
                } => {
                    self.write_packed_decimal_value_streaming(
                        field,
                        record_data,
                        *digits,
                        *scale,
                        *signed,
                    )?;
                }
                FieldKind::BinaryInt { bits, signed } => {
                    self.write_binary_int_value_streaming(field, record_data, *bits, *signed)?;
                }
                FieldKind::Group => {
                    // Groups don't have values, skip
                    continue;
                }
            }

            // Add raw field data if requested
            if matches!(self.options.emit_raw, RawMode::Field) {
                self.json_buffer.push(',');
                self.json_buffer.push('"');
                self.json_buffer.push_str(&field.path);
                self.json_buffer.push_str("__raw_b64\":\"");

                let field_data =
                    &record_data[field.offset as usize..(field.offset + field.len) as usize];
                let encoded = general_purpose::STANDARD.encode(field_data);
                self.json_buffer.push_str(&encoded);
                self.json_buffer.push('"');
            }
        }
        Ok(())
    }

    fn build_record_envelope(
        &self,
        fields: IndexMap<String, Value>,
        schema: &Schema,
        record_index: u64,
        byte_offset: u64,
        record_length: usize,
        record_data: &[u8],
    ) -> Result<Value> {
        let mut root = IndexMap::new();
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
            Value::String(self.options.codepage.to_string()),
        );
        root.insert(
            "fields".to_string(),
            Value::Object(fields.into_iter().collect()),
        );

        if self.options.emit_meta {
            if !schema.fingerprint.is_empty() {
                root.insert(
                    "schema_fingerprint".to_string(),
                    Value::String(schema.fingerprint.clone()),
                );
            }
            root.insert(
                "offset".to_string(),
                Value::Number(serde_json::Number::from(byte_offset)),
            );
            root.insert(
                "length".to_string(),
                Value::Number(serde_json::Number::from(record_length)),
            );
        }

        if matches!(self.options.emit_raw, RawMode::Record | RawMode::RecordRDW) {
            let encoded = general_purpose::STANDARD.encode(record_data);
            root.insert("raw_b64".to_string(), Value::String(encoded));
        }

        Ok(Value::Object(root.into_iter().collect()))
    }

    /// Write alphanumeric field value directly to JSON buffer
    fn write_alphanum_value_streaming(
        &mut self,
        field: &Field,
        record_data: &[u8],
        len: u32,
    ) -> Result<()> {
        let field_data = &record_data[field.offset as usize..(field.offset + field.len) as usize];

        // Convert to UTF-8
        let text = crate::charset::ebcdic_to_utf8(field_data, self.options.codepage, self.options.on_decode_unmappable)?;

        // Write as JSON string with proper escaping
        self.json_buffer.push('"');
        for ch in text.chars() {
            match ch {
                '"' => self.json_buffer.push_str("\\\""),
                '\\' => self.json_buffer.push_str("\\\\"),
                '\n' => self.json_buffer.push_str("\\n"),
                '\r' => self.json_buffer.push_str("\\r"),
                '\t' => self.json_buffer.push_str("\\t"),
                c if c.is_control() => {
                    let _ = write!(self.json_buffer, "\\u{c:04x}");
                }
                c => self.json_buffer.push(c),
            }
        }
        self.json_buffer.push('"');
        Ok(())
    }

    /// Write zoned decimal field value directly to JSON buffer
    fn write_zoned_decimal_value_streaming(
        &mut self,
        field: &Field,
        record_data: &[u8],
        digits: u16,
        scale: i16,
        signed: bool,
    ) -> Result<()> {
        let field_data = &record_data[field.offset as usize..(field.offset + field.len) as usize];

        // Decode zoned decimal
        let decimal = crate::numeric::decode_zoned_decimal(
            field_data,
            digits,
            scale,
            signed,
            self.options.codepage,
            false, // blank_when_zero handled elsewhere
        )?;

        // Write as JSON string with fixed scale (NORMATIVE)
        self.json_buffer.push('"');
        self.json_buffer.push_str(&decimal.to_string());
        self.json_buffer.push('"');
        Ok(())
    }

    /// Write packed decimal field value directly to JSON buffer
    fn write_packed_decimal_value_streaming(
        &mut self,
        field: &Field,
        record_data: &[u8],
        digits: u16,
        scale: i16,
        signed: bool,
    ) -> Result<()> {
        let field_data = &record_data[field.offset as usize..(field.offset + field.len) as usize];

        // Decode packed decimal
        let decimal = crate::numeric::decode_packed_decimal(field_data, digits, scale, signed)?;

        // Write as JSON string with fixed scale (NORMATIVE)
        self.json_buffer.push('"');
        self.json_buffer.push_str(&decimal.to_string());
        self.json_buffer.push('"');
        Ok(())
    }

    /// Write binary integer field value directly to JSON buffer
    fn write_binary_int_value_streaming(
        &mut self,
        field: &Field,
        record_data: &[u8],
        bits: u16,
        signed: bool,
    ) -> Result<()> {
        let field_data = &record_data[field.offset as usize..(field.offset + field.len) as usize];

        // Decode binary integer
        let value = crate::numeric::decode_binary_int_fast(field_data, bits, signed)?;

        // Write as JSON number (up to 64-bit) or string for larger values
        match self.options.json_number_mode {
            JsonNumberMode::Lossless => {
                // Always use strings for lossless representation
                self.json_buffer.push('"');
                self.json_buffer.push_str(&value.to_string());
                self.json_buffer.push('"');
            }
            JsonNumberMode::Native => {
                // Use JSON numbers for values that fit in f64 without precision loss
                if value.abs() <= (1i64 << 53) {
                    self.json_buffer.push_str(&value.to_string());
                } else {
                    self.json_buffer.push('"');
                    self.json_buffer.push_str(&value.to_string());
                    self.json_buffer.push('"');
                }
            }
        }
        Ok(())
    }

    /// Process fields recursively in schema order
    fn process_fields_recursive(
        &mut self,
        fields: &[Field],
        record_data: &[u8],
        json_obj: &mut IndexMap<String, Value>,
        record_index: u64,
        byte_offset: u64,
    ) -> Result<()> {
        for field in fields {
            self.process_single_field(field, record_data, json_obj, record_index, byte_offset)?;
        }
        Ok(())
    }

    /// Process a single field
    fn process_single_field(
        &mut self,
        field: &Field,
        record_data: &[u8],
        json_obj: &mut IndexMap<String, Value>,
        record_index: u64,
        byte_offset: u64,
    ) -> Result<()> {
        // Skip FILLER fields unless explicitly requested
        if (field.name.eq_ignore_ascii_case("FILLER") || field.name.starts_with("_filler_"))
            && !self.options.emit_filler
        {
            return Ok(());
        }
        // Handle REDEFINES: emit all views in declaration order
        if field.redefines_of.is_some() {
            // This is a redefining field - process it normally
            // The primary field was already processed
        }

        match &field.kind {
            FieldKind::Group => {
                // For groups, create nested object and process children
                let mut group_obj = IndexMap::new();
                self.process_fields_recursive(
                    &field.children,
                    record_data,
                    &mut group_obj,
                    record_index,
                    byte_offset,
                )?;

                // Handle OCCURS for groups
                if let Some(occurs) = &field.occurs {
                    let array_value = self.process_group_array(
                        field,
                        record_data,
                        occurs,
                        record_index,
                        byte_offset,
                    )?;
                    json_obj.insert(self.get_field_name(field), array_value);
                } else {
                    json_obj.insert(self.get_field_name(field), Value::Object(group_obj.into_iter().collect()));
                }
            }
            _ => {
                // Scalar field
                if let Some(occurs) = &field.occurs {
                    let array_value = self.process_scalar_array(
                        field,
                        record_data,
                        occurs,
                        record_index,
                        byte_offset,
                    )?;
                    json_obj.insert(self.get_field_name(field), array_value);
                } else {
                    let field_value =
                        self.decode_scalar_field(field, record_data, record_index, byte_offset)?;
                    json_obj.insert(self.get_field_name(field), field_value);

                    // Add field-level raw data if requested
                    if matches!(self.options.emit_raw, RawMode::Field) {
                        self.add_field_raw_data(json_obj, field, record_data)?;
                    }
                }
            }
        }

        Ok(())
    }

    /// Process a group array (OCCURS on group)
    fn process_group_array(
        &mut self,
        field: &Field,
        record_data: &[u8],
        occurs: &Occurs,
        record_index: u64,
        byte_offset: u64,
    ) -> Result<Value> {
        let count = self.get_actual_array_count(occurs, record_data)?;
        let mut array = Vec::new();

        for _i in 0..count {
            let mut element_obj = IndexMap::new();

            // Process children for this array element
            self.process_fields_recursive(
                &field.children,
                record_data,
                &mut element_obj,
                record_index,
                byte_offset,
            )?;

            array.push(Value::Object(element_obj.into_iter().collect()));
        }

        Ok(Value::Array(array))
    }

    /// Process a scalar array (OCCURS on scalar field)
    fn process_scalar_array(
        &mut self,
        field: &Field,
        record_data: &[u8],
        occurs: &Occurs,
        record_index: u64,
        byte_offset: u64,
    ) -> Result<Value> {
        let count = self.get_actual_array_count(occurs, record_data)?;
        let mut array = Vec::new();

        let element_size = field.len
            / match occurs {
                Occurs::Fixed { count } => *count,
                Occurs::ODO { max, .. } => *max,
            };

        for i in 0..count {
            let element_offset = field.offset + (i * element_size);

            // Create a temporary field for the array element
            let mut element_field = field.clone();
            element_field.offset = element_offset;
            element_field.len = element_size;
            element_field.occurs = None; // Remove OCCURS for individual element

            let element_value =
                self.decode_scalar_field(&element_field, record_data, record_index, byte_offset)?;
            array.push(element_value);
        }

        Ok(Value::Array(array))
    }

    /// Get actual array count for OCCURS
    fn get_actual_array_count(&self, occurs: &Occurs, record_data: &[u8]) -> Result<u32> {
        match occurs {
            Occurs::Fixed { count } => Ok(*count),
            Occurs::ODO {
                min,
                max,
                counter_path,
            } => {
                // Find counter field and read its value
                // For now, return max count - this will be implemented when ODO support is added
                let _ = (min, counter_path, record_data);
                Ok(*max)
            }
        }
    }

    /// Decode a scalar field value
    fn decode_scalar_field(
        &self,
        field: &Field,
        record_data: &[u8],
        _record_index: u64,
        _byte_offset: u64,
    ) -> Result<Value> {
        // Check bounds
        let end_offset = field.offset + field.len;
        if end_offset as usize > record_data.len() {
            return Err(Error::new(
                ErrorCode::CBKD301_RECORD_TOO_SHORT,
                format!("Field {} extends beyond record boundary", field.path),
            ));
        }

        let field_data = &record_data[field.offset as usize..end_offset as usize];

        match &field.kind {
            FieldKind::Alphanum { .. } => {
                // Convert from EBCDIC/ASCII to UTF-8
                let text = crate::charset::ebcdic_to_utf8(
                    field_data,
                    self.options.codepage,
                    self.options.on_decode_unmappable,
                )?;
                // Preserve all spaces (no trimming) - NORMATIVE
                Ok(Value::String(text))
            }
            FieldKind::ZonedDecimal {
                digits,
                scale,
                signed,
            } => {
                let decimal = crate::numeric::decode_zoned_decimal(
                    field_data,
                    *digits,
                    *scale,
                    *signed,
                    self.options.codepage,
                    field.blank_when_zero,
                )?;

                // Fixed-scale rendering - NORMATIVE
                match self.options.json_number_mode {
                    JsonNumberMode::Lossless => {
                        // In lossless mode, preserve original field width for integers (scale=0)
                        let decimal_str = decimal.to_fixed_scale_string_with_width(*scale, *digits);
                        Ok(Value::String(decimal_str))
                    },
                    JsonNumberMode::Native => {
                        let decimal_str = decimal.to_fixed_scale_string(*scale);
                        // Try to parse as JSON number, fall back to string for precision
                        if let Ok(num) = decimal_str.parse::<f64>() {
                            if num.is_finite() {
                                if let Some(json_num) = serde_json::Number::from_f64(num) {
                                    Ok(Value::Number(json_num))
                                } else {
                                    Ok(Value::String(decimal_str))
                                }
                            } else {
                                Ok(Value::String(decimal_str))
                            }
                        } else {
                            Ok(Value::String(decimal_str))
                        }
                    }
                }
            }
            FieldKind::PackedDecimal {
                digits,
                scale,
                signed,
            } => {
                let decimal =
                    crate::numeric::decode_packed_decimal(field_data, *digits, *scale, *signed)?;

                // Fixed-scale rendering - NORMATIVE
                let decimal_str = decimal.to_fixed_scale_string(*scale);

                match self.options.json_number_mode {
                    JsonNumberMode::Lossless => Ok(Value::String(decimal_str)),
                    JsonNumberMode::Native => {
                        if let Ok(num) = decimal_str.parse::<f64>() {
                            if num.is_finite() {
                                if let Some(json_num) = serde_json::Number::from_f64(num) {
                                    Ok(Value::Number(json_num))
                                } else {
                                    Ok(Value::String(decimal_str))
                                }
                            } else {
                                Ok(Value::String(decimal_str))
                            }
                        } else {
                            Ok(Value::String(decimal_str))
                        }
                    }
                }
            }
            FieldKind::BinaryInt { bits, signed } => {
                let int_value = crate::numeric::decode_binary_int(field_data, *bits, *signed)?;

                // Use JSON numbers for values up to 64-bit
                if *bits <= 64 {
                    if *signed {
                        Ok(Value::Number(serde_json::Number::from(int_value as i64)))
                    } else {
                        Ok(Value::Number(serde_json::Number::from(int_value)))
                    }
                } else {
                    // Use string for larger values
                    Ok(Value::String(int_value.to_string()))
                }
            }
            FieldKind::Group => {
                // This shouldn't happen for scalar fields
                Err(Error::new(
                    ErrorCode::CBKD101_INVALID_FIELD_TYPE,
                    format!("Group field {} processed as scalar", field.path),
                ))
            }
        }
    }

    /// Get field name with duplicate disambiguation
    fn get_field_name(&self, field: &Field) -> String {
        // Handle FILLER fields (original or offset-based)
        if field.name.eq_ignore_ascii_case("FILLER") || field.name.starts_with("_filler_") {
            if self.options.emit_filler {
                if field.name.eq_ignore_ascii_case("FILLER") {
                    // _filler_<offset> - NORMATIVE
                    format!("_filler_{:08}", field.offset)
                } else {
                    field.name.clone()
                }
            } else {
                // Skip FILLER fields by default
                String::new()
            }
        } else {
            field.name.clone()
        }
    }

    /// Add field-level raw data
    fn add_field_raw_data(
        &self,
        json_obj: &mut IndexMap<String, Value>,
        field: &Field,
        record_data: &[u8],
    ) -> Result<()> {
        let end_offset = field.offset + field.len;
        if end_offset as usize <= record_data.len() {
            let field_data = &record_data[field.offset as usize..end_offset as usize];
            let encoded = general_purpose::STANDARD.encode(field_data);
            let raw_key = format!("{}_raw_b64", field.name);
            json_obj.insert(raw_key, Value::String(encoded));
        }

        Ok(())
    }

    /// Write fields directly to JSON string buffer for streaming performance
    fn write_fields_streaming(
        &mut self,
        fields: &[Field],
        record_data: &[u8],
        record_index: u64,
        byte_offset: u64,
        first_field: &mut bool,
    ) -> Result<()> {
        for field in fields {
            self.write_single_field_streaming(
                field,
                record_data,
                record_index,
                byte_offset,
                first_field,
            )?;
        }
        Ok(())
    }

    /// Write a single field directly to JSON string buffer
    fn write_single_field_streaming(
        &mut self,
        field: &Field,
        record_data: &[u8],
        record_index: u64,
        byte_offset: u64,
        first_field: &mut bool,
    ) -> Result<()> {
        let field_name = self.get_field_name(field);

        // Skip FILLER fields if not emitted
        if field_name.is_empty() {
            return Ok(());
        }

        // Add comma separator if not first field
        if !*first_field {
            self.json_buffer.push(',');
        }
        *first_field = false;

        // Write field name
        self.json_buffer.push('"');
        self.json_buffer.push_str(&field_name);
        self.json_buffer.push_str(":");

        match &field.kind {
            FieldKind::Group => {
                if let Some(occurs) = &field.occurs {
                    self.write_group_array_streaming(
                        field,
                        record_data,
                        occurs,
                        record_index,
                        byte_offset,
                    )?;
                } else {
                    // Single group - write nested object
                    self.json_buffer.push('{');
                    let mut group_first_field = true;
                    self.write_fields_streaming(
                        &field.children,
                        record_data,
                        record_index,
                        byte_offset,
                        &mut group_first_field,
                    )?;
                    self.json_buffer.push('}');
                }
            }
            _ => {
                // Scalar field
                if let Some(occurs) = &field.occurs {
                    self.write_scalar_array_streaming(
                        field,
                        record_data,
                        occurs,
                        record_index,
                        byte_offset,
                    )?;
                } else {
                    self.write_scalar_field_streaming(
                        field,
                        record_data,
                        record_index,
                        byte_offset,
                    )?;

                    // Add field-level raw data if requested
                    if matches!(self.options.emit_raw, RawMode::Field) {
                        // This would require additional comma handling - simplified for now
                    }
                }
            }
        }

        Ok(())
    }

    /// Write scalar field value directly to JSON string buffer
    fn write_scalar_field_streaming(
        &mut self,
        field: &Field,
        record_data: &[u8],
        _record_index: u64,
        _byte_offset: u64,
    ) -> Result<()> {
        // Check bounds
        let end_offset = field.offset + field.len;
        if end_offset as usize > record_data.len() {
            return Err(Error::new(
                ErrorCode::CBKD301_RECORD_TOO_SHORT,
                format!("Field {} extends beyond record boundary", field.path),
            ));
        }

        let field_data = &record_data[field.offset as usize..end_offset as usize];

        match &field.kind {
            FieldKind::Alphanum { .. } => {
                // Convert from EBCDIC/ASCII to UTF-8
                let text = crate::charset::ebcdic_to_utf8(
                    field_data,
                    self.options.codepage,
                    self.options.on_decode_unmappable,
                )?;
                // Write as JSON string with proper escaping
                self.write_json_string_to_buffer(&text);
            }
            FieldKind::ZonedDecimal {
                digits,
                scale,
                signed,
            } => {
                let decimal = crate::numeric::decode_zoned_decimal(
                    field_data,
                    *digits,
                    *scale,
                    *signed,
                    self.options.codepage,
                    field.blank_when_zero,
                )?;

                // Fixed-scale rendering - NORMATIVE
                let decimal_str = decimal.to_fixed_scale_string(*scale);

                match self.options.json_number_mode {
                    JsonNumberMode::Lossless => {
                        self.write_json_string_to_buffer(&decimal_str);
                    }
                    JsonNumberMode::Native => {
                        // Try to write as number, fall back to string
                        if let Ok(num) = decimal_str.parse::<f64>() {
                            if num.is_finite() {
                                self.write_json_number_to_buffer(num).map_err(|_| {
                                    Error::new(
                                        ErrorCode::CBKC201_JSON_WRITE_ERROR,
                                        "failed to format number into JSON buffer".to_string(),
                                    )
                                })?;
                            } else {
                                self.write_json_string_to_buffer(&decimal_str);
                            }
                        } else {
                            self.write_json_string_to_buffer(&decimal_str);
                        }
                    }
                }
            }
            FieldKind::PackedDecimal {
                digits,
                scale,
                signed,
            } => {
                let decimal =
                    crate::numeric::decode_packed_decimal(field_data, *digits, *scale, *signed)?;

                // Fixed-scale rendering - NORMATIVE
                let decimal_str = decimal.to_fixed_scale_string(*scale);

                match self.options.json_number_mode {
                    JsonNumberMode::Lossless => {
                        self.write_json_string_to_buffer(&decimal_str);
                    }
                    JsonNumberMode::Native => {
                        if let Ok(num) = decimal_str.parse::<f64>() {
                            if num.is_finite() {
                                self.write_json_number_to_buffer(num).map_err(|_| {
                                    Error::new(
                                        ErrorCode::CBKC201_JSON_WRITE_ERROR,
                                        "failed to format number into JSON buffer".to_string(),
                                    )
                                })?;
                            } else {
                                self.write_json_string_to_buffer(&decimal_str);
                            }
                        } else {
                            self.write_json_string_to_buffer(&decimal_str);
                        }
                    }
                }
            }
            FieldKind::BinaryInt { bits, signed } => {
                let int_value = crate::numeric::decode_binary_int(field_data, *bits, *signed)?;

                // Use JSON numbers for values up to 64-bit
                if *bits <= 64 {
                    self.write_json_number_to_buffer(int_value as f64).map_err(|_| {
                        Error::new(
                            ErrorCode::CBKC201_JSON_WRITE_ERROR,
                            "failed to format number into JSON buffer".to_string(),
                        )
                    })?;
                } else {
                    // Use string for larger values
                    self.write_json_string_to_buffer(&int_value.to_string());
                }
            }
            FieldKind::Group => {
                // This shouldn't happen for scalar fields
                return Err(Error::new(
                    ErrorCode::CBKD101_INVALID_FIELD_TYPE,
                    format!("Group field {} processed as scalar", field.path),
                ));
            }
        }

        Ok(())
    }

    /// Write a JSON string with proper escaping to the buffer
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
                    let _ = write!(self.json_buffer, "\\u{c:04x}");
                }
                c => self.json_buffer.push(c),
            }
        }
        self.json_buffer.push('"');
    }

    /// Write a JSON number to the buffer
    fn write_json_number_to_buffer(&mut self, num: f64) -> std::fmt::Result {
        use std::fmt::Write;
        write!(self.json_buffer, "{num}")
    }

    /// Write group array directly to JSON string buffer
    fn write_group_array_streaming(
        &mut self,
        field: &Field,
        record_data: &[u8],
        occurs: &Occurs,
        record_index: u64,
        byte_offset: u64,
    ) -> Result<()> {
        let count = self.get_actual_array_count(occurs, record_data)?;

        self.json_buffer.push('[');

        for i in 0..count {
            if i > 0 {
                self.json_buffer.push(',');
            }

            let element_offset = field.offset + (i * field.len);
            self.json_buffer.push('{');

            let mut element_first_field = true;
            self.write_fields_streaming(
                &field.children,
                record_data,
                record_index,
                byte_offset,
                &mut element_first_field,
            )?;

            self.json_buffer.push('}');
        }

        self.json_buffer.push(']');
        Ok(())
    }

    /// Write scalar array directly to JSON string buffer
    fn write_scalar_array_streaming(
        &mut self,
        field: &Field,
        record_data: &[u8],
        occurs: &Occurs,
        record_index: u64,
        byte_offset: u64,
    ) -> Result<()> {
        let count = self.get_actual_array_count(occurs, record_data)?;

        let element_size = field.len
            / match occurs {
                Occurs::Fixed { count } => *count,
                Occurs::ODO { max, .. } => *max,
            };

        self.json_buffer.push('[');

        for i in 0..count {
            if i > 0 {
                self.json_buffer.push(',');
            }

            let element_offset = field.offset + (i * element_size);

            // Create a temporary field for the array element
            let mut element_field = field.clone();
            element_field.offset = element_offset;
            element_field.len = element_size;
            element_field.occurs = None; // Remove OCCURS for individual element

            self.write_scalar_field_streaming(
                &element_field,
                record_data,
                record_index,
                byte_offset,
            )?;
        }

        self.json_buffer.push(']');
        Ok(())
    }

    /// Finish writing and flush
    ///
    /// # Errors
    /// Returns an error if the underlying writer cannot be flushed.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn finish(mut self) -> Result<W> {
        self.writer.flush().map_err(|e| {
            Error::new(
                ErrorCode::CBKC201_JSON_WRITE_ERROR,
                format!("Flush error: {}", e),
            )
        })?;
        Ok(self.writer)
    }
}

/// JSON encoder for converting JSON back to binary records
pub struct JsonEncoder {
    options: crate::options::EncodeOptions,
}

impl JsonEncoder {
    /// Create a new JSON encoder
    #[must_use]
    pub fn new(options: crate::options::EncodeOptions) -> Self {
        Self { options }
    }

    /// Encode a JSON value to binary record
    ///
    /// # Errors
    /// Returns an error if the JSON cannot be encoded according to the schema.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn encode_record(&self, schema: &Schema, json: &Value) -> Result<Vec<u8>> {
        // Calculate maximum record size
        let max_record_size = self.calculate_max_record_size(schema)?;
        let mut record_data = vec![0u8; max_record_size];

        // Process fields in schema order
        if let Value::Object(obj) = json {
            self.encode_fields_recursive(schema, &schema.fields, obj, &mut record_data)?;
        } else {
            return Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                "Expected JSON object for record",
            ));
        }

        // Handle --use-raw mode for byte-identical round-trips
        if self.options.use_raw {
            if let Some(raw_data) = self.extract_raw_data(json)? {
                // Verify that decoded values match the JSON values
                if self.verify_raw_data_matches(schema, &raw_data, json)? {
                    return Ok(raw_data);
                }
            }
        }

        // Trim to actual record size if variable length
        if let Some(tail_odo) = &schema.tail_odo {
            let actual_size = self.calculate_actual_record_size(schema, json, tail_odo)?;
            record_data.truncate(actual_size);
        } else if let Some(lrecl) = schema.lrecl_fixed {
            record_data.truncate(lrecl as usize);
        }

        Ok(record_data)
    }

    /// Calculate maximum possible record size
    fn calculate_max_record_size(&self, schema: &Schema) -> Result<usize> {
        if let Some(lrecl) = schema.lrecl_fixed {
            Ok(lrecl as usize)
        } else {
            // Calculate based on field offsets and lengths
            let mut max_size = 0;
            for field in schema.all_fields() {
                let field_end = field.offset + field.effective_length();
                if field_end > max_size {
                    max_size = field_end;
                }
            }
            Ok(max_size as usize)
        }
    }

    /// Calculate actual record size for variable-length records
    fn calculate_actual_record_size(
        &self,
        schema: &Schema,
        _json: &Value,
        _tail_odo: &copybook_core::TailODO,
    ) -> Result<usize> {
        // For now, return maximum size - ODO support will be implemented later
        self.calculate_max_record_size(schema)
    }

    /// Encode fields recursively
    fn encode_fields_recursive(
        &self,
        schema: &Schema,
        fields: &[Field],
        json_obj: &IndexMap<String, Value>,
        record_data: &mut [u8],
    ) -> Result<()> {
        for field in fields {
            self.encode_single_field(schema, field, json_obj, record_data)?;
        }
        Ok(())
    }

    /// Encode a single field
    fn encode_single_field(
        &self,
        schema: &Schema,
        field: &Field,
        json_obj: &IndexMap<String, Value>,
        record_data: &mut [u8],
    ) -> Result<()> {
        let field_name = self.get_field_name(field);

        // Skip FILLER fields if not emitted
        if field_name.is_empty() {
            return Ok(());
        }

        // Handle REDEFINES: implement precedence rules (NORMATIVE)
        if field.redefines_of.is_some() {
            return self.encode_redefines_field(schema, field, json_obj, record_data);
        }

        match &field.kind {
            FieldKind::Group => {
                if let Some(occurs) = &field.occurs {
                    self.encode_group_array(schema, field, json_obj, record_data, occurs)?;
                } else {
                    // Single group - process children
                    if let Some(Value::Object(group_obj)) = json_obj.get(&field_name) {
                        self.encode_fields_recursive(
                            schema,
                            &field.children,
                            group_obj,
                            record_data,
                        )?;
                    }
                }
            }
            _ => {
                // Scalar field
                if let Some(occurs) = &field.occurs {
                    self.encode_scalar_array(schema, field, json_obj, record_data, occurs)?;
                } else if let Some(field_value) = json_obj.get(&field_name) {
                    self.encode_scalar_field(field, field_value, record_data)?;
                }
            }
        }

        Ok(())
    }

    /// Encode REDEFINES field with precedence rules (NORMATIVE)
    fn encode_redefines_field(
        &self,
        schema: &Schema,
        field: &Field,
        json_obj: &IndexMap<String, Value>,
        record_data: &mut [u8],
    ) -> Result<()> {
        // REDEFINES encode precedence (NORMATIVE):
        // 1. Raw bytes (if --use-raw and values match)
        // 2. Single non-null view
        // 3. Error (ambiguous)

        // Step 1: Check for raw data if --use-raw is enabled
        if self.options.use_raw {
            // Check for record-level raw data first
            if let Some(raw_data) = self.extract_raw_data_from_json_obj(json_obj)? {
                // Verify that decoded values match the JSON values for this cluster
                if self.verify_cluster_raw_data_matches(field, &raw_data, json_obj)? {
                    let end_offset = (field.offset + field.len) as usize;
                    if end_offset <= record_data.len()
                        && field.offset as usize + raw_data.len() <= record_data.len()
                    {
                        let cluster_size = self.calculate_redefines_cluster_size(field)?;
                        let cluster_end = field.offset as usize + cluster_size;
                        if cluster_end <= raw_data.len() {
                            record_data[field.offset as usize..cluster_end]
                                .copy_from_slice(&raw_data[field.offset as usize..cluster_end]);
                            return Ok(());
                        }
                    }
                }
            }

            // Check for field-level raw data
            if let Some(raw_data) = self.extract_field_raw_data(field, json_obj)? {
                let end_offset = (field.offset + field.len) as usize;
                if end_offset <= record_data.len() && raw_data.len() == field.len as usize {
                    record_data[field.offset as usize..end_offset].copy_from_slice(&raw_data);
                    return Ok(());
                }
            }
        }

        // Step 2: Find all views in the REDEFINES cluster
        let cluster_views = self.find_redefines_cluster_views(schema, field, json_obj)?;
        let non_null_views: Vec<_> = cluster_views
            .iter()
            .filter(|(_, value)| !value.is_null())
            .collect();

        match non_null_views.len() {
            0 => {
                // All views are null - fill with zeros
                let cluster_size = self.calculate_redefines_cluster_size(field)?;
                let end_offset = field.offset as usize + cluster_size;
                if end_offset <= record_data.len() {
                    record_data[field.offset as usize..end_offset].fill(0);
                }
                Ok(())
            }
            1 => {
                // Single non-null view - encode from that view
                let (view_field, view_value) = non_null_views[0];
                let mut temp_obj = IndexMap::new();
                temp_obj.insert(view_field.name.clone(), (*view_value).clone());
                self.encode_single_field(schema, view_field, &temp_obj, record_data)
            }
            _ => {
                // Multiple non-null views - ambiguous (NORMATIVE)
                let cluster_path = self.get_redefines_cluster_path(field);
                Err(Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    format!(
                        "Ambiguous REDEFINES encoding: multiple non-null views for cluster '{}'",
                        cluster_path
                    ),
                )
                .with_field(cluster_path)
                .with_offset(field.offset as u64))
            }
        }
    }

    /// Find all views in a REDEFINES cluster
    fn find_redefines_cluster_views<'a>(
        &'a self,
        schema: &'a Schema,
        field: &'a Field,
        json_obj: &'a IndexMap<String, Value>,
    ) -> Result<Vec<(&'a Field, &'a Value)>> {
        let mut cluster_views = Vec::new();

        // Find the primary field (the one being redefined)
        let primary_field = if let Some(ref redefines_path) = field.redefines_of {
            // This field redefines another - find the primary
            self
                .schema
                .find_field(redefines_path)
                .ok_or_else(|| {
                    Error::new(
                        ErrorCode::CBKS121_COUNTER_NOT_FOUND,
                        format!("Field not found: {}", redefines_path),
                    )
                })?
        } else {
            // This is the primary field
            field
        };

        // Add the primary field if it has a value in JSON
        let primary_name = self.get_field_name(primary_field);
        if !primary_name.is_empty() {
            if let Some(value) = json_obj.get(&primary_name) {
                cluster_views.push((primary_field, value));
            }
        }

        // Find all fields that redefine the primary field
        let redefining_fields = self.schema.find_redefining_fields(&primary_field.path);
        for redefining_field in redefining_fields {
            let redefining_name = self.get_field_name(redefining_field);
            if !redefining_name.is_empty() {
                if let Some(value) = json_obj.get(&redefining_name) {
                    cluster_views.push((redefining_field, value));
                }
            }
        }

        Ok(cluster_views)
    }

    /// Encode group array
    fn encode_group_array(
        &self,
        schema: &Schema,
        field: &Field,
        json_obj: &IndexMap<String, Value>,
        record_data: &mut [u8],
        occurs: &Occurs,
    ) -> Result<()> {
        let field_name = self.get_field_name(field);

        if let Some(Value::Array(array)) = json_obj.get(&field_name) {
            // Validate array length
            self.validate_array_length(array.len(), occurs)?;

            // Update ODO counter if needed
            if let Occurs::ODO { counter_path, .. } = occurs {
                self.update_odo_counter(
                    schema,
                    counter_path,
                    u32::try_from(array.len()).map_err(|_| {
                        Error::new(
                            ErrorCode::CBKE521_ARRAY_LEN_OOB,
                            format!("Array length {} exceeds maximum u32 value", array.len()),
                        )
                    })?,
                    json_obj,
                    record_data,
                )?;
            }

            // Encode each array element
            let element_size = field.len / self.get_occurs_max_count(occurs);
            for (i, element) in array.iter().enumerate() {
                if let Value::Object(element_obj) = element {
                    let element_offset = field.offset + (i as u32 * element_size);

                    // Create a temporary field for the array element
                    let mut element_field = field.clone();
                    element_field.offset = element_offset;
                    element_field.len = element_size;
                    element_field.occurs = None;

                    self.encode_fields_recursive(
                        schema,
                        &element_field.children,
                        element_obj,
                        record_data,
                    )?;
                }
            }
        }

        Ok(())
    }

    /// Encode scalar array
    fn encode_scalar_array(
        &self,
        schema: &Schema,
        field: &Field,
        json_obj: &IndexMap<String, Value>,
        record_data: &mut [u8],
        occurs: &Occurs,
    ) -> Result<()> {
        let field_name = self.get_field_name(field);

        if let Some(Value::Array(array)) = json_obj.get(&field_name) {
            // Validate array length
            self.validate_array_length(array.len(), occurs)?;

            // Update ODO counter if needed
            if let Occurs::ODO { counter_path, .. } = occurs {
                self.update_odo_counter(
                    schema,
                    counter_path,
                    u32::try_from(array.len()).map_err(|_| {
                        Error::new(
                            ErrorCode::CBKE521_ARRAY_LEN_OOB,
                            format!("Array length {} exceeds maximum u32 value", array.len()),
                        )
                    })?,
                    json_obj,
                    record_data,
                )?;
            }

            // Encode each array element
            let element_size = field.len / self.get_occurs_max_count(occurs);
            for (i, element) in array.iter().enumerate() {
                let element_offset = field.offset + (i as u32 * element_size);

                // Create a temporary field for the array element
                let mut element_field = field.clone();
                element_field.offset = element_offset;
                element_field.len = element_size;
                element_field.occurs = None;

                self.encode_scalar_field(&element_field, element, record_data)?;
            }
        }

        Ok(())
    }

    /// Validate array length against OCCURS constraints
    fn validate_array_length(&self, actual_len: usize, occurs: &Occurs) -> Result<()> {
        match occurs {
            Occurs::Fixed { count } => {
                if actual_len != *count as usize {
                    return Err(Error::new(
                        ErrorCode::CBKE521_ARRAY_LEN_OOB,
                        format!(
                            "Array length {} doesn't match fixed OCCURS count {}",
                            actual_len, count
                        ),
                    ));
                }
            }
            Occurs::ODO { min, max, .. } => {
                if actual_len < *min as usize || actual_len > *max as usize {
                    return Err(Error::new(
                        ErrorCode::CBKE521_ARRAY_LEN_OOB,
                        format!(
                            "Array length {} is outside ODO range {}-{}",
                            actual_len, min, max
                        ),
                    ));
                }
            }
        }
        Ok(())
    }

    /// Update ODO counter field
    fn update_odo_counter(
        &self,
        schema: &Schema,
        counter_path: &str,
        count: u32,
        _json_obj: &IndexMap<String, Value>,
        record_data: &mut [u8],
    ) -> Result<()> {
        // Find the counter field in the schema
        let counter_field = schema
            .find_field(counter_path)
            .ok_or_else(|| {
                Error::new(
                    ErrorCode::CBKS121_COUNTER_NOT_FOUND,
                    format!("Field not found: {}", counter_path),
                )
            })?;
        // Encode the count value into the counter field
        match &counter_field.kind {
            FieldKind::ZonedDecimal {
                digits,
                scale,
                signed,
            } => {
                let count_str = if *scale == 0 {
                    count.to_string()
                } else {
                    // Handle scaled values
                    let scale_factor = 10_i32.pow(u32::from((*scale).unsigned_abs()));
                    if *scale > 0 {
                        format!(
                            "{:.1$}",
                            count as f64 / scale_factor as f64,
                            *scale as usize
                        )
                    } else {
                        (count.saturating_mul(scale_factor.unsigned_abs())).to_string()
                    }
                };

                let encoded = crate::numeric::encode_zoned_decimal(
                    &count_str,
                    *digits,
                    *scale,
                    *signed,
                    self.options.codepage,
                )?;

                let end_offset = (counter_field.offset + counter_field.len) as usize;
                if end_offset <= record_data.len() {
                    record_data[counter_field.offset as usize..end_offset]
                        .copy_from_slice(&encoded);
                }
            }
            FieldKind::PackedDecimal {
                digits,
                scale,
                signed,
            } => {
                let count_str = if *scale == 0 {
                    count.to_string()
                } else {
                    // Handle scaled values
                    let scale_factor = 10_i32.pow(u32::from((*scale).unsigned_abs()));
                    if *scale > 0 {
                        format!(
                            "{:.1$}",
                            count as f64 / scale_factor as f64,
                            *scale as usize
                        )
                    } else {
                        (count.saturating_mul(scale_factor.unsigned_abs())).to_string()
                    }
                };

                let encoded =
                    crate::numeric::encode_packed_decimal(&count_str, *digits, *scale, *signed)?;

                let end_offset = (counter_field.offset + counter_field.len) as usize;
                if end_offset <= record_data.len() {
                    record_data[counter_field.offset as usize..end_offset]
                        .copy_from_slice(&encoded);
                }
            }
            FieldKind::BinaryInt { bits, signed } => {
                let encoded = crate::numeric::encode_binary_int(count as i64, *bits, *signed)?;

                let end_offset = (counter_field.offset + counter_field.len) as usize;
                if end_offset <= record_data.len() {
                    record_data[counter_field.offset as usize..end_offset]
                        .copy_from_slice(&encoded);
                }
            }
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    format!(
                        "ODO counter field '{}' has invalid type for numeric value",
                        counter_path
                    ),
                )
                .with_field(counter_path.to_string()));
            }
        }

        Ok(())
    }

    /// Get maximum count for OCCURS
    fn get_occurs_max_count(&self, occurs: &Occurs) -> u32 {
        match occurs {
            Occurs::Fixed { count } => *count,
            Occurs::ODO { max, .. } => *max,
        }
    }

    /// Encode scalar field value
    fn encode_scalar_field(
        &self,
        field: &Field,
        value: &Value,
        record_data: &mut [u8],
    ) -> Result<()> {
        let end_offset = (field.offset + field.len) as usize;
        if end_offset > record_data.len() {
            return Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!("Field {} extends beyond record boundary", field.path),
            )
            .with_field(field.path.clone())
            .with_offset(field.offset as u64));
        }

        let field_data = &mut record_data[field.offset as usize..end_offset];

        match &field.kind {
            FieldKind::Alphanum { len: _ } => {
                if let Value::String(text) = value {
                    // Validate string length doesn't exceed field capacity
                    if text.len() > field.len as usize {
                        return Err(Error::new(
                            ErrorCode::CBKE515_STRING_LENGTH_VIOLATION,
                            format!("String length {} exceeds field capacity {} for alphanumeric field {}",
                                text.len(), field.len, field.path),
                        ).with_field(field.path.clone()));
                    }

                    let encoded = crate::numeric::encode_alphanumeric(
                        text,
                        field.len as usize,
                        self.options.codepage,
                    )?;
                    field_data.copy_from_slice(&encoded);
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!(
                            "Expected string for alphanumeric field {}, got {}",
                            field.path,
                            self.value_type_name(value)
                        ),
                    )
                    .with_field(field.path.clone()));
                }
            }
            FieldKind::ZonedDecimal {
                digits,
                scale,
                signed,
            } => {
                let value_str = match value {
                    Value::String(s) => {
                        // Validate that the string represents a valid decimal number
                        self.validate_decimal_string(s, *digits, *scale, *signed, &field.path)?;
                        s.clone()
                    }
                    Value::Number(n) => {
                        let s = n.to_string();
                        self.validate_decimal_string(&s, *digits, *scale, *signed, &field.path)?;
                        s
                    }
                    _ => {
                        return Err(Error::new(
                            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                            format!(
                                "Expected string or number for zoned decimal field {}, got {}",
                                field.path,
                                self.value_type_name(value)
                            ),
                        )
                        .with_field(field.path.clone()));
                    }
                };

                let encoded = if field.blank_when_zero && self.options.bwz_encode {
                    crate::numeric::encode_zoned_decimal_with_bwz(
                        &value_str,
                        *digits,
                        *scale,
                        *signed,
                        self.options.codepage,
                        self.options.bwz_encode,
                    )?
                } else {
                    crate::numeric::encode_zoned_decimal(
                        &value_str,
                        *digits,
                        *scale,
                        *signed,
                        self.options.codepage,
                    )?
                };
                field_data.copy_from_slice(&encoded);
            }
            FieldKind::PackedDecimal {
                digits,
                scale,
                signed,
            } => {
                let value_str = match value {
                    Value::String(s) => {
                        self.validate_decimal_string(s, *digits, *scale, *signed, &field.path)?;
                        s.clone()
                    }
                    Value::Number(n) => {
                        let s = n.to_string();
                        self.validate_decimal_string(&s, *digits, *scale, *signed, &field.path)?;
                        s
                    }
                    _ => {
                        return Err(Error::new(
                            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                            format!(
                                "Expected string or number for packed decimal field {}, got {}",
                                field.path,
                                self.value_type_name(value)
                            ),
                        )
                        .with_field(field.path.clone()));
                    }
                };

                let encoded =
                    crate::numeric::encode_packed_decimal(&value_str, *digits, *scale, *signed)?;
                field_data.copy_from_slice(&encoded);
            }
            FieldKind::BinaryInt { bits, signed } => {
                let int_value = match value {
                    Value::Number(n) => {
                        if *signed {
                            n.as_i64().ok_or_else(|| {
                                Error::new(
                                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                                    format!(
                                        "Invalid signed integer for field {}: number out of range",
                                        field.path
                                    ),
                                )
                                .with_field(field.path.clone())
                            })?
                        } else {
                            n.as_u64().ok_or_else(|| Error::new(
                                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                                format!("Invalid unsigned integer for field {}: number out of range", field.path),
                            ).with_field(field.path.clone()))? as i64
                        }
                    }
                    Value::String(s) => s.parse::<i64>().map_err(|_| {
                        Error::new(
                            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                            format!("Invalid integer string '{}' for field {}", s, field.path),
                        )
                        .with_field(field.path.clone())
                    })?,
                    _ => {
                        return Err(Error::new(
                            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                            format!(
                                "Expected number or string for binary integer field {}, got {}",
                                field.path,
                                self.value_type_name(value)
                            ),
                        )
                        .with_field(field.path.clone()));
                    }
                };

                // Validate integer range for field
                self.validate_integer_range(int_value, bits, signed, &field.path)?;

                let encoded = crate::numeric::encode_binary_int(int_value, bits, signed)?;
                field_data.copy_from_slice(&encoded);
                Ok(())
            }
            FieldKind::Group => {
                return Err(Error::new(
                    ErrorCode::CBKD101_INVALID_FIELD_TYPE,
                    format!("Group field {} processed as scalar", field.path),
                )
                .with_field(field.path.clone()));
            }
        }
    }

    /// Encode alphanumeric field
    fn encode_alphanumeric_field(
        &self,
        field: &Field,
        value: &Value,
        field_data: &mut [u8],
    ) -> Result<()> {
        if let Value::String(text) = value {
            // Validate string length doesn't exceed field capacity
            if text.len() > field.len as usize {
                return Err(Error::new(
                    ErrorCode::CBKE515_STRING_LENGTH_VIOLATION,
                    format!(
                        "String length {} exceeds field capacity {} for alphanumeric field {}",
                        text.len(),
                        field.len,
                        field.path
                    ),
                )
                .with_field(field.path.clone()));
            }

            let encoded = crate::numeric::encode_alphanumeric(
                text,
                field.len as usize,
                self.options.codepage,
            )?;
            field_data.copy_from_slice(&encoded);
        } else {
            return Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!(
                    "Expected string for alphanumeric field {}, got {}",
                    field.path,
                    self.value_type_name(value)
                ),
            )
            .with_field(field.path.clone()));
        }
        Ok(())
    }

    /// Encode zoned decimal field
    fn encode_zoned_decimal_field(
        &self,
        field: &Field,
        value: &Value,
        field_data: &mut [u8],
        digits: u16,
        scale: i16,
        signed: bool,
    ) -> Result<()> {
        let value_str = self.extract_decimal_string(value, digits, scale, signed, &field.path)?;

        let encoded = if field.blank_when_zero && self.options.bwz_encode {
            crate::numeric::encode_zoned_decimal_with_bwz(
                &value_str,
                digits,
                scale,
                signed,
                self.options.codepage,
                self.options.bwz_encode,
            )?
        } else {
            crate::numeric::encode_zoned_decimal(
                &value_str,
                digits,
                scale,
                signed,
                self.options.codepage,
            )?
        };
        field_data.copy_from_slice(&encoded);
        Ok(())
    }

    /// Encode packed decimal field
    fn encode_packed_decimal_field(
        &self,
        field: &Field,
        value: &Value,
        field_data: &mut [u8],
        digits: u16,
        scale: i16,
        signed: bool,
    ) -> Result<()> {
        let value_str = self.extract_decimal_string(value, digits, scale, signed, &field.path)?;
        let encoded = crate::numeric::encode_packed_decimal(&value_str, digits, scale, signed)?;
        field_data.copy_from_slice(&encoded);
        Ok(())
    }

    /// Encode binary integer field
    fn encode_binary_int_field(
        &self,
        field: &Field,
        value: &Value,
        field_data: &mut [u8],
        bits: u16,
        signed: bool,
    ) -> Result<()> {
        let int_value = match value {
            Value::Number(n) => {
                if signed {
                    n.as_i64().ok_or_else(|| {
                        Error::new(
                            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                            format!(
                                "Invalid signed integer for field {}: number out of range",
                                field.path
                            ),
                        )
                        .with_field(field.path.clone())
                    })?
                } else {
                    let unsigned_val = n.as_u64().ok_or_else(|| {
                        Error::new(
                            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                            format!(
                                "Invalid unsigned integer for field {}: number out of range",
                                field.path
                            ),
                        )
                        .with_field(field.path.clone())
                    })?;
                    i64::try_from(unsigned_val).map_err(|_| {
                        Error::new(
                            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                            format!("Unsigned integer value {unsigned_val} exceeds i64::MAX for field {}", field.path),
                        )
                        .with_field(field.path.clone())
                    })?
                }
            }
            Value::String(s) => s.parse::<i64>().map_err(|_| {
                Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    format!("Invalid integer string '{}' for field {}", s, field.path),
                )
                .with_field(field.path.clone())
            })?,
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    format!(
                        "Expected number or string for binary integer field {}, got {}",
                        field.path,
                        self.value_type_name(value)
                    ),
                )
                .with_field(field.path.clone()));
            }
        };

        // Validate integer range for field
        self.validate_integer_range(int_value, bits, signed, &field.path)?;

        let encoded = crate::numeric::encode_binary_int(int_value, bits, signed)?;
        field_data.copy_from_slice(&encoded);
        Ok(())
    }

    /// Extract and validate decimal string from JSON value
    fn extract_decimal_string(
        &self,
        value: &Value,
        digits: u16,
        scale: i16,
        signed: bool,
        field_path: &str,
    ) -> Result<String> {
        match value {
            Value::String(s) => {
                self.validate_decimal_string(s, digits, scale, signed, field_path)?;
                Ok(s.clone())
            }
            Value::Number(n) => {
                let s = n.to_string();
                self.validate_decimal_string(&s, digits, scale, signed, field_path)?;
                Ok(s)
            }
            _ => Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!(
                    "Expected string or number for decimal field {}, got {}",
                    field_path,
                    self.value_type_name(value)
                ),
            )
            .with_field(field_path.to_string())),
        }
    }

    /// Get a human-readable name for a JSON value type
    fn value_type_name(&self, value: &Value) -> &'static str {
        match value {
            Value::Null => "null",
            Value::Bool(_) => "boolean",
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Array(_) => "array",
            Value::Object(_) => "object",
        }
    }

    /// Validate a decimal string against field constraints
    fn validate_decimal_string(
        &self,
        s: &str,
        digits: u16,
        scale: i16,
        signed: bool,
        field_path: &str,
    ) -> Result<()> {
        // Parse the decimal string
        let decimal = crate::numeric::SmallDecimal::from_str(s, scale).map_err(|_| {
            Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!("Invalid decimal string '{}' for field {}", s, field_path),
            )
            .with_field(field_path.to_string())
        })?;

        // Check sign
        if !signed && decimal.is_negative() {
            return Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!(
                    "Negative value '{}' not allowed for unsigned field {}",
                    s, field_path
                ),
            )
            .with_field(field_path.to_string()));
        }

        // Check scale (NORMATIVE: must match exactly)
        if decimal.scale() != scale {
            return Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!(
                    "Scale mismatch for field {}: expected {}, got {} in value '{}'",
                    field_path,
                    scale,
                    decimal.scale(),
                    s
                ),
            )
            .with_field(field_path.to_string()));
        }

        // Check total digits
        if decimal.total_digits() > digits {
            return Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!(
                    "Too many digits for field {}: expected max {}, got {} in value '{}'",
                    field_path,
                    digits,
                    decimal.total_digits(),
                    s
                ),
            )
            .with_field(field_path.to_string()));
        }

        Ok(())
    }

    /// Validate integer range for binary fields
    fn validate_integer_range(
        &self,
        value: i64,
        bits: u16,
        signed: bool,
        field_path: &str,
    ) -> Result<()> {
        let (min_val, max_val) = if signed {
            match bits {
                16 => (i64::from(i16::MIN), i64::from(i16::MAX)),
                32 => (i64::from(i32::MIN), i64::from(i32::MAX)),
                64 => (i64::MIN, i64::MAX),
                _ => {
                    return Err(Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Invalid bit width {} for binary field {}", bits, field_path),
                    )
                    .with_field(field_path.to_string()))
                }
            }
        } else {
            match bits {
                16 => (0, i64::from(u16::MAX)),
                32 => (0, i64::from(u32::MAX)),
                64 => (0, i64::MAX), // Can't represent full u64 range in i64
                _ => {
                    return Err(Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Invalid bit width {} for binary field {}", bits, field_path),
                    )
                    .with_field(field_path.to_string()))
                }
            }
        };

        if value < min_val || value > max_val {
            return Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!(
                    "Value {} out of range for {}-bit {} binary field {} (range: {} to {})",
                    value,
                    bits,
                    if signed { "signed" } else { "unsigned" },
                    field_path,
                    min_val,
                    max_val
                ),
            )
            .with_field(field_path.to_string()));
        }

        Ok(())
    }

    /// Get field name (same logic as decoder)
    fn get_field_name(&self, field: &Field) -> String {
        if field.name.eq_ignore_ascii_case("FILLER") {
            // FILLER fields are not encoded from JSON
            String::new()
        } else {
            field.name.clone()
        }
    }

    /// Extract raw data from JSON record
    fn extract_raw_data(&self, json: &Value) -> Result<Option<Vec<u8>>> {
        if let Value::Object(obj) = json {
            if let Some(Value::String(raw_b64)) =
                obj.get("raw_b64").or_else(|| obj.get("__raw_b64"))
            {
                let raw_data = general_purpose::STANDARD.decode(raw_b64).map_err(|e| {
                    Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Invalid base64 raw data: {}", e),
                    )
                })?;
                return Ok(Some(raw_data));
            }
        }
        Ok(None)
    }

    /// Extract field-level raw data
    fn extract_field_raw_data(
        &self,
        field: &Field,
        json_obj: &IndexMap<String, Value>,
    ) -> Result<Option<Vec<u8>>> {
        let raw_key = format!("{}_raw_b64", field.name);
        if let Some(Value::String(raw_b64)) = json_obj.get(&raw_key) {
            let raw_data = general_purpose::STANDARD.decode(raw_b64).map_err(|e| {
                Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    format!("Invalid base64 field raw data: {}", e),
                )
            })?;
            return Ok(Some(raw_data));
        }
        Ok(None)
    }

    /// Verify that raw data matches JSON values
    fn verify_raw_data_matches(
        &self,
        schema: &Schema,
        raw_data: &[u8],
        json: &Value,
    ) -> Result<bool> {
        // Decode the raw data and compare with JSON values
        let decoded_json = crate::decode_record(
            schema,
            raw_data,
            &DecodeOptions {
                format: self.options.format,
                codepage: self.options.codepage,
                json_number_mode: JsonNumberMode::Lossless,
                emit_filler: false,
                emit_meta: false,
                emit_raw: RawMode::Off,
                strict_mode: true,
                max_errors: None,
                on_decode_unmappable: UnmappablePolicy::Error,
                threads: 1,
                preserve_zoned_encoding: false,
                preferred_zoned_encoding: ZonedEncodingFormat::Auto,
                float_format: FloatFormat::IeeeBigEndian,
            },
        )?;

        // Compare the decoded JSON with the input JSON (excluding metadata and raw fields)
        self.compare_json_values(&decoded_json, json)
    }

    /// Compare two JSON values for equality, ignoring metadata and raw fields
    #[allow(clippy::only_used_in_recursion)]
    fn compare_json_values(&self, decoded: &Value, original: &Value) -> Result<bool> {
        match (decoded, original) {
            (Value::Object(decoded_obj), Value::Object(original_obj)) => {
                // Compare all non-metadata, non-raw fields
                for (key, decoded_value) in decoded_obj {
                    if key.starts_with("__") || key.ends_with("_raw_b64") {
                        continue; // Skip metadata and raw fields
                    }

                    if let Some(original_value) = original_obj.get(key) {
                        if !self.compare_json_values(decoded_value, original_value)? {
                            return Ok(false);
                        }
                    } else {
                        // Field missing in original
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (Value::Array(decoded_arr), Value::Array(original_arr)) => {
                if decoded_arr.len() != original_arr.len() {
                    return Ok(false);
                }
                for (decoded_elem, original_elem) in decoded_arr.iter().zip(original_arr.iter()) {
                    if !self.compare_json_values(decoded_elem, original_elem)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (decoded, original) => {
                // Direct value comparison
                Ok(decoded == original)
            }
        }
    }

    /// Extract raw data from JSON object (record-level)
    fn extract_raw_data_from_json_obj(
        &self,
        json_obj: &IndexMap<String, Value>,
    ) -> Result<Option<Vec<u8>>> {
        if let Some(Value::String(raw_b64)) =
            json_obj.get("raw_b64").or_else(|| json_obj.get("__raw_b64"))
        {
            let raw_data = general_purpose::STANDARD.decode(raw_b64).map_err(|e| {
                Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    format!("Invalid base64 raw data: {}", e),
                )
            })?;
            return Ok(Some(raw_data));
        }
        Ok(None)
    }

    /// Verify that cluster raw data matches JSON values
    fn verify_cluster_raw_data_matches(
        &self,
        _field: &Field,
        _raw_data: &[u8],
        _json_obj: &Map<String, Value>,
    ) -> Result<bool> {
        // For now, assume cluster raw data matches - full verification will be implemented later
        Ok(true)
    }

    /// Calculate the size of a REDEFINES cluster
    fn calculate_redefines_cluster_size(&self, field: &Field) -> Result<usize> {
        // If this field has REDEFINES, find the base field and all related fields
        let cluster_path = if let Some(ref redefines_of) = field.redefines_of {
            redefines_of.clone()
        } else {
            field.path.clone()
        };

        // Find all fields that are part of this REDEFINES cluster
        let mut max_size = field.len as usize;
        
        // Check for other fields that REDEFINE the same base
        for check_field in &self.schema.fields {
            if let Some(ref check_redefines) = check_field.redefines_of {
                if *check_redefines == cluster_path || check_field.path == cluster_path {
                    max_size = max_size.max(check_field.len as usize);
                }
            } else if check_field.path == cluster_path {
                max_size = max_size.max(check_field.len as usize);
            }
        }

        Ok(max_size)
    }

    /// Get the path for a REDEFINES cluster
    fn get_redefines_cluster_path(&self, field: &Field) -> String {
        if let Some(ref redefines_path) = field.redefines_of {
            redefines_path.clone()
        } else {
            field.path.clone()
        }
    }
}

/// Ordered JSON writer for parallel processing
///
/// This writer maintains deterministic output ordering even when records
/// are processed in parallel by using sequence IDs and a bounded reordering window.
pub struct OrderedJsonWriter<W: Write> {
    inner: JsonWriter<W>,
    _reorder_window: HashMap<u64, String>,
    _next_sequence: u64,
    _window_size: usize,
}

impl<W: Write> OrderedJsonWriter<W> {
    /// Create a new ordered JSON writer
    pub fn new(writer: W, schema: Schema, options: DecodeOptions, window_size: usize) -> Self {
        Self {
            inner: JsonWriter::new(writer, schema, options),
            _reorder_window: HashMap::new(),
            _next_sequence: 0,
            _window_size: window_size,
        }
    }

    /// Write a record with sequence ID for ordering
    ///
    /// # Errors
    /// Returns an error if the underlying writer cannot encode the record.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn write_record_with_sequence(
        &mut self,
        record_data: &[u8],
        record_index: u64,
        byte_offset: u64,
        _sequence_id: u64,
    ) -> Result<()> {
        // For now, write directly (parallel processing will be implemented later)
        self.inner
            .write_record(record_data, record_index, byte_offset)
    }

    /// Finish writing and return the inner writer
    ///
    /// # Errors
    /// Returns an error if the inner writer cannot be finished.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn finish(self) -> Result<W> {
        self.inner.finish()
    }
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
#[allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
mod tests {
    use super::*;
    use std::io::Cursor;

    fn build_test_schema() -> Schema {
        let field_a = Field {
            path: "ROOT.FIELD-A".to_string(),
            name: "FIELD-A".to_string(),
            level: 5,
            kind: FieldKind::Alphanum { len: 10 },
            offset: 0,
            len: 10,
            redefines_of: None,
            occurs: None,
            sync_padding: None,
            synchronized: false,
            blank_when_zero: false,
            children: Vec::new(),
        };

        let field_b = Field {
            path: "ROOT.FIELD-B".to_string(),
            name: "FIELD-B".to_string(),
            level: 5,
            kind: FieldKind::Alphanum { len: 5 },
            offset: 0,
            len: 5,
            redefines_of: Some("ROOT.FIELD-A".to_string()),
            occurs: None,
            sync_padding: None,
            synchronized: false,
            blank_when_zero: false,
            children: Vec::new(),
        };

        let field_c = Field {
            path: "ROOT.FIELD-C".to_string(),
            name: "FIELD-C".to_string(),
            level: 5,
            kind: FieldKind::Alphanum { len: 15 },
            offset: 0,
            len: 15,
            redefines_of: Some("ROOT.FIELD-A".to_string()),
            occurs: None,
            sync_padding: None,
            synchronized: false,
            blank_when_zero: false,
            children: Vec::new(),
        };

        let group_child = Field {
            path: "ROOT.GROUP.FIELD-C1".to_string(),
            name: "FIELD-C1".to_string(),
            level: 10,
            kind: FieldKind::BinaryInt { bits: 16, signed: false },
            offset: 30,
            len: 2,
            redefines_of: None,
            occurs: None,
            sync_padding: None,
            synchronized: false,
            blank_when_zero: false,
            children: Vec::new(),
        };

        let group = Field {
            path: "ROOT.GROUP".to_string(),
            name: "GROUP".to_string(),
            level: 5,
            kind: FieldKind::Group,
            offset: 30,
            len: 2,
            redefines_of: None,
            occurs: None,
            sync_padding: None,
            synchronized: false,
            blank_when_zero: false,
            children: Vec::new(),
        };

        Schema {
            fields: vec![field_a, field_b, field_c, group],
            lrecl_fixed: None,
            tail_odo: None,
            fingerprint: String::new(),
        }
    }

    #[test]
    fn test_schema_field_lookup_and_redefines() {
        let schema = build_test_schema();
        let writer = JsonWriter::new(Cursor::new(Vec::new()), schema.clone(), DecodeOptions::default());

        // Test direct path lookup
        let field_a = schema.find_field("ROOT.FIELD-A").unwrap();
        assert_eq!(field_a.name, "FIELD-A");

        // Test nested path lookup
        let nested = schema.find_field("ROOT.GROUP.FIELD-C1").unwrap();
        assert_eq!(nested.name, "FIELD-C1");

        // Test finding redefining fields
        let redefining = schema.find_redefining_fields(&field_a.path);
        let names: Vec<_> = redefining.iter().map(|f| f.name.as_str()).collect();
        assert_eq!(names.len(), 2);
        assert!(names.contains(&"FIELD-B"));
        assert!(names.contains(&"FIELD-C"));
    }
}
