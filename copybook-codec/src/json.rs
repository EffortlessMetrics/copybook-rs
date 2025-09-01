//! Deterministic JSON output for COBOL records
//!
//! This module implements streaming JSON writer that produces deterministic output
//! following the normative requirements for field ordering, numeric representation,
//! and REDEFINES handling.

use copybook_core::{Field, FieldKind, Occurs, Schema, Result, Error, ErrorCode};
use crate::options::{DecodeOptions, RawMode, JsonNumberMode};
use serde_json::{Map, Value};
use std::io::Write;
use std::collections::HashMap;


/// Streaming JSON writer for deterministic output
pub struct JsonWriter<W: Write> {
    writer: W,
    options: DecodeOptions,
    sequence_id: u64,
}

impl<W: Write> JsonWriter<W> {
    /// Create a new JSON writer
    pub fn new(writer: W, options: DecodeOptions) -> Self {
        Self {
            writer,
            options,
            sequence_id: 0,
        }
    }

    /// Write a single record as JSON line
    /// 
    /// # Errors
    /// 
    /// Returns an error if the record cannot be written or JSON serialization fails
    pub fn write_record(
        &mut self,
        schema: &Schema,
        record_data: &[u8],
        record_index: u64,
        byte_offset: u64,
    ) -> Result<()> {
        // Create JSON object in schema order (pre-order traversal)
        let mut json_obj = Map::new();
        
        // Process fields in schema order
        self.process_fields_recursive(
            &schema.fields,
            record_data,
            &mut json_obj,
            record_index,
            byte_offset,
        )?;
        
        // Add metadata if requested
        if self.options.emit_meta {
            self.add_metadata(&mut json_obj, schema, record_index, byte_offset, record_data.len())?;
        }
        
        // Add raw data if requested
        if matches!(self.options.emit_raw, RawMode::Record | RawMode::RecordRDW) {
            self.add_raw_data(&mut json_obj, record_data)?;
        }
        
        // Write JSON line
        let json_value = Value::Object(json_obj);
        serde_json::to_writer(&mut self.writer, &json_value)
            .map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, format!("JSON write error: {}", e)))?;
        
        writeln!(self.writer)
            .map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, format!("Write error: {}", e)))?;
        
        self.sequence_id += 1;
        Ok(())
    }

    /// Process fields recursively in schema order
    fn process_fields_recursive(
        &mut self,
        fields: &[Field],
        record_data: &[u8],
        json_obj: &mut Map<String, Value>,
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
        json_obj: &mut Map<String, Value>,
        record_index: u64,
        byte_offset: u64,
    ) -> Result<()> {
        // Handle REDEFINES: emit all views in declaration order
        if field.redefines_of.is_some() {
            // This is a redefining field - process it normally
            // The primary field was already processed
        }

        match &field.kind {
            FieldKind::Group => {
                // For groups, create nested object and process children
                let mut group_obj = Map::new();
                self.process_fields_recursive(
                    &field.children,
                    record_data,
                    &mut group_obj,
                    record_index,
                    byte_offset,
                )?;
                
                // Handle OCCURS for groups
                if let Some(occurs) = &field.occurs {
                    let array_value = self.process_group_array(field, record_data, occurs, record_index, byte_offset)?;
                    json_obj.insert(self.get_field_name(field), array_value);
                } else {
                    json_obj.insert(self.get_field_name(field), Value::Object(group_obj));
                }
            }
            _ => {
                // Scalar field
                if let Some(occurs) = &field.occurs {
                    let array_value = self.process_scalar_array(field, record_data, occurs, record_index, byte_offset)?;
                    json_obj.insert(self.get_field_name(field), array_value);
                } else {
                    let field_value = self.decode_scalar_field(field, record_data, record_index, byte_offset)?;
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
        
        for i in 0..count {
            let _element_offset = field.offset + (i * field.len);
            let mut element_obj = Map::new();
            
            // Process children for this array element
            self.process_fields_recursive(
                &field.children,
                record_data,
                &mut element_obj,
                record_index,
                byte_offset,
            )?;
            
            array.push(Value::Object(element_obj));
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
        
        let element_size = field.len / match occurs {
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
            
            let element_value = self.decode_scalar_field(&element_field, record_data, record_index, byte_offset)?;
            array.push(element_value);
        }
        
        Ok(Value::Array(array))
    }

    /// Get actual array count for OCCURS
    fn get_actual_array_count(&self, occurs: &Occurs, record_data: &[u8]) -> Result<u32> {
        match occurs {
            Occurs::Fixed { count } => Ok(*count),
            Occurs::ODO { min, max, counter_path } => {
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
                let text = crate::charset::ebcdic_to_utf8(field_data, self.options.codepage, self.options.on_decode_unmappable)?;
                // Preserve all spaces (no trimming) - NORMATIVE
                Ok(Value::String(text))
            }
            FieldKind::ZonedDecimal { digits, scale, signed } => {
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
                    JsonNumberMode::Lossless => Ok(Value::String(decimal_str)),
                    JsonNumberMode::Native => {
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
            FieldKind::PackedDecimal { digits, scale, signed } => {
                let decimal = crate::numeric::decode_packed_decimal(field_data, *digits, *scale, *signed)?;
                
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
        // Handle FILLER fields
        if field.name.eq_ignore_ascii_case("FILLER") {
            if self.options.emit_filler {
                // _filler_<offset> - NORMATIVE
                format!("_filler_{:08}", field.offset)
            } else {
                // Skip FILLER fields by default
                return String::new();
            }
        } else {
            // Use field name as-is (duplicate disambiguation handled during parsing)
            field.name.clone()
        }
    }

    /// Add metadata to JSON object
    fn add_metadata(
        &self,
        json_obj: &mut Map<String, Value>,
        schema: &Schema,
        record_index: u64,
        byte_offset: u64,
        record_length: usize,
    ) -> Result<()> {
        json_obj.insert("__schema_id".to_string(), Value::String(schema.fingerprint.clone()));
        json_obj.insert("__record_index".to_string(), Value::Number(record_index.into()));
        json_obj.insert("__offset".to_string(), Value::Number(byte_offset.into()));
        json_obj.insert("__length".to_string(), Value::Number(record_length.into()));
        Ok(())
    }

    /// Add raw record data
    fn add_raw_data(&self, json_obj: &mut Map<String, Value>, record_data: &[u8]) -> Result<()> {
        use base64::{Engine as _, engine::general_purpose};
        let encoded = general_purpose::STANDARD.encode(record_data);
        json_obj.insert("__raw_b64".to_string(), Value::String(encoded));
        Ok(())
    }

    /// Add field-level raw data
    fn add_field_raw_data(
        &self,
        json_obj: &mut Map<String, Value>,
        field: &Field,
        record_data: &[u8],
    ) -> Result<()> {
        use base64::{Engine as _, engine::general_purpose};
        
        let end_offset = field.offset + field.len;
        if end_offset as usize <= record_data.len() {
            let field_data = &record_data[field.offset as usize..end_offset as usize];
            let encoded = general_purpose::STANDARD.encode(field_data);
            let raw_key = format!("{}_raw_b64", field.name);
            json_obj.insert(raw_key, Value::String(encoded));
        }
        
        Ok(())
    }

    /// Finish writing and flush
    pub fn finish(mut self) -> Result<W> {
        self.writer.flush()
            .map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, format!("Flush error: {}", e)))?;
        Ok(self.writer)
    }
}

/// JSON encoder for converting JSON back to binary records
pub struct JsonEncoder {
    options: crate::options::EncodeOptions,
}

impl JsonEncoder {
    /// Create a new JSON encoder
    pub fn new(options: crate::options::EncodeOptions) -> Self {
        Self { options }
    }

    /// Encode a JSON value to binary record
    /// 
    /// # Errors
    /// 
    /// Returns an error if the JSON cannot be encoded according to the schema
    pub fn encode_record(&self, schema: &Schema, json: &Value) -> Result<Vec<u8>> {
        // Calculate maximum record size
        let max_record_size = self.calculate_max_record_size(schema)?;
        let mut record_data = vec![0u8; max_record_size];

        // Process fields in schema order
        if let Value::Object(obj) = json {
            self.encode_fields_recursive(&schema.fields, obj, &mut record_data)?;
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
        _schema: &Schema,
        _json: &Value,
        _tail_odo: &copybook_core::TailODO,
    ) -> Result<usize> {
        // For now, return maximum size - ODO support will be implemented later
        self.calculate_max_record_size(_schema)
    }

    /// Encode fields recursively
    fn encode_fields_recursive(
        &self,
        fields: &[Field],
        json_obj: &Map<String, Value>,
        record_data: &mut [u8],
    ) -> Result<()> {
        for field in fields {
            self.encode_single_field(field, json_obj, record_data)?;
        }
        Ok(())
    }

    /// Encode a single field
    fn encode_single_field(
        &self,
        field: &Field,
        json_obj: &Map<String, Value>,
        record_data: &mut [u8],
    ) -> Result<()> {
        let field_name = self.get_field_name(field);
        
        // Skip FILLER fields if not emitted
        if field_name.is_empty() {
            return Ok(());
        }

        // Handle REDEFINES: implement precedence rules (NORMATIVE)
        if field.redefines_of.is_some() {
            return self.encode_redefines_field(field, json_obj, record_data);
        }

        match &field.kind {
            FieldKind::Group => {
                if let Some(occurs) = &field.occurs {
                    self.encode_group_array(field, json_obj, record_data, occurs)?;
                } else {
                    // Single group - process children
                    if let Some(Value::Object(group_obj)) = json_obj.get(&field_name) {
                        self.encode_fields_recursive(&field.children, group_obj, record_data)?;
                    }
                }
            }
            _ => {
                // Scalar field
                if let Some(occurs) = &field.occurs {
                    self.encode_scalar_array(field, json_obj, record_data, occurs)?;
                } else {
                    if let Some(field_value) = json_obj.get(&field_name) {
                        self.encode_scalar_field(field, field_value, record_data)?;
                    }
                }
            }
        }

        Ok(())
    }

    /// Encode REDEFINES field with precedence rules (NORMATIVE)
    fn encode_redefines_field(
        &self,
        field: &Field,
        json_obj: &Map<String, Value>,
        record_data: &mut [u8],
    ) -> Result<()> {
        // REDEFINES encode precedence (NORMATIVE):
        // 1. Raw bytes (if --use-raw and values match)
        // 2. Single non-null view
        // 3. Error (ambiguous)

        if self.options.use_raw {
            if let Some(raw_data) = self.extract_field_raw_data(field, json_obj)? {
                let end_offset = (field.offset + field.len) as usize;
                if end_offset <= record_data.len() {
                    record_data[field.offset as usize..end_offset].copy_from_slice(&raw_data);
                    return Ok(());
                }
            }
        }

        // Find all views in the REDEFINES cluster
        let cluster_views = self.find_redefines_cluster_views(field, json_obj);
        let non_null_views: Vec<_> = cluster_views.iter()
            .filter(|(_, value)| !value.is_null())
            .collect();

        match non_null_views.len() {
            0 => {
                // All views are null - fill with zeros
                let end_offset = (field.offset + field.len) as usize;
                if end_offset <= record_data.len() {
                    record_data[field.offset as usize..end_offset].fill(0);
                }
                Ok(())
            }
            1 => {
                // Single non-null view - encode from that view
                let (view_field, view_value) = non_null_views[0];
                self.encode_single_field(view_field, 
                    &[(view_field.name.clone(), view_value.clone())].into_iter().collect::<Map<String, Value>>(), 
                    record_data)
            }
            _ => {
                // Multiple non-null views - ambiguous (NORMATIVE)
                Err(Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    format!("Ambiguous REDEFINES encoding: multiple non-null views for cluster at offset {}", field.offset),
                ))
            }
        }
    }

    /// Find all views in a REDEFINES cluster
    fn find_redefines_cluster_views<'a>(
        &self,
        _field: &'a Field,
        _json_obj: &'a Map<String, Value>,
    ) -> Vec<(&'a Field, &'a Value)> {
        // For now, return empty - full REDEFINES support will be implemented later
        Vec::new()
    }

    /// Encode group array
    fn encode_group_array(
        &self,
        field: &Field,
        json_obj: &Map<String, Value>,
        record_data: &mut [u8],
        occurs: &Occurs,
    ) -> Result<()> {
        let field_name = self.get_field_name(field);
        
        if let Some(Value::Array(array)) = json_obj.get(&field_name) {
            // Validate array length
            self.validate_array_length(array.len(), occurs)?;
            
            // Update ODO counter if needed
            if let Occurs::ODO { counter_path, .. } = occurs {
                self.update_odo_counter(counter_path, array.len() as u32, json_obj, record_data)?;
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
                    
                    self.encode_fields_recursive(&element_field.children, element_obj, record_data)?;
                }
            }
        }
        
        Ok(())
    }

    /// Encode scalar array
    fn encode_scalar_array(
        &self,
        field: &Field,
        json_obj: &Map<String, Value>,
        record_data: &mut [u8],
        occurs: &Occurs,
    ) -> Result<()> {
        let field_name = self.get_field_name(field);
        
        if let Some(Value::Array(array)) = json_obj.get(&field_name) {
            // Validate array length
            self.validate_array_length(array.len(), occurs)?;
            
            // Update ODO counter if needed
            if let Occurs::ODO { counter_path, .. } = occurs {
                self.update_odo_counter(counter_path, array.len() as u32, json_obj, record_data)?;
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
                        format!("Array length {} doesn't match fixed OCCURS count {}", actual_len, count),
                    ));
                }
            }
            Occurs::ODO { min, max, .. } => {
                if actual_len < *min as usize || actual_len > *max as usize {
                    return Err(Error::new(
                        ErrorCode::CBKE521_ARRAY_LEN_OOB,
                        format!("Array length {} is outside ODO range {}-{}", actual_len, min, max),
                    ));
                }
            }
        }
        Ok(())
    }

    /// Update ODO counter field
    fn update_odo_counter(
        &self,
        _counter_path: &str,
        _count: u32,
        _json_obj: &Map<String, Value>,
        _record_data: &mut [u8],
    ) -> Result<()> {
        // ODO counter update will be implemented when ODO support is added
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
            ));
        }

        let field_data = &mut record_data[field.offset as usize..end_offset];

        match &field.kind {
            FieldKind::Alphanum { len: _ } => {
                if let Value::String(text) = value {
                    let encoded = crate::numeric::encode_alphanumeric(text, field.len as usize, self.options.codepage)?;
                    field_data.copy_from_slice(&encoded);
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Expected string for alphanumeric field {}", field.path),
                    ));
                }
            }
            FieldKind::ZonedDecimal { digits, scale, signed } => {
                let value_str = match value {
                    Value::String(s) => s.clone(),
                    Value::Number(n) => n.to_string(),
                    _ => {
                        return Err(Error::new(
                            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                            format!("Expected string or number for zoned decimal field {}", field.path),
                        ));
                    }
                };

                let encoded = if field.blank_when_zero && self.options.bwz_encode {
                    crate::numeric::encode_zoned_decimal_with_bwz(
                        &value_str, *digits, *scale, *signed, self.options.codepage, self.options.bwz_encode,
                    )?
                } else {
                    crate::numeric::encode_zoned_decimal(
                        &value_str, *digits, *scale, *signed, self.options.codepage,
                    )?
                };
                field_data.copy_from_slice(&encoded);
            }
            FieldKind::PackedDecimal { digits, scale, signed } => {
                let value_str = match value {
                    Value::String(s) => s.clone(),
                    Value::Number(n) => n.to_string(),
                    _ => {
                        return Err(Error::new(
                            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                            format!("Expected string or number for packed decimal field {}", field.path),
                        ));
                    }
                };

                let encoded = crate::numeric::encode_packed_decimal(
                    &value_str, *digits, *scale, *signed,
                )?;
                field_data.copy_from_slice(&encoded);
            }
            FieldKind::BinaryInt { bits, signed } => {
                let int_value = match value {
                    Value::Number(n) => {
                        if *signed {
                            n.as_i64().ok_or_else(|| Error::new(
                                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                                format!("Invalid signed integer for field {}", field.path),
                            ))?
                        } else {
                            n.as_u64().ok_or_else(|| Error::new(
                                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                                format!("Invalid unsigned integer for field {}", field.path),
                            ))? as i64
                        }
                    }
                    Value::String(s) => s.parse::<i64>().map_err(|_| Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Invalid integer string for field {}", field.path),
                    ))?,
                    _ => {
                        return Err(Error::new(
                            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                            format!("Expected number or string for binary integer field {}", field.path),
                        ));
                    }
                };

                let encoded = crate::numeric::encode_binary_int(int_value, *bits, *signed)?;
                field_data.copy_from_slice(&encoded);
            }
            FieldKind::Group => {
                return Err(Error::new(
                    ErrorCode::CBKD101_INVALID_FIELD_TYPE,
                    format!("Group field {} processed as scalar", field.path),
                ));
            }
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
            if let Some(Value::String(raw_b64)) = obj.get("__raw_b64") {
                use base64::{Engine as _, engine::general_purpose};
                let raw_data = general_purpose::STANDARD.decode(raw_b64)
                    .map_err(|e| Error::new(ErrorCode::CBKE501_JSON_TYPE_MISMATCH, format!("Invalid base64 raw data: {}", e)))?;
                return Ok(Some(raw_data));
            }
        }
        Ok(None)
    }

    /// Extract field-level raw data
    fn extract_field_raw_data(&self, field: &Field, json_obj: &Map<String, Value>) -> Result<Option<Vec<u8>>> {
        let raw_key = format!("{}_raw_b64", field.name);
        if let Some(Value::String(ref raw_b64)) = json_obj.get(&raw_key) {
            use base64::{Engine as _, engine::general_purpose};
            let raw_data = general_purpose::STANDARD.decode(raw_b64)
                .map_err(|e| Error::new(ErrorCode::CBKE501_JSON_TYPE_MISMATCH, format!("Invalid base64 field raw data: {}", e)))?;
            return Ok(Some(raw_data));
        }
        Ok(None)
    }

    /// Verify that raw data matches JSON values
    fn verify_raw_data_matches(&self, _schema: &Schema, _raw_data: &[u8], _json: &Value) -> Result<bool> {
        // For now, assume raw data matches - full verification will be implemented later
        Ok(true)
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
    pub fn new(writer: W, options: DecodeOptions, window_size: usize) -> Self {
        Self {
            inner: JsonWriter::new(writer, options),
            _reorder_window: HashMap::new(),
            _next_sequence: 0,
            _window_size: window_size,
        }
    }

    /// Write a record with sequence ID for ordering
    pub fn write_record_with_sequence(
        &mut self,
        schema: &Schema,
        record_data: &[u8],
        record_index: u64,
        byte_offset: u64,
        _sequence_id: u64,
    ) -> Result<()> {
        // For now, write directly (parallel processing will be implemented later)
        self.inner.write_record(schema, record_data, record_index, byte_offset)
    }

    /// Finish writing and return the inner writer
    pub fn finish(self) -> Result<W> {
        self.inner.finish()
    }
}