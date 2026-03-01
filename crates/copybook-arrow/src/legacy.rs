// SPDX-License-Identifier: AGPL-3.0-or-later
//! Legacy JSON-based Arrow/Parquet conversion (deprecated)
//!
//! This module preserves the original naive JSON-to-Arrow conversion API.
//! All items are deprecated in favor of the typed conversion in the parent crate.

use arrow::array::{ArrayRef, RecordBatch, StringArray};
use arrow::datatypes::{DataType, Field, Fields, Schema, SchemaRef};
use parquet::arrow::ArrowWriter as ParquetArrowWriter;
use parquet::file::properties::WriterProperties;
use serde_json::Value;
use std::fs::File;
use std::io::BufWriter;
use std::path::Path;
use std::sync::Arc;

use crate::{ArrowError, Result};

/// Convert a JSON value to an Arrow `DataType`
#[deprecated(
    since = "0.5.0",
    note = "Use cobol_schema_to_arrow for typed conversion"
)]
fn json_type_to_arrow(json_value: &Value) -> DataType {
    match json_value {
        Value::Null => DataType::Null,
        Value::Bool(_) => DataType::Boolean,
        Value::Number(n) => {
            if n.is_i64() {
                DataType::Int64
            } else {
                DataType::Float64
            }
        }
        Value::String(_) => DataType::Utf8,
        Value::Array(_) => DataType::List(Arc::new(Field::new("item", DataType::Utf8, true))),
        Value::Object(_) => DataType::Struct(Fields::empty()),
    }
}

/// Convert a JSON object to an Arrow `Schema`
///
/// # Errors
/// Returns `ArrowError::JsonConversion` if the input is not a JSON object.
#[deprecated(
    since = "0.5.0",
    note = "Use cobol_schema_to_arrow for typed conversion"
)]
#[inline]
pub fn json_to_schema(json_value: &Value) -> Result<Schema> {
    if let Value::Object(map) = json_value {
        let fields: Vec<Field> = map
            .iter()
            .map(|(key, value)| {
                let data_type = json_type_to_arrow(value);
                Field::new(key, data_type, true)
            })
            .collect();

        Ok(Schema::new(fields))
    } else {
        Err(ArrowError::JsonConversion(
            "Expected JSON object for schema".to_string(),
        ))
    }
}

/// Convert a JSON value to an Arrow Array
#[deprecated(since = "0.5.0", note = "Use typed column accumulators instead")]
fn json_value_to_array(_key: &str, json_value: &Value, data_type: &DataType) -> Result<ArrayRef> {
    let array: ArrayRef = match json_value {
        Value::Null => arrow::array::new_null_array(data_type, 1),
        Value::Bool(b) => Arc::new(arrow::array::BooleanArray::from(vec![Some(*b)])),
        Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Arc::new(arrow::array::Int64Array::from(vec![Some(i)]))
            } else if let Some(f) = n.as_f64() {
                Arc::new(arrow::array::Float64Array::from(vec![Some(f)]))
            } else {
                arrow::array::new_null_array(data_type, 1)
            }
        }
        Value::String(s) => Arc::new(StringArray::from(vec![Some(s.as_str())])),
        Value::Array(arr) => {
            let json_str = serde_json::to_string(arr).map_err(|e| {
                ArrowError::JsonConversion(format!("Failed to serialize array: {e}"))
            })?;
            Arc::new(StringArray::from(vec![Some(json_str.as_str())]))
        }
        Value::Object(_) => {
            let json_str = serde_json::to_string(json_value).map_err(|e| {
                ArrowError::JsonConversion(format!("Failed to serialize object: {e}"))
            })?;
            Arc::new(StringArray::from(vec![Some(json_str.as_str())]))
        }
    };

    Ok(array)
}

/// Convert a JSON object to an Arrow `RecordBatch`
///
/// # Errors
/// Returns `ArrowError::JsonConversion` if the input is not a JSON object,
/// or `ArrowError::Arrow` if batch creation fails.
#[deprecated(since = "0.5.0", note = "Use RecordBatchBuilder for typed conversion")]
#[inline]
pub fn json_to_record_batch(schema: &Schema, json_value: &Value) -> Result<RecordBatch> {
    if let Value::Object(map) = json_value {
        let mut columns: Vec<ArrayRef> = Vec::new();

        for field in schema.fields() {
            let value = map.get(field.name()).unwrap_or(&Value::Null);
            let array = json_value_to_array(field.name(), value, field.data_type())?;
            columns.push(array);
        }

        RecordBatch::try_new(Arc::new(schema.clone()), columns).map_err(ArrowError::Arrow)
    } else {
        Err(ArrowError::JsonConversion(
            "Expected JSON object for record batch".to_string(),
        ))
    }
}

/// Legacy Arrow writer for converting copybook data to Arrow format
#[deprecated(since = "0.5.0", note = "Use RecordBatchBuilder for typed conversion")]
pub struct LegacyArrowWriter {
    schema: SchemaRef,
    batches: Vec<RecordBatch>,
}

#[allow(deprecated)]
impl LegacyArrowWriter {
    /// Create a new `LegacyArrowWriter` with the given schema
    #[inline]
    #[must_use]
    pub fn new(schema: Schema) -> Self {
        Self {
            schema: Arc::new(schema),
            batches: Vec::new(),
        }
    }

    /// Create a new `LegacyArrowWriter` from a JSON schema
    ///
    /// # Errors
    /// Returns an error if the JSON schema is invalid.
    #[inline]
    pub fn from_json_schema(json_schema: &Value) -> Result<Self> {
        let schema = json_to_schema(json_schema)?;
        Ok(Self::new(schema))
    }

    /// Add a record batch to the writer
    #[inline]
    pub fn add_batch(&mut self, batch: RecordBatch) {
        self.batches.push(batch);
    }

    /// Add a JSON record as a record batch
    ///
    /// # Errors
    /// Returns an error if the JSON record does not match the schema.
    #[inline]
    pub fn add_json_record(&mut self, json_record: &Value) -> Result<()> {
        let batch = json_to_record_batch(&self.schema, json_record)?;
        self.add_batch(batch);
        Ok(())
    }

    /// Get the number of batches written
    #[inline]
    #[must_use]
    pub fn batch_count(&self) -> usize {
        self.batches.len()
    }

    /// Get the schema
    #[inline]
    #[must_use]
    pub fn schema(&self) -> &SchemaRef {
        &self.schema
    }

    /// Get all batches
    #[inline]
    #[must_use]
    pub fn batches(&self) -> &[RecordBatch] {
        &self.batches
    }
}

/// Legacy Parquet writer for converting copybook data to Parquet format
#[deprecated(since = "0.5.0", note = "Use write_parquet for typed conversion")]
pub struct LegacyParquetFileWriter {
    schema: SchemaRef,
    writer_properties: WriterProperties,
}

#[allow(deprecated)]
impl LegacyParquetFileWriter {
    /// Create a new `LegacyParquetFileWriter` with the given schema
    #[inline]
    #[must_use]
    pub fn new(schema: Schema) -> Self {
        Self {
            schema: Arc::new(schema),
            writer_properties: WriterProperties::builder().build(),
        }
    }

    /// Create a new `LegacyParquetFileWriter` from a JSON schema
    ///
    /// # Errors
    /// Returns an error if the JSON schema is invalid.
    #[inline]
    pub fn from_json_schema(json_schema: &Value) -> Result<Self> {
        let schema = json_to_schema(json_schema)?;
        Ok(Self::new(schema))
    }

    /// Set custom writer properties
    #[inline]
    #[must_use]
    pub fn with_writer_properties(mut self, properties: WriterProperties) -> Self {
        self.writer_properties = properties;
        self
    }

    /// Write record batches to a Parquet file
    ///
    /// # Errors
    /// Returns an error if the file cannot be created or writing fails.
    #[inline]
    pub fn write_to_file<P: AsRef<Path>>(&self, path: P, batches: &[RecordBatch]) -> Result<()> {
        let file = File::create(path)?;
        let buf_writer = BufWriter::new(file);

        let mut writer = ParquetArrowWriter::try_new(
            buf_writer,
            self.schema.clone(),
            Some(self.writer_properties.clone()),
        )
        .map_err(|e| ArrowError::ParquetWrite(format!("Failed to create writer: {e}")))?;

        for batch in batches {
            writer
                .write(batch)
                .map_err(|e| ArrowError::ParquetWrite(format!("Failed to write batch: {e}")))?;
        }

        writer
            .close()
            .map_err(|e| ArrowError::ParquetWrite(format!("Failed to close writer: {e}")))?;

        Ok(())
    }

    /// Write JSON records to a Parquet file
    ///
    /// # Errors
    /// Returns an error if batch creation or file writing fails.
    #[inline]
    pub fn write_json_records<P: AsRef<Path>>(
        &self,
        path: P,
        json_records: &[Value],
    ) -> Result<()> {
        let mut batches = Vec::new();

        for record in json_records {
            let batch = json_to_record_batch(&self.schema, record)?;
            batches.push(batch);
        }

        self.write_to_file(path, &batches)
    }

    /// Get the schema
    #[inline]
    #[must_use]
    pub fn schema(&self) -> &SchemaRef {
        &self.schema
    }
}
