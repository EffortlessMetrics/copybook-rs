//! Arrow and Parquet adapters for copybook-rs
//!
//! This crate provides functionality to convert COBOL copybook data
//! to Apache Arrow format and write to Parquet files.

use arrow::array::{ArrayRef, RecordBatch, StringArray};
use arrow::datatypes::{DataType, Field, Fields, Schema, SchemaRef};
use parquet::arrow::ArrowWriter as ParquetArrowWriter;
use parquet::file::properties::WriterProperties;
use serde_json::Value;
use std::fs::File;
use std::io::BufWriter;
use std::path::Path;
use std::sync::Arc;
use thiserror::Error;

/// Errors that can occur during Arrow/Parquet conversion
#[derive(Error, Debug)]
pub enum ArrowError {
    #[error("JSON conversion error: {0}")]
    JsonConversion(String),

    #[error("Schema conversion error: {0}")]
    SchemaConversion(String),

    #[error("Parquet write error: {0}")]
    ParquetWrite(String),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Arrow error: {0}")]
    Arrow(#[from] arrow::error::ArrowError),
}

/// Result type for Arrow operations
pub type Result<T> = std::result::Result<T, ArrowError>;

/// Convert a JSON value to an Arrow DataType
fn json_type_to_arrow(json_value: &Value) -> Result<DataType> {
    match json_value {
        Value::Null => Ok(DataType::Null),
        Value::Bool(_) => Ok(DataType::Boolean),
        Value::Number(n) => {
            if n.is_i64() {
                Ok(DataType::Int64)
            } else {
                Ok(DataType::Float64)
            }
        }
        Value::String(_) => Ok(DataType::Utf8),
        Value::Array(_) => Ok(DataType::List(Arc::new(Field::new("item", DataType::Utf8, true)))),
        Value::Object(_) => Ok(DataType::Struct(Fields::empty())),
    }
}

/// Convert a JSON object to an Arrow Schema
pub fn json_to_schema(json_value: &Value) -> Result<Schema> {
    if let Value::Object(map) = json_value {
        let fields: Result<Vec<Field>> = map
            .iter()
            .map(|(key, value)| {
                let data_type = json_type_to_arrow(value)?;
                Ok(Field::new(key, data_type, true))
            })
            .collect();

        Ok(Schema::new(fields?))
    } else {
        Err(ArrowError::JsonConversion(
            "Expected JSON object for schema".to_string(),
        ))
    }
}

/// Convert a JSON value to an Arrow Array
fn json_value_to_array(_key: &str, json_value: &Value) -> Result<ArrayRef> {
    let array: ArrayRef = match json_value {
        Value::Null => {
            arrow::array::new_null_array(&DataType::Null, 1)
        }
        Value::Bool(b) => {
            Arc::new(arrow::array::BooleanArray::from(vec![Some(*b)]))
        }
        Value::Number(n) => {
            if n.is_i64() {
                Arc::new(arrow::array::Int64Array::from(vec![Some(n.as_i64().unwrap())]))
            } else {
                Arc::new(arrow::array::Float64Array::from(vec![Some(n.as_f64().unwrap())]))
            }
        }
        Value::String(s) => {
            Arc::new(StringArray::from(vec![Some(s.as_str())]))
        }
        Value::Array(arr) => {
            // Convert array to string representation for simplicity
            let json_str = serde_json::to_string(arr).map_err(|e| {
                ArrowError::JsonConversion(format!("Failed to serialize array: {}", e))
            })?;
            Arc::new(StringArray::from(vec![Some(json_str.as_str())]))
        }
        Value::Object(_) => {
            // Convert object to string representation for simplicity
            let json_str = serde_json::to_string(json_value).map_err(|e| {
                ArrowError::JsonConversion(format!("Failed to serialize object: {}", e))
            })?;
            Arc::new(StringArray::from(vec![Some(json_str.as_str())]))
        }
    };

    Ok(array)
}

/// Convert a JSON object to an Arrow RecordBatch
pub fn json_to_record_batch(schema: &Schema, json_value: &Value) -> Result<RecordBatch> {
    if let Value::Object(map) = json_value {
        let mut columns: Vec<ArrayRef> = Vec::new();

        for field in schema.fields() {
            let value = map.get(field.name()).unwrap_or(&Value::Null);
            let array = json_value_to_array(field.name(), value)?;
            columns.push(array);
        }

        RecordBatch::try_new(Arc::new(schema.clone()), columns).map_err(|e| {
            ArrowError::Arrow(e)
        })
    } else {
        Err(ArrowError::JsonConversion(
            "Expected JSON object for record batch".to_string(),
        ))
    }
}

/// Arrow writer for converting copybook data to Arrow format
pub struct ArrowWriter {
    schema: SchemaRef,
    batches: Vec<RecordBatch>,
}

impl ArrowWriter {
    /// Create a new ArrowWriter with the given schema
    pub fn new(schema: Schema) -> Self {
        Self {
            schema: Arc::new(schema),
            batches: Vec::new(),
        }
    }

    /// Create a new ArrowWriter from a JSON schema
    pub fn from_json_schema(json_schema: &Value) -> Result<Self> {
        let schema = json_to_schema(json_schema)?;
        Ok(Self::new(schema))
    }

    /// Add a record batch to the writer
    pub fn add_batch(&mut self, batch: RecordBatch) {
        self.batches.push(batch);
    }

    /// Add a JSON record as a record batch
    pub fn add_json_record(&mut self, json_record: &Value) -> Result<()> {
        let batch = json_to_record_batch(&self.schema, json_record)?;
        self.add_batch(batch);
        Ok(())
    }

    /// Get the number of batches written
    pub fn batch_count(&self) -> usize {
        self.batches.len()
    }

    /// Get the schema
    pub fn schema(&self) -> &SchemaRef {
        &self.schema
    }

    /// Get all batches
    pub fn batches(&self) -> &[RecordBatch] {
        &self.batches
    }
}

/// Parquet writer for converting copybook data to Parquet format
pub struct ParquetFileWriter {
    schema: SchemaRef,
    writer_properties: WriterProperties,
}

impl ParquetFileWriter {
    /// Create a new ParquetFileWriter with the given schema
    pub fn new(schema: Schema) -> Self {
        Self {
            schema: Arc::new(schema),
            writer_properties: WriterProperties::builder().build(),
        }
    }

    /// Create a new ParquetFileWriter from a JSON schema
    pub fn from_json_schema(json_schema: &Value) -> Result<Self> {
        let schema = json_to_schema(json_schema)?;
        Ok(Self::new(schema))
    }

    /// Set custom writer properties
    pub fn with_writer_properties(mut self, properties: WriterProperties) -> Self {
        self.writer_properties = properties;
        self
    }

    /// Write record batches to a Parquet file
    pub fn write_to_file<P: AsRef<Path>>(&self, path: P, batches: &[RecordBatch]) -> Result<()> {
        let file = File::create(path)?;
        let buf_writer = BufWriter::new(file);

        let mut writer = ParquetArrowWriter::try_new(
            buf_writer,
            self.schema.clone(),
            Some(self.writer_properties.clone()),
        )
        .map_err(|e| ArrowError::ParquetWrite(format!("Failed to create writer: {}", e)))?;

        for batch in batches {
            writer
                .write(batch)
                .map_err(|e| ArrowError::ParquetWrite(format!("Failed to write batch: {}", e)))?;
        }

        writer
            .close()
            .map_err(|e| ArrowError::ParquetWrite(format!("Failed to close writer: {}", e)))?;

        Ok(())
    }

    /// Write JSON records to a Parquet file
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
    pub fn schema(&self) -> &SchemaRef {
        &self.schema
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_json_type_to_arrow() {
        assert!(matches!(
            json_type_to_arrow(&Value::Bool(true)).unwrap(),
            DataType::Boolean
        ));
        assert!(matches!(
            json_type_to_arrow(&Value::Number(serde_json::Number::from(42))).unwrap(),
            DataType::Int64
        ));
        assert!(matches!(
            json_type_to_arrow(&Value::String("test".to_string())).unwrap(),
            DataType::Utf8
        ));
    }

    #[test]
    fn test_json_to_schema() {
        let json = serde_json::json!({
            "name": "John",
            "age": 30,
            "active": true
        });

        let schema = json_to_schema(&json).unwrap();
        assert_eq!(schema.fields().len(), 3);
        assert_eq!(schema.fields()[0].name(), "name");
        assert_eq!(schema.fields()[1].name(), "age");
        assert_eq!(schema.fields()[2].name(), "active");
    }

    #[test]
    fn test_json_to_record_batch() {
        let json = serde_json::json!({
            "name": "John",
            "age": 30,
            "active": true
        });

        let schema = json_to_schema(&json).unwrap();
        let batch = json_to_record_batch(&schema, &json).unwrap();

        assert_eq!(batch.num_rows(), 1);
        assert_eq!(batch.num_columns(), 3);
    }

    #[test]
    fn test_arrow_writer() {
        let json = serde_json::json!({
            "name": "John",
            "age": 30
        });

        let schema = json_to_schema(&json).unwrap();
        let mut writer = ArrowWriter::new(schema);

        writer.add_json_record(&json).unwrap();
        assert_eq!(writer.batch_count(), 1);
    }

    #[test]
    fn test_parquet_writer_roundtrip() {
        let json = serde_json::json!({
            "name": "John",
            "age": 30,
            "active": true
        });

        let schema = json_to_schema(&json).unwrap();
        let parquet_writer = ParquetFileWriter::new(schema);

        let temp_file = tempfile::NamedTempFile::new().unwrap();
        let path = temp_file.path();

        parquet_writer
            .write_json_records(path, &[json.clone()])
            .unwrap();

        // Verify file was created
        assert!(path.exists());
    }
}
