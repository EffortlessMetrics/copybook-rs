//! Typed Arrow and Parquet output for copybook-rs
//!
//! Converts COBOL binary data directly to Apache Arrow columnar format,
//! preserving COBOL type precision (Decimal128 for COMP-3/Zoned, proper int widths, etc.)

pub mod batch_builder;
pub mod builders;
pub mod decode_direct;
pub mod ipc;
pub mod options;
pub mod parquet_writer;
pub mod schema_convert;
pub mod streaming;

// Legacy API (deprecated)
#[allow(deprecated)]
pub mod legacy;

// New typed API re-exports
pub use batch_builder::RecordBatchBuilder;
pub use ipc::write_ipc;
pub use options::{ArrowOptions, Compression, EditedPicRepresentation};
pub use parquet_writer::write_parquet;
pub use schema_convert::cobol_schema_to_arrow;
pub use streaming::stream_to_batches;

// Legacy re-exports (deprecated, preserved for backward compatibility)
#[allow(deprecated)]
pub use legacy::{
    LegacyArrowWriter as ArrowWriter, LegacyParquetFileWriter as ParquetFileWriter,
    json_to_record_batch, json_to_schema,
};

// Error types
use thiserror::Error;

/// Errors that can occur during Arrow/Parquet conversion
#[derive(Error, Debug)]
pub enum ArrowError {
    /// JSON conversion error (legacy API)
    #[error("JSON conversion error: {0}")]
    JsonConversion(String),

    /// Schema conversion error
    #[error("Schema conversion error: {0}")]
    SchemaConversion(String),

    /// Column build error
    #[error("Column build error: {0}")]
    ColumnBuild(String),

    /// Parquet write error
    #[error("Parquet write error: {0}")]
    ParquetWrite(String),

    /// IPC write error
    #[error("IPC write error: {0}")]
    IpcWrite(String),

    /// IO error
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    /// Arrow error
    #[error("Arrow error: {0}")]
    Arrow(#[from] arrow::error::ArrowError),

    /// Codec error
    #[error("Codec error: {0}")]
    Codec(String),
}

/// Result type for Arrow operations
pub type Result<T> = std::result::Result<T, ArrowError>;

#[cfg(test)]
#[allow(clippy::unwrap_used)]
#[allow(deprecated)]
mod tests {
    use super::*;

    #[test]
    fn test_legacy_json_to_schema() {
        let json = serde_json::json!({
            "name": "John",
            "age": 30,
            "active": true
        });

        let schema = json_to_schema(&json).unwrap();
        assert_eq!(schema.fields().len(), 3);
    }

    #[test]
    fn test_legacy_json_to_record_batch() {
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
    fn test_legacy_arrow_writer() {
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
    fn test_legacy_parquet_writer_roundtrip() {
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
            .write_json_records(path, std::slice::from_ref(&json))
            .unwrap();

        assert!(path.exists());
    }
}
