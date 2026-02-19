//! Parquet file writer with configurable compression and metadata embedding

use arrow::array::RecordBatch;
use arrow::datatypes::Schema as ArrowSchema;
use parquet::arrow::ArrowWriter as ParquetArrowWriter;
use parquet::basic::{Compression as ParquetCompression, GzipLevel, ZstdLevel};
use parquet::file::properties::WriterProperties;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufWriter;
use std::path::Path;
use std::sync::Arc;

use crate::options::{ArrowOptions, Compression};
use crate::{ArrowError, Result};

/// Write Arrow `RecordBatch` objects to a Parquet file.
///
/// # Errors
///
/// Returns an error if file creation, schema conversion, or writing fails.
#[inline]
pub fn write_parquet<P: AsRef<Path>>(
    path: P,
    batches: &[RecordBatch],
    schema: &ArrowSchema,
    options: &ArrowOptions,
    copybook_text: Option<&str>,
) -> Result<()> {
    let file = File::create(path)?;
    let buf_writer = BufWriter::new(file);

    let compression = match options.compression {
        Compression::None => ParquetCompression::UNCOMPRESSED,
        Compression::Snappy => ParquetCompression::SNAPPY,
        Compression::Gzip => ParquetCompression::GZIP(GzipLevel::default()),
        Compression::Lz4 => ParquetCompression::LZ4,
        Compression::Zstd => ParquetCompression::ZSTD(ZstdLevel::default()),
    };

    // Build key-value metadata
    let mut kv_metadata: HashMap<String, String> = HashMap::new();
    kv_metadata.insert(
        "copybook_rs.version".to_string(),
        env!("CARGO_PKG_VERSION").to_string(),
    );
    kv_metadata.insert(
        "copybook_rs.codepage".to_string(),
        format!("{:?}", options.codepage),
    );
    if let Some(text) = copybook_text.filter(|_| options.embed_copybook) {
        kv_metadata.insert("copybook_rs.copybook_text".to_string(), text.to_string());
    }

    // Merge metadata into the Arrow schema
    let schema_with_meta = schema.clone().with_metadata(kv_metadata);

    let props = WriterProperties::builder()
        .set_compression(compression)
        .set_max_row_group_size(options.row_group_size)
        .build();

    let mut writer =
        ParquetArrowWriter::try_new(buf_writer, Arc::new(schema_with_meta), Some(props)).map_err(
            |e| ArrowError::ParquetWrite(format!("Failed to create Parquet writer: {e}")),
        )?;

    for batch in batches {
        writer
            .write(batch)
            .map_err(|e| ArrowError::ParquetWrite(format!("Failed to write batch: {e}")))?;
    }

    writer
        .close()
        .map_err(|e| ArrowError::ParquetWrite(format!("Failed to close Parquet writer: {e}")))?;

    Ok(())
}
