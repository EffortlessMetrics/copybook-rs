//! Streaming record reader that produces Arrow `RecordBatch` objects
//!
//! Reads fixed-length records from a byte reader and feeds them through
//! the `RecordBatchBuilder` to produce batches.

use arrow::array::RecordBatch;
use arrow::datatypes::Schema as ArrowSchema;
use std::io::Read;
use std::sync::Arc;

use crate::batch_builder::RecordBatchBuilder;
use crate::options::ArrowOptions;
use crate::schema_convert::cobol_schema_to_arrow;
use crate::{ArrowError, Result};

/// Stream binary records into Arrow `RecordBatch` objects.
///
/// Reads fixed-length records from `reader` (using the schema's `lrecl_fixed`)
/// and returns a vector of `RecordBatch` objects.
///
/// # Errors
///
/// Returns an error if schema conversion, I/O, or decoding fails.
#[inline]
pub fn stream_to_batches<R: Read>(
    mut reader: R,
    cobol_schema: &copybook_core::Schema,
    options: &ArrowOptions,
) -> Result<Vec<RecordBatch>> {
    let arrow_schema = cobol_schema_to_arrow(cobol_schema, options)?;
    let arrow_schema = Arc::new(arrow_schema);

    let record_len = cobol_schema.lrecl_fixed.ok_or_else(|| {
        ArrowError::SchemaConversion(
            "Fixed record length (lrecl_fixed) required for streaming".to_string(),
        )
    })? as usize;

    let mut builder = RecordBatchBuilder::new(Arc::clone(&arrow_schema), cobol_schema, options)?;
    let mut batches = Vec::new();
    let mut buf = vec![0u8; record_len];

    loop {
        match reader.read_exact(&mut buf) {
            Ok(()) => {
                if let Some(batch) = builder.append_record(&buf)? {
                    batches.push(batch);
                }
            }
            Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => break,
            Err(e) => return Err(ArrowError::Io(e)),
        }
    }

    // Flush remaining records
    if let Some(batch) = builder.flush()? {
        batches.push(batch);
    }

    Ok(batches)
}

/// Stream binary records into Arrow `RecordBatch` objects, returning the Arrow schema as well.
///
/// Convenience wrapper around [`stream_to_batches`] that also returns the generated
/// Arrow schema for use in downstream writers.
///
/// # Errors
///
/// Returns an error if schema conversion, I/O, or decoding fails.
#[inline]
pub fn stream_to_batches_with_schema<R: Read>(
    reader: R,
    cobol_schema: &copybook_core::Schema,
    options: &ArrowOptions,
) -> Result<(ArrowSchema, Vec<RecordBatch>)> {
    let arrow_schema = cobol_schema_to_arrow(cobol_schema, options)?;
    let batches = stream_to_batches(reader, cobol_schema, options)?;
    Ok((arrow_schema, batches))
}
