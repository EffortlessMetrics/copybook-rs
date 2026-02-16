//! `RecordBatch` builder that accumulates records and flushes Arrow batches
//!
//! Feeds decoded COBOL records into column accumulators and produces
//! Arrow `RecordBatch` objects when the configured batch size is reached.

use arrow::array::RecordBatch;
use arrow::datatypes::Schema as ArrowSchema;
use std::sync::Arc;

use copybook_core::schema::FieldKind;

use crate::builders::{ColumnAccumulator, create_accumulator};
use crate::decode_direct::decode_record_to_columns;
use crate::options::ArrowOptions;
use crate::{ArrowError, Result};

/// Builds Arrow `RecordBatch` objects from a stream of raw COBOL records.
pub struct RecordBatchBuilder {
    arrow_schema: Arc<ArrowSchema>,
    cobol_schema: copybook_core::Schema,
    accumulators: Vec<Box<dyn ColumnAccumulator>>,
    options: ArrowOptions,
    current_count: usize,
}

impl RecordBatchBuilder {
    /// Create a new builder.
    ///
    /// # Errors
    ///
    /// Returns an error if accumulator creation fails for any field.
    #[inline]
    pub fn new(
        arrow_schema: Arc<ArrowSchema>,
        cobol_schema: &copybook_core::Schema,
        options: &ArrowOptions,
    ) -> Result<Self> {
        let accumulators = create_accumulators(cobol_schema, options)?;
        Ok(Self {
            arrow_schema,
            cobol_schema: cobol_schema.clone(),
            accumulators,
            options: options.clone(),
            current_count: 0,
        })
    }

    /// Append a raw record. Returns `Some(batch)` when `batch_size` is reached.
    ///
    /// # Errors
    ///
    /// Returns an error if decoding or batch construction fails.
    #[inline]
    pub fn append_record(&mut self, record: &[u8]) -> Result<Option<RecordBatch>> {
        decode_record_to_columns(
            &self.cobol_schema,
            record,
            &mut self.accumulators,
            &self.options,
        )?;
        self.current_count += 1;

        if self.current_count >= self.options.batch_size {
            let batch = self.build_batch()?;
            self.reset_accumulators()?;
            return Ok(Some(batch));
        }

        Ok(None)
    }

    /// Flush remaining records as a partial batch (may be empty).
    ///
    /// # Errors
    ///
    /// Returns an error if batch construction fails.
    #[inline]
    pub fn flush(&mut self) -> Result<Option<RecordBatch>> {
        if self.current_count == 0 {
            return Ok(None);
        }
        let batch = self.build_batch()?;
        self.reset_accumulators()?;
        Ok(Some(batch))
    }

    /// Build a `RecordBatch` from the current accumulators.
    fn build_batch(&mut self) -> Result<RecordBatch> {
        let columns: Vec<_> = self.accumulators.iter_mut().map(|a| a.finish()).collect();
        RecordBatch::try_new(self.arrow_schema.clone(), columns)
            .map_err(|e| ArrowError::ColumnBuild(format!("RecordBatch build failed: {e}")))
    }

    /// Reset accumulators for the next batch.
    fn reset_accumulators(&mut self) -> Result<()> {
        self.accumulators = create_accumulators(&self.cobol_schema, &self.options)?;
        self.current_count = 0;
        Ok(())
    }
}

/// Create one accumulator per leaf field, matching the Arrow schema field order.
fn create_accumulators(
    schema: &copybook_core::Schema,
    options: &ArrowOptions,
) -> Result<Vec<Box<dyn ColumnAccumulator>>> {
    let mut accumulators = Vec::new();
    for field in &schema.fields {
        collect_accumulators(field, options, &mut accumulators)?;
    }
    Ok(accumulators)
}

/// Recursively collect accumulators for leaf fields.
fn collect_accumulators(
    field: &copybook_core::schema::Field,
    options: &ArrowOptions,
    output: &mut Vec<Box<dyn ColumnAccumulator>>,
) -> Result<()> {
    // Skip non-storage fields
    if matches!(
        field.kind,
        FieldKind::Condition { .. } | FieldKind::Renames { .. }
    ) {
        return Ok(());
    }

    // Skip FILLER unless configured
    if (field.name.starts_with("_filler_") || field.name.eq_ignore_ascii_case("FILLER"))
        && !options.emit_filler
    {
        return Ok(());
    }

    // Handle groups (flatten only for now)
    if matches!(field.kind, FieldKind::Group) {
        if options.flatten_groups {
            for child in &field.children {
                collect_accumulators(child, options, output)?;
            }
        }
        return Ok(());
    }

    // Scalar field
    let acc = create_accumulator(&field.kind, field.len, options)?;
    output.push(acc);
    Ok(())
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use crate::schema_convert::cobol_schema_to_arrow;
    use copybook_core::schema::{Field, FieldKind, Schema};

    fn make_field(name: &str, kind: FieldKind, offset: u32, len: u32) -> Field {
        let mut f = Field::with_kind(5, name.to_string(), kind);
        f.path = name.to_string();
        f.offset = offset;
        f.len = len;
        f
    }

    #[test]
    fn test_batch_builder_flush() {
        let schema = Schema::from_fields(vec![make_field(
            "AMOUNT",
            FieldKind::PackedDecimal {
                digits: 5,
                scale: 2,
                signed: true,
            },
            0,
            3,
        )]);
        let opts = ArrowOptions::default();
        let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
        let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

        // Packed decimal: 12345 with sign C (positive) -> 0x12 0x34 0x5C
        let record = [0x12, 0x34, 0x5C];
        let batch = builder.append_record(&record).unwrap();
        assert!(batch.is_none()); // Not at batch size yet

        let batch = builder.flush().unwrap();
        assert!(batch.is_some());
        let batch = batch.unwrap();
        assert_eq!(batch.num_rows(), 1);
        assert_eq!(batch.num_columns(), 1);
    }

    #[test]
    fn test_batch_builder_auto_flush() {
        let schema = Schema::from_fields(vec![make_field(
            "NAME",
            FieldKind::Alphanum { len: 4 },
            0,
            4,
        )]);
        let opts = ArrowOptions {
            batch_size: 2, // small batch for testing
            codepage: copybook_codec::Codepage::ASCII,
            ..ArrowOptions::default()
        };

        let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
        let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

        let r1 = b"ABCD";
        let r2 = b"EFGH";

        assert!(builder.append_record(r1).unwrap().is_none());
        let batch = builder.append_record(r2).unwrap();
        assert!(batch.is_some());
        assert_eq!(batch.unwrap().num_rows(), 2);
    }
}
