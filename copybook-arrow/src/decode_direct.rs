//! Direct record-to-column decoding without JSON intermediate representation
//!
//! Walks the schema field tree and dispatches each field's byte slice to the
//! appropriate column accumulator.

use copybook_core::schema::{Field, FieldKind};

use crate::Result;
use crate::builders::ColumnAccumulator;
use crate::options::ArrowOptions;

/// Walk the schema and dispatch record bytes to the appropriate column accumulators.
///
/// The `accumulators` slice must have one entry per leaf field in the same order
/// as produced by `schema_convert::cobol_schema_to_arrow`.
///
/// # Errors
///
/// Returns an error if a field's byte range exceeds the record or if decoding fails.
#[inline]
pub(crate) fn decode_record_to_columns(
    schema: &copybook_core::Schema,
    record: &[u8],
    accumulators: &mut [Box<dyn ColumnAccumulator>],
    options: &ArrowOptions,
) -> Result<()> {
    let mut idx = 0;
    for field in &schema.fields {
        dispatch_field(field, record, accumulators, &mut idx, options)?;
    }
    Ok(())
}

/// Recursively dispatch a field (or its children for groups) to accumulators.
fn dispatch_field(
    field: &Field,
    record: &[u8],
    accumulators: &mut [Box<dyn ColumnAccumulator>],
    idx: &mut usize,
    options: &ArrowOptions,
) -> Result<()> {
    // Skip non-storage fields
    if matches!(
        field.kind,
        FieldKind::Condition { .. } | FieldKind::Renames { .. }
    ) {
        return Ok(());
    }

    // Skip FILLER unless configured
    if is_filler(field) && !options.emit_filler {
        return Ok(());
    }

    // Handle groups
    if matches!(field.kind, FieldKind::Group) {
        if options.flatten_groups {
            for child in &field.children {
                dispatch_field(child, record, accumulators, idx, options)?;
            }
        }
        // When not flattening, groups are handled as struct columns
        // which is more complex; for the initial implementation we only support flattening
        return Ok(());
    }

    // Scalar field: extract the byte slice and feed it to the accumulator
    if *idx < accumulators.len() {
        let offset = field.offset as usize;
        let len = field.len as usize;
        let end = offset + len;

        if end <= record.len() {
            accumulators[*idx].append_value(&record[offset..end])?;
        } else {
            // Record too short for this field; append null
            accumulators[*idx].append_null();
        }
        *idx += 1;
    }

    Ok(())
}

/// Check if a field is a FILLER.
fn is_filler(field: &Field) -> bool {
    field.name.starts_with("_filler_") || field.name.eq_ignore_ascii_case("FILLER")
}
