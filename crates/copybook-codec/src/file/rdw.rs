// SPDX-License-Identifier: AGPL-3.0-or-later
//! Schema-aware RDW framing integration.
//!
//! `copybook-rdw` owns byte-stream framing only. Schema-derived minimum
//! payload validation belongs here, at the codec integration boundary.

use copybook_core::{Field, Occurs, Schema};
use copybook_error::{Error, ErrorCode, ErrorContext, Result};

/// Validate a zero-length RDW payload against schema-derived fixed bytes.
///
/// This is intentionally kept outside `copybook-rdw`: the minimum payload is
/// derived from schema/layout semantics rather than RDW framing.
///
/// # Errors
/// Returns `CBKF221_RDW_UNDERFLOW` when the schema requires non-zero bytes.
#[inline]
pub fn validate_zero_length_record(schema: &Schema, record_index: u64) -> Result<()> {
    let minimum_payload_len = schema_fixed_prefix(schema);
    if minimum_payload_len == 0 {
        return Ok(());
    }

    Err(Error::new(
        ErrorCode::CBKF221_RDW_UNDERFLOW,
        format!(
            "Zero-length RDW record invalid: schema requires minimum {minimum_payload_len} bytes"
        ),
    )
    .with_context(ErrorContext {
        record_index: Some(record_index),
        field_path: None,
        byte_offset: None,
        line_number: None,
        details: Some("Zero-length record with non-zero schema prefix".to_string()),
    }))
}

#[inline]
fn schema_fixed_prefix(schema: &Schema) -> u32 {
    let mut first_odo_offset = None;
    find_first_odo_offset(&schema.fields, &mut first_odo_offset);

    first_odo_offset
        .or(schema.lrecl_fixed)
        .unwrap_or_else(|| record_end(&schema.fields))
}

fn find_first_odo_offset(fields: &[Field], current: &mut Option<u32>) {
    for field in fields {
        if let Some(Occurs::ODO { .. }) = &field.occurs {
            let offset = field.offset;
            match current {
                Some(existing) if offset < *existing => *current = Some(offset),
                None => *current = Some(offset),
                _ => {}
            }
        }

        if !field.children.is_empty() {
            find_first_odo_offset(&field.children, current);
        }
    }
}

fn record_end(fields: &[Field]) -> u32 {
    fields.iter().fold(0, |max_end, field| {
        let child_end = record_end(&field.children);
        max_end
            .max(field.offset.saturating_add(field.len))
            .max(child_end)
    })
}

#[cfg(test)]
mod tests {
    use super::validate_zero_length_record;
    use copybook_core::{Field, FieldKind, Occurs, Schema, TailODO};
    use copybook_error::ErrorCode;

    #[test]
    fn zero_length_payload_is_allowed_without_schema_prefix() {
        validate_zero_length_record(&Schema::new(), 1).unwrap();
    }

    #[test]
    fn zero_length_payload_rejects_fixed_schema_prefix() {
        let schema = Schema {
            lrecl_fixed: Some(10),
            ..Schema::new()
        };

        let error = validate_zero_length_record(&schema, 4).unwrap_err();

        assert_eq!(error.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
        assert_eq!(
            error
                .context
                .as_ref()
                .and_then(|context| context.record_index),
            Some(4)
        );
    }

    #[test]
    fn zero_length_payload_rejects_odo_fixed_prefix() {
        let mut counter = Field::with_kind(
            5,
            "CTR".to_string(),
            FieldKind::BinaryInt {
                bits: 16,
                signed: false,
            },
        );
        counter.offset = 0;
        counter.len = 2;

        let mut array = Field::with_kind(5, "ARR".to_string(), FieldKind::Alphanum { len: 1 });
        array.offset = 2;
        array.len = 1;
        array.occurs = Some(Occurs::ODO {
            min: 0,
            max: 5,
            counter_path: "CTR".to_string(),
        });

        let schema = Schema {
            fields: vec![counter, array],
            tail_odo: Some(TailODO {
                counter_path: "CTR".to_string(),
                min_count: 0,
                max_count: 5,
                array_path: "ARR".to_string(),
            }),
            ..Schema::new()
        };

        let error = validate_zero_length_record(&schema, 2).unwrap_err();

        assert_eq!(error.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
        assert!(error.message.contains("minimum 2 bytes"));
    }
}
