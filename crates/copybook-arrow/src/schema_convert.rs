// SPDX-License-Identifier: AGPL-3.0-or-later
//! COBOL schema to Arrow schema conversion
//!
//! Converts `copybook_core::Schema` to `arrow::datatypes::Schema` with proper
//! COBOL type-aware mapping (Decimal128, correct int widths, etc.)

use arrow::datatypes::{DataType, Field as ArrowField, Schema as ArrowSchema};
use std::sync::Arc;

use copybook_core::schema::{Field, FieldKind, Occurs};

use crate::options::{ArrowOptions, EditedPicRepresentation};
use crate::{ArrowError, Result};

/// Maximum precision for Arrow Decimal128 type
const MAX_DECIMAL128_PRECISION: u16 = 38;

/// Convert a COBOL copybook schema to an Arrow schema.
///
/// # Errors
///
/// Returns `ArrowError::SchemaConversion` if any field type cannot be mapped.
#[inline]
pub fn cobol_schema_to_arrow(
    schema: &copybook_core::Schema,
    options: &ArrowOptions,
) -> Result<ArrowSchema> {
    let mut arrow_fields = Vec::new();

    for field in &schema.fields {
        collect_arrow_fields(field, options, &mut arrow_fields)?;
    }

    Ok(ArrowSchema::new(arrow_fields))
}

/// Recursively collect Arrow fields from a COBOL field tree.
fn collect_arrow_fields(
    field: &Field,
    options: &ArrowOptions,
    output: &mut Vec<ArrowField>,
) -> Result<()> {
    // Skip FILLER fields unless configured to emit them
    if is_filler_field(field) && !options.emit_filler {
        return Ok(());
    }

    // Skip non-storage fields (Condition, Renames)
    if matches!(
        field.kind,
        FieldKind::Condition { .. } | FieldKind::Renames { .. }
    ) {
        return Ok(());
    }

    // Handle groups
    if matches!(field.kind, FieldKind::Group) {
        if options.flatten_groups {
            // Flatten: recurse into children, promoting them to the parent level
            for child in &field.children {
                collect_arrow_fields(child, options, output)?;
            }
        } else {
            // Nested struct: build child fields and wrap in Struct
            let mut child_fields = Vec::new();
            for child in &field.children {
                collect_arrow_fields(child, options, &mut child_fields)?;
            }
            if !child_fields.is_empty() {
                let struct_type = DataType::Struct(child_fields.into());
                let arrow_field = wrap_with_occurs(&field.name, struct_type, field.occurs.as_ref());
                output.push(arrow_field);
            }
        }
        return Ok(());
    }

    // Map scalar FieldKind to Arrow DataType
    let data_type = field_kind_to_arrow(&field.kind, options)?;
    let arrow_field = wrap_with_occurs(&field.name, data_type, field.occurs.as_ref());
    output.push(arrow_field);

    Ok(())
}

/// Map a scalar `FieldKind` to an Arrow `DataType`.
fn field_kind_to_arrow(kind: &FieldKind, options: &ArrowOptions) -> Result<DataType> {
    match kind {
        FieldKind::Alphanum { .. } => Ok(DataType::Utf8),

        FieldKind::ZonedDecimal { digits, scale, .. }
        | FieldKind::PackedDecimal { digits, scale, .. } => {
            Ok(decimal_or_fallback(*digits, *scale))
        }

        FieldKind::BinaryInt { bits, signed } => match (bits, signed) {
            (16, true) => Ok(DataType::Int16),
            (16, false) => Ok(DataType::UInt16),
            (32, true) => Ok(DataType::Int32),
            (32, false) => Ok(DataType::UInt32),
            (64, true) => Ok(DataType::Int64),
            (64, false) => Ok(DataType::UInt64),
            _ => Err(ArrowError::SchemaConversion(format!(
                "Unsupported binary int width: {bits} bits"
            ))),
        },

        FieldKind::EditedNumeric { scale, .. } => match options.edited_pic_as {
            EditedPicRepresentation::Decimal => Ok(DataType::Decimal128(
                u8::try_from(MAX_DECIMAL128_PRECISION).unwrap_or(u8::MAX),
                i8::try_from(*scale).unwrap_or(0),
            )),
            EditedPicRepresentation::String => Ok(DataType::Utf8),
        },

        FieldKind::FloatSingle => Ok(DataType::Float32),
        FieldKind::FloatDouble => Ok(DataType::Float64),

        FieldKind::Group | FieldKind::Condition { .. } | FieldKind::Renames { .. } => {
            Err(ArrowError::SchemaConversion(
                "Group/Condition/Renames should be handled before reaching field_kind_to_arrow"
                    .to_string(),
            ))
        }
    }
}

/// Map COBOL decimal precision to Arrow `Decimal128` or fall back to `Utf8` for wide decimals.
fn decimal_or_fallback(digits: u16, scale: i16) -> DataType {
    if digits <= MAX_DECIMAL128_PRECISION {
        DataType::Decimal128(
            u8::try_from(digits).unwrap_or(u8::MAX),
            i8::try_from(scale).unwrap_or(0),
        )
    } else {
        // Fall back to string for precision > 38
        DataType::Utf8
    }
}

/// Check whether a field is a FILLER (by name convention).
fn is_filler_field(field: &Field) -> bool {
    field.name.starts_with("_filler_") || field.name.eq_ignore_ascii_case("FILLER")
}

/// Wrap a data type with list types for OCCURS arrays.
fn wrap_with_occurs(name: &str, inner_type: DataType, occurs: Option<&Occurs>) -> ArrowField {
    match occurs {
        Some(Occurs::Fixed { count }) => {
            let child = ArrowField::new("item", inner_type, true);
            ArrowField::new(
                name,
                DataType::FixedSizeList(Arc::new(child), i32::try_from(*count).unwrap_or(i32::MAX)),
                true,
            )
        }
        Some(Occurs::ODO { .. }) => {
            let child = ArrowField::new("item", inner_type, true);
            ArrowField::new(name, DataType::List(Arc::new(child)), true)
        }
        None => ArrowField::new(name, inner_type, true),
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use copybook_core::schema::{Field, FieldKind, Schema};

    fn make_field(name: &str, kind: FieldKind, offset: u32, len: u32) -> Field {
        let mut f = Field::with_kind(5, name.to_string(), kind);
        f.path = name.to_string();
        f.offset = offset;
        f.len = len;
        f
    }

    #[test]
    fn test_alphanum_to_utf8() {
        let schema = Schema::from_fields(vec![make_field(
            "NAME",
            FieldKind::Alphanum { len: 20 },
            0,
            20,
        )]);
        let opts = ArrowOptions::default();
        let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
        assert_eq!(arrow.fields().len(), 1);
        assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
    }

    #[test]
    fn test_zoned_decimal_to_decimal128() {
        let schema = Schema::from_fields(vec![make_field(
            "AMOUNT",
            FieldKind::ZonedDecimal {
                digits: 9,
                scale: 2,
                signed: true,
                sign_separate: None,
            },
            0,
            9,
        )]);
        let opts = ArrowOptions::default();
        let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
        assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(9, 2));
    }

    #[test]
    fn test_packed_decimal_to_decimal128() {
        let schema = Schema::from_fields(vec![make_field(
            "BAL",
            FieldKind::PackedDecimal {
                digits: 7,
                scale: 2,
                signed: true,
            },
            0,
            4,
        )]);
        let opts = ArrowOptions::default();
        let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
        assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(7, 2));
    }

    #[test]
    fn test_binary_int_types() {
        let schema = Schema::from_fields(vec![
            make_field(
                "I16S",
                FieldKind::BinaryInt {
                    bits: 16,
                    signed: true,
                },
                0,
                2,
            ),
            make_field(
                "I16U",
                FieldKind::BinaryInt {
                    bits: 16,
                    signed: false,
                },
                2,
                2,
            ),
            make_field(
                "I32S",
                FieldKind::BinaryInt {
                    bits: 32,
                    signed: true,
                },
                4,
                4,
            ),
            make_field(
                "I32U",
                FieldKind::BinaryInt {
                    bits: 32,
                    signed: false,
                },
                8,
                4,
            ),
            make_field(
                "I64S",
                FieldKind::BinaryInt {
                    bits: 64,
                    signed: true,
                },
                12,
                8,
            ),
            make_field(
                "I64U",
                FieldKind::BinaryInt {
                    bits: 64,
                    signed: false,
                },
                20,
                8,
            ),
        ]);
        let opts = ArrowOptions::default();
        let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
        assert_eq!(*arrow.field(0).data_type(), DataType::Int16);
        assert_eq!(*arrow.field(1).data_type(), DataType::UInt16);
        assert_eq!(*arrow.field(2).data_type(), DataType::Int32);
        assert_eq!(*arrow.field(3).data_type(), DataType::UInt32);
        assert_eq!(*arrow.field(4).data_type(), DataType::Int64);
        assert_eq!(*arrow.field(5).data_type(), DataType::UInt64);
    }

    #[test]
    fn test_wide_decimal_falls_back_to_utf8() {
        let schema = Schema::from_fields(vec![make_field(
            "WIDE",
            FieldKind::ZonedDecimal {
                digits: 39,
                scale: 0,
                signed: false,
                sign_separate: None,
            },
            0,
            39,
        )]);
        let opts = ArrowOptions::default();
        let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
        assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
    }

    #[test]
    fn test_condition_and_renames_skipped() {
        let schema = Schema::from_fields(vec![
            make_field("DATA", FieldKind::Alphanum { len: 10 }, 0, 10),
            make_field(
                "COND",
                FieldKind::Condition {
                    values: vec!["Y".to_string()],
                },
                0,
                0,
            ),
            make_field(
                "ALIAS",
                FieldKind::Renames {
                    from_field: "A".to_string(),
                    thru_field: "B".to_string(),
                },
                0,
                0,
            ),
        ]);
        let opts = ArrowOptions::default();
        let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
        assert_eq!(arrow.fields().len(), 1);
        assert_eq!(arrow.field(0).name(), "DATA");
    }

    #[test]
    fn test_group_flattening() {
        let mut group = Field::new(1, "GROUP".to_string());
        group.path = "GROUP".to_string();
        let mut child1 = Field::with_kind(5, "A".to_string(), FieldKind::Alphanum { len: 5 });
        child1.path = "GROUP.A".to_string();
        child1.offset = 0;
        child1.len = 5;
        let mut child2 = Field::with_kind(5, "B".to_string(), FieldKind::Alphanum { len: 5 });
        child2.path = "GROUP.B".to_string();
        child2.offset = 5;
        child2.len = 5;
        group.children = vec![child1, child2];

        let schema = Schema::from_fields(vec![group]);
        let opts = ArrowOptions::default();
        let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
        // Flattened: A and B at top level
        assert_eq!(arrow.fields().len(), 2);
        assert_eq!(arrow.field(0).name(), "A");
        assert_eq!(arrow.field(1).name(), "B");
    }

    #[test]
    fn test_filler_skipped() {
        let schema = Schema::from_fields(vec![
            make_field("DATA", FieldKind::Alphanum { len: 10 }, 0, 10),
            make_field("_filler_00000010", FieldKind::Alphanum { len: 5 }, 10, 5),
        ]);
        let opts = ArrowOptions::default();
        let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
        assert_eq!(arrow.fields().len(), 1);
    }

    #[test]
    fn test_filler_included_when_configured() {
        let schema = Schema::from_fields(vec![
            make_field("DATA", FieldKind::Alphanum { len: 10 }, 0, 10),
            make_field("_filler_00000010", FieldKind::Alphanum { len: 5 }, 10, 5),
        ]);
        let opts = ArrowOptions {
            emit_filler: true,
            ..ArrowOptions::default()
        };
        let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
        assert_eq!(arrow.fields().len(), 2);
    }

    #[test]
    fn test_float_types() {
        let schema = Schema::from_fields(vec![
            make_field("F32", FieldKind::FloatSingle, 0, 4),
            make_field("F64", FieldKind::FloatDouble, 4, 8),
        ]);
        let opts = ArrowOptions::default();
        let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
        assert_eq!(*arrow.field(0).data_type(), DataType::Float32);
        assert_eq!(*arrow.field(1).data_type(), DataType::Float64);
    }
}
