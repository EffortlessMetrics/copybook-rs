// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep integration tests for Arrow crate covering all COBOL field types and edge cases.
//!
//! Coverage:
//! 1. Every COBOL field type → Arrow data type mapping
//! 2. Null/missing field handling
//! 3. Multiple records → RecordBatch
//! 4. Schema with OCCURS arrays → Arrow list types
//! 5. COMP-3 fields → Arrow decimal
//! 6. Edited PIC fields → Arrow string/numeric
//! 7. REDEFINES fields → Arrow handling
//! 8. Large record batches (100+ records)
//! 9. Arrow → Parquet roundtrip
//! 10. Schema metadata preservation

#![allow(clippy::unwrap_used, clippy::panic)]

use std::io::Cursor;
use std::sync::Arc;

use arrow::array::{
    Array, AsArray, Decimal128Array, Float32Array, Float64Array, Int16Array, Int32Array,
    Int64Array, UInt16Array, UInt32Array, UInt64Array,
};
use arrow::datatypes::DataType;
use copybook_arrow::batch_builder::RecordBatchBuilder;
use copybook_arrow::options::{ArrowOptions, Compression, EditedPicRepresentation};
use copybook_arrow::schema_convert::cobol_schema_to_arrow;
use copybook_arrow::streaming::stream_to_batches;
use copybook_arrow::{write_ipc, write_parquet};
use copybook_core::schema::{Field, FieldKind, Occurs, Schema};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn make_field(name: &str, kind: FieldKind, offset: u32, len: u32) -> Field {
    let mut f = Field::with_kind(5, name.to_string(), kind);
    f.path = name.to_string();
    f.offset = offset;
    f.len = len;
    f
}

fn make_schema_with_lrecl(fields: Vec<Field>, lrecl: u32) -> Schema {
    let mut s = Schema::from_fields(fields);
    s.lrecl_fixed = Some(lrecl);
    s
}

fn ascii_opts() -> ArrowOptions {
    ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        ..ArrowOptions::default()
    }
}

// ===================================================================
// Section 1: Individual COBOL field type → Arrow data type mapping
// ===================================================================

#[test]
fn alphanum_maps_to_utf8() {
    let schema = Schema::from_fields(vec![make_field(
        "CUST-NAME",
        FieldKind::Alphanum { len: 30 },
        0,
        30,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 1);
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
    assert_eq!(arrow.field(0).name(), "CUST-NAME");
}

#[test]
fn zoned_decimal_signed_maps_to_decimal128() {
    let schema = Schema::from_fields(vec![make_field(
        "SALARY",
        FieldKind::ZonedDecimal {
            digits: 11,
            scale: 2,
            signed: true,
            sign_separate: None,
        },
        0,
        11,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(11, 2));
}

#[test]
fn zoned_decimal_unsigned_maps_to_decimal128() {
    let schema = Schema::from_fields(vec![make_field(
        "QTY",
        FieldKind::ZonedDecimal {
            digits: 5,
            scale: 0,
            signed: false,
            sign_separate: None,
        },
        0,
        5,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(5, 0));
}

#[test]
fn packed_decimal_signed_maps_to_decimal128() {
    let schema = Schema::from_fields(vec![make_field(
        "BALANCE",
        FieldKind::PackedDecimal {
            digits: 9,
            scale: 2,
            signed: true,
        },
        0,
        5,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(9, 2));
}

#[test]
fn packed_decimal_unsigned_maps_to_decimal128() {
    let schema = Schema::from_fields(vec![make_field(
        "COUNT",
        FieldKind::PackedDecimal {
            digits: 3,
            scale: 0,
            signed: false,
        },
        0,
        2,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(3, 0));
}

#[test]
fn binary_int_16_signed_maps_to_int16() {
    let schema = Schema::from_fields(vec![make_field(
        "FLAGS",
        FieldKind::BinaryInt {
            bits: 16,
            signed: true,
        },
        0,
        2,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Int16);
}

#[test]
fn binary_int_16_unsigned_maps_to_uint16() {
    let schema = Schema::from_fields(vec![make_field(
        "PORT",
        FieldKind::BinaryInt {
            bits: 16,
            signed: false,
        },
        0,
        2,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::UInt16);
}

#[test]
fn binary_int_32_signed_maps_to_int32() {
    let schema = Schema::from_fields(vec![make_field(
        "RECNUM",
        FieldKind::BinaryInt {
            bits: 32,
            signed: true,
        },
        0,
        4,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Int32);
}

#[test]
fn binary_int_32_unsigned_maps_to_uint32() {
    let schema = Schema::from_fields(vec![make_field(
        "ADDR",
        FieldKind::BinaryInt {
            bits: 32,
            signed: false,
        },
        0,
        4,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::UInt32);
}

#[test]
fn binary_int_64_signed_maps_to_int64() {
    let schema = Schema::from_fields(vec![make_field(
        "BIG-NUM",
        FieldKind::BinaryInt {
            bits: 64,
            signed: true,
        },
        0,
        8,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Int64);
}

#[test]
fn binary_int_64_unsigned_maps_to_uint64() {
    let schema = Schema::from_fields(vec![make_field(
        "EPOCH",
        FieldKind::BinaryInt {
            bits: 64,
            signed: false,
        },
        0,
        8,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::UInt64);
}

#[test]
fn float_single_maps_to_float32() {
    let schema = Schema::from_fields(vec![make_field("RATE", FieldKind::FloatSingle, 0, 4)]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Float32);
}

#[test]
fn float_double_maps_to_float64() {
    let schema = Schema::from_fields(vec![make_field("PRECISE", FieldKind::FloatDouble, 0, 8)]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Float64);
}

#[test]
fn edited_numeric_default_maps_to_decimal128() {
    let schema = Schema::from_fields(vec![make_field(
        "DISPLAY-AMT",
        FieldKind::EditedNumeric {
            pic_string: "ZZZ9.99".to_string(),
            width: 7,
            scale: 2,
            signed: false,
        },
        0,
        7,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert!(matches!(
        arrow.field(0).data_type(),
        DataType::Decimal128(38, 2)
    ));
}

#[test]
fn edited_numeric_as_string_maps_to_utf8() {
    let schema = Schema::from_fields(vec![make_field(
        "DISPLAY-AMT",
        FieldKind::EditedNumeric {
            pic_string: "$ZZ,ZZ9.99".to_string(),
            width: 10,
            scale: 2,
            signed: false,
        },
        0,
        10,
    )]);
    let opts = ArrowOptions {
        edited_pic_as: EditedPicRepresentation::String,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
}

// ===================================================================
// Section 2: Null/missing field handling
// ===================================================================

#[test]
fn null_for_completely_missing_field() {
    // Schema expects 20 bytes, record only has 5
    let schema = Schema::from_fields(vec![
        make_field("A", FieldKind::Alphanum { len: 5 }, 0, 5),
        make_field(
            "B",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            5,
            4,
        ),
        make_field("C", FieldKind::Alphanum { len: 11 }, 9, 11),
    ]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // Only 5 bytes provided — B and C are missing
    builder.append_record(b"HELLO").unwrap();
    let batch = builder.flush().unwrap().unwrap();

    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.column(0).null_count(), 0); // A fits
    assert_eq!(batch.column(1).null_count(), 1); // B missing
    assert_eq!(batch.column(2).null_count(), 1); // C missing
}

#[test]
fn null_for_short_float32_data() {
    let schema = Schema::from_fields(vec![make_field("F", FieldKind::FloatSingle, 0, 4)]);
    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // Only 2 bytes — float accumulator should produce null
    builder.append_record(&[0x00, 0x00]).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Float32Array>()
        .unwrap();
    assert!(col.is_null(0));
}

#[test]
fn null_for_short_float64_data() {
    let schema = Schema::from_fields(vec![make_field("D", FieldKind::FloatDouble, 0, 8)]);
    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    builder.append_record(&[0x00, 0x00, 0x00]).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Float64Array>()
        .unwrap();
    assert!(col.is_null(0));
}

#[test]
fn mixed_null_and_valid_across_records() {
    let schema = Schema::from_fields(vec![
        make_field("A", FieldKind::Alphanum { len: 4 }, 0, 4),
        make_field(
            "B",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            4,
            4,
        ),
    ]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // Record 1: full data (8 bytes)
    let mut r1 = Vec::new();
    r1.extend_from_slice(b"FULL");
    r1.extend_from_slice(&100_i32.to_be_bytes());
    builder.append_record(&r1).unwrap();

    // Record 2: partial (only 4 bytes → B is missing)
    builder.append_record(b"PART").unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 2);
    assert_eq!(batch.column(0).null_count(), 0); // A always fits
    assert_eq!(batch.column(1).null_count(), 1); // B null in row 2
}

// ===================================================================
// Section 3: Multiple records → RecordBatch
// ===================================================================

#[test]
fn multiple_alphanum_records_to_batch() {
    let schema = Schema::from_fields(vec![make_field(
        "NAME",
        FieldKind::Alphanum { len: 5 },
        0,
        5,
    )]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    builder.append_record(b"ALICE").unwrap();
    builder.append_record(b"BOB  ").unwrap();
    builder.append_record(b"CAROL").unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 3);

    let col = batch.column(0).as_string::<i32>();
    assert_eq!(col.value(0), "ALICE");
    assert_eq!(col.value(1), "BOB"); // trailing spaces trimmed
    assert_eq!(col.value(2), "CAROL");
}

#[test]
fn multiple_int_records_to_batch() {
    let schema = Schema::from_fields(vec![make_field(
        "COUNT",
        FieldKind::BinaryInt {
            bits: 32,
            signed: true,
        },
        0,
        4,
    )]);
    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    for val in [0_i32, 1, -1, i32::MAX, i32::MIN] {
        builder.append_record(&val.to_be_bytes()).unwrap();
    }
    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 5);

    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(col.value(0), 0);
    assert_eq!(col.value(1), 1);
    assert_eq!(col.value(2), -1);
    assert_eq!(col.value(3), i32::MAX);
    assert_eq!(col.value(4), i32::MIN);
}

#[test]
fn multiple_packed_decimal_records_positive_and_negative() {
    let schema = Schema::from_fields(vec![make_field(
        "AMT",
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

    // +123.45 → 0x12 0x34 0x5C
    builder.append_record(&[0x12, 0x34, 0x5C]).unwrap();
    // -678.90 → 0x67 0x89 0x0D
    builder.append_record(&[0x67, 0x89, 0x0D]).unwrap();
    // +000.00 → 0x00 0x00 0x0C
    builder.append_record(&[0x00, 0x00, 0x0C]).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 3);

    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), 12345);
    assert_eq!(col.value(1), -67890);
    assert_eq!(col.value(2), 0);
}

// ===================================================================
// Section 4: OCCURS arrays → Arrow list types
// ===================================================================

#[test]
fn occurs_fixed_alphanum_produces_fixed_size_list_utf8() {
    let mut f = make_field("NAMES", FieldKind::Alphanum { len: 10 }, 0, 10);
    f.occurs = Some(Occurs::Fixed { count: 5 });

    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    match arrow.field(0).data_type() {
        DataType::FixedSizeList(inner, 5) => {
            assert_eq!(*inner.data_type(), DataType::Utf8);
        }
        other => panic!("Expected FixedSizeList(Utf8, 5), got {other:?}"),
    }
}

#[test]
fn occurs_fixed_packed_decimal_produces_fixed_size_list_decimal() {
    let mut f = make_field(
        "AMTS",
        FieldKind::PackedDecimal {
            digits: 7,
            scale: 2,
            signed: true,
        },
        0,
        4,
    );
    f.occurs = Some(Occurs::Fixed { count: 10 });

    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    match arrow.field(0).data_type() {
        DataType::FixedSizeList(inner, 10) => {
            assert_eq!(*inner.data_type(), DataType::Decimal128(7, 2));
        }
        other => panic!("Expected FixedSizeList(Decimal128(7,2), 10), got {other:?}"),
    }
}

#[test]
fn occurs_odo_alphanum_produces_variable_list_utf8() {
    let mut f = make_field("ITEMS", FieldKind::Alphanum { len: 20 }, 0, 20);
    f.occurs = Some(Occurs::ODO {
        min: 0,
        max: 100,
        counter_path: "ITEM-CNT".to_string(),
    });

    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    match arrow.field(0).data_type() {
        DataType::List(inner) => {
            assert_eq!(*inner.data_type(), DataType::Utf8);
        }
        other => panic!("Expected List(Utf8), got {other:?}"),
    }
}

#[test]
fn occurs_fixed_float_produces_fixed_size_list_float() {
    let mut f = make_field("RATES", FieldKind::FloatDouble, 0, 8);
    f.occurs = Some(Occurs::Fixed { count: 3 });

    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    match arrow.field(0).data_type() {
        DataType::FixedSizeList(inner, 3) => {
            assert_eq!(*inner.data_type(), DataType::Float64);
        }
        other => panic!("Expected FixedSizeList(Float64, 3), got {other:?}"),
    }
}

#[test]
fn occurs_odo_int32_produces_variable_list_int32() {
    let mut f = make_field(
        "NUMS",
        FieldKind::BinaryInt {
            bits: 32,
            signed: true,
        },
        0,
        4,
    );
    f.occurs = Some(Occurs::ODO {
        min: 1,
        max: 25,
        counter_path: "NUM-CNT".to_string(),
    });

    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    match arrow.field(0).data_type() {
        DataType::List(inner) => {
            assert_eq!(*inner.data_type(), DataType::Int32);
        }
        other => panic!("Expected List(Int32), got {other:?}"),
    }
}

#[test]
fn occurs_on_group_non_flattened_produces_fixed_size_list_struct() {
    let mut child = Field::with_kind(5, "ITEM".to_string(), FieldKind::Alphanum { len: 10 });
    child.path = "GRP.ITEM".to_string();
    child.offset = 0;
    child.len = 10;

    let mut group = Field::new(1, "GRP".to_string());
    group.path = "GRP".to_string();
    group.children = vec![child];
    group.occurs = Some(Occurs::Fixed { count: 4 });

    let schema = Schema::from_fields(vec![group]);
    let opts = ArrowOptions {
        flatten_groups: false,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();

    assert_eq!(arrow.fields().len(), 1);
    match arrow.field(0).data_type() {
        DataType::FixedSizeList(inner, 4) => {
            assert!(
                matches!(inner.data_type(), DataType::Struct(_)),
                "Expected Struct inside list, got {:?}",
                inner.data_type()
            );
        }
        other => panic!("Expected FixedSizeList(Struct, 4), got {other:?}"),
    }
}

// ===================================================================
// Section 5: COMP-3 (packed decimal) fields → Arrow decimal
// ===================================================================

#[test]
fn comp3_zero_value_decodes_correctly() {
    let schema = Schema::from_fields(vec![make_field(
        "ZERO-BAL",
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

    // +0.00 → 0x00 0x00 0x0C
    builder.append_record(&[0x00, 0x00, 0x0C]).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), 0);
}

#[test]
fn comp3_max_digits_within_decimal128_range() {
    // 15 digits → 8 bytes packed (digits/2+1)
    let schema = Schema::from_fields(vec![make_field(
        "BIG",
        FieldKind::PackedDecimal {
            digits: 15,
            scale: 0,
            signed: true,
        },
        0,
        8,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(15, 0));
}

#[test]
fn comp3_wide_precision_falls_back_to_utf8() {
    // 39 digits exceeds Decimal128 max of 38
    let schema = Schema::from_fields(vec![make_field(
        "HUGE",
        FieldKind::PackedDecimal {
            digits: 39,
            scale: 0,
            signed: true,
        },
        0,
        20,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
}

#[test]
fn comp3_boundary_38_digits_is_decimal128() {
    let schema = Schema::from_fields(vec![make_field(
        "BOUNDARY",
        FieldKind::PackedDecimal {
            digits: 38,
            scale: 4,
            signed: true,
        },
        0,
        20,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(38, 4));
}

#[test]
fn comp3_multiple_scale_values() {
    let fields = vec![
        make_field(
            "SCALE0",
            FieldKind::PackedDecimal {
                digits: 5,
                scale: 0,
                signed: true,
            },
            0,
            3,
        ),
        make_field(
            "SCALE2",
            FieldKind::PackedDecimal {
                digits: 7,
                scale: 2,
                signed: true,
            },
            3,
            4,
        ),
        make_field(
            "SCALE5",
            FieldKind::PackedDecimal {
                digits: 9,
                scale: 5,
                signed: true,
            },
            7,
            5,
        ),
    ];
    let schema = Schema::from_fields(fields);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(5, 0));
    assert_eq!(*arrow.field(1).data_type(), DataType::Decimal128(7, 2));
    assert_eq!(*arrow.field(2).data_type(), DataType::Decimal128(9, 5));
}

// ===================================================================
// Section 6: Edited PIC fields → Arrow string/numeric
// ===================================================================

#[test]
fn edited_pic_zero_suppression_as_decimal() {
    let schema = Schema::from_fields(vec![make_field(
        "QTY",
        FieldKind::EditedNumeric {
            pic_string: "ZZZ9".to_string(),
            width: 4,
            scale: 0,
            signed: false,
        },
        0,
        4,
    )]);
    let opts = ArrowOptions::default(); // Decimal mode
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert!(matches!(
        arrow.field(0).data_type(),
        DataType::Decimal128(38, 0)
    ));
}

#[test]
fn edited_pic_currency_as_string() {
    let schema = Schema::from_fields(vec![make_field(
        "PRICE",
        FieldKind::EditedNumeric {
            pic_string: "$ZZ,ZZZ.99".to_string(),
            width: 10,
            scale: 2,
            signed: false,
        },
        0,
        10,
    )]);
    let opts = ArrowOptions {
        edited_pic_as: EditedPicRepresentation::String,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
}

#[test]
fn edited_pic_signed_with_cr_as_decimal() {
    let schema = Schema::from_fields(vec![make_field(
        "NET",
        FieldKind::EditedNumeric {
            pic_string: "ZZ,ZZ9.99CR".to_string(),
            width: 11,
            scale: 2,
            signed: true,
        },
        0,
        11,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert!(matches!(
        arrow.field(0).data_type(),
        DataType::Decimal128(38, 2)
    ));
}

#[test]
fn edited_pic_check_protect_as_string() {
    let schema = Schema::from_fields(vec![make_field(
        "CHECK-AMT",
        FieldKind::EditedNumeric {
            pic_string: "***,**9.99".to_string(),
            width: 10,
            scale: 2,
            signed: false,
        },
        0,
        10,
    )]);
    let opts = ArrowOptions {
        edited_pic_as: EditedPicRepresentation::String,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
}

// ===================================================================
// Section 7: REDEFINES fields → Arrow handling
// ===================================================================

#[test]
fn redefines_field_included_in_schema() {
    // REDEFINES just means the field overlaps another in storage.
    // Arrow schema should include both since they have different names.
    let mut redef_field = make_field("AMOUNT-STR", FieldKind::Alphanum { len: 9 }, 0, 9);
    redef_field.redefines_of = Some("AMOUNT".to_string());

    let schema = Schema::from_fields(vec![
        make_field(
            "AMOUNT",
            FieldKind::ZonedDecimal {
                digits: 9,
                scale: 2,
                signed: true,
                sign_separate: None,
            },
            0,
            9,
        ),
        redef_field,
    ]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 2);
    assert_eq!(arrow.field(0).name(), "AMOUNT");
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(9, 2));
    assert_eq!(arrow.field(1).name(), "AMOUNT-STR");
    assert_eq!(*arrow.field(1).data_type(), DataType::Utf8);
}

#[test]
fn redefines_with_different_types_both_decoded() {
    // Original: packed decimal; redefines: alphanum
    let mut redef = make_field("BAL-TEXT", FieldKind::Alphanum { len: 4 }, 0, 4);
    redef.redefines_of = Some("BAL-NUM".to_string());

    let schema = Schema::from_fields(vec![
        make_field(
            "BAL-NUM",
            FieldKind::PackedDecimal {
                digits: 7,
                scale: 2,
                signed: true,
            },
            0,
            4,
        ),
        redef,
    ]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    // 4 bytes: interpreted as packed decimal AND as text
    builder.append_record(&[0x01, 0x23, 0x45, 0x6C]).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.num_columns(), 2);
}

#[test]
fn condition_fields_excluded_from_arrow_schema() {
    let schema = Schema::from_fields(vec![
        make_field("STATUS", FieldKind::Alphanum { len: 1 }, 0, 1),
        make_field(
            "IS-ACTIVE",
            FieldKind::Condition {
                values: vec!["A".to_string()],
            },
            0,
            0,
        ),
        make_field(
            "IS-CLOSED",
            FieldKind::Condition {
                values: vec!["C".to_string()],
            },
            0,
            0,
        ),
    ]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 1);
    assert_eq!(arrow.field(0).name(), "STATUS");
}

#[test]
fn renames_fields_excluded_from_arrow_schema() {
    let schema = Schema::from_fields(vec![
        make_field("FIRST", FieldKind::Alphanum { len: 10 }, 0, 10),
        make_field("LAST", FieldKind::Alphanum { len: 10 }, 10, 10),
        make_field(
            "FULL-NAME",
            FieldKind::Renames {
                from_field: "FIRST".to_string(),
                thru_field: "LAST".to_string(),
            },
            0,
            0,
        ),
    ]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 2);
    assert_eq!(arrow.field(0).name(), "FIRST");
    assert_eq!(arrow.field(1).name(), "LAST");
}

// ===================================================================
// Section 8: Large record batches (100+ records)
// ===================================================================

#[test]
fn large_batch_200_records_via_builder() {
    let schema = Schema::from_fields(vec![
        make_field("ID", FieldKind::Alphanum { len: 4 }, 0, 4),
        make_field(
            "VAL",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            4,
            4,
        ),
    ]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    for i in 0..200_i32 {
        let mut rec = Vec::with_capacity(8);
        let id = format!("{i:04}");
        rec.extend_from_slice(id.as_bytes());
        rec.extend_from_slice(&i.to_be_bytes());
        builder.append_record(&rec).unwrap();
    }
    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 200);

    let id_col = batch.column(0).as_string::<i32>();
    assert_eq!(id_col.value(0), "0000");
    assert_eq!(id_col.value(199), "0199");

    let val_col = batch
        .column(1)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(val_col.value(0), 0);
    assert_eq!(val_col.value(199), 199);
}

#[test]
fn large_batch_500_records_streaming() {
    let schema = make_schema_with_lrecl(
        vec![
            make_field(
                "SEQ",
                FieldKind::BinaryInt {
                    bits: 16,
                    signed: false,
                },
                0,
                2,
            ),
            make_field(
                "AMT",
                FieldKind::PackedDecimal {
                    digits: 5,
                    scale: 2,
                    signed: true,
                },
                2,
                3,
            ),
        ],
        5,
    );

    let mut data = Vec::with_capacity(500 * 5);
    for i in 0_u16..500 {
        data.extend_from_slice(&i.to_be_bytes());
        // Valid packed decimal: 0x00 0x0i 0xjC where i and j are BCD digits
        let d1 = (i % 10) as u8;
        let d0 = ((i / 10) % 10) as u8;
        data.extend_from_slice(&[0x00, (d0 << 4) | d1, 0x0C]);
    }

    let reader = Cursor::new(data);
    let opts = ArrowOptions {
        batch_size: 100,
        ..ArrowOptions::default()
    };
    let batches = stream_to_batches(reader, &schema, &opts).unwrap();

    let total_rows: usize = batches.iter().map(|b| b.num_rows()).sum();
    assert_eq!(total_rows, 500);
    assert_eq!(batches.len(), 5); // 500/100 = 5 batches

    // Verify first batch
    let seq_col = batches[0]
        .column(0)
        .as_any()
        .downcast_ref::<UInt16Array>()
        .unwrap();
    assert_eq!(seq_col.value(0), 0);
    assert_eq!(seq_col.value(99), 99);
}

#[test]
fn large_batch_auto_flush_boundary() {
    let schema = make_schema_with_lrecl(
        vec![make_field("X", FieldKind::Alphanum { len: 1 }, 0, 1)],
        1,
    );

    // 150 single-byte records, batch_size=50 → 3 full batches
    let data: Vec<u8> = (0..150).map(|i| b'A' + (i % 26)).collect();
    let reader = Cursor::new(data);
    let opts = ArrowOptions {
        batch_size: 50,
        codepage: copybook_codec::Codepage::ASCII,
        ..ArrowOptions::default()
    };
    let batches = stream_to_batches(reader, &schema, &opts).unwrap();

    assert_eq!(batches.len(), 3);
    for batch in &batches {
        assert_eq!(batch.num_rows(), 50);
    }
}

// ===================================================================
// Section 9: Arrow → Parquet roundtrip
// ===================================================================

#[test]
fn parquet_roundtrip_packed_decimal_values_preserved() {
    let schema = Schema::from_fields(vec![make_field(
        "AMT",
        FieldKind::PackedDecimal {
            digits: 5,
            scale: 2,
            signed: true,
        },
        0,
        3,
    )]);
    let opts = ArrowOptions {
        compression: Compression::None,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    builder.append_record(&[0x12, 0x34, 0x5C]).unwrap(); // +123.45
    builder.append_record(&[0x99, 0x99, 0x9D]).unwrap(); // -999.99
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    // Read back
    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader = parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap()
        .build()
        .unwrap();
    let read_batches: Vec<_> = reader.map(|b| b.unwrap()).collect();
    assert_eq!(read_batches.len(), 1);

    let col = read_batches[0]
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), 12345);
    assert_eq!(col.value(1), -99999);
}

#[test]
fn parquet_roundtrip_multi_type_data() {
    let schema = Schema::from_fields(vec![
        make_field("NAME", FieldKind::Alphanum { len: 6 }, 0, 6),
        make_field(
            "SCORE",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            6,
            4,
        ),
        make_field("RATE", FieldKind::FloatDouble, 10, 8),
    ]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::Snappy,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    let mut rec = Vec::new();
    rec.extend_from_slice(b"ALICE ");
    rec.extend_from_slice(&42_i32.to_be_bytes());
    rec.extend_from_slice(&3.14_f64.to_be_bytes());
    builder.append_record(&rec).unwrap();

    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader = parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap()
        .build()
        .unwrap();
    let read_batches: Vec<_> = reader.map(|b| b.unwrap()).collect();

    let name_col = read_batches[0].column(0).as_string::<i32>();
    assert_eq!(name_col.value(0), "ALICE");

    let score_col = read_batches[0]
        .column(1)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(score_col.value(0), 42);

    let rate_col = read_batches[0]
        .column(2)
        .as_any()
        .downcast_ref::<Float64Array>()
        .unwrap();
    assert!((rate_col.value(0) - 3.14).abs() < 1e-10);
}

#[test]
fn parquet_roundtrip_snappy_compression() {
    let schema = Schema::from_fields(vec![make_field(
        "DATA",
        FieldKind::Alphanum { len: 4 },
        0,
        4,
    )]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::Snappy,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    builder.append_record(b"TEST").unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    let meta = std::fs::metadata(tmp.path()).unwrap();
    assert!(meta.len() > 0);
}

#[test]
fn ipc_roundtrip_int_types_preserved() {
    let schema = Schema::from_fields(vec![
        make_field(
            "I16",
            FieldKind::BinaryInt {
                bits: 16,
                signed: true,
            },
            0,
            2,
        ),
        make_field(
            "I64",
            FieldKind::BinaryInt {
                bits: 64,
                signed: true,
            },
            2,
            8,
        ),
    ]);
    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    let mut rec = Vec::new();
    rec.extend_from_slice(&256_i16.to_be_bytes());
    rec.extend_from_slice(&(-42_i64).to_be_bytes());
    builder.append_record(&rec).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_ipc(tmp.path(), &[batch], &arrow_schema).unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader = arrow::ipc::reader::FileReader::try_new(file, None).unwrap();
    let read_batches: Vec<_> = reader.map(|b| b.unwrap()).collect();

    let i16_col = read_batches[0]
        .column(0)
        .as_any()
        .downcast_ref::<Int16Array>()
        .unwrap();
    assert_eq!(i16_col.value(0), 256);

    let i64_col = read_batches[0]
        .column(1)
        .as_any()
        .downcast_ref::<Int64Array>()
        .unwrap();
    assert_eq!(i64_col.value(0), -42);
}

// ===================================================================
// Section 10: Schema metadata preservation
// ===================================================================

#[test]
fn parquet_metadata_includes_version_and_codepage() {
    let schema = Schema::from_fields(vec![make_field("X", FieldKind::Alphanum { len: 2 }, 0, 2)]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::None,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder.append_record(b"OK").unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let pq_builder =
        parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
    let meta = pq_builder.schema().metadata();

    assert!(
        meta.contains_key("copybook_rs.version"),
        "Missing version in metadata: {meta:?}"
    );
    assert!(
        meta.contains_key("copybook_rs.codepage"),
        "Missing codepage in metadata: {meta:?}"
    );
}

#[test]
fn parquet_metadata_embeds_copybook_text() {
    let schema = Schema::from_fields(vec![make_field("A", FieldKind::Alphanum { len: 3 }, 0, 3)]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::None,
        embed_copybook: true,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder.append_record(b"ABC").unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let copybook_text = "       01 REC.\n         05 A PIC X(3).";
    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(
        tmp.path(),
        &[batch],
        &arrow_schema,
        &opts,
        Some(copybook_text),
    )
    .unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let pq_builder =
        parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
    let meta = pq_builder.schema().metadata();

    assert_eq!(
        meta.get("copybook_rs.copybook_text").map(String::as_str),
        Some(copybook_text)
    );
}

#[test]
fn parquet_metadata_omits_copybook_text_when_not_configured() {
    let schema = Schema::from_fields(vec![make_field("A", FieldKind::Alphanum { len: 3 }, 0, 3)]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::None,
        embed_copybook: false,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder.append_record(b"XYZ").unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(
        tmp.path(),
        &[batch],
        &arrow_schema,
        &opts,
        Some("copybook text here"),
    )
    .unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let pq_builder =
        parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
    let meta = pq_builder.schema().metadata();

    assert!(
        !meta.contains_key("copybook_rs.copybook_text"),
        "copybook text should not be in metadata when embed_copybook=false"
    );
}

// ===================================================================
// Additional edge cases: all integer decode through builder
// ===================================================================

#[test]
fn batch_builder_int16_signed_decode() {
    let schema = Schema::from_fields(vec![make_field(
        "S16",
        FieldKind::BinaryInt {
            bits: 16,
            signed: true,
        },
        0,
        2,
    )]);
    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    builder.append_record(&1000_i16.to_be_bytes()).unwrap();
    builder.append_record(&(-500_i16).to_be_bytes()).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Int16Array>()
        .unwrap();
    assert_eq!(col.value(0), 1000);
    assert_eq!(col.value(1), -500);
}

#[test]
fn batch_builder_uint16_decode() {
    let schema = Schema::from_fields(vec![make_field(
        "U16",
        FieldKind::BinaryInt {
            bits: 16,
            signed: false,
        },
        0,
        2,
    )]);
    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    builder.append_record(&50000_u16.to_be_bytes()).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<UInt16Array>()
        .unwrap();
    assert_eq!(col.value(0), 50000);
}

#[test]
fn batch_builder_uint32_decode() {
    let schema = Schema::from_fields(vec![make_field(
        "U32",
        FieldKind::BinaryInt {
            bits: 32,
            signed: false,
        },
        0,
        4,
    )]);
    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    builder
        .append_record(&3_000_000_000_u32.to_be_bytes())
        .unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<UInt32Array>()
        .unwrap();
    assert_eq!(col.value(0), 3_000_000_000);
}

#[test]
fn batch_builder_int64_decode() {
    let schema = Schema::from_fields(vec![make_field(
        "I64",
        FieldKind::BinaryInt {
            bits: 64,
            signed: true,
        },
        0,
        8,
    )]);
    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    builder
        .append_record(&9_999_999_999_i64.to_be_bytes())
        .unwrap();
    builder
        .append_record(&(-9_999_999_999_i64).to_be_bytes())
        .unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Int64Array>()
        .unwrap();
    assert_eq!(col.value(0), 9_999_999_999);
    assert_eq!(col.value(1), -9_999_999_999);
}

#[test]
fn batch_builder_uint64_decode() {
    let schema = Schema::from_fields(vec![make_field(
        "U64",
        FieldKind::BinaryInt {
            bits: 64,
            signed: false,
        },
        0,
        8,
    )]);
    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // Use a value within codec's supported range (i64::MAX fits in u64)
    let val: u64 = 1_000_000_000;
    builder.append_record(&val.to_be_bytes()).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<UInt64Array>()
        .unwrap();
    assert_eq!(col.value(0), val);
}

#[test]
fn batch_builder_float32_decode() {
    let schema = Schema::from_fields(vec![make_field("F", FieldKind::FloatSingle, 0, 4)]);
    let opts = ArrowOptions {
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    builder.append_record(&2.5_f32.to_be_bytes()).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Float32Array>()
        .unwrap();
    assert!((col.value(0) - 2.5).abs() < f32::EPSILON);
}

// ===================================================================
// Additional: Group and nested schema tests
// ===================================================================

#[test]
fn deeply_nested_groups_flattened() {
    // OUTER > MIDDLE > INNER > FIELD
    let mut inner_field = Field::with_kind(15, "LEAF".to_string(), FieldKind::Alphanum { len: 8 });
    inner_field.path = "OUTER.MIDDLE.INNER.LEAF".to_string();
    inner_field.offset = 0;
    inner_field.len = 8;

    let mut inner_group = Field::new(10, "INNER".to_string());
    inner_group.path = "OUTER.MIDDLE.INNER".to_string();
    inner_group.children = vec![inner_field];

    let mut middle_group = Field::new(5, "MIDDLE".to_string());
    middle_group.path = "OUTER.MIDDLE".to_string();
    middle_group.children = vec![inner_group];

    let mut outer_group = Field::new(1, "OUTER".to_string());
    outer_group.path = "OUTER".to_string();
    outer_group.children = vec![middle_group];

    let schema = Schema::from_fields(vec![outer_group]);
    let opts = ArrowOptions {
        flatten_groups: true,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();

    // With flatten_groups, only leaf fields appear
    assert_eq!(arrow.fields().len(), 1);
    assert_eq!(arrow.field(0).name(), "LEAF");
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
}

#[test]
fn mixed_group_and_scalar_at_same_level() {
    let mut child_a = Field::with_kind(5, "GA".to_string(), FieldKind::Alphanum { len: 5 });
    child_a.path = "GRP.GA".to_string();
    child_a.offset = 0;
    child_a.len = 5;

    let mut group = Field::new(1, "GRP".to_string());
    group.path = "GRP".to_string();
    group.children = vec![child_a];

    let scalar = make_field(
        "STANDALONE",
        FieldKind::BinaryInt {
            bits: 32,
            signed: true,
        },
        5,
        4,
    );

    let schema = Schema::from_fields(vec![group, scalar]);
    let opts = ArrowOptions::default(); // flatten_groups = true
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();

    assert_eq!(arrow.fields().len(), 2);
    assert_eq!(arrow.field(0).name(), "GA");
    assert_eq!(arrow.field(1).name(), "STANDALONE");
}

#[test]
fn filler_inside_group_skipped_when_flattened() {
    let mut child = Field::with_kind(5, "DATA".to_string(), FieldKind::Alphanum { len: 10 });
    child.path = "GRP.DATA".to_string();
    child.offset = 0;
    child.len = 10;

    let mut filler = Field::with_kind(
        5,
        "_filler_00000010".to_string(),
        FieldKind::Alphanum { len: 5 },
    );
    filler.path = "GRP._filler_00000010".to_string();
    filler.offset = 10;
    filler.len = 5;

    let mut group = Field::new(1, "GRP".to_string());
    group.path = "GRP".to_string();
    group.children = vec![child, filler];

    let schema = Schema::from_fields(vec![group]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    assert_eq!(arrow.fields().len(), 1);
    assert_eq!(arrow.field(0).name(), "DATA");
}

// ===================================================================
// Additional: Zoned decimal decode through builder
// ===================================================================

#[test]
fn zoned_decimal_decode_through_builder_cp037() {
    // PIC S9(5)V99 DISPLAY → 5 digits, scale 2, signed, 5 bytes in CP037
    let schema = Schema::from_fields(vec![make_field(
        "ZD",
        FieldKind::ZonedDecimal {
            digits: 5,
            scale: 2,
            signed: true,
            sign_separate: None,
        },
        0,
        5,
    )]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::CP037,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // CP037 encoding of "12345" with positive sign: F1 F2 F3 F4 C5
    builder
        .append_record(&[0xF1, 0xF2, 0xF3, 0xF4, 0xC5])
        .unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), 12345);
}

// ===================================================================
// Additional: Error path tests
// ===================================================================

#[test]
fn unsupported_int_width_returns_schema_error() {
    let schema = Schema::from_fields(vec![make_field(
        "BAD",
        FieldKind::BinaryInt {
            bits: 8,
            signed: true,
        },
        0,
        1,
    )]);
    let err = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap_err();
    match err {
        copybook_arrow::ArrowError::SchemaConversion(msg) => {
            assert!(msg.contains("8"), "Expected '8' in error: {msg}");
        }
        other => panic!("Expected SchemaConversion, got {other:?}"),
    }
}

#[test]
fn empty_schema_produces_zero_field_arrow_schema() {
    let schema = Schema::from_fields(vec![]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 0);
}

#[test]
fn streaming_without_lrecl_returns_error() {
    let schema = Schema::from_fields(vec![make_field("X", FieldKind::Alphanum { len: 5 }, 0, 5)]);
    let err = stream_to_batches(
        Cursor::new(Vec::<u8>::new()),
        &schema,
        &ArrowOptions::default(),
    )
    .unwrap_err();
    match err {
        copybook_arrow::ArrowError::SchemaConversion(msg) => {
            assert!(msg.contains("lrecl_fixed"), "Expected lrecl mention: {msg}");
        }
        other => panic!("Expected SchemaConversion, got {other:?}"),
    }
}
