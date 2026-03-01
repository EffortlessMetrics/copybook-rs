// SPDX-License-Identifier: AGPL-3.0-or-later
//! Edge-case and extended coverage tests for copybook-arrow.
//!
//! Covers: empty/single-field schemas, deeply nested groups, OCCURS arrays,
//! COMP-1/2/3 to Arrow type mapping, null handling, missing fields,
//! error paths, mixed schema decode, and Parquet/IPC roundtrips.

#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]

use std::io::Cursor;
use std::sync::Arc;

use arrow::array::{
    AsArray, Decimal128Array, Float32Array, Float64Array, Int16Array, Int32Array, Int64Array,
    UInt16Array, UInt32Array, UInt64Array,
};
use arrow::datatypes::DataType;
use copybook_arrow::batch_builder::RecordBatchBuilder;
use copybook_arrow::options::{ArrowOptions, Compression, EditedPicRepresentation};
use copybook_arrow::schema_convert::cobol_schema_to_arrow;
use copybook_arrow::streaming::stream_to_batches;
use copybook_arrow::{ArrowError, write_ipc, write_parquet};
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
// 1. Schema conversion: single field schemas
// ===================================================================

#[test]
fn single_alphanum_field_schema() {
    let schema = Schema::from_fields(vec![make_field(
        "ONLY",
        FieldKind::Alphanum { len: 1 },
        0,
        1,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 1);
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
}

#[test]
fn single_packed_decimal_field_schema() {
    let schema = Schema::from_fields(vec![make_field(
        "BAL",
        FieldKind::PackedDecimal {
            digits: 1,
            scale: 0,
            signed: true,
        },
        0,
        1,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(1, 0));
}

// ===================================================================
// 2. Schema conversion: all COMP-1/2/3 types
// ===================================================================

#[test]
fn comp1_maps_to_float32() {
    let schema = Schema::from_fields(vec![make_field("F", FieldKind::FloatSingle, 0, 4)]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Float32);
}

#[test]
fn comp2_maps_to_float64() {
    let schema = Schema::from_fields(vec![make_field("D", FieldKind::FloatDouble, 0, 8)]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Float64);
}

#[test]
fn comp3_various_precisions() {
    // 1-digit
    let s1 = Schema::from_fields(vec![make_field(
        "P1",
        FieldKind::PackedDecimal {
            digits: 1,
            scale: 0,
            signed: true,
        },
        0,
        1,
    )]);
    let a1 = cobol_schema_to_arrow(&s1, &ArrowOptions::default()).unwrap();
    assert_eq!(*a1.field(0).data_type(), DataType::Decimal128(1, 0));

    // 18-digit
    let s18 = Schema::from_fields(vec![make_field(
        "P18",
        FieldKind::PackedDecimal {
            digits: 18,
            scale: 4,
            signed: true,
        },
        0,
        10,
    )]);
    let a18 = cobol_schema_to_arrow(&s18, &ArrowOptions::default()).unwrap();
    assert_eq!(*a18.field(0).data_type(), DataType::Decimal128(18, 4));

    // 38-digit (max Decimal128)
    let s38 = Schema::from_fields(vec![make_field(
        "P38",
        FieldKind::PackedDecimal {
            digits: 38,
            scale: 0,
            signed: false,
        },
        0,
        20,
    )]);
    let a38 = cobol_schema_to_arrow(&s38, &ArrowOptions::default()).unwrap();
    assert_eq!(*a38.field(0).data_type(), DataType::Decimal128(38, 0));

    // 39-digit -> falls back to Utf8
    let s39 = Schema::from_fields(vec![make_field(
        "P39",
        FieldKind::PackedDecimal {
            digits: 39,
            scale: 0,
            signed: false,
        },
        0,
        20,
    )]);
    let a39 = cobol_schema_to_arrow(&s39, &ArrowOptions::default()).unwrap();
    assert_eq!(*a39.field(0).data_type(), DataType::Utf8);
}

// ===================================================================
// 3. Schema conversion: OCCURS arrays to Arrow list types
// ===================================================================

#[test]
fn fixed_occurs_alphanum_wraps_in_fixed_size_list() {
    let mut f = make_field("NAMES", FieldKind::Alphanum { len: 20 }, 0, 20);
    f.occurs = Some(Occurs::Fixed { count: 10 });

    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    match arrow.field(0).data_type() {
        DataType::FixedSizeList(inner, 10) => {
            assert_eq!(*inner.data_type(), DataType::Utf8);
        }
        other => panic!("Expected FixedSizeList(_, 10), got {other:?}"),
    }
}

#[test]
fn odo_alphanum_wraps_in_variable_list() {
    let mut f = make_field("ITEMS", FieldKind::Alphanum { len: 5 }, 0, 5);
    f.occurs = Some(Occurs::ODO {
        min: 0,
        max: 99,
        counter_path: "COUNT".to_string(),
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
fn fixed_occurs_on_zoned_decimal_wraps_correctly() {
    let mut f = make_field(
        "AMOUNTS",
        FieldKind::ZonedDecimal {
            digits: 9,
            scale: 2,
            signed: true,
            sign_separate: None,
        },
        0,
        9,
    );
    f.occurs = Some(Occurs::Fixed { count: 5 });

    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    match arrow.field(0).data_type() {
        DataType::FixedSizeList(inner, 5) => {
            assert_eq!(*inner.data_type(), DataType::Decimal128(9, 2));
        }
        other => panic!("Expected FixedSizeList(Decimal128, 5), got {other:?}"),
    }
}

#[test]
fn odo_on_packed_decimal_wraps_in_list() {
    let mut f = make_field(
        "VALS",
        FieldKind::PackedDecimal {
            digits: 7,
            scale: 2,
            signed: true,
        },
        0,
        4,
    );
    f.occurs = Some(Occurs::ODO {
        min: 1,
        max: 20,
        counter_path: "NUM-VALS".to_string(),
    });

    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    match arrow.field(0).data_type() {
        DataType::List(inner) => {
            assert_eq!(*inner.data_type(), DataType::Decimal128(7, 2));
        }
        other => panic!("Expected List(Decimal128(7,2)), got {other:?}"),
    }
}

#[test]
fn fixed_occurs_on_group_wraps_struct_in_list() {
    let mut child = Field::with_kind(5, "CHILD".to_string(), FieldKind::Alphanum { len: 5 });
    child.path = "GRP.CHILD".to_string();
    child.offset = 0;
    child.len = 5;

    let mut group = Field::new(1, "GRP".to_string());
    group.path = "GRP".to_string();
    group.children = vec![child];
    group.occurs = Some(Occurs::Fixed { count: 3 });

    let schema = Schema::from_fields(vec![group]);
    let opts = ArrowOptions {
        flatten_groups: false,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();

    match arrow.field(0).data_type() {
        DataType::FixedSizeList(inner, 3) => {
            assert!(
                matches!(inner.data_type(), DataType::Struct(_)),
                "Expected Struct inside list, got {:?}",
                inner.data_type()
            );
        }
        other => panic!("Expected FixedSizeList(Struct, 3), got {other:?}"),
    }
}

// ===================================================================
// 4. Deeply nested groups
// ===================================================================

#[test]
fn deeply_nested_group_flattened() {
    // 3 levels deep: L1 > L2 > L3 > LEAF
    let mut leaf = Field::with_kind(5, "LEAF".to_string(), FieldKind::Alphanum { len: 10 });
    leaf.path = "L1.L2.L3.LEAF".to_string();
    leaf.offset = 0;
    leaf.len = 10;

    let mut l3 = Field::new(1, "L3".to_string());
    l3.path = "L1.L2.L3".to_string();
    l3.children = vec![leaf];

    let mut l2 = Field::new(1, "L2".to_string());
    l2.path = "L1.L2".to_string();
    l2.children = vec![l3];

    let mut l1 = Field::new(1, "L1".to_string());
    l1.path = "L1".to_string();
    l1.children = vec![l2];

    let schema = Schema::from_fields(vec![l1]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    // Flattened: only LEAF at top level
    assert_eq!(arrow.fields().len(), 1);
    assert_eq!(arrow.field(0).name(), "LEAF");
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
}

#[test]
fn deeply_nested_group_as_struct() {
    let mut leaf = Field::with_kind(5, "LEAF".to_string(), FieldKind::Alphanum { len: 10 });
    leaf.path = "L1.L2.LEAF".to_string();
    leaf.offset = 0;
    leaf.len = 10;

    let mut l2 = Field::new(1, "L2".to_string());
    l2.path = "L1.L2".to_string();
    l2.children = vec![leaf];

    let mut l1 = Field::new(1, "L1".to_string());
    l1.path = "L1".to_string();
    l1.children = vec![l2];

    let schema = Schema::from_fields(vec![l1]);
    let opts = ArrowOptions {
        flatten_groups: false,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();

    // Struct(L1) -> Struct(L2) -> Utf8(LEAF)
    assert_eq!(arrow.fields().len(), 1);
    assert_eq!(arrow.field(0).name(), "L1");
    match arrow.field(0).data_type() {
        DataType::Struct(l1_fields) => {
            assert_eq!(l1_fields.len(), 1);
            assert_eq!(l1_fields[0].name(), "L2");
            match l1_fields[0].data_type() {
                DataType::Struct(l2_fields) => {
                    assert_eq!(l2_fields.len(), 1);
                    assert_eq!(l2_fields[0].name(), "LEAF");
                    assert_eq!(*l2_fields[0].data_type(), DataType::Utf8);
                }
                other => panic!("Expected inner Struct, got {other:?}"),
            }
        }
        other => panic!("Expected outer Struct, got {other:?}"),
    }
}

// ===================================================================
// 5. Empty schema and empty records
// ===================================================================

#[test]
fn empty_schema_produces_no_arrow_fields() {
    let schema = Schema::from_fields(vec![]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 0);
}

#[test]
fn schema_with_only_conditions_produces_no_fields() {
    let schema = Schema::from_fields(vec![
        make_field(
            "C1",
            FieldKind::Condition {
                values: vec!["Y".to_string()],
            },
            0,
            0,
        ),
        make_field(
            "C2",
            FieldKind::Condition {
                values: vec!["N".to_string()],
            },
            0,
            0,
        ),
    ]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 0);
}

#[test]
fn schema_with_only_renames_produces_no_fields() {
    let schema = Schema::from_fields(vec![make_field(
        "ALIAS",
        FieldKind::Renames {
            from_field: "X".to_string(),
            thru_field: "Z".to_string(),
        },
        0,
        0,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 0);
}

#[test]
fn streaming_empty_input_produces_no_batches() {
    let schema = make_schema_with_lrecl(
        vec![make_field("X", FieldKind::Alphanum { len: 4 }, 0, 4)],
        4,
    );
    let reader = Cursor::new(Vec::<u8>::new());
    let batches = stream_to_batches(reader, &schema, &ArrowOptions::default()).unwrap();
    assert!(batches.is_empty());
}

// ===================================================================
// 6. Error handling: invalid schemas
// ===================================================================

#[test]
fn unsupported_binary_int_8bit_returns_error() {
    let schema = Schema::from_fields(vec![make_field(
        "TINY",
        FieldKind::BinaryInt {
            bits: 8,
            signed: true,
        },
        0,
        1,
    )]);
    let err = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap_err();
    assert!(matches!(err, ArrowError::SchemaConversion(_)));
}

#[test]
fn unsupported_binary_int_128bit_returns_error() {
    let schema = Schema::from_fields(vec![make_field(
        "HUGE",
        FieldKind::BinaryInt {
            bits: 128,
            signed: true,
        },
        0,
        16,
    )]);
    let err = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap_err();
    match &err {
        ArrowError::SchemaConversion(msg) => {
            assert!(msg.contains("128"), "expected '128' in message: {msg}");
        }
        other => panic!("Expected SchemaConversion, got {other:?}"),
    }
}

#[test]
fn stream_without_lrecl_returns_error() {
    let schema = Schema::from_fields(vec![make_field("X", FieldKind::Alphanum { len: 5 }, 0, 5)]);
    let reader = Cursor::new(Vec::<u8>::new());
    let err = stream_to_batches(reader, &schema, &ArrowOptions::default()).unwrap_err();
    match &err {
        ArrowError::SchemaConversion(msg) => {
            assert!(msg.contains("lrecl_fixed"), "expected lrecl_fixed: {msg}");
        }
        other => panic!("Expected SchemaConversion, got {other:?}"),
    }
}

// ===================================================================
// 7. Null handling: short records produce nulls
// ===================================================================

#[test]
fn short_record_alphanum_produces_null() {
    let schema = Schema::from_fields(vec![make_field(
        "NAME",
        FieldKind::Alphanum { len: 20 },
        0,
        20,
    )]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // Supply only 5 bytes for a 20-byte field
    builder.append_record(b"SHORT").unwrap();
    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.column(0).null_count(), 1);
}

#[test]
fn short_record_int_produces_null() {
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

    // Only 2 bytes for a 4-byte int32 field
    builder.append_record(&[0x00, 0x01]).unwrap();
    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.column(0).null_count(), 1);
}

#[test]
fn empty_record_produces_null_for_all_fields() {
    let schema = Schema::from_fields(vec![
        make_field("A", FieldKind::Alphanum { len: 10 }, 0, 10),
        make_field(
            "B",
            FieldKind::PackedDecimal {
                digits: 5,
                scale: 2,
                signed: true,
            },
            10,
            3,
        ),
    ]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // Zero-length record
    builder.append_record(&[]).unwrap();
    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.column(0).null_count(), 1);
    assert_eq!(batch.column(1).null_count(), 1);
}

// ===================================================================
// 8. RecordBatch creation: multi-type decode
// ===================================================================

#[test]
fn batch_builder_alphanum_ascii_decode() {
    let schema = Schema::from_fields(vec![make_field(
        "MSG",
        FieldKind::Alphanum { len: 5 },
        0,
        5,
    )]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    builder.append_record(b"HELLO").unwrap();
    builder.append_record(b"WORLD").unwrap();
    builder.append_record(b"TEST ").unwrap(); // trailing space should be trimmed

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 3);

    let col = batch.column(0).as_string::<i32>();
    assert_eq!(col.value(0), "HELLO");
    assert_eq!(col.value(1), "WORLD");
    assert_eq!(col.value(2), "TEST");
}

#[test]
fn batch_builder_packed_decimal_zero_value() {
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

    // Packed zero: 0x00 0x00 0x0C (positive zero)
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
fn batch_builder_all_int_widths() {
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
            "U16",
            FieldKind::BinaryInt {
                bits: 16,
                signed: false,
            },
            2,
            2,
        ),
        make_field(
            "I32",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            4,
            4,
        ),
        make_field(
            "U32",
            FieldKind::BinaryInt {
                bits: 32,
                signed: false,
            },
            8,
            4,
        ),
        make_field(
            "I64",
            FieldKind::BinaryInt {
                bits: 64,
                signed: true,
            },
            12,
            8,
        ),
        make_field(
            "U64",
            FieldKind::BinaryInt {
                bits: 64,
                signed: false,
            },
            20,
            8,
        ),
    ]);
    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // Build a record: I16=1, U16=2, I32=3, U32=4, I64=5, U64=6 (big-endian)
    let mut record = Vec::new();
    record.extend_from_slice(&1_i16.to_be_bytes());
    record.extend_from_slice(&2_u16.to_be_bytes());
    record.extend_from_slice(&3_i32.to_be_bytes());
    record.extend_from_slice(&4_u32.to_be_bytes());
    record.extend_from_slice(&5_i64.to_be_bytes());
    record.extend_from_slice(&6_u64.to_be_bytes());

    builder.append_record(&record).unwrap();
    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.num_columns(), 6);

    let i16_col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Int16Array>()
        .unwrap();
    assert_eq!(i16_col.value(0), 1);

    let u16_col = batch
        .column(1)
        .as_any()
        .downcast_ref::<UInt16Array>()
        .unwrap();
    assert_eq!(u16_col.value(0), 2);

    let i32_col = batch
        .column(2)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(i32_col.value(0), 3);

    let u32_col = batch
        .column(3)
        .as_any()
        .downcast_ref::<UInt32Array>()
        .unwrap();
    assert_eq!(u32_col.value(0), 4);

    let i64_col = batch
        .column(4)
        .as_any()
        .downcast_ref::<Int64Array>()
        .unwrap();
    assert_eq!(i64_col.value(0), 5);

    let u64_col = batch
        .column(5)
        .as_any()
        .downcast_ref::<UInt64Array>()
        .unwrap();
    assert_eq!(u64_col.value(0), 6);
}

#[test]
fn batch_builder_float32_ieee() {
    let schema = Schema::from_fields(vec![make_field("F", FieldKind::FloatSingle, 0, 4)]);
    let opts = ArrowOptions {
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    let val = std::f32::consts::PI;
    builder.append_record(&val.to_be_bytes()).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Float32Array>()
        .unwrap();
    assert!((col.value(0) - std::f32::consts::PI).abs() < 0.001);
}

#[test]
fn batch_builder_float64_ieee() {
    let schema = Schema::from_fields(vec![make_field("D", FieldKind::FloatDouble, 0, 8)]);
    let opts = ArrowOptions {
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    let val = std::f64::consts::E;
    builder.append_record(&val.to_be_bytes()).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Float64Array>()
        .unwrap();
    assert!((col.value(0) - std::f64::consts::E).abs() < 0.0001);
}

// ===================================================================
// 9. Float special values: NaN/Infinity -> null
// ===================================================================

#[test]
fn float32_nan_becomes_null() {
    let schema = Schema::from_fields(vec![make_field("F", FieldKind::FloatSingle, 0, 4)]);
    let opts = ArrowOptions {
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    builder.append_record(&f32::NAN.to_be_bytes()).unwrap();
    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.column(0).null_count(), 1);
}

#[test]
fn float64_infinity_becomes_null() {
    let schema = Schema::from_fields(vec![make_field("D", FieldKind::FloatDouble, 0, 8)]);
    let opts = ArrowOptions {
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    builder.append_record(&f64::INFINITY.to_be_bytes()).unwrap();
    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.column(0).null_count(), 1);
}

#[test]
fn float32_too_short_data_becomes_null() {
    let schema = Schema::from_fields(vec![make_field("F", FieldKind::FloatSingle, 0, 4)]);
    let opts = ArrowOptions {
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // Only 2 bytes for 4-byte float -> null via short-record path
    builder.append_record(&[0x00, 0x00]).unwrap();
    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.column(0).null_count(), 1);
}

// ===================================================================
// 10. FILLER handling
// ===================================================================

#[test]
fn filler_uppercase_skipped() {
    let schema = Schema::from_fields(vec![
        make_field("DATA", FieldKind::Alphanum { len: 5 }, 0, 5),
        make_field("FILLER", FieldKind::Alphanum { len: 5 }, 5, 5),
    ]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 1);
    assert_eq!(arrow.field(0).name(), "DATA");
}

#[test]
fn filler_lowercase_skipped() {
    // Test case-insensitive match
    let schema = Schema::from_fields(vec![
        make_field("DATA", FieldKind::Alphanum { len: 5 }, 0, 5),
        make_field("filler", FieldKind::Alphanum { len: 5 }, 5, 5),
    ]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 1);
}

#[test]
fn filler_emitted_when_configured() {
    let schema = Schema::from_fields(vec![
        make_field("DATA", FieldKind::Alphanum { len: 5 }, 0, 5),
        make_field("FILLER", FieldKind::Alphanum { len: 5 }, 5, 5),
        make_field("_filler_00000010", FieldKind::Alphanum { len: 5 }, 10, 5),
    ]);
    let opts = ArrowOptions {
        emit_filler: true,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(arrow.fields().len(), 3);
}

// ===================================================================
// 11. Edited PIC representation options
// ===================================================================

#[test]
fn edited_pic_default_is_decimal() {
    let schema = Schema::from_fields(vec![make_field(
        "TOT",
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
fn edited_pic_as_string_option() {
    let schema = Schema::from_fields(vec![make_field(
        "TOT",
        FieldKind::EditedNumeric {
            pic_string: "ZZZ9.99".to_string(),
            width: 7,
            scale: 2,
            signed: false,
        },
        0,
        7,
    )]);
    let opts = ArrowOptions {
        edited_pic_as: EditedPicRepresentation::String,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
}

// ===================================================================
// 12. Streaming: multiple batches with boundary conditions
// ===================================================================

#[test]
fn streaming_single_record_produces_one_batch() {
    let schema = make_schema_with_lrecl(
        vec![make_field("X", FieldKind::Alphanum { len: 3 }, 0, 3)],
        3,
    );
    let data = b"ABC";
    let reader = Cursor::new(data.as_slice());
    let batches = stream_to_batches(reader, &schema, &ascii_opts()).unwrap();
    assert_eq!(batches.len(), 1);
    assert_eq!(batches[0].num_rows(), 1);

    let col = batches[0].column(0).as_string::<i32>();
    assert_eq!(col.value(0), "ABC");
}

#[test]
fn streaming_exact_batch_boundary() {
    // 4 records, batch_size=2 => exactly 2 full batches
    let schema = make_schema_with_lrecl(
        vec![make_field("X", FieldKind::Alphanum { len: 2 }, 0, 2)],
        2,
    );
    let data = b"AABBCCDD";
    let reader = Cursor::new(data.as_slice());
    let opts = ArrowOptions {
        batch_size: 2,
        codepage: copybook_codec::Codepage::ASCII,
        ..ArrowOptions::default()
    };
    let batches = stream_to_batches(reader, &schema, &opts).unwrap();
    assert_eq!(batches.len(), 2);
    assert_eq!(batches[0].num_rows(), 2);
    assert_eq!(batches[1].num_rows(), 2);
}

#[test]
fn streaming_batch_boundary_plus_remainder() {
    // 3 records, batch_size=2 => 1 full batch + 1 partial
    let schema = make_schema_with_lrecl(
        vec![make_field("X", FieldKind::Alphanum { len: 2 }, 0, 2)],
        2,
    );
    let data = b"AABBCC";
    let reader = Cursor::new(data.as_slice());
    let opts = ArrowOptions {
        batch_size: 2,
        codepage: copybook_codec::Codepage::ASCII,
        ..ArrowOptions::default()
    };
    let batches = stream_to_batches(reader, &schema, &opts).unwrap();
    assert_eq!(batches.len(), 2);
    assert_eq!(batches[0].num_rows(), 2);
    assert_eq!(batches[1].num_rows(), 1);
}

#[test]
fn streaming_packed_decimal_multiple_records() {
    // PIC S9(5)V99 COMP-3 => 3 bytes per record
    let schema = make_schema_with_lrecl(
        vec![make_field(
            "AMT",
            FieldKind::PackedDecimal {
                digits: 5,
                scale: 2,
                signed: true,
            },
            0,
            3,
        )],
        3,
    );
    // 3 records: 12345C, 00000C, 99999D
    let data: Vec<u8> = vec![
        0x12, 0x34, 0x5C, // +123.45
        0x00, 0x00, 0x0C, // +0.00
        0x99, 0x99, 0x9D, // -999.99
    ];
    let reader = Cursor::new(data);
    let batches = stream_to_batches(reader, &schema, &ArrowOptions::default()).unwrap();
    assert_eq!(batches.len(), 1);
    assert_eq!(batches[0].num_rows(), 3);

    let col = batches[0]
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), 12345);
    assert_eq!(col.value(1), 0);
    assert_eq!(col.value(2), -99999);
}

// ===================================================================
// 13. RecordBatch builder: flush without records returns None
// ===================================================================

#[test]
fn flush_empty_builder_returns_none() {
    let schema = Schema::from_fields(vec![make_field("X", FieldKind::Alphanum { len: 5 }, 0, 5)]);
    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();
    assert!(builder.flush().unwrap().is_none());
}

#[test]
fn flush_after_auto_flush_with_no_additional_records() {
    let schema = Schema::from_fields(vec![make_field("X", FieldKind::Alphanum { len: 2 }, 0, 2)]);
    let opts = ArrowOptions {
        batch_size: 1,
        codepage: copybook_codec::Codepage::ASCII,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // batch_size=1 => auto-flush on first record
    let batch = builder.append_record(b"AB").unwrap();
    assert!(batch.is_some());
    assert_eq!(batch.unwrap().num_rows(), 1);

    // No more records: flush should return None
    assert!(builder.flush().unwrap().is_none());
}

// ===================================================================
// 14. Multi-column schema with mixed types
// ===================================================================

#[test]
fn multi_column_mixed_types_decode() {
    // Alphanum(5) + PackedDecimal(5,2,signed) + Int32
    let schema = Schema::from_fields(vec![
        make_field("NAME", FieldKind::Alphanum { len: 5 }, 0, 5),
        make_field(
            "AMT",
            FieldKind::PackedDecimal {
                digits: 5,
                scale: 2,
                signed: true,
            },
            5,
            3,
        ),
        make_field(
            "CNT",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            8,
            4,
        ),
    ]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    let mut record = Vec::new();
    record.extend_from_slice(b"ALICE");
    record.extend_from_slice(&[0x12, 0x34, 0x5C]); // 12345
    record.extend_from_slice(&42_i32.to_be_bytes());

    builder.append_record(&record).unwrap();
    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.num_columns(), 3);

    let name_col = batch.column(0).as_string::<i32>();
    assert_eq!(name_col.value(0), "ALICE");

    let amt_col = batch
        .column(1)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(amt_col.value(0), 12345);

    let cnt_col = batch
        .column(2)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(cnt_col.value(0), 42);
}

// ===================================================================
// 15. IPC and Parquet roundtrip edge cases
// ===================================================================

#[test]
fn ipc_empty_batch_list_writes_valid_file() {
    let schema = Schema::from_fields(vec![make_field("V", FieldKind::Alphanum { len: 3 }, 0, 3)]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();

    // Write with no batches
    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_ipc(tmp.path(), &[], &arrow_schema).unwrap();
    let metadata = std::fs::metadata(tmp.path()).unwrap();
    assert!(metadata.len() > 0);
}

#[test]
fn parquet_multiple_batches_writes_valid_file() {
    let schema = Schema::from_fields(vec![make_field("V", FieldKind::Alphanum { len: 3 }, 0, 3)]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::None,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();

    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder.append_record(b"AAA").unwrap();
    let batch1 = builder.flush().unwrap().unwrap();

    let mut builder2 =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder2.append_record(b"BBB").unwrap();
    builder2.append_record(b"CCC").unwrap();
    let batch2 = builder2.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch1, batch2], &arrow_schema, &opts, None).unwrap();

    // Verify file with parquet reader
    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader_builder =
        parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
    let mut reader = reader_builder.build().unwrap();
    let read_batch = reader.next().unwrap().unwrap();
    // Total rows across all row groups
    assert!(read_batch.num_rows() >= 1);
}

#[test]
fn parquet_snappy_compression_writes_valid_file() {
    let schema = Schema::from_fields(vec![make_field("V", FieldKind::Alphanum { len: 4 }, 0, 4)]);
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
    let metadata = std::fs::metadata(tmp.path()).unwrap();
    assert!(metadata.len() > 0);
}

#[test]
fn parquet_with_metadata_embedding() {
    let schema = Schema::from_fields(vec![make_field("V", FieldKind::Alphanum { len: 2 }, 0, 2)]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        embed_copybook: true,
        compression: Compression::None,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder.append_record(b"OK").unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let copybook_text = "       01 REC.\n         05 V PIC X(2).";
    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(
        tmp.path(),
        &[batch],
        &arrow_schema,
        &opts,
        Some(copybook_text),
    )
    .unwrap();

    // Read back and verify metadata
    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader_builder =
        parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
    let meta = reader_builder.schema().metadata();
    assert!(meta.contains_key("copybook_rs.version"));
    assert!(meta.contains_key("copybook_rs.copybook_text"));
    assert_eq!(meta["copybook_rs.copybook_text"], copybook_text);
}

// ===================================================================
// 16. ArrowError variant coverage
// ===================================================================

#[test]
fn arrow_error_codec_variant() {
    let err = ArrowError::Codec("codec failure".into());
    let msg = format!("{err}");
    assert!(msg.contains("Codec error"));
    assert!(msg.contains("codec failure"));
}

#[test]
fn arrow_error_ipc_write_variant() {
    let err = ArrowError::IpcWrite("ipc failure".into());
    let msg = format!("{err}");
    assert!(msg.contains("IPC write error"));
}

#[test]
fn arrow_error_column_build_variant() {
    let err = ArrowError::ColumnBuild("build failure".into());
    let msg = format!("{err}");
    assert!(msg.contains("Column build error"));
}

#[test]
fn arrow_error_io_conversion() {
    let io_err = std::io::Error::new(std::io::ErrorKind::PermissionDenied, "denied");
    let err: ArrowError = io_err.into();
    assert!(matches!(err, ArrowError::Io(_)));
    let msg = format!("{err}");
    assert!(msg.contains("denied"));
}

// ===================================================================
// 17. Options defaults verification
// ===================================================================

#[test]
fn arrow_options_default_values() {
    let opts = ArrowOptions::default();
    assert_eq!(opts.batch_size, 8192);
    assert!(opts.flatten_groups);
    assert!(!opts.emit_filler);
    assert!(!opts.emit_meta);
    assert!(!opts.embed_copybook);
    assert_eq!(opts.row_group_size, 1_000_000);
    assert!(matches!(opts.compression, Compression::Zstd));
    assert!(matches!(
        opts.edited_pic_as,
        EditedPicRepresentation::Decimal
    ));
}

// ===================================================================
// 18. Zoned decimal with various scale/sign combinations
// ===================================================================

#[test]
fn zoned_decimal_zero_scale() {
    let schema = Schema::from_fields(vec![make_field(
        "WHOLE",
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
fn zoned_decimal_max_arrow_precision() {
    let schema = Schema::from_fields(vec![make_field(
        "P38",
        FieldKind::ZonedDecimal {
            digits: 38,
            scale: 10,
            signed: true,
            sign_separate: None,
        },
        0,
        38,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(38, 10));
}

#[test]
fn zoned_decimal_over_max_precision_falls_to_utf8() {
    let schema = Schema::from_fields(vec![make_field(
        "P39",
        FieldKind::ZonedDecimal {
            digits: 39,
            scale: 0,
            signed: false,
            sign_separate: None,
        },
        0,
        39,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
}

// ===================================================================
// 19. Group with only non-storage children (conditions/renames)
// ===================================================================

#[test]
fn group_with_only_conditions_omitted_in_struct_mode() {
    let mut group = Field::new(1, "GRP".to_string());
    group.path = "GRP".to_string();

    let mut cond = Field::with_kind(
        88,
        "IS_ACTIVE".to_string(),
        FieldKind::Condition {
            values: vec!["Y".to_string()],
        },
    );
    cond.path = "GRP.IS_ACTIVE".to_string();
    group.children = vec![cond];

    let schema = Schema::from_fields(vec![group]);
    let opts = ArrowOptions {
        flatten_groups: false,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    // Group has no storage children => empty struct => omitted
    assert_eq!(arrow.fields().len(), 0);
}

#[test]
fn group_with_mixed_storage_and_nonstorage_children() {
    let mut group = Field::new(1, "GRP".to_string());
    group.path = "GRP".to_string();

    let mut data = Field::with_kind(5, "DATA".to_string(), FieldKind::Alphanum { len: 10 });
    data.path = "GRP.DATA".to_string();
    data.offset = 0;
    data.len = 10;

    let mut cond = Field::with_kind(
        88,
        "IS_ACTIVE".to_string(),
        FieldKind::Condition {
            values: vec!["Y".to_string()],
        },
    );
    cond.path = "GRP.IS_ACTIVE".to_string();

    group.children = vec![data, cond];

    let schema = Schema::from_fields(vec![group]);

    // Flattened mode: only DATA should appear
    let arrow_flat = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow_flat.fields().len(), 1);
    assert_eq!(arrow_flat.field(0).name(), "DATA");

    // Struct mode: struct with only DATA
    let opts = ArrowOptions {
        flatten_groups: false,
        ..ArrowOptions::default()
    };
    let arrow_struct = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(arrow_struct.fields().len(), 1);
    match arrow_struct.field(0).data_type() {
        DataType::Struct(fields) => {
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].name(), "DATA");
        }
        other => panic!("Expected Struct, got {other:?}"),
    }
}

// ===================================================================
// 20. Streaming multi-field records end-to-end
// ===================================================================

#[test]
fn streaming_multi_field_records() {
    // Alphanum(4) + Int32 = 8 bytes per record
    let schema = make_schema_with_lrecl(
        vec![
            make_field("NAME", FieldKind::Alphanum { len: 4 }, 0, 4),
            make_field(
                "NUM",
                FieldKind::BinaryInt {
                    bits: 32,
                    signed: true,
                },
                4,
                4,
            ),
        ],
        8,
    );

    let mut data = Vec::new();
    data.extend_from_slice(b"ABCD");
    data.extend_from_slice(&100_i32.to_be_bytes());
    data.extend_from_slice(b"EFGH");
    data.extend_from_slice(&200_i32.to_be_bytes());

    let reader = Cursor::new(data);
    let opts = ascii_opts();
    let batches = stream_to_batches(reader, &schema, &opts).unwrap();
    assert_eq!(batches.len(), 1);
    assert_eq!(batches[0].num_rows(), 2);
    assert_eq!(batches[0].num_columns(), 2);

    let name_col = batches[0].column(0).as_string::<i32>();
    assert_eq!(name_col.value(0), "ABCD");
    assert_eq!(name_col.value(1), "EFGH");

    let num_col = batches[0]
        .column(1)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(num_col.value(0), 100);
    assert_eq!(num_col.value(1), 200);
}

// ===================================================================
// 21. Zoned decimal (DISPLAY) actual data decode
// ===================================================================

#[test]
fn zoned_decimal_display_positive_decode() {
    // PIC S9(5)V99 DISPLAY: 7 digit zoned decimal, 7 bytes, scale=2
    let schema = Schema::from_fields(vec![make_field(
        "AMOUNT",
        FieldKind::ZonedDecimal {
            digits: 7,
            scale: 2,
            signed: true,
            sign_separate: None,
        },
        0,
        7,
    )]);
    let opts = ArrowOptions::default(); // CP037
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // +12345.67 in CP037 zoned: F1 F2 F3 F4 F5 F6 C7
    let record: [u8; 7] = [0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xC7];
    builder.append_record(&record).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), 1_234_567); // unscaled value
}

#[test]
fn zoned_decimal_display_negative_decode() {
    // PIC S9(3)V99 DISPLAY: 5 digits, scale=2
    let schema = Schema::from_fields(vec![make_field(
        "BAL",
        FieldKind::ZonedDecimal {
            digits: 5,
            scale: 2,
            signed: true,
            sign_separate: None,
        },
        0,
        5,
    )]);
    let opts = ArrowOptions::default(); // CP037
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // -123.45 in CP037 zoned: F1 F2 F3 F4 D5 (D = negative sign)
    let record: [u8; 5] = [0xF1, 0xF2, 0xF3, 0xF4, 0xD5];
    builder.append_record(&record).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), -12345); // unscaled negative
}

#[test]
fn zoned_decimal_display_multiple_records() {
    // 3-digit unsigned: PIC 9(3)
    let schema = make_schema_with_lrecl(
        vec![make_field(
            "CODE",
            FieldKind::ZonedDecimal {
                digits: 3,
                scale: 0,
                signed: false,
                sign_separate: None,
            },
            0,
            3,
        )],
        3,
    );
    let data: Vec<u8> = vec![
        0xF0, 0xF0, 0xF1, // 001
        0xF1, 0xF2, 0xF3, // 123
        0xF9, 0xF9, 0xF9, // 999
    ];
    let reader = Cursor::new(data);
    let batches = stream_to_batches(reader, &schema, &ArrowOptions::default()).unwrap();
    assert_eq!(batches.len(), 1);
    assert_eq!(batches[0].num_rows(), 3);

    let col = batches[0]
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), 1);
    assert_eq!(col.value(1), 123);
    assert_eq!(col.value(2), 999);
}

// ===================================================================
// 22. Record batch schema matches copybook field names
// ===================================================================

#[test]
fn record_batch_schema_matches_copybook_field_names() {
    let schema = Schema::from_fields(vec![
        make_field("CUSTOMER-ID", FieldKind::Alphanum { len: 10 }, 0, 10),
        make_field(
            "BALANCE",
            FieldKind::PackedDecimal {
                digits: 9,
                scale: 2,
                signed: true,
            },
            10,
            5,
        ),
        make_field(
            "ACCT-TYPE",
            FieldKind::BinaryInt {
                bits: 16,
                signed: false,
            },
            15,
            2,
        ),
    ]);
    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();

    // Verify Arrow schema field names match COBOL field names
    assert_eq!(arrow_schema.field(0).name(), "CUSTOMER-ID");
    assert_eq!(arrow_schema.field(1).name(), "BALANCE");
    assert_eq!(arrow_schema.field(2).name(), "ACCT-TYPE");

    // Verify types
    assert_eq!(*arrow_schema.field(0).data_type(), DataType::Utf8);
    assert_eq!(
        *arrow_schema.field(1).data_type(),
        DataType::Decimal128(9, 2)
    );
    assert_eq!(*arrow_schema.field(2).data_type(), DataType::UInt16);
}

#[test]
fn roundtrip_column_names_match_field_names() {
    let schema = Schema::from_fields(vec![
        make_field("FIRST-NAME", FieldKind::Alphanum { len: 8 }, 0, 8),
        make_field("LAST-NAME", FieldKind::Alphanum { len: 8 }, 8, 8),
        make_field(
            "SALARY",
            FieldKind::PackedDecimal {
                digits: 7,
                scale: 2,
                signed: true,
            },
            16,
            4,
        ),
    ]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    let mut record = Vec::new();
    record.extend_from_slice(b"JOHN    ");
    record.extend_from_slice(b"DOE     ");
    record.extend_from_slice(&[0x05, 0x00, 0x00, 0x0C]); // packed +50000.00

    builder.append_record(&record).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    // Verify column names in the RecordBatch match the COBOL field names
    let batch_schema = batch.schema();
    let field_names: Vec<&str> = batch_schema
        .fields()
        .iter()
        .map(|f| f.name().as_str())
        .collect();
    assert_eq!(field_names, vec!["FIRST-NAME", "LAST-NAME", "SALARY"]);
}

// ===================================================================
// 23. FILLER fields in batch builder decode
// ===================================================================

#[test]
fn filler_skipped_in_batch_builder_decode() {
    // Schema: DATA(5) + FILLER(3) + CODE(2) = 10 bytes
    // But Arrow schema should only have DATA and CODE
    let schema = Schema::from_fields(vec![
        make_field("DATA", FieldKind::Alphanum { len: 5 }, 0, 5),
        make_field("_filler_00000005", FieldKind::Alphanum { len: 3 }, 5, 3),
        make_field("CODE", FieldKind::Alphanum { len: 2 }, 8, 2),
    ]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();

    // Arrow schema should only have 2 fields (FILLER skipped)
    assert_eq!(arrow_schema.fields().len(), 2);
    assert_eq!(arrow_schema.field(0).name(), "DATA");
    assert_eq!(arrow_schema.field(1).name(), "CODE");
}

// ===================================================================
// 24. Schema field type mapping comprehensive check
// ===================================================================

#[test]
fn schema_type_mapping_pic_x_to_utf8() {
    let schema = Schema::from_fields(vec![make_field(
        "TEXT",
        FieldKind::Alphanum { len: 100 },
        0,
        100,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
}

#[test]
fn schema_type_mapping_pic_9_to_decimal128() {
    // PIC 9(9)V99 (zoned decimal) -> Decimal128(9, 2)
    let schema = Schema::from_fields(vec![make_field(
        "NUM",
        FieldKind::ZonedDecimal {
            digits: 9,
            scale: 2,
            signed: false,
            sign_separate: None,
        },
        0,
        9,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(9, 2));
}

#[test]
fn schema_type_mapping_comp3_to_decimal128() {
    // COMP-3 PIC S9(7)V99 -> Decimal128(7, 2)
    let schema = Schema::from_fields(vec![make_field(
        "PKD",
        FieldKind::PackedDecimal {
            digits: 7,
            scale: 2,
            signed: true,
        },
        0,
        4,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(7, 2));
}

#[test]
fn schema_type_mapping_binary_int_to_int64() {
    // COMP PIC S9(18) -> Int64
    let schema = Schema::from_fields(vec![make_field(
        "BIG",
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

// ===================================================================
// 25. Multiple records to single RecordBatch verification
// ===================================================================

#[test]
fn multiple_records_to_single_batch_with_values() {
    let schema = make_schema_with_lrecl(
        vec![
            make_field("ID", FieldKind::Alphanum { len: 3 }, 0, 3),
            make_field(
                "VAL",
                FieldKind::PackedDecimal {
                    digits: 5,
                    scale: 2,
                    signed: true,
                },
                3,
                3,
            ),
        ],
        6,
    );

    let mut data = Vec::new();
    // Record 1: ID="A01", VAL=+123.45
    data.extend_from_slice(b"A01");
    data.extend_from_slice(&[0x12, 0x34, 0x5C]);
    // Record 2: ID="B02", VAL=+000.00
    data.extend_from_slice(b"B02");
    data.extend_from_slice(&[0x00, 0x00, 0x0C]);
    // Record 3: ID="C03", VAL=-500.00
    data.extend_from_slice(b"C03");
    data.extend_from_slice(&[0x50, 0x00, 0x0D]);

    let reader = Cursor::new(data);
    let opts = ascii_opts();
    let batches = stream_to_batches(reader, &schema, &opts).unwrap();
    assert_eq!(batches.len(), 1);
    assert_eq!(batches[0].num_rows(), 3);
    assert_eq!(batches[0].num_columns(), 2);

    let id_col = batches[0].column(0).as_string::<i32>();
    assert_eq!(id_col.value(0), "A01");
    assert_eq!(id_col.value(1), "B02");
    assert_eq!(id_col.value(2), "C03");

    let val_col = batches[0]
        .column(1)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(val_col.value(0), 12345);
    assert_eq!(val_col.value(1), 0);
    assert_eq!(val_col.value(2), -50000);
}
