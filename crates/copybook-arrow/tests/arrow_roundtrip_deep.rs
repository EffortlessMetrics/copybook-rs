// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep integration tests for copybook-arrow.
//!
//! Covers schema type mapping, data roundtrip through RecordBatch,
//! Parquet write/read, and edge cases.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use arrow::array::{
    AsArray, Decimal128Array, Float32Array, Float64Array, Int16Array, Int32Array, Int64Array,
    UInt16Array, UInt32Array, UInt64Array,
};
use arrow::datatypes::DataType;
use copybook_arrow::batch_builder::RecordBatchBuilder;
use copybook_arrow::options::{ArrowOptions, Compression};
use copybook_arrow::schema_convert::cobol_schema_to_arrow;
use copybook_arrow::write_parquet;
use copybook_core::schema::{Field, FieldKind, Occurs, Schema};
use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;

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

fn ascii_opts() -> ArrowOptions {
    ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::None,
        ..ArrowOptions::default()
    }
}

fn ebcdic_opts() -> ArrowOptions {
    ArrowOptions {
        compression: Compression::None,
        ..ArrowOptions::default()
    }
}

// ===================================================================
// 1. PIC X → Utf8
// ===================================================================

#[test]
fn pic_x_maps_to_utf8_schema() {
    let schema = Schema::from_fields(vec![make_field(
        "CUSTOMER-NAME",
        FieldKind::Alphanum { len: 30 },
        0,
        30,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 1);
    assert_eq!(arrow.field(0).name(), "CUSTOMER-NAME");
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
}

#[test]
fn pic_x_data_roundtrip() {
    let schema = Schema::from_fields(vec![make_field(
        "NAME",
        FieldKind::Alphanum { len: 8 },
        0,
        8,
    )]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    builder.append_record(b"ALICE   ").unwrap();
    builder.append_record(b"BOB     ").unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 2);
    let col = batch.column(0).as_string::<i32>();
    assert_eq!(col.value(0), "ALICE");
    assert_eq!(col.value(1), "BOB");
}

// ===================================================================
// 2. PIC 9 (Zoned Decimal) → Decimal128
// ===================================================================

#[test]
fn pic_9_maps_to_decimal128_schema() {
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
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(9, 2));
}

#[test]
fn pic_9_zoned_decimal_data_roundtrip() {
    // EBCDIC CP037 zoned decimal: PIC S9(5)V99 → 7 digits, scale 2
    // "0012345" in CP037 with trailing sign overpunch.
    // CP037 digit 0=0xF0..9=0xF9. Positive sign overpunch on last byte: 5→0xC5
    let schema = Schema::from_fields(vec![make_field(
        "AMT",
        FieldKind::ZonedDecimal {
            digits: 7,
            scale: 2,
            signed: true,
            sign_separate: None,
        },
        0,
        7,
    )]);
    let opts = ebcdic_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // +00123.45 → unscaled 12345 → digits: 0012345, sign positive
    // CP037: F0 F0 F1 F2 F3 F4 C5 (last nibble C = positive)
    let record = [0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xC5];
    builder.append_record(&record).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 1);
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), 12345);
}

// ===================================================================
// 3. COMP-3 (Packed Decimal) → Decimal128
// ===================================================================

#[test]
fn comp3_maps_to_decimal128_schema() {
    let schema = Schema::from_fields(vec![make_field(
        "BALANCE",
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
fn comp3_positive_and_negative_data_roundtrip() {
    let schema = Schema::from_fields(vec![make_field(
        "BAL",
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
    // -999.99 → 0x99 0x99 0x9D
    builder.append_record(&[0x99, 0x99, 0x9D]).unwrap();
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
    assert_eq!(col.value(1), -99999);
    assert_eq!(col.value(2), 0);
}

// ===================================================================
// 4. COMP (BinaryInt) → Int Arrow types
// ===================================================================

#[test]
fn comp_maps_to_int_types_schema() {
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
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Int16);
    assert_eq!(*arrow.field(1).data_type(), DataType::UInt16);
    assert_eq!(*arrow.field(2).data_type(), DataType::Int32);
    assert_eq!(*arrow.field(3).data_type(), DataType::UInt32);
    assert_eq!(*arrow.field(4).data_type(), DataType::Int64);
    assert_eq!(*arrow.field(5).data_type(), DataType::UInt64);
}

#[test]
fn comp_binary_int_data_roundtrip() {
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

    let mut record = Vec::new();
    record.extend_from_slice(&(-42_i16).to_be_bytes()); // I16
    record.extend_from_slice(&(65000_u16).to_be_bytes()); // U16
    record.extend_from_slice(&(-100_000_i32).to_be_bytes()); // I32
    record.extend_from_slice(&(3_000_000_u32).to_be_bytes()); // U32
    record.extend_from_slice(&(-1_000_000_000_i64).to_be_bytes()); // I64
    record.extend_from_slice(&(9_999_999_999_u64).to_be_bytes()); // U64
    builder.append_record(&record).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.num_columns(), 6);

    let i16_col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Int16Array>()
        .unwrap();
    assert_eq!(i16_col.value(0), -42);
    let u16_col = batch
        .column(1)
        .as_any()
        .downcast_ref::<UInt16Array>()
        .unwrap();
    assert_eq!(u16_col.value(0), 65000);
    let i32_col = batch
        .column(2)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(i32_col.value(0), -100_000);
    let u32_col = batch
        .column(3)
        .as_any()
        .downcast_ref::<UInt32Array>()
        .unwrap();
    assert_eq!(u32_col.value(0), 3_000_000);
    let i64_col = batch
        .column(4)
        .as_any()
        .downcast_ref::<Int64Array>()
        .unwrap();
    assert_eq!(i64_col.value(0), -1_000_000_000);
    let u64_col = batch
        .column(5)
        .as_any()
        .downcast_ref::<UInt64Array>()
        .unwrap();
    assert_eq!(u64_col.value(0), 9_999_999_999);
}

// ===================================================================
// 5. COMP-1 → Float32, COMP-2 → Float64
// ===================================================================

#[test]
fn comp1_maps_to_float32_schema() {
    let schema = Schema::from_fields(vec![make_field("RATE", FieldKind::FloatSingle, 0, 4)]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Float32);
}

#[test]
fn comp2_maps_to_float64_schema() {
    let schema = Schema::from_fields(vec![make_field("RATE", FieldKind::FloatDouble, 0, 8)]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Float64);
}

#[test]
fn comp1_float32_data_roundtrip() {
    let schema = Schema::from_fields(vec![make_field("RATE", FieldKind::FloatSingle, 0, 4)]);
    let opts = ArrowOptions {
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    let val: f32 = 3.14;
    builder.append_record(&val.to_be_bytes()).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Float32Array>()
        .unwrap();
    assert!((col.value(0) - 3.14_f32).abs() < 0.001);
}

#[test]
fn comp2_float64_data_roundtrip() {
    let schema = Schema::from_fields(vec![make_field("RATE", FieldKind::FloatDouble, 0, 8)]);
    let opts = ArrowOptions {
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    let val: f64 = 2.718_281_828;
    builder.append_record(&val.to_be_bytes()).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Float64Array>()
        .unwrap();
    assert!((col.value(0) - 2.718_281_828_f64).abs() < 1e-6);
}

// ===================================================================
// 6. Multi-field schema → RecordBatch correct column count
// ===================================================================

#[test]
fn multi_field_schema_column_count() {
    let schema = Schema::from_fields(vec![
        make_field("NAME", FieldKind::Alphanum { len: 10 }, 0, 10),
        make_field(
            "AMT",
            FieldKind::PackedDecimal {
                digits: 5,
                scale: 2,
                signed: true,
            },
            10,
            3,
        ),
        make_field(
            "CODE",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            13,
            4,
        ),
        make_field("RATE", FieldKind::FloatDouble, 17, 8),
    ]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(arrow_schema.fields().len(), 4);

    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    let mut record = vec![0u8; 25];
    record[..10].copy_from_slice(b"TESTRECORD");
    record[10..13].copy_from_slice(&[0x12, 0x34, 0x5C]);
    record[13..17].copy_from_slice(&42_i32.to_be_bytes());
    record[17..25].copy_from_slice(&1.5_f64.to_be_bytes());
    builder.append_record(&record).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_columns(), 4);
    assert_eq!(batch.num_rows(), 1);
}

// ===================================================================
// 7. Multi-record → RecordBatch correct row count
// ===================================================================

#[test]
fn multi_record_row_count() {
    let schema = Schema::from_fields(vec![make_field("ID", FieldKind::Alphanum { len: 4 }, 0, 4)]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    for i in 0..7 {
        let rec = format!("{i:04}");
        builder.append_record(rec.as_bytes()).unwrap();
    }

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 7);
}

// ===================================================================
// 8. OCCURS Fixed array → FixedSizeList Arrow type
// ===================================================================

#[test]
fn occurs_fixed_maps_to_fixed_size_list() {
    let mut field = make_field("ITEMS", FieldKind::Alphanum { len: 10 }, 0, 10);
    field.occurs = Some(Occurs::Fixed { count: 5 });

    let schema = Schema::from_fields(vec![field]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 1);

    match arrow.field(0).data_type() {
        DataType::FixedSizeList(inner, size) => {
            assert_eq!(*size, 5);
            assert_eq!(*inner.data_type(), DataType::Utf8);
        }
        other => panic!("Expected FixedSizeList, got {other:?}"),
    }
}

#[test]
fn occurs_odo_maps_to_list() {
    let mut field = make_field("ITEMS", FieldKind::Alphanum { len: 10 }, 0, 10);
    field.occurs = Some(Occurs::ODO {
        min: 1,
        max: 10,
        counter_path: "COUNT".to_string(),
    });

    let schema = Schema::from_fields(vec![field]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    match arrow.field(0).data_type() {
        DataType::List(inner) => {
            assert_eq!(*inner.data_type(), DataType::Utf8);
        }
        other => panic!("Expected List, got {other:?}"),
    }
}

#[test]
fn occurs_fixed_numeric_maps_to_fixed_size_list_of_decimal() {
    let mut field = make_field(
        "AMOUNTS",
        FieldKind::PackedDecimal {
            digits: 7,
            scale: 2,
            signed: true,
        },
        0,
        4,
    );
    field.occurs = Some(Occurs::Fixed { count: 3 });

    let schema = Schema::from_fields(vec![field]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    match arrow.field(0).data_type() {
        DataType::FixedSizeList(inner, size) => {
            assert_eq!(*size, 3);
            assert_eq!(*inner.data_type(), DataType::Decimal128(7, 2));
        }
        other => panic!("Expected FixedSizeList(Decimal128), got {other:?}"),
    }
}

// ===================================================================
// 9. Group field → Struct Arrow type
// ===================================================================

#[test]
fn group_maps_to_struct_when_not_flattened() {
    let mut group = Field::new(1, "ADDRESS".to_string());
    group.path = "ADDRESS".to_string();

    let mut street = Field::with_kind(5, "STREET".to_string(), FieldKind::Alphanum { len: 30 });
    street.path = "ADDRESS.STREET".to_string();
    street.offset = 0;
    street.len = 30;

    let mut city = Field::with_kind(5, "CITY".to_string(), FieldKind::Alphanum { len: 20 });
    city.path = "ADDRESS.CITY".to_string();
    city.offset = 30;
    city.len = 20;

    group.children = vec![street, city];

    let schema = Schema::from_fields(vec![group]);
    let opts = ArrowOptions {
        flatten_groups: false,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(arrow.fields().len(), 1);
    assert_eq!(arrow.field(0).name(), "ADDRESS");

    match arrow.field(0).data_type() {
        DataType::Struct(children) => {
            assert_eq!(children.len(), 2);
            assert_eq!(children[0].name(), "STREET");
            assert_eq!(*children[0].data_type(), DataType::Utf8);
            assert_eq!(children[1].name(), "CITY");
            assert_eq!(*children[1].data_type(), DataType::Utf8);
        }
        other => panic!("Expected Struct, got {other:?}"),
    }
}

#[test]
fn group_flattened_promotes_children() {
    let mut group = Field::new(1, "HEADER".to_string());
    group.path = "HEADER".to_string();

    let mut f1 = Field::with_kind(5, "ID".to_string(), FieldKind::Alphanum { len: 5 });
    f1.path = "HEADER.ID".to_string();
    f1.offset = 0;
    f1.len = 5;

    let mut f2 = Field::with_kind(
        5,
        "QTY".to_string(),
        FieldKind::BinaryInt {
            bits: 32,
            signed: true,
        },
    );
    f2.path = "HEADER.QTY".to_string();
    f2.offset = 5;
    f2.len = 4;

    group.children = vec![f1, f2];

    let schema = Schema::from_fields(vec![group]);
    let opts = ArrowOptions {
        flatten_groups: true,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    // Flattened: children promoted to top level
    assert_eq!(arrow.fields().len(), 2);
    assert_eq!(arrow.field(0).name(), "ID");
    assert_eq!(arrow.field(1).name(), "QTY");
}

#[test]
fn group_flattened_data_roundtrip() {
    let mut group = Field::new(1, "REC".to_string());
    group.path = "REC".to_string();

    let mut name_f = Field::with_kind(5, "NAME".to_string(), FieldKind::Alphanum { len: 5 });
    name_f.path = "REC.NAME".to_string();
    name_f.offset = 0;
    name_f.len = 5;

    let mut amt_f = Field::with_kind(
        5,
        "AMT".to_string(),
        FieldKind::PackedDecimal {
            digits: 5,
            scale: 2,
            signed: true,
        },
    );
    amt_f.path = "REC.AMT".to_string();
    amt_f.offset = 5;
    amt_f.len = 3;

    group.children = vec![name_f, amt_f];

    let schema = Schema::from_fields(vec![group]);
    let opts = ArrowOptions {
        flatten_groups: true,
        codepage: copybook_codec::Codepage::ASCII,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    let mut record = Vec::new();
    record.extend_from_slice(b"ALICE");
    record.extend_from_slice(&[0x12, 0x34, 0x5C]);
    builder.append_record(&record).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_columns(), 2);
    assert_eq!(batch.num_rows(), 1);

    let name_col = batch.column(0).as_string::<i32>();
    assert_eq!(name_col.value(0), "ALICE");

    let amt_col = batch
        .column(1)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(amt_col.value(0), 12345);
}

// ===================================================================
// 10. REDEFINES → primary view in Arrow
// ===================================================================

#[test]
fn redefines_primary_field_appears_in_schema() {
    // Both the original and the redefines field should appear in the Arrow schema
    // (redefines_of is metadata; schema_convert doesn't skip them)
    let primary = make_field("AMOUNT-TEXT", FieldKind::Alphanum { len: 8 }, 0, 8);
    let mut redef = make_field(
        "AMOUNT-NUM",
        FieldKind::PackedDecimal {
            digits: 7,
            scale: 2,
            signed: true,
        },
        0,
        4,
    );
    redef.redefines_of = Some("AMOUNT-TEXT".to_string());

    let schema = Schema::from_fields(vec![primary, redef]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    // Both fields are present; downstream consumers choose which view to use
    assert_eq!(arrow.fields().len(), 2);
    assert_eq!(arrow.field(0).name(), "AMOUNT-TEXT");
    assert_eq!(arrow.field(1).name(), "AMOUNT-NUM");
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
    assert_eq!(*arrow.field(1).data_type(), DataType::Decimal128(7, 2));
}

// ===================================================================
// 11. Null/empty field handling
// ===================================================================

#[test]
fn short_record_produces_null() {
    let schema = Schema::from_fields(vec![
        make_field("A", FieldKind::Alphanum { len: 5 }, 0, 5),
        make_field("B", FieldKind::Alphanum { len: 5 }, 5, 5),
    ]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // Record is only 5 bytes but field B starts at offset 5
    builder.append_record(b"HELLO").unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 1);

    let col_a = batch.column(0).as_string::<i32>();
    assert_eq!(col_a.value(0), "HELLO");

    // Field B should be null due to short record
    assert!(batch.column(1).is_null(0));
}

#[test]
fn short_record_numeric_produces_null() {
    let schema = Schema::from_fields(vec![make_field(
        "AMT",
        FieldKind::PackedDecimal {
            digits: 5,
            scale: 2,
            signed: true,
        },
        10,
        3,
    )]);
    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // Record is only 5 bytes but field starts at offset 10
    builder.append_record(&[0u8; 5]).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert!(batch.column(0).is_null(0));
}

#[test]
fn float_nan_produces_null() {
    let schema = Schema::from_fields(vec![make_field("F", FieldKind::FloatSingle, 0, 4)]);
    let opts = ArrowOptions {
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // IEEE NaN
    builder.append_record(&f32::NAN.to_be_bytes()).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert!(batch.column(0).is_null(0));
}

#[test]
fn float_infinity_produces_null() {
    let schema = Schema::from_fields(vec![make_field("F", FieldKind::FloatDouble, 0, 8)]);
    let opts = ArrowOptions {
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    builder.append_record(&f64::INFINITY.to_be_bytes()).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert!(batch.column(0).is_null(0));
}

// ===================================================================
// 12. Schema metadata preservation through Parquet
// ===================================================================

#[test]
fn parquet_metadata_includes_version_and_codepage() {
    let schema = Schema::from_fields(vec![make_field("V", FieldKind::Alphanum { len: 4 }, 0, 4)]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::None,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder.append_record(b"TEST").unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader_builder = ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
    let meta = reader_builder.schema().metadata();

    assert!(meta.contains_key("copybook_rs.version"));
    assert!(meta.contains_key("copybook_rs.codepage"));
}

#[test]
fn parquet_metadata_embeds_copybook_text() {
    let schema = Schema::from_fields(vec![make_field("V", FieldKind::Alphanum { len: 4 }, 0, 4)]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::None,
        embed_copybook: true,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder.append_record(b"TEST").unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let copybook = "       01 REC.\n         05 V PIC X(4).";
    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, Some(copybook)).unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader_builder = ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
    let meta = reader_builder.schema().metadata();
    assert_eq!(
        meta.get("copybook_rs.copybook_text").map(String::as_str),
        Some(copybook)
    );
}

// ===================================================================
// 13. Parquet write and read roundtrip
// ===================================================================

#[test]
fn parquet_mixed_types_full_roundtrip() {
    let schema = Schema::from_fields(vec![
        make_field("NAME", FieldKind::Alphanum { len: 6 }, 0, 6),
        make_field(
            "AMT",
            FieldKind::PackedDecimal {
                digits: 5,
                scale: 2,
                signed: true,
            },
            6,
            3,
        ),
        make_field(
            "QTY",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            9,
            4,
        ),
    ]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    let mut r1 = Vec::new();
    r1.extend_from_slice(b"ALPHA ");
    r1.extend_from_slice(&[0x12, 0x34, 0x5C]);
    r1.extend_from_slice(&100_i32.to_be_bytes());
    builder.append_record(&r1).unwrap();

    let mut r2 = Vec::new();
    r2.extend_from_slice(b"BRAVO ");
    r2.extend_from_slice(&[0x99, 0x99, 0x9D]);
    r2.extend_from_slice(&(-50_i32).to_be_bytes());
    builder.append_record(&r2).unwrap();

    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    // Read back
    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader = ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap()
        .build()
        .unwrap();
    let read_batches: Vec<_> = reader.map(|b| b.unwrap()).collect();
    let total_rows: usize = read_batches.iter().map(|b| b.num_rows()).sum();
    assert_eq!(total_rows, 2);

    let rb = &read_batches[0];
    let name_col = rb.column(0).as_string::<i32>();
    assert_eq!(name_col.value(0), "ALPHA");
    assert_eq!(name_col.value(1), "BRAVO");

    let amt_col = rb
        .column(1)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(amt_col.value(0), 12345);
    assert_eq!(amt_col.value(1), -99999);

    let qty_col = rb.column(2).as_any().downcast_ref::<Int32Array>().unwrap();
    assert_eq!(qty_col.value(0), 100);
    assert_eq!(qty_col.value(1), -50);
}

#[test]
fn parquet_schema_field_types_preserved() {
    let schema = Schema::from_fields(vec![
        make_field("TXT", FieldKind::Alphanum { len: 10 }, 0, 10),
        make_field(
            "DEC",
            FieldKind::PackedDecimal {
                digits: 9,
                scale: 4,
                signed: true,
            },
            10,
            5,
        ),
        make_field(
            "I32",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            15,
            4,
        ),
        make_field("F64", FieldKind::FloatDouble, 19, 8),
    ]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    let mut record = vec![0u8; 27];
    record[..10].copy_from_slice(b"TESTFIELD ");
    record[10..15].copy_from_slice(&[0x00, 0x00, 0x00, 0x00, 0x0C]);
    record[15..19].copy_from_slice(&1_i32.to_be_bytes());
    record[19..27].copy_from_slice(&1.0_f64.to_be_bytes());
    builder.append_record(&record).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader_builder = ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
    let read_schema = reader_builder.schema();

    assert_eq!(*read_schema.field(0).data_type(), DataType::Utf8);
    assert_eq!(
        *read_schema.field(1).data_type(),
        DataType::Decimal128(9, 4)
    );
    assert_eq!(*read_schema.field(2).data_type(), DataType::Int32);
    assert_eq!(*read_schema.field(3).data_type(), DataType::Float64);
}

// ===================================================================
// 14. Large batch (1000 records) memory test
// ===================================================================

#[test]
fn large_batch_1000_records() {
    let schema = Schema::from_fields(vec![
        make_field(
            "ID",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            0,
            4,
        ),
        make_field("NAME", FieldKind::Alphanum { len: 10 }, 4, 10),
        make_field(
            "BAL",
            FieldKind::PackedDecimal {
                digits: 5,
                scale: 2,
                signed: true,
            },
            14,
            3,
        ),
    ]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    for i in 0..1000_i32 {
        let mut record = Vec::with_capacity(17);
        record.extend_from_slice(&i.to_be_bytes());
        let name = format!("REC{i:<7}");
        record.extend_from_slice(&name.as_bytes()[..10]);
        // Packed decimal: fixed +100.00 = 0x01 0x00 0x0C
        record.extend_from_slice(&[0x01, 0x00, 0x0C]);
        builder.append_record(&record).unwrap();
    }

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 1000);
    assert_eq!(batch.num_columns(), 3);

    // Spot check first and last
    let id_col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(id_col.value(0), 0);
    assert_eq!(id_col.value(999), 999);

    let name_col = batch.column(1).as_string::<i32>();
    assert!(name_col.value(0).starts_with("REC0"));
    assert!(name_col.value(999).starts_with("REC999"));
}

#[test]
fn large_batch_parquet_roundtrip() {
    let schema = Schema::from_fields(vec![
        make_field(
            "SEQ",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            0,
            4,
        ),
        make_field("VAL", FieldKind::Alphanum { len: 6 }, 4, 6),
    ]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    for i in 0..1000_i32 {
        let mut record = Vec::with_capacity(10);
        record.extend_from_slice(&i.to_be_bytes());
        let val = format!("V{i:<5}");
        record.extend_from_slice(&val.as_bytes()[..6]);
        builder.append_record(&record).unwrap();
    }

    let batch = builder.flush().unwrap().unwrap();
    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader = ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap()
        .build()
        .unwrap();
    let read_batches: Vec<_> = reader.map(|b| b.unwrap()).collect();
    let total_rows: usize = read_batches.iter().map(|b| b.num_rows()).sum();
    assert_eq!(total_rows, 1000);
}

// ===================================================================
// 15. Type mapping table verification
// ===================================================================

#[test]
fn comprehensive_type_mapping_table() {
    // Verify the complete COBOL → Arrow type mapping in a single schema
    let schema = Schema::from_fields(vec![
        make_field("F_ALPHANUM", FieldKind::Alphanum { len: 20 }, 0, 20),
        make_field(
            "F_ZONED",
            FieldKind::ZonedDecimal {
                digits: 9,
                scale: 2,
                signed: true,
                sign_separate: None,
            },
            20,
            9,
        ),
        make_field(
            "F_PACKED",
            FieldKind::PackedDecimal {
                digits: 7,
                scale: 3,
                signed: true,
            },
            29,
            4,
        ),
        make_field(
            "F_INT16S",
            FieldKind::BinaryInt {
                bits: 16,
                signed: true,
            },
            33,
            2,
        ),
        make_field(
            "F_INT16U",
            FieldKind::BinaryInt {
                bits: 16,
                signed: false,
            },
            35,
            2,
        ),
        make_field(
            "F_INT32S",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            37,
            4,
        ),
        make_field(
            "F_INT32U",
            FieldKind::BinaryInt {
                bits: 32,
                signed: false,
            },
            41,
            4,
        ),
        make_field(
            "F_INT64S",
            FieldKind::BinaryInt {
                bits: 64,
                signed: true,
            },
            45,
            8,
        ),
        make_field(
            "F_INT64U",
            FieldKind::BinaryInt {
                bits: 64,
                signed: false,
            },
            53,
            8,
        ),
        make_field("F_FLOAT32", FieldKind::FloatSingle, 61, 4),
        make_field("F_FLOAT64", FieldKind::FloatDouble, 65, 8),
    ]);

    let opts = ArrowOptions::default();
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();

    let expected: Vec<(&str, DataType)> = vec![
        ("F_ALPHANUM", DataType::Utf8),
        ("F_ZONED", DataType::Decimal128(9, 2)),
        ("F_PACKED", DataType::Decimal128(7, 3)),
        ("F_INT16S", DataType::Int16),
        ("F_INT16U", DataType::UInt16),
        ("F_INT32S", DataType::Int32),
        ("F_INT32U", DataType::UInt32),
        ("F_INT64S", DataType::Int64),
        ("F_INT64U", DataType::UInt64),
        ("F_FLOAT32", DataType::Float32),
        ("F_FLOAT64", DataType::Float64),
    ];

    assert_eq!(arrow.fields().len(), expected.len());
    for (i, (name, dtype)) in expected.iter().enumerate() {
        assert_eq!(
            arrow.field(i).name(),
            *name,
            "Field name mismatch at index {i}"
        );
        assert_eq!(
            *arrow.field(i).data_type(),
            *dtype,
            "Data type mismatch for field {name}"
        );
    }
}

#[test]
fn wide_decimal_falls_back_to_utf8() {
    // Precision > 38 cannot fit in Decimal128; should fall back to Utf8
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
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
}

// ===================================================================
// 16. Condition and Renames fields skipped
// ===================================================================

#[test]
fn condition_fields_excluded_from_arrow() {
    let schema = Schema::from_fields(vec![
        make_field("STATUS", FieldKind::Alphanum { len: 1 }, 0, 1),
        make_field(
            "IS-ACTIVE",
            FieldKind::Condition {
                values: vec!["Y".to_string()],
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
fn renames_fields_excluded_from_arrow() {
    let schema = Schema::from_fields(vec![
        make_field("A", FieldKind::Alphanum { len: 5 }, 0, 5),
        make_field("B", FieldKind::Alphanum { len: 5 }, 5, 5),
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
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 2);
    assert_eq!(arrow.field(0).name(), "A");
    assert_eq!(arrow.field(1).name(), "B");
}

// ===================================================================
// 17. FILLER handling
// ===================================================================

#[test]
fn filler_excluded_by_default() {
    let schema = Schema::from_fields(vec![
        make_field("DATA", FieldKind::Alphanum { len: 10 }, 0, 10),
        make_field("_filler_00000010", FieldKind::Alphanum { len: 5 }, 10, 5),
        make_field("FILLER", FieldKind::Alphanum { len: 3 }, 15, 3),
    ]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 1);
    assert_eq!(arrow.field(0).name(), "DATA");
}

#[test]
fn filler_included_when_emit_filler_enabled() {
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

// ===================================================================
// 18. Streaming API test
// ===================================================================

#[test]
fn streaming_batches_from_reader() {
    let schema = Schema::from_fields(vec![make_field(
        "NAME",
        FieldKind::Alphanum { len: 5 },
        0,
        5,
    )]);
    let mut schema_with_lrecl = schema;
    schema_with_lrecl.lrecl_fixed = Some(5);

    let opts = ascii_opts();

    // 3 records of 5 bytes each
    let data: Vec<u8> = b"ALICEBOB  CAROL".to_vec();
    let cursor = std::io::Cursor::new(data);

    let batches = copybook_arrow::stream_to_batches(cursor, &schema_with_lrecl, &opts).unwrap();
    let total_rows: usize = batches.iter().map(|b| b.num_rows()).sum();
    assert_eq!(total_rows, 3);
}

// ===================================================================
// 19. EditedNumeric handling
// ===================================================================

#[test]
fn edited_numeric_default_maps_to_decimal() {
    let schema = Schema::from_fields(vec![make_field(
        "DISPLAY-AMT",
        FieldKind::EditedNumeric {
            pic_string: "ZZ,ZZZ.99".to_string(),
            width: 9,
            scale: 2,
            signed: false,
        },
        0,
        9,
    )]);
    let opts = ArrowOptions::default(); // edited_pic_as defaults to Decimal
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(38, 2));
}

#[test]
fn edited_numeric_string_mode_maps_to_utf8() {
    use copybook_arrow::options::EditedPicRepresentation;

    let schema = Schema::from_fields(vec![make_field(
        "DISPLAY-AMT",
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

// ===================================================================
// 20. Batch auto-flush and reset
// ===================================================================

#[test]
fn batch_auto_flush_and_continue() {
    let schema = Schema::from_fields(vec![make_field(
        "CODE",
        FieldKind::Alphanum { len: 3 },
        0,
        3,
    )]);
    let opts = ArrowOptions {
        batch_size: 3,
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::None,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // Feed 5 records with batch_size=3
    assert!(builder.append_record(b"AAA").unwrap().is_none());
    assert!(builder.append_record(b"BBB").unwrap().is_none());
    let batch1 = builder.append_record(b"CCC").unwrap();
    assert!(batch1.is_some());
    assert_eq!(batch1.unwrap().num_rows(), 3);

    // Continue after auto-flush
    assert!(builder.append_record(b"DDD").unwrap().is_none());
    assert!(builder.append_record(b"EEE").unwrap().is_none());

    let batch2 = builder.flush().unwrap().unwrap();
    assert_eq!(batch2.num_rows(), 2);

    let col = batch2.column(0).as_string::<i32>();
    assert_eq!(col.value(0), "DDD");
    assert_eq!(col.value(1), "EEE");
}

// ===================================================================
// 21. Empty flush returns None
// ===================================================================

#[test]
fn empty_builder_flush_returns_none() {
    let schema = Schema::from_fields(vec![make_field("X", FieldKind::Alphanum { len: 1 }, 0, 1)]);
    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    assert!(builder.flush().unwrap().is_none());
}

// ===================================================================
// 22. Group with OCCURS → FixedSizeList of Struct
// ===================================================================

#[test]
fn group_with_occurs_maps_to_list_of_struct() {
    let mut group = Field::new(5, "LINE-ITEM".to_string());
    group.path = "LINE-ITEM".to_string();
    group.occurs = Some(Occurs::Fixed { count: 10 });

    let mut desc = Field::with_kind(10, "DESC".to_string(), FieldKind::Alphanum { len: 20 });
    desc.path = "LINE-ITEM.DESC".to_string();
    desc.offset = 0;
    desc.len = 20;

    let mut qty = Field::with_kind(
        10,
        "QTY".to_string(),
        FieldKind::BinaryInt {
            bits: 32,
            signed: true,
        },
    );
    qty.path = "LINE-ITEM.QTY".to_string();
    qty.offset = 20;
    qty.len = 4;

    group.children = vec![desc, qty];

    let schema = Schema::from_fields(vec![group]);
    let opts = ArrowOptions {
        flatten_groups: false,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(arrow.fields().len(), 1);

    // Should be FixedSizeList(Struct(...), 10)
    match arrow.field(0).data_type() {
        DataType::FixedSizeList(inner, size) => {
            assert_eq!(*size, 10);
            match inner.data_type() {
                DataType::Struct(children) => {
                    assert_eq!(children.len(), 2);
                    assert_eq!(children[0].name(), "DESC");
                    assert_eq!(children[1].name(), "QTY");
                }
                other => panic!("Expected Struct inside list, got {other:?}"),
            }
        }
        other => panic!("Expected FixedSizeList, got {other:?}"),
    }
}

// ===================================================================
// 23. Unsigned binary int edge values
// ===================================================================

#[test]
fn binary_int_boundary_values() {
    let schema = Schema::from_fields(vec![
        make_field(
            "I16_MIN",
            FieldKind::BinaryInt {
                bits: 16,
                signed: true,
            },
            0,
            2,
        ),
        make_field(
            "I16_MAX",
            FieldKind::BinaryInt {
                bits: 16,
                signed: true,
            },
            2,
            2,
        ),
        make_field(
            "U16_MAX",
            FieldKind::BinaryInt {
                bits: 16,
                signed: false,
            },
            4,
            2,
        ),
    ]);
    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    let mut record = Vec::new();
    record.extend_from_slice(&i16::MIN.to_be_bytes());
    record.extend_from_slice(&i16::MAX.to_be_bytes());
    record.extend_from_slice(&u16::MAX.to_be_bytes());
    builder.append_record(&record).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    let i16_min = batch
        .column(0)
        .as_any()
        .downcast_ref::<Int16Array>()
        .unwrap();
    assert_eq!(i16_min.value(0), i16::MIN);
    let i16_max = batch
        .column(1)
        .as_any()
        .downcast_ref::<Int16Array>()
        .unwrap();
    assert_eq!(i16_max.value(0), i16::MAX);
    let u16_max = batch
        .column(2)
        .as_any()
        .downcast_ref::<UInt16Array>()
        .unwrap();
    assert_eq!(u16_max.value(0), u16::MAX);
}

// ===================================================================
// 24. Parquet with compression
// ===================================================================

#[test]
fn parquet_zstd_compression_roundtrip() {
    let schema = Schema::from_fields(vec![make_field(
        "DATA",
        FieldKind::Alphanum { len: 10 },
        0,
        10,
    )]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::Zstd,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    builder.append_record(b"COMPRESSED").unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader = ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap()
        .build()
        .unwrap();
    let read_batches: Vec<_> = reader.map(|b| b.unwrap()).collect();
    assert_eq!(read_batches[0].num_rows(), 1);

    let col = read_batches[0].column(0).as_string::<i32>();
    assert_eq!(col.value(0), "COMPRESSED");
}
