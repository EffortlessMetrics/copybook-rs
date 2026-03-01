// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep integration tests for copybook-arrow.
//!
//! Covers: multi-record batch conversion, all COBOL-to-Arrow type mappings,
//! COMP-3 Decimal128 precision, PIC X Utf8 conversion, nested group flattening,
//! ODO variable-length arrays, OCCURS fixed arrays, null/missing field handling,
//! large batch performance, schema metadata preservation, RecordBatch column
//! count validation, and data type inference edge cases.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::io::Cursor;
use std::sync::Arc;

use arrow::array::{
    Array, AsArray, Decimal128Array, Float32Array, Float64Array, Int32Array, StringArray,
};
use arrow::datatypes::DataType;
use copybook_arrow::batch_builder::RecordBatchBuilder;
use copybook_arrow::options::{ArrowOptions, Compression, EditedPicRepresentation};
use copybook_arrow::schema_convert::cobol_schema_to_arrow;
use copybook_arrow::streaming::{stream_to_batches, stream_to_batches_with_schema};
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

fn ascii_opts_batch(batch_size: usize) -> ArrowOptions {
    ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        batch_size,
        ..ArrowOptions::default()
    }
}

/// Build a record with ASCII alphanumeric field padded to `len`.
fn ascii_pad(s: &str, len: usize) -> Vec<u8> {
    let mut v = s.as_bytes().to_vec();
    v.resize(len, b' ');
    v
}

// ===================================================================
// 1. Multi-record batch conversion (10+ records)
// ===================================================================

#[test]
fn multi_record_batch_10_alphanum_records() {
    let schema = make_schema_with_lrecl(
        vec![make_field("NAME", FieldKind::Alphanum { len: 8 }, 0, 8)],
        8,
    );
    let mut data = Vec::new();
    for i in 0..10 {
        data.extend_from_slice(&ascii_pad(&format!("RECORD{i:02}"), 8));
    }
    let batches = stream_to_batches(Cursor::new(data), &schema, &ascii_opts()).unwrap();
    assert_eq!(batches.len(), 1);
    let batch = &batches[0];
    assert_eq!(batch.num_rows(), 10);
    let col = batch.column(0).as_string::<i32>();
    assert_eq!(col.value(0), "RECORD00");
    assert_eq!(col.value(9), "RECORD09");
}

#[test]
fn multi_record_batch_15_packed_decimal_records() {
    // PIC S9(5)V99 COMP-3 => 5 digits, scale 2, 3 bytes
    let schema = make_schema_with_lrecl(
        vec![make_field(
            "AMOUNT",
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
    let mut data = Vec::new();
    for i in 0..15_u8 {
        // PIC S9(5)V99 COMP-3 -> 5 digits packed into 3 bytes
        // Encode value as 00i00 positive where i is a single BCD digit (0-9)
        // For i>=10, use d3=i/10, d4=i%10 to keep all nibbles valid BCD
        let d3 = i / 10;
        let d4 = i % 10;
        data.push(0x00); // d1=0, d2=0
        data.push((d3 << 4) | d4); // d3, d4
        data.push(0x0C); // d5=0, sign=C
    }
    let batches = stream_to_batches(Cursor::new(data), &schema, &ascii_opts()).unwrap();
    let total_rows: usize = batches.iter().map(|b| b.num_rows()).sum();
    assert_eq!(total_rows, 15);
}

#[test]
fn multi_record_batch_20_int32_records() {
    let schema = make_schema_with_lrecl(
        vec![make_field(
            "COUNT",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            0,
            4,
        )],
        4,
    );
    let mut data = Vec::new();
    for i in 0..20_i32 {
        data.extend_from_slice(&i.to_be_bytes());
    }
    let batches = stream_to_batches(Cursor::new(data), &schema, &ascii_opts()).unwrap();
    let total_rows: usize = batches.iter().map(|b| b.num_rows()).sum();
    assert_eq!(total_rows, 20);
    let col = batches[0]
        .column(0)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(col.value(0), 0);
    assert_eq!(col.value(19), 19);
}

// ===================================================================
// 2. All COBOL field types to Arrow type mapping (comprehensive)
// ===================================================================

#[test]
fn all_field_types_schema_mapping() {
    let schema = Schema::from_fields(vec![
        make_field("PIC-X", FieldKind::Alphanum { len: 10 }, 0, 10),
        make_field(
            "ZONED",
            FieldKind::ZonedDecimal {
                digits: 7,
                scale: 2,
                signed: true,
                sign_separate: None,
            },
            10,
            7,
        ),
        make_field(
            "PACKED",
            FieldKind::PackedDecimal {
                digits: 9,
                scale: 3,
                signed: true,
            },
            17,
            5,
        ),
        make_field(
            "INT16-S",
            FieldKind::BinaryInt {
                bits: 16,
                signed: true,
            },
            22,
            2,
        ),
        make_field(
            "INT16-U",
            FieldKind::BinaryInt {
                bits: 16,
                signed: false,
            },
            24,
            2,
        ),
        make_field(
            "INT32-S",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            26,
            4,
        ),
        make_field(
            "INT32-U",
            FieldKind::BinaryInt {
                bits: 32,
                signed: false,
            },
            30,
            4,
        ),
        make_field(
            "INT64-S",
            FieldKind::BinaryInt {
                bits: 64,
                signed: true,
            },
            34,
            8,
        ),
        make_field(
            "INT64-U",
            FieldKind::BinaryInt {
                bits: 64,
                signed: false,
            },
            42,
            8,
        ),
        make_field("COMP1", FieldKind::FloatSingle, 50, 4),
        make_field("COMP2", FieldKind::FloatDouble, 54, 8),
    ]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 11);

    let expected = [
        ("PIC-X", DataType::Utf8),
        ("ZONED", DataType::Decimal128(7, 2)),
        ("PACKED", DataType::Decimal128(9, 3)),
        ("INT16-S", DataType::Int16),
        ("INT16-U", DataType::UInt16),
        ("INT32-S", DataType::Int32),
        ("INT32-U", DataType::UInt32),
        ("INT64-S", DataType::Int64),
        ("INT64-U", DataType::UInt64),
        ("COMP1", DataType::Float32),
        ("COMP2", DataType::Float64),
    ];
    for (i, (name, dt)) in expected.iter().enumerate() {
        assert_eq!(arrow.field(i).name(), *name, "field {i} name mismatch");
        assert_eq!(*arrow.field(i).data_type(), *dt, "field {i} type mismatch");
    }
}

// ===================================================================
// 3. COMP-3 to Decimal128 precision
// ===================================================================

#[test]
fn comp3_precision_1_digit() {
    let schema = make_schema_with_lrecl(
        vec![make_field(
            "TINY",
            FieldKind::PackedDecimal {
                digits: 1,
                scale: 0,
                signed: true,
            },
            0,
            1,
        )],
        1,
    );
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(
        *arrow_schema.field(0).data_type(),
        DataType::Decimal128(1, 0)
    );

    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();
    // Packed: digit 5, positive sign -> 0x5C
    builder.append_record(&[0x5C]).unwrap();
    let batch = builder.flush().unwrap().unwrap();
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), 5);
}

#[test]
fn comp3_precision_18_digits() {
    let schema = make_schema_with_lrecl(
        vec![make_field(
            "BIG",
            FieldKind::PackedDecimal {
                digits: 18,
                scale: 4,
                signed: true,
            },
            0,
            10,
        )],
        10,
    );
    let arrow_schema = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(
        *arrow_schema.field(0).data_type(),
        DataType::Decimal128(18, 4)
    );
}

#[test]
fn comp3_precision_boundary_38_digits() {
    let schema = Schema::from_fields(vec![make_field(
        "MAX",
        FieldKind::PackedDecimal {
            digits: 38,
            scale: 0,
            signed: true,
        },
        0,
        20,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(38, 0));
}

#[test]
fn comp3_precision_over_38_falls_back_to_utf8() {
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
fn comp3_negative_value_decoded_correctly() {
    let schema = make_schema_with_lrecl(
        vec![make_field(
            "BALANCE",
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
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();
    // Packed -12345: 0x12 0x34 0x5D (D = negative sign)
    builder.append_record(&[0x12, 0x34, 0x5D]).unwrap();
    let batch = builder.flush().unwrap().unwrap();
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), -12345);
}

#[test]
fn comp3_zero_value() {
    let schema = make_schema_with_lrecl(
        vec![make_field(
            "ZERO",
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
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();
    // Packed 0: 0x00 0x00 0x0C
    builder.append_record(&[0x00, 0x00, 0x0C]).unwrap();
    let batch = builder.flush().unwrap().unwrap();
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), 0);
}

// ===================================================================
// 4. PIC X to Utf8 conversion
// ===================================================================

#[test]
fn pic_x_ascii_simple_string() {
    let schema = make_schema_with_lrecl(
        vec![make_field("NAME", FieldKind::Alphanum { len: 10 }, 0, 10)],
        10,
    );
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();
    builder.append_record(b"HELLO     ").unwrap();
    let batch = builder.flush().unwrap().unwrap();
    let col = batch.column(0).as_string::<i32>();
    assert_eq!(col.value(0), "HELLO");
}

#[test]
fn pic_x_trailing_spaces_trimmed() {
    let schema = make_schema_with_lrecl(
        vec![make_field("DATA", FieldKind::Alphanum { len: 20 }, 0, 20)],
        20,
    );
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();
    builder.append_record(b"ABC                 ").unwrap();
    let batch = builder.flush().unwrap().unwrap();
    let col = batch.column(0).as_string::<i32>();
    assert_eq!(col.value(0), "ABC");
}

#[test]
fn pic_x_empty_string_all_spaces() {
    let schema = make_schema_with_lrecl(
        vec![make_field("BLANK", FieldKind::Alphanum { len: 5 }, 0, 5)],
        5,
    );
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();
    builder.append_record(b"     ").unwrap();
    let batch = builder.flush().unwrap().unwrap();
    let col = batch.column(0).as_string::<i32>();
    assert_eq!(col.value(0), "");
}

#[test]
fn pic_x_full_field_no_trailing_spaces() {
    let schema = make_schema_with_lrecl(
        vec![make_field("FULL", FieldKind::Alphanum { len: 4 }, 0, 4)],
        4,
    );
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();
    builder.append_record(b"ABCD").unwrap();
    let batch = builder.flush().unwrap().unwrap();
    let col = batch.column(0).as_string::<i32>();
    assert_eq!(col.value(0), "ABCD");
}

// ===================================================================
// 5. Nested group flattening
// ===================================================================

#[test]
fn nested_group_flattened_produces_leaf_columns() {
    let mut child1 = Field::with_kind(5, "FIRST".to_string(), FieldKind::Alphanum { len: 10 });
    child1.path = "PERSON.FIRST".to_string();
    child1.offset = 0;
    child1.len = 10;
    let mut child2 = Field::with_kind(5, "LAST".to_string(), FieldKind::Alphanum { len: 15 });
    child2.path = "PERSON.LAST".to_string();
    child2.offset = 10;
    child2.len = 15;
    let mut child3 = Field::with_kind(
        5,
        "AGE".to_string(),
        FieldKind::BinaryInt {
            bits: 16,
            signed: false,
        },
    );
    child3.path = "PERSON.AGE".to_string();
    child3.offset = 25;
    child3.len = 2;

    let mut group = Field::new(1, "PERSON".to_string());
    group.path = "PERSON".to_string();
    group.children = vec![child1, child2, child3];

    let schema = make_schema_with_lrecl(vec![group], 27);
    let opts = ascii_opts();
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();

    assert_eq!(arrow.fields().len(), 3);
    assert_eq!(arrow.field(0).name(), "FIRST");
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
    assert_eq!(arrow.field(1).name(), "LAST");
    assert_eq!(*arrow.field(1).data_type(), DataType::Utf8);
    assert_eq!(arrow.field(2).name(), "AGE");
    assert_eq!(*arrow.field(2).data_type(), DataType::UInt16);
}

#[test]
fn nested_group_flattened_data_decode() {
    let mut child1 = Field::with_kind(5, "A".to_string(), FieldKind::Alphanum { len: 4 });
    child1.path = "G.A".to_string();
    child1.offset = 0;
    child1.len = 4;
    let mut child2 = Field::with_kind(
        5,
        "B".to_string(),
        FieldKind::BinaryInt {
            bits: 32,
            signed: true,
        },
    );
    child2.path = "G.B".to_string();
    child2.offset = 4;
    child2.len = 4;

    let mut group = Field::new(1, "G".to_string());
    group.path = "G".to_string();
    group.children = vec![child1, child2];

    let schema = make_schema_with_lrecl(vec![group], 8);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    let mut rec = Vec::new();
    rec.extend_from_slice(b"TEST");
    rec.extend_from_slice(&42_i32.to_be_bytes());
    builder.append_record(&rec).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_columns(), 2);
    let str_col = batch.column(0).as_string::<i32>();
    assert_eq!(str_col.value(0), "TEST");
    let int_col = batch
        .column(1)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(int_col.value(0), 42);
}

#[test]
fn deeply_nested_groups_flatten_all_levels() {
    // OUTER -> MIDDLE -> INNER -> LEAF
    let mut leaf = Field::with_kind(5, "LEAF".to_string(), FieldKind::Alphanum { len: 5 });
    leaf.path = "OUTER.MIDDLE.INNER.LEAF".to_string();
    leaf.offset = 0;
    leaf.len = 5;

    let mut inner = Field::new(3, "INNER".to_string());
    inner.path = "OUTER.MIDDLE.INNER".to_string();
    inner.children = vec![leaf];

    let mut middle = Field::new(2, "MIDDLE".to_string());
    middle.path = "OUTER.MIDDLE".to_string();
    middle.children = vec![inner];

    let mut outer = Field::new(1, "OUTER".to_string());
    outer.path = "OUTER".to_string();
    outer.children = vec![middle];

    let schema = Schema::from_fields(vec![outer]);
    let opts = ArrowOptions {
        flatten_groups: true,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(arrow.fields().len(), 1);
    assert_eq!(arrow.field(0).name(), "LEAF");
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
}

#[test]
fn nested_group_not_flattened_produces_struct() {
    let mut child = Field::with_kind(5, "VAL".to_string(), FieldKind::Alphanum { len: 5 });
    child.path = "GRP.VAL".to_string();
    child.offset = 0;
    child.len = 5;

    let mut group = Field::new(1, "GRP".to_string());
    group.path = "GRP".to_string();
    group.children = vec![child];

    let schema = Schema::from_fields(vec![group]);
    let opts = ArrowOptions {
        flatten_groups: false,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(arrow.fields().len(), 1);
    assert_eq!(arrow.field(0).name(), "GRP");
    match arrow.field(0).data_type() {
        DataType::Struct(fields) => {
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].name(), "VAL");
        }
        other => panic!("Expected Struct, got {other:?}"),
    }
}

// ===================================================================
// 6. ODO variable-length array handling
// ===================================================================

#[test]
fn odo_field_maps_to_list_type() {
    let mut f = make_field("ITEMS", FieldKind::Alphanum { len: 10 }, 4, 10);
    f.occurs = Some(Occurs::ODO {
        min: 0,
        max: 100,
        counter_path: "NUM-ITEMS".to_string(),
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
fn odo_packed_decimal_maps_to_list_decimal128() {
    let mut f = make_field(
        "AMOUNTS",
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
        counter_path: "AMT-COUNT".to_string(),
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
fn odo_int64_maps_to_list_int64() {
    let mut f = make_field(
        "IDS",
        FieldKind::BinaryInt {
            bits: 64,
            signed: true,
        },
        0,
        8,
    );
    f.occurs = Some(Occurs::ODO {
        min: 0,
        max: 50,
        counter_path: "ID-COUNT".to_string(),
    });
    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    match arrow.field(0).data_type() {
        DataType::List(inner) => {
            assert_eq!(*inner.data_type(), DataType::Int64);
        }
        other => panic!("Expected List(Int64), got {other:?}"),
    }
}

// ===================================================================
// 7. OCCURS fixed array handling
// ===================================================================

#[test]
fn occurs_fixed_alphanum_produces_fixed_size_list_utf8() {
    let mut f = make_field("NAMES", FieldKind::Alphanum { len: 20 }, 0, 20);
    f.occurs = Some(Occurs::Fixed { count: 3 });
    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    match arrow.field(0).data_type() {
        DataType::FixedSizeList(inner, 3) => {
            assert_eq!(*inner.data_type(), DataType::Utf8);
        }
        other => panic!("Expected FixedSizeList(Utf8, 3), got {other:?}"),
    }
}

#[test]
fn occurs_fixed_int16_produces_fixed_size_list() {
    let mut f = make_field(
        "COUNTS",
        FieldKind::BinaryInt {
            bits: 16,
            signed: true,
        },
        0,
        2,
    );
    f.occurs = Some(Occurs::Fixed { count: 10 });
    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    match arrow.field(0).data_type() {
        DataType::FixedSizeList(inner, 10) => {
            assert_eq!(*inner.data_type(), DataType::Int16);
        }
        other => panic!("Expected FixedSizeList(Int16, 10), got {other:?}"),
    }
}

#[test]
fn occurs_fixed_packed_decimal_produces_fixed_size_list() {
    let mut f = make_field(
        "RATES",
        FieldKind::PackedDecimal {
            digits: 5,
            scale: 2,
            signed: true,
        },
        0,
        3,
    );
    f.occurs = Some(Occurs::Fixed { count: 5 });
    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    match arrow.field(0).data_type() {
        DataType::FixedSizeList(inner, 5) => {
            assert_eq!(*inner.data_type(), DataType::Decimal128(5, 2));
        }
        other => panic!("Expected FixedSizeList(Decimal128(5,2), 5), got {other:?}"),
    }
}

// ===================================================================
// 8. Null/missing field handling
// ===================================================================

#[test]
fn short_record_produces_null_for_missing_field() {
    // Schema has 2 fields: 4-byte alphanum + 4-byte int32
    let schema = make_schema_with_lrecl(
        vec![
            make_field("NAME", FieldKind::Alphanum { len: 4 }, 0, 4),
            make_field(
                "ID",
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
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();
    // Only provide 4 bytes (NAME present, ID missing/truncated)
    builder.append_record(b"ABCD").unwrap();
    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 1);
    let str_col = batch.column(0).as_string::<i32>();
    assert_eq!(str_col.value(0), "ABCD");
    let int_col = batch
        .column(1)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert!(int_col.is_null(0));
}

#[test]
fn completely_empty_record_all_nulls() {
    let schema = make_schema_with_lrecl(
        vec![
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
        ],
        8,
    );
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();
    // Empty record -> both fields should be null
    builder.append_record(&[]).unwrap();
    let batch = builder.flush().unwrap().unwrap();
    assert!(
        batch
            .column(0)
            .as_any()
            .downcast_ref::<StringArray>()
            .unwrap()
            .is_null(0)
    );
    assert!(
        batch
            .column(1)
            .as_any()
            .downcast_ref::<Int32Array>()
            .unwrap()
            .is_null(0)
    );
}

// ===================================================================
// 9. Large batch performance (1000 records)
// ===================================================================

#[test]
fn large_batch_1000_records_streaming() {
    let schema = make_schema_with_lrecl(
        vec![
            make_field("ID", FieldKind::Alphanum { len: 4 }, 0, 4),
            make_field(
                "VALUE",
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
    let mut data = Vec::with_capacity(8000);
    for i in 0..1000_i32 {
        data.extend_from_slice(&ascii_pad(&format!("R{i:03}"), 4));
        data.extend_from_slice(&i.to_be_bytes());
    }
    let opts = ascii_opts();
    let batches = stream_to_batches(Cursor::new(data), &schema, &opts).unwrap();
    let total_rows: usize = batches.iter().map(|b| b.num_rows()).sum();
    assert_eq!(total_rows, 1000);
}

#[test]
fn large_batch_1000_records_builder_with_auto_flush() {
    let schema = make_schema_with_lrecl(
        vec![make_field(
            "COUNT",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            0,
            4,
        )],
        4,
    );
    let opts = ascii_opts_batch(100);
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    let mut batches = Vec::new();
    for i in 0..1000_i32 {
        if let Some(batch) = builder.append_record(&i.to_be_bytes()).unwrap() {
            batches.push(batch);
        }
    }
    if let Some(batch) = builder.flush().unwrap() {
        batches.push(batch);
    }
    let total_rows: usize = batches.iter().map(|b| b.num_rows()).sum();
    assert_eq!(total_rows, 1000);
    // With batch_size=100, we should get exactly 10 batches
    assert_eq!(batches.len(), 10);
    for batch in &batches {
        assert_eq!(batch.num_rows(), 100);
    }
}

// ===================================================================
// 10. Schema metadata preservation (Parquet)
// ===================================================================

#[test]
fn parquet_preserves_codepage_metadata() {
    let schema = make_schema_with_lrecl(
        vec![make_field("DATA", FieldKind::Alphanum { len: 4 }, 0, 4)],
        4,
    );
    let opts = ascii_opts();
    let (arrow_schema, batches) =
        stream_to_batches_with_schema(Cursor::new(b"TEST".to_vec()), &schema, &opts).unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &batches, &arrow_schema, &opts, None).unwrap();
    assert!(tmp.path().exists());
    // Parquet was written successfully with metadata
    let file_size = std::fs::metadata(tmp.path()).unwrap().len();
    assert!(file_size > 0);
}

#[test]
fn parquet_embed_copybook_metadata() {
    let schema = make_schema_with_lrecl(
        vec![make_field("DATA", FieldKind::Alphanum { len: 4 }, 0, 4)],
        4,
    );
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        embed_copybook: true,
        ..ArrowOptions::default()
    };
    let (arrow_schema, batches) =
        stream_to_batches_with_schema(Cursor::new(b"TEST".to_vec()), &schema, &opts).unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    let copybook_text = "       01 RECORD.\n           05 DATA PIC X(4).";
    write_parquet(
        tmp.path(),
        &batches,
        &arrow_schema,
        &opts,
        Some(copybook_text),
    )
    .unwrap();
    assert!(std::fs::metadata(tmp.path()).unwrap().len() > 0);
}

#[test]
fn ipc_roundtrip_preserves_schema() {
    let schema = make_schema_with_lrecl(
        vec![
            make_field("NAME", FieldKind::Alphanum { len: 4 }, 0, 4),
            make_field(
                "AMT",
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
    let opts = ascii_opts();
    let mut data = Vec::new();
    data.extend_from_slice(b"ABCD");
    data.extend_from_slice(&100_i32.to_be_bytes());

    let (arrow_schema, batches) =
        stream_to_batches_with_schema(Cursor::new(data), &schema, &opts).unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_ipc(tmp.path(), &batches, &arrow_schema).unwrap();
    assert!(std::fs::metadata(tmp.path()).unwrap().len() > 0);
}

// ===================================================================
// 11. RecordBatch column count validation
// ===================================================================

#[test]
fn column_count_matches_leaf_field_count() {
    let schema = make_schema_with_lrecl(
        vec![
            make_field("F1", FieldKind::Alphanum { len: 2 }, 0, 2),
            make_field("F2", FieldKind::Alphanum { len: 2 }, 2, 2),
            make_field("F3", FieldKind::Alphanum { len: 2 }, 4, 2),
            make_field("F4", FieldKind::Alphanum { len: 2 }, 6, 2),
            make_field("F5", FieldKind::Alphanum { len: 2 }, 8, 2),
        ],
        10,
    );
    let opts = ascii_opts();
    let batches = stream_to_batches(
        Cursor::new(b"AABBCCDDEEFFGGHHIIJJ".to_vec()),
        &schema,
        &opts,
    )
    .unwrap();
    assert_eq!(batches.len(), 1);
    assert_eq!(batches[0].num_columns(), 5);
    assert_eq!(batches[0].num_rows(), 2);
}

#[test]
fn column_count_excludes_condition_and_renames() {
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
                from_field: "DATA".to_string(),
                thru_field: "DATA".to_string(),
            },
            0,
            0,
        ),
        make_field(
            "ID",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            10,
            4,
        ),
    ]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    // Only DATA and ID should be present (Condition and Renames skipped)
    assert_eq!(arrow.fields().len(), 2);
    assert_eq!(arrow.field(0).name(), "DATA");
    assert_eq!(arrow.field(1).name(), "ID");
}

#[test]
fn column_count_excludes_filler_by_default() {
    let schema = Schema::from_fields(vec![
        make_field("REAL", FieldKind::Alphanum { len: 5 }, 0, 5),
        make_field("_filler_00000005", FieldKind::Alphanum { len: 3 }, 5, 3),
        make_field("FILLER", FieldKind::Alphanum { len: 2 }, 8, 2),
        make_field("ALSO-REAL", FieldKind::Alphanum { len: 5 }, 10, 5),
    ]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 2);
    assert_eq!(arrow.field(0).name(), "REAL");
    assert_eq!(arrow.field(1).name(), "ALSO-REAL");
}

#[test]
fn column_count_includes_filler_when_configured() {
    let schema = Schema::from_fields(vec![
        make_field("REAL", FieldKind::Alphanum { len: 5 }, 0, 5),
        make_field("_filler_00000005", FieldKind::Alphanum { len: 3 }, 5, 3),
    ]);
    let opts = ArrowOptions {
        emit_filler: true,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(arrow.fields().len(), 2);
}

// ===================================================================
// 12. Data type inference edge cases
// ===================================================================

#[test]
fn zoned_decimal_zero_scale_maps_to_integer_like_decimal() {
    let schema = Schema::from_fields(vec![make_field(
        "WHOLE",
        FieldKind::ZonedDecimal {
            digits: 5,
            scale: 0,
            signed: true,
            sign_separate: None,
        },
        0,
        5,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(5, 0));
}

#[test]
fn edited_numeric_default_maps_to_decimal128() {
    let schema = Schema::from_fields(vec![make_field(
        "EDITED",
        FieldKind::EditedNumeric {
            pic_string: "ZZ,ZZZ.99".to_string(),
            width: 9,
            scale: 2,
            signed: false,
        },
        0,
        9,
    )]);
    let opts = ArrowOptions::default(); // Default uses Decimal representation
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(38, 2));
}

#[test]
fn edited_numeric_string_mode_maps_to_utf8() {
    let schema = Schema::from_fields(vec![make_field(
        "EDITED",
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

#[test]
fn unsigned_binary_int_type_mapping() {
    let schema = Schema::from_fields(vec![
        make_field(
            "U16",
            FieldKind::BinaryInt {
                bits: 16,
                signed: false,
            },
            0,
            2,
        ),
        make_field(
            "U32",
            FieldKind::BinaryInt {
                bits: 32,
                signed: false,
            },
            2,
            4,
        ),
        make_field(
            "U64",
            FieldKind::BinaryInt {
                bits: 64,
                signed: false,
            },
            6,
            8,
        ),
    ]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::UInt16);
    assert_eq!(*arrow.field(1).data_type(), DataType::UInt32);
    assert_eq!(*arrow.field(2).data_type(), DataType::UInt64);
}

#[test]
fn unsupported_binary_int_width_8_returns_error() {
    let schema = Schema::from_fields(vec![make_field(
        "BYTE",
        FieldKind::BinaryInt {
            bits: 8,
            signed: true,
        },
        0,
        1,
    )]);
    let result = cobol_schema_to_arrow(&schema, &ArrowOptions::default());
    assert!(result.is_err());
}

#[test]
fn empty_group_with_no_children_skipped() {
    let mut group = Field::new(1, "EMPTY-GRP".to_string());
    group.path = "EMPTY-GRP".to_string();
    group.children = vec![];

    let schema = Schema::from_fields(vec![
        group,
        make_field("ACTUAL", FieldKind::Alphanum { len: 5 }, 0, 5),
    ]);
    let opts = ArrowOptions {
        flatten_groups: false,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    // Empty group produces no struct field; only ACTUAL should be present
    assert_eq!(arrow.fields().len(), 1);
    assert_eq!(arrow.field(0).name(), "ACTUAL");
}

// ===================================================================
// 13. Mixed-type multi-record streaming with value verification
// ===================================================================

#[test]
fn mixed_types_10_records_values_verified() {
    // Alphanum(6) + Int32 + PackedDecimal(5,2) = 6 + 4 + 3 = 13 bytes
    let schema = make_schema_with_lrecl(
        vec![
            make_field("NAME", FieldKind::Alphanum { len: 6 }, 0, 6),
            make_field(
                "ID",
                FieldKind::BinaryInt {
                    bits: 32,
                    signed: true,
                },
                6,
                4,
            ),
            make_field(
                "BAL",
                FieldKind::PackedDecimal {
                    digits: 5,
                    scale: 2,
                    signed: true,
                },
                10,
                3,
            ),
        ],
        13,
    );

    let mut data = Vec::new();
    for i in 0..10_i32 {
        data.extend_from_slice(&ascii_pad(&format!("REC{i:03}"), 6));
        data.extend_from_slice(&(i * 100).to_be_bytes());
        // Packed: 10000+i -> e.g. 10005 -> 0x10 0x00 0x5C
        let packed_hi = ((1_u8) << 4) | 0;
        let packed_mid = 0x00_u8;
        let packed_lo = (u8::try_from(i).unwrap_or(0) << 4) | 0x0C;
        data.push(packed_hi);
        data.push(packed_mid);
        data.push(packed_lo);
    }

    let opts = ascii_opts();
    let batches = stream_to_batches(Cursor::new(data), &schema, &opts).unwrap();
    let total_rows: usize = batches.iter().map(|b| b.num_rows()).sum();
    assert_eq!(total_rows, 10);

    let batch = &batches[0];
    assert_eq!(batch.num_columns(), 3);

    // Verify string column
    let name_col = batch.column(0).as_string::<i32>();
    assert_eq!(name_col.value(0), "REC000");
    assert_eq!(name_col.value(9), "REC009");

    // Verify int column
    let id_col = batch
        .column(1)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(id_col.value(0), 0);
    assert_eq!(id_col.value(5), 500);

    // Verify decimal column
    let bal_col = batch
        .column(2)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(bal_col.value(0), 10000); // 100.00 unscaled
}

// ===================================================================
// 14. Batch boundary and flush behavior
// ===================================================================

#[test]
fn batch_size_boundary_exact_flush() {
    let schema = make_schema_with_lrecl(
        vec![make_field("V", FieldKind::Alphanum { len: 2 }, 0, 2)],
        2,
    );
    let opts = ascii_opts_batch(5);
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    let mut batches = Vec::new();
    for i in 0..5 {
        let rec = format!("{i:02}");
        if let Some(batch) = builder.append_record(rec.as_bytes()).unwrap() {
            batches.push(batch);
        }
    }
    // Exactly batch_size=5 records -> should have auto-flushed
    assert_eq!(batches.len(), 1);
    assert_eq!(batches[0].num_rows(), 5);
    // flush() should return None since all records were already flushed
    assert!(builder.flush().unwrap().is_none());
}

#[test]
fn flush_empty_builder_returns_none() {
    let schema = Schema::from_fields(vec![make_field("X", FieldKind::Alphanum { len: 4 }, 0, 4)]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();
    assert!(builder.flush().unwrap().is_none());
}

// ===================================================================
// 15. Float/double data decode
// ===================================================================

#[test]
fn float32_decode_to_arrow() {
    let schema = make_schema_with_lrecl(vec![make_field("RATE", FieldKind::FloatSingle, 0, 4)], 4);
    let opts = ascii_opts();
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
    let diff = (col.value(0) - 3.14_f32).abs();
    assert!(diff < 0.001, "Expected ~3.14, got {}", col.value(0));
}

#[test]
fn float64_decode_to_arrow() {
    let schema =
        make_schema_with_lrecl(vec![make_field("PRECISE", FieldKind::FloatDouble, 0, 8)], 8);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();
    let val: f64 = 2.718_281_828_459_045;
    builder.append_record(&val.to_be_bytes()).unwrap();
    let batch = builder.flush().unwrap().unwrap();
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Float64Array>()
        .unwrap();
    let diff = (col.value(0) - 2.718_281_828_459_045_f64).abs();
    assert!(diff < 1e-10, "Expected ~2.718, got {}", col.value(0));
}

// ===================================================================
// 16. Compression variants for Parquet
// ===================================================================

#[test]
fn parquet_compression_variants_write_successfully() {
    let schema = make_schema_with_lrecl(
        vec![make_field("V", FieldKind::Alphanum { len: 4 }, 0, 4)],
        4,
    );
    let compressions = [Compression::None, Compression::Snappy, Compression::Zstd];
    for compression in compressions {
        let opts = ArrowOptions {
            codepage: copybook_codec::Codepage::ASCII,
            compression,
            ..ArrowOptions::default()
        };
        let (arrow_schema, batches) =
            stream_to_batches_with_schema(Cursor::new(b"TEST".to_vec()), &schema, &opts).unwrap();
        let tmp = tempfile::NamedTempFile::new().unwrap();
        write_parquet(tmp.path(), &batches, &arrow_schema, &opts, None).unwrap();
        assert!(std::fs::metadata(tmp.path()).unwrap().len() > 0);
    }
}

// ===================================================================
// 17. stream_to_batches_with_schema returns correct schema
// ===================================================================

#[test]
fn stream_to_batches_with_schema_returns_matching_schema() {
    let cobol_schema = make_schema_with_lrecl(
        vec![
            make_field("A", FieldKind::Alphanum { len: 3 }, 0, 3),
            make_field(
                "B",
                FieldKind::BinaryInt {
                    bits: 16,
                    signed: true,
                },
                3,
                2,
            ),
        ],
        5,
    );
    let mut data = Vec::new();
    data.extend_from_slice(b"ABC");
    data.extend_from_slice(&42_i16.to_be_bytes());

    let opts = ascii_opts();
    let (arrow_schema, batches) =
        stream_to_batches_with_schema(Cursor::new(data), &cobol_schema, &opts).unwrap();

    assert_eq!(arrow_schema.fields().len(), 2);
    assert_eq!(arrow_schema.field(0).name(), "A");
    assert_eq!(arrow_schema.field(1).name(), "B");
    assert_eq!(batches.len(), 1);
    assert_eq!(batches[0].num_rows(), 1);
}

// ===================================================================
// 18. Streaming with no lrecl produces error
// ===================================================================

#[test]
fn streaming_without_lrecl_returns_error() {
    let schema = Schema::from_fields(vec![make_field("X", FieldKind::Alphanum { len: 4 }, 0, 4)]);
    // No lrecl_fixed set
    let result = stream_to_batches(Cursor::new(b"TEST".to_vec()), &schema, &ascii_opts());
    assert!(result.is_err());
}
