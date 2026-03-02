// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive Arrow integration tests for copybook-rs
//!
//! This test suite validates Arrow and Parquet conversion functionality with 55+ tests
//! covering: multi-record batch conversion, type mapping edge cases, Parquet roundtrip,
//! and error handling.

#![allow(clippy::unwrap_used)]
#![allow(clippy::expect_used)]
#![allow(clippy::panic)]
#![allow(deprecated)]

use arrow::array::{
    ArrayRef, AsArray, Decimal128Array, Float32Array, Float64Array, Int16Array, Int32Array,
    StringArray, UInt16Array,
};
use arrow::datatypes::{DataType, Field as ArrowField, Schema as ArrowSchema};
use arrow::record_batch::RecordBatch;
use copybook_arrow::batch_builder::RecordBatchBuilder;
use copybook_arrow::options::{ArrowOptions, Compression};
use copybook_arrow::schema_convert::cobol_schema_to_arrow;
use copybook_arrow::{
    ArrowError, ArrowWriter, json_to_record_batch, json_to_schema, write_ipc, write_parquet,
};
use copybook_core::schema::{Field, FieldKind, Occurs, Schema};
use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;
use serde_json::json;
use std::io::Cursor;
use std::sync::Arc;

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
        compression: Compression::None,
        ..ArrowOptions::default()
    }
}

// ===========================================================================
// Section 1: Multi-record batch conversion (12 tests)
// ===========================================================================

#[test]
fn batch_multiple_alphanum_records() {
    let schema = make_schema_with_lrecl(
        vec![make_field("NAME", FieldKind::Alphanum { len: 5 }, 0, 5)],
        5,
    );
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    builder.append_record(b"ALICE").unwrap();
    builder.append_record(b"BOB  ").unwrap();
    builder.append_record(b"CAROL").unwrap();
    let batch = builder.flush().unwrap().unwrap();

    assert_eq!(batch.num_rows(), 3);
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<StringArray>()
        .unwrap();
    assert_eq!(col.value(0), "ALICE");
    assert_eq!(col.value(1), "BOB");
    assert_eq!(col.value(2), "CAROL");
}

#[test]
fn batch_row_count_matches_record_count() {
    let schema = make_schema_with_lrecl(
        vec![make_field("X", FieldKind::Alphanum { len: 2 }, 0, 2)],
        2,
    );
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    for _ in 0..7 {
        builder.append_record(b"AB").unwrap();
    }
    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 7);
}

#[test]
fn batch_column_names_match_field_names() {
    let schema = Schema::from_fields(vec![
        make_field("CUSTOMER-ID", FieldKind::Alphanum { len: 4 }, 0, 4),
        make_field(
            "BALANCE",
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

    assert_eq!(arrow_schema.field(0).name(), "CUSTOMER-ID");
    assert_eq!(arrow_schema.field(1).name(), "BALANCE");
}

#[test]
fn batch_empty_input_zero_records() {
    let schema = make_schema_with_lrecl(
        vec![make_field("V", FieldKind::Alphanum { len: 4 }, 0, 4)],
        4,
    );
    let opts = ascii_opts();
    let reader = Cursor::new(Vec::<u8>::new());
    let batches = copybook_arrow::stream_to_batches(reader, &schema, &opts).unwrap();
    assert!(batches.is_empty());
}

#[test]
fn batch_single_record() {
    let schema = make_schema_with_lrecl(
        vec![make_field("V", FieldKind::Alphanum { len: 3 }, 0, 3)],
        3,
    );
    let opts = ascii_opts();
    let reader = Cursor::new(b"ABC".to_vec());
    let batches = copybook_arrow::stream_to_batches(reader, &schema, &opts).unwrap();
    assert_eq!(batches.len(), 1);
    assert_eq!(batches[0].num_rows(), 1);
}

#[test]
fn batch_auto_flush_at_batch_size() {
    let schema = make_schema_with_lrecl(
        vec![make_field("D", FieldKind::Alphanum { len: 2 }, 0, 2)],
        2,
    );
    let opts = ArrowOptions {
        batch_size: 3,
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::None,
        ..ArrowOptions::default()
    };
    // 7 records -> 2 full batches (3 each) + 1 partial (1)
    let data = b"AABBCCDDEEFFGG".to_vec();
    let reader = Cursor::new(data);
    let batches = copybook_arrow::stream_to_batches(reader, &schema, &opts).unwrap();
    assert_eq!(batches.len(), 3);
    assert_eq!(batches[0].num_rows(), 3);
    assert_eq!(batches[1].num_rows(), 3);
    assert_eq!(batches[2].num_rows(), 1);
}

#[test]
fn batch_exact_batch_size_no_partial() {
    let schema = make_schema_with_lrecl(
        vec![make_field("D", FieldKind::Alphanum { len: 2 }, 0, 2)],
        2,
    );
    let opts = ArrowOptions {
        batch_size: 3,
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::None,
        ..ArrowOptions::default()
    };
    // 6 records -> exactly 2 full batches
    let data = b"AABBCCDDEEFF".to_vec();
    let reader = Cursor::new(data);
    let batches = copybook_arrow::stream_to_batches(reader, &schema, &opts).unwrap();
    assert_eq!(batches.len(), 2);
    assert_eq!(batches[0].num_rows(), 3);
    assert_eq!(batches[1].num_rows(), 3);
}

#[test]
fn batch_multi_column_records() {
    let schema = Schema::from_fields(vec![
        make_field("TAG", FieldKind::Alphanum { len: 4 }, 0, 4),
        make_field(
            "QTY",
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

    let mut r1 = b"AAAA".to_vec();
    r1.extend_from_slice(&10_i32.to_be_bytes());
    builder.append_record(&r1).unwrap();

    let mut r2 = b"BBBB".to_vec();
    r2.extend_from_slice(&20_i32.to_be_bytes());
    builder.append_record(&r2).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 2);
    assert_eq!(batch.num_columns(), 2);

    let tag = batch
        .column(0)
        .as_any()
        .downcast_ref::<StringArray>()
        .unwrap();
    assert_eq!(tag.value(0), "AAAA");
    assert_eq!(tag.value(1), "BBBB");

    let qty = batch
        .column(1)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(qty.value(0), 10);
    assert_eq!(qty.value(1), 20);
}

#[test]
fn batch_flush_empty_returns_none() {
    let schema = Schema::from_fields(vec![make_field("X", FieldKind::Alphanum { len: 4 }, 0, 4)]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();
    let result = builder.flush().unwrap();
    assert!(result.is_none());
}

#[test]
fn batch_large_record_count() {
    let schema = make_schema_with_lrecl(
        vec![make_field("V", FieldKind::Alphanum { len: 1 }, 0, 1)],
        1,
    );
    let opts = ArrowOptions {
        batch_size: 1000,
        codepage: copybook_codec::Codepage::ASCII,
        ..ArrowOptions::default()
    };
    let data = vec![b'X'; 500];
    let reader = Cursor::new(data);
    let batches = copybook_arrow::stream_to_batches(reader, &schema, &opts).unwrap();
    let total_rows: usize = batches.iter().map(RecordBatch::num_rows).sum();
    assert_eq!(total_rows, 500);
}

#[test]
fn batch_with_schema_returns_matching_schema() {
    let schema = make_schema_with_lrecl(
        vec![make_field("NAME", FieldKind::Alphanum { len: 4 }, 0, 4)],
        4,
    );
    let opts = ascii_opts();
    let reader = Cursor::new(b"ABCD".to_vec());
    let (arrow_schema, batches) =
        copybook_arrow::streaming::stream_to_batches_with_schema(reader, &schema, &opts).unwrap();
    assert_eq!(arrow_schema.fields().len(), 1);
    assert_eq!(arrow_schema.field(0).name(), "NAME");
    assert_eq!(batches[0].num_rows(), 1);
}

#[test]
fn batch_legacy_writer_accumulates_multiple_records() {
    let schema = ArrowSchema::new(vec![
        ArrowField::new("name", DataType::Utf8, true),
        ArrowField::new("age", DataType::Int64, true),
    ]);
    let mut writer = ArrowWriter::new(schema);

    for i in 0..10 {
        let json_val = json!({ "name": format!("Person{i}"), "age": 20 + i });
        writer.add_json_record(&json_val).unwrap();
    }
    assert_eq!(writer.batch_count(), 10);
    let total_rows: usize = writer.batches().iter().map(RecordBatch::num_rows).sum();
    assert_eq!(total_rows, 10);
}

// ===========================================================================
// Section 2: Type mapping edge cases (17 tests)
// ===========================================================================

#[test]
fn type_pic_9_integer_maps_to_decimal128() {
    // PIC 9(5) → ZonedDecimal with scale 0 → Decimal128(5,0)
    let schema = Schema::from_fields(vec![make_field(
        "COUNT",
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
fn type_pic_9v99_maps_to_decimal128_with_scale() {
    // PIC 9(5)V99 → ZonedDecimal digits=7, scale=2 → Decimal128(7,2)
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
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(7, 2));
}

#[test]
fn type_pic_x_maps_to_utf8() {
    let schema = Schema::from_fields(vec![make_field(
        "NAME",
        FieldKind::Alphanum { len: 20 },
        0,
        20,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
}

#[test]
fn type_comp3_maps_to_decimal128() {
    // COMP-3 (packed decimal)
    let schema = Schema::from_fields(vec![make_field(
        "BAL",
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
fn type_comp3_no_scale_maps_to_decimal128() {
    let schema = Schema::from_fields(vec![make_field(
        "COUNT",
        FieldKind::PackedDecimal {
            digits: 7,
            scale: 0,
            signed: false,
        },
        0,
        4,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(7, 0));
}

#[test]
fn type_comp_int16_signed() {
    let schema = Schema::from_fields(vec![make_field(
        "I16",
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
fn type_comp_int16_unsigned() {
    let schema = Schema::from_fields(vec![make_field(
        "U16",
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
fn type_comp_int32_signed() {
    let schema = Schema::from_fields(vec![make_field(
        "I32",
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
fn type_comp_int32_unsigned() {
    let schema = Schema::from_fields(vec![make_field(
        "U32",
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
fn type_comp_int64_signed() {
    let schema = Schema::from_fields(vec![make_field(
        "I64",
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
fn type_comp_int64_unsigned() {
    let schema = Schema::from_fields(vec![make_field(
        "U64",
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
fn type_comp1_maps_to_float32() {
    let schema = Schema::from_fields(vec![make_field("F32", FieldKind::FloatSingle, 0, 4)]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Float32);
}

#[test]
fn type_comp2_maps_to_float64() {
    let schema = Schema::from_fields(vec![make_field("F64", FieldKind::FloatDouble, 0, 8)]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Float64);
}

#[test]
fn type_fixed_occurs_wraps_in_fixed_size_list() {
    let mut f = make_field("ITEMS", FieldKind::Alphanum { len: 10 }, 0, 10);
    f.occurs = Some(Occurs::Fixed { count: 5 });
    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    match arrow.field(0).data_type() {
        DataType::FixedSizeList(inner, 5) => {
            assert_eq!(*inner.data_type(), DataType::Utf8);
        }
        other => panic!("Expected FixedSizeList(_, 5), got {other:?}"),
    }
}

#[test]
fn type_odo_wraps_in_variable_list() {
    let mut f = make_field(
        "VALS",
        FieldKind::BinaryInt {
            bits: 32,
            signed: true,
        },
        0,
        4,
    );
    f.occurs = Some(Occurs::ODO {
        min: 0,
        max: 100,
        counter_path: "CNT".to_string(),
    });
    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    match arrow.field(0).data_type() {
        DataType::List(inner) => {
            assert_eq!(*inner.data_type(), DataType::Int32);
        }
        other => panic!("Expected List, got {other:?}"),
    }
}

#[test]
fn type_group_no_flatten_produces_struct() {
    let mut group = Field::new(1, "GRP".to_string());
    group.path = "GRP".to_string();
    let mut c1 = Field::with_kind(5, "A".to_string(), FieldKind::Alphanum { len: 5 });
    c1.path = "GRP.A".to_string();
    c1.offset = 0;
    c1.len = 5;
    let mut c2 = Field::with_kind(5, "B".to_string(), FieldKind::Alphanum { len: 5 });
    c2.path = "GRP.B".to_string();
    c2.offset = 5;
    c2.len = 5;
    group.children = vec![c1, c2];

    let schema = Schema::from_fields(vec![group]);
    let opts = ArrowOptions {
        flatten_groups: false,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(arrow.fields().len(), 1);
    match arrow.field(0).data_type() {
        DataType::Struct(fields) => {
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].name(), "A");
            assert_eq!(fields[1].name(), "B");
        }
        other => panic!("Expected Struct, got {other:?}"),
    }
}

#[test]
fn type_wide_decimal_falls_back_to_utf8() {
    // Precision > 38 → Utf8 fallback
    let schema = Schema::from_fields(vec![make_field(
        "HUGE",
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

// ===========================================================================
// Section 3: Parquet roundtrip (12 tests)
// ===========================================================================

#[test]
fn parquet_write_and_read_alphanum() {
    let schema = make_schema_with_lrecl(
        vec![make_field("NAME", FieldKind::Alphanum { len: 5 }, 0, 5)],
        5,
    );
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder.append_record(b"HELLO").unwrap();
    builder.append_record(b"WORLD").unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader = ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap()
        .build()
        .unwrap();
    let read_batches: Vec<_> = reader.map(|b| b.unwrap()).collect();
    let total_rows: usize = read_batches.iter().map(RecordBatch::num_rows).sum();
    assert_eq!(total_rows, 2);
    let col = read_batches[0].column(0).as_string::<i32>();
    assert_eq!(col.value(0), "HELLO");
    assert_eq!(col.value(1), "WORLD");
}

#[test]
fn parquet_write_and_read_packed_decimal() {
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

    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader = ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap()
        .build()
        .unwrap();
    let read_batches: Vec<_> = reader.map(|b| b.unwrap()).collect();
    let col = read_batches[0]
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), 12345);
    assert_eq!(col.value(1), -99999);
}

#[test]
fn parquet_schema_field_names_preserved() {
    let schema = Schema::from_fields(vec![
        make_field("CUSTOMER-ID", FieldKind::Alphanum { len: 8 }, 0, 8),
        make_field(
            "BALANCE",
            FieldKind::PackedDecimal {
                digits: 9,
                scale: 2,
                signed: true,
            },
            8,
            5,
        ),
    ]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    let mut record = b"CUST0001".to_vec();
    record.extend_from_slice(&[0x00, 0x10, 0x00, 0x00, 0x0C]);
    builder.append_record(&record).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let rb = ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
    let read_schema = rb.schema();
    assert_eq!(read_schema.field(0).name(), "CUSTOMER-ID");
    assert_eq!(read_schema.field(1).name(), "BALANCE");
    assert_eq!(*read_schema.field(0).data_type(), DataType::Utf8);
    assert_eq!(
        *read_schema.field(1).data_type(),
        DataType::Decimal128(9, 2)
    );
}

#[test]
fn parquet_data_types_preserved() {
    let schema = Schema::from_fields(vec![
        make_field("TXT", FieldKind::Alphanum { len: 4 }, 0, 4),
        make_field(
            "NUM",
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
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    let mut rec = b"TEST".to_vec();
    rec.extend_from_slice(&42_i32.to_be_bytes());
    builder.append_record(&rec).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let rb = ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
    assert_eq!(*rb.schema().field(0).data_type(), DataType::Utf8);
    assert_eq!(*rb.schema().field(1).data_type(), DataType::Int32);
}

#[test]
fn parquet_snappy_compression_roundtrip() {
    let schema = Schema::from_fields(vec![make_field("D", FieldKind::Alphanum { len: 4 }, 0, 4)]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::Snappy,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder.append_record(b"SNAP").unwrap();
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
    assert_eq!(col.value(0), "SNAP");
}

#[test]
fn parquet_gzip_compression_roundtrip() {
    let schema = Schema::from_fields(vec![make_field("D", FieldKind::Alphanum { len: 4 }, 0, 4)]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::Gzip,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder.append_record(b"GZIP").unwrap();
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
}

#[test]
fn parquet_zstd_compression_roundtrip() {
    let schema = Schema::from_fields(vec![make_field("D", FieldKind::Alphanum { len: 4 }, 0, 4)]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::Zstd,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder.append_record(b"ZSTD").unwrap();
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
}

#[test]
fn parquet_multiple_batches_roundtrip() {
    let schema = Schema::from_fields(vec![make_field(
        "CODE",
        FieldKind::Alphanum { len: 3 },
        0,
        3,
    )]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();

    let mut b1 = RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    b1.append_record(b"AAA").unwrap();
    b1.append_record(b"BBB").unwrap();
    let batch1 = b1.flush().unwrap().unwrap();

    let mut b2 = RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    b2.append_record(b"CCC").unwrap();
    let batch2 = b2.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch1, batch2], &arrow_schema, &opts, None).unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader = ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap()
        .build()
        .unwrap();
    let read_batches: Vec<_> = reader.map(|b| b.unwrap()).collect();
    let total: usize = read_batches.iter().map(RecordBatch::num_rows).sum();
    assert_eq!(total, 3);
}

#[test]
fn parquet_embeds_copybook_metadata() {
    let schema = Schema::from_fields(vec![make_field("F", FieldKind::Alphanum { len: 2 }, 0, 2)]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::None,
        embed_copybook: true,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder.append_record(b"AB").unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let cpy = "       01 REC.\n         05 F PIC X(2).";
    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, Some(cpy)).unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let rb = ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
    let meta = rb.schema().metadata();
    assert!(meta.contains_key("copybook_rs.version"));
    assert!(meta.contains_key("copybook_rs.codepage"));
    assert_eq!(
        meta.get("copybook_rs.copybook_text").map(String::as_str),
        Some(cpy)
    );
}

#[test]
fn parquet_no_embed_omits_copybook_text() {
    let schema = Schema::from_fields(vec![make_field("F", FieldKind::Alphanum { len: 2 }, 0, 2)]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::None,
        embed_copybook: false,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder.append_record(b"AB").unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(
        tmp.path(),
        &[batch],
        &arrow_schema,
        &opts,
        Some("dummy copybook"),
    )
    .unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let rb = ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
    let meta = rb.schema().metadata();
    assert!(meta.contains_key("copybook_rs.version"));
    assert!(!meta.contains_key("copybook_rs.copybook_text"));
}

#[test]
fn ipc_roundtrip_write_produces_nonempty_file() {
    let schema = Schema::from_fields(vec![make_field(
        "VAL",
        FieldKind::Alphanum { len: 3 },
        0,
        3,
    )]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder.append_record(b"IPC").unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_ipc(tmp.path(), &[batch], &arrow_schema).unwrap();
    let metadata = std::fs::metadata(tmp.path()).unwrap();
    assert!(metadata.len() > 0);
}

#[test]
fn parquet_int32_data_roundtrip() {
    let schema = Schema::from_fields(vec![make_field(
        "COUNT",
        FieldKind::BinaryInt {
            bits: 32,
            signed: true,
        },
        0,
        4,
    )]);
    let opts = ArrowOptions {
        compression: Compression::None,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder.append_record(&42_i32.to_be_bytes()).unwrap();
    builder.append_record(&(-1_i32).to_be_bytes()).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader = ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap()
        .build()
        .unwrap();
    let read_batches: Vec<_> = reader.map(|b| b.unwrap()).collect();
    let col = read_batches[0]
        .column(0)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(col.value(0), 42);
    assert_eq!(col.value(1), -1);
}

// ===========================================================================
// Section 4: Error handling (14 tests)
// ===========================================================================

#[test]
fn error_unsupported_binary_int_width() {
    let schema = Schema::from_fields(vec![make_field(
        "BAD",
        FieldKind::BinaryInt {
            bits: 24,
            signed: true,
        },
        0,
        3,
    )]);
    let err = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap_err();
    match err {
        ArrowError::SchemaConversion(msg) => assert!(msg.contains("24")),
        other => panic!("Expected SchemaConversion, got {other:?}"),
    }
}

#[test]
fn error_stream_without_lrecl() {
    let schema = Schema::from_fields(vec![make_field("X", FieldKind::Alphanum { len: 5 }, 0, 5)]);
    let reader = Cursor::new(Vec::<u8>::new());
    let err =
        copybook_arrow::stream_to_batches(reader, &schema, &ArrowOptions::default()).unwrap_err();
    match err {
        ArrowError::SchemaConversion(msg) => {
            assert!(msg.contains("lrecl_fixed"));
        }
        other => panic!("Expected SchemaConversion, got {other:?}"),
    }
}

#[test]
fn error_json_schema_from_non_object() {
    let err = json_to_schema(&json!("not an object")).unwrap_err();
    match err {
        ArrowError::JsonConversion(msg) => {
            assert!(msg.contains("Expected JSON object"));
        }
        other => panic!("Expected JsonConversion, got {other:?}"),
    }
}

#[test]
fn error_json_record_batch_from_non_object() {
    let schema = ArrowSchema::new(vec![ArrowField::new("f", DataType::Utf8, true)]);
    let err = json_to_record_batch(&schema, &json!(42)).unwrap_err();
    match err {
        ArrowError::JsonConversion(msg) => {
            assert!(msg.contains("Expected JSON object"));
        }
        other => panic!("Expected JsonConversion, got {other:?}"),
    }
}

#[test]
fn error_json_schema_from_array() {
    let err = json_to_schema(&json!([1, 2, 3])).unwrap_err();
    assert!(matches!(err, ArrowError::JsonConversion(_)));
}

#[test]
fn error_json_schema_from_null() {
    let err = json_to_schema(&json!(null)).unwrap_err();
    assert!(matches!(err, ArrowError::JsonConversion(_)));
}

#[test]
fn error_json_schema_from_bool() {
    let err = json_to_schema(&json!(true)).unwrap_err();
    assert!(matches!(err, ArrowError::JsonConversion(_)));
}

#[test]
fn error_json_schema_from_number() {
    let err = json_to_schema(&json!(42)).unwrap_err();
    assert!(matches!(err, ArrowError::JsonConversion(_)));
}

#[test]
fn error_arrow_from_io_error() {
    let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
    let arrow_err: ArrowError = io_err.into();
    assert!(matches!(arrow_err, ArrowError::Io(_)));
}

#[test]
fn error_arrow_from_arrow_error() {
    let native_err = arrow::error::ArrowError::ParseError("bad parse".to_string());
    let arrow_err: ArrowError = native_err.into();
    assert!(matches!(arrow_err, ArrowError::Arrow(_)));
}

#[test]
fn error_display_all_variants() {
    let cases: Vec<(ArrowError, &str)> = vec![
        (
            ArrowError::JsonConversion("j".into()),
            "JSON conversion error",
        ),
        (
            ArrowError::SchemaConversion("s".into()),
            "Schema conversion error",
        ),
        (ArrowError::ColumnBuild("c".into()), "Column build error"),
        (ArrowError::ParquetWrite("p".into()), "Parquet write error"),
        (ArrowError::IpcWrite("i".into()), "IPC write error"),
        (ArrowError::Codec("d".into()), "Codec error"),
    ];
    for (err, expected) in cases {
        let msg = format!("{err}");
        assert!(
            msg.contains(expected),
            "'{msg}' should contain '{expected}'"
        );
    }
}

#[test]
fn error_legacy_writer_from_invalid_json_schema() {
    let result = ArrowWriter::from_json_schema(&json!("bad"));
    assert!(result.is_err());
}

#[test]
fn error_short_record_produces_null() {
    let schema = Schema::from_fields(vec![make_field(
        "BIG",
        FieldKind::Alphanum { len: 10 },
        0,
        10,
    )]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();
    builder.append_record(b"SHORT").unwrap();
    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.column(0).null_count(), 1);
}

#[test]
fn error_empty_schema_produces_empty_arrow() {
    let schema = Schema::from_fields(vec![]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 0);
}

// ===========================================================================
// Section 5: Additional type decode through builder (verify values, 5 tests)
// ===========================================================================

#[test]
fn builder_decode_int16_signed() {
    let schema = Schema::from_fields(vec![make_field(
        "I16",
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
    builder.append_record(&100_i16.to_be_bytes()).unwrap();
    builder.append_record(&(-200_i16).to_be_bytes()).unwrap();
    let batch = builder.flush().unwrap().unwrap();
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Int16Array>()
        .unwrap();
    assert_eq!(col.value(0), 100);
    assert_eq!(col.value(1), -200);
}

#[test]
fn builder_decode_uint16() {
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
fn builder_decode_float32() {
    let schema = Schema::from_fields(vec![make_field("F32", FieldKind::FloatSingle, 0, 4)]);
    let opts = ArrowOptions {
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();
    builder.append_record(&1.5_f32.to_be_bytes()).unwrap();
    let batch = builder.flush().unwrap().unwrap();
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Float32Array>()
        .unwrap();
    assert!((col.value(0) - 1.5).abs() < 0.001);
}

#[test]
fn builder_decode_float64() {
    let schema = Schema::from_fields(vec![make_field("F64", FieldKind::FloatDouble, 0, 8)]);
    let opts = ArrowOptions {
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();
    builder.append_record(&1.5_f64.to_be_bytes()).unwrap();
    let batch = builder.flush().unwrap().unwrap();
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Float64Array>()
        .unwrap();
    assert!((col.value(0) - 1.5).abs() < f64::EPSILON * 10.0);
}

#[test]
fn builder_decode_negative_packed_decimal() {
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
    builder.append_record(&[0x12, 0x34, 0x5D]).unwrap(); // -123.45
    let batch = builder.flush().unwrap().unwrap();
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), -12345);
}

// ===========================================================================
// Section 6: Legacy API additional coverage (5 tests)
// ===========================================================================

#[test]
fn legacy_json_to_schema_empty_object() {
    let schema = json_to_schema(&json!({})).unwrap();
    assert_eq!(schema.fields().len(), 0);
}

#[test]
fn legacy_json_to_schema_nested_object() {
    let schema = json_to_schema(&json!({
        "simple": "value",
        "nested": { "inner": 123 }
    }))
    .unwrap();
    assert_eq!(schema.fields().len(), 2);
}

#[test]
fn legacy_record_batch_missing_field_is_null() {
    let schema = ArrowSchema::new(vec![
        ArrowField::new("a", DataType::Utf8, true),
        ArrowField::new("b", DataType::Int64, true),
    ]);
    let batch = json_to_record_batch(&schema, &json!({"a": "ok"})).unwrap();
    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.column(1).null_count(), 1);
}

#[test]
fn legacy_record_batch_all_nulls() {
    let schema = ArrowSchema::new(vec![
        ArrowField::new("x", DataType::Utf8, true),
        ArrowField::new("y", DataType::Int64, true),
    ]);
    let batch = json_to_record_batch(&schema, &json!({"x": null, "y": null})).unwrap();
    assert_eq!(batch.column(0).null_count(), 1);
    assert_eq!(batch.column(1).null_count(), 1);
}

#[test]
fn legacy_writer_add_batch_directly() {
    let schema = ArrowSchema::new(vec![ArrowField::new("f", DataType::Utf8, true)]);
    let mut writer = ArrowWriter::new(schema.clone());

    let array = Arc::new(StringArray::from(vec![Some("v1"), Some("v2")]));
    let batch = RecordBatch::try_new(Arc::new(schema), vec![array as ArrayRef]).unwrap();

    writer.add_batch(batch);
    assert_eq!(writer.batch_count(), 1);
    assert_eq!(writer.batches()[0].num_rows(), 2);
}
