// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive integration tests for copybook-arrow.
//!
//! Covers: schema-to-Arrow conversion for all COBOL types, data conversion
//! through streaming, round-trip through IPC/Parquet, and edge cases
//! (empty batches, single records, large batches, FILLER-only schemas).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::io::Cursor;
use std::sync::Arc;

use arrow::array::{AsArray, Decimal128Array, Float32Array, Float64Array, Int32Array, Int64Array};
use arrow::datatypes::DataType;
use copybook_arrow::batch_builder::RecordBatchBuilder;
use copybook_arrow::options::{ArrowOptions, Compression};
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

// ===================================================================
// 1. Schema-to-Arrow: all COBOL types in a single schema
// ===================================================================

#[test]
fn all_cobol_types_single_schema_maps_correctly() {
    let schema = Schema::from_fields(vec![
        make_field("NAME", FieldKind::Alphanum { len: 20 }, 0, 20),
        make_field(
            "ZONED-AMT",
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
            "PACKED-BAL",
            FieldKind::PackedDecimal {
                digits: 7,
                scale: 2,
                signed: true,
            },
            29,
            4,
        ),
        make_field(
            "CNT-16",
            FieldKind::BinaryInt {
                bits: 16,
                signed: true,
            },
            33,
            2,
        ),
        make_field(
            "CNT-32",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            35,
            4,
        ),
        make_field(
            "CNT-64",
            FieldKind::BinaryInt {
                bits: 64,
                signed: true,
            },
            39,
            8,
        ),
        make_field("RATE-F32", FieldKind::FloatSingle, 47, 4),
        make_field("RATE-F64", FieldKind::FloatDouble, 51, 8),
    ]);

    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 8);
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
    assert_eq!(*arrow.field(1).data_type(), DataType::Decimal128(9, 2));
    assert_eq!(*arrow.field(2).data_type(), DataType::Decimal128(7, 2));
    assert_eq!(*arrow.field(3).data_type(), DataType::Int16);
    assert_eq!(*arrow.field(4).data_type(), DataType::Int32);
    assert_eq!(*arrow.field(5).data_type(), DataType::Int64);
    assert_eq!(*arrow.field(6).data_type(), DataType::Float32);
    assert_eq!(*arrow.field(7).data_type(), DataType::Float64);
}

// ===================================================================
// 2. Schema: Group fields produce Struct arrays when not flattened
// ===================================================================

#[test]
fn group_produces_struct_with_multiple_children() {
    let mut child_a = Field::with_kind(5, "A".to_string(), FieldKind::Alphanum { len: 5 });
    child_a.path = "GRP.A".to_string();
    child_a.offset = 0;
    child_a.len = 5;

    let mut child_b = Field::with_kind(
        5,
        "B".to_string(),
        FieldKind::PackedDecimal {
            digits: 5,
            scale: 2,
            signed: true,
        },
    );
    child_b.path = "GRP.B".to_string();
    child_b.offset = 5;
    child_b.len = 3;

    let mut child_c = Field::with_kind(
        5,
        "C".to_string(),
        FieldKind::BinaryInt {
            bits: 32,
            signed: true,
        },
    );
    child_c.path = "GRP.C".to_string();
    child_c.offset = 8;
    child_c.len = 4;

    let mut group = Field::new(1, "GRP".to_string());
    group.path = "GRP".to_string();
    group.children = vec![child_a, child_b, child_c];

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
            assert_eq!(fields.len(), 3);
            assert_eq!(fields[0].name(), "A");
            assert_eq!(*fields[0].data_type(), DataType::Utf8);
            assert_eq!(fields[1].name(), "B");
            assert_eq!(*fields[1].data_type(), DataType::Decimal128(5, 2));
            assert_eq!(fields[2].name(), "C");
            assert_eq!(*fields[2].data_type(), DataType::Int32);
        }
        other => panic!("Expected Struct, got {other:?}"),
    }
}

// ===================================================================
// 3. Schema: OCCURS Fixed produces FixedSizeList
// ===================================================================

#[test]
fn occurs_fixed_on_binary_int_produces_fixed_list() {
    let mut f = make_field(
        "COUNTS",
        FieldKind::BinaryInt {
            bits: 32,
            signed: true,
        },
        0,
        4,
    );
    f.occurs = Some(Occurs::Fixed { count: 5 });

    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    match arrow.field(0).data_type() {
        DataType::FixedSizeList(inner, 5) => {
            assert_eq!(*inner.data_type(), DataType::Int32);
        }
        other => panic!("Expected FixedSizeList(Int32, 5), got {other:?}"),
    }
}

// ===================================================================
// 4. Schema: OCCURS ODO produces variable List
// ===================================================================

#[test]
fn occurs_odo_on_binary_int_produces_list() {
    let mut f = make_field(
        "ITEMS",
        FieldKind::BinaryInt {
            bits: 64,
            signed: false,
        },
        0,
        8,
    );
    f.occurs = Some(Occurs::ODO {
        min: 0,
        max: 50,
        counter_path: "NUM-ITEMS".to_string(),
    });

    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    match arrow.field(0).data_type() {
        DataType::List(inner) => {
            assert_eq!(*inner.data_type(), DataType::UInt64);
        }
        other => panic!("Expected List(UInt64), got {other:?}"),
    }
}

// ===================================================================
// 5. Data conversion: decode multi-type records to Arrow RecordBatch
// ===================================================================

#[test]
fn decode_multi_type_records_column_and_row_counts() {
    // Alphanum(4) + PackedDecimal(5,2) + Int32 = 4+3+4 = 11 bytes
    let schema = make_schema_with_lrecl(
        vec![
            make_field("NAME", FieldKind::Alphanum { len: 4 }, 0, 4),
            make_field(
                "AMT",
                FieldKind::PackedDecimal {
                    digits: 5,
                    scale: 2,
                    signed: true,
                },
                4,
                3,
            ),
            make_field(
                "CNT",
                FieldKind::BinaryInt {
                    bits: 32,
                    signed: true,
                },
                7,
                4,
            ),
        ],
        11,
    );

    let mut data = Vec::new();
    for i in 0..5_u8 {
        data.extend_from_slice(&[b'R', b'E', b'C', b'0' + i]); // RECx
        data.extend_from_slice(&[0x10, i, 0x0C]); // packed positive
        data.extend_from_slice(&((i as i32 + 1) * 10).to_be_bytes());
    }

    let reader = Cursor::new(data);
    let opts = ascii_opts();
    let batches = stream_to_batches(reader, &schema, &opts).unwrap();

    assert_eq!(batches.len(), 1);
    let batch = &batches[0];
    assert_eq!(batch.num_columns(), 3);
    assert_eq!(batch.num_rows(), 5);

    // Verify column types
    assert_eq!(*batch.schema().field(0).data_type(), DataType::Utf8);
    assert!(matches!(
        batch.schema().field(1).data_type(),
        DataType::Decimal128(5, 2)
    ));
    assert_eq!(*batch.schema().field(2).data_type(), DataType::Int32);
}

// ===================================================================
// 6. Data conversion: null handling for short records
// ===================================================================

#[test]
fn null_handling_partial_record_multi_column() {
    // Schema has 3 fields: Alphanum(4) + Int32(4) + Int64(8) = 16 bytes
    let schema = Schema::from_fields(vec![
        make_field("NAME", FieldKind::Alphanum { len: 4 }, 0, 4),
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
            "I64",
            FieldKind::BinaryInt {
                bits: 64,
                signed: true,
            },
            8,
            8,
        ),
    ]);

    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // Only 6 bytes: NAME is OK, I32 is OK (partial), I64 is completely missing
    let mut record = Vec::new();
    record.extend_from_slice(b"TEST");
    record.extend_from_slice(&42_i32.to_be_bytes());
    // I64 field at offset 8 is missing (record too short)

    builder.append_record(&record).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    assert_eq!(batch.num_rows(), 1);
    // NAME should have data
    assert_eq!(batch.column(0).null_count(), 0);
    // I32 should have data
    assert_eq!(batch.column(1).null_count(), 0);
    // I64 should be null (record too short)
    assert_eq!(batch.column(2).null_count(), 1);
}

// ===================================================================
// 7. stream_to_batches_with_schema returns matching schema
// ===================================================================

#[test]
fn stream_to_batches_with_schema_returns_correct_schema() {
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
    data.extend_from_slice(b"A01");
    data.extend_from_slice(&[0x12, 0x34, 0x5C]);

    let reader = Cursor::new(data);
    let opts = ascii_opts();
    let (arrow_schema, batches) = stream_to_batches_with_schema(reader, &schema, &opts).unwrap();

    assert_eq!(arrow_schema.fields().len(), 2);
    assert_eq!(arrow_schema.field(0).name(), "ID");
    assert_eq!(arrow_schema.field(1).name(), "VAL");
    assert_eq!(*arrow_schema.field(0).data_type(), DataType::Utf8);
    assert!(matches!(
        arrow_schema.field(1).data_type(),
        DataType::Decimal128(5, 2)
    ));
    assert_eq!(batches.len(), 1);
    assert_eq!(batches[0].num_rows(), 1);
}

// ===================================================================
// 8. Round-trip: Arrow → IPC file → read back → verify
// ===================================================================

#[test]
fn ipc_roundtrip_multi_type_data() {
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
    ]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    let mut record = Vec::new();
    record.extend_from_slice(b"ALICE");
    record.extend_from_slice(&[0x12, 0x34, 0x5C]);
    builder.append_record(&record).unwrap();

    let mut record2 = Vec::new();
    record2.extend_from_slice(b"BOB  ");
    record2.extend_from_slice(&[0x00, 0x99, 0x9D]); // negative
    builder.append_record(&record2).unwrap();

    let batch = builder.flush().unwrap().unwrap();

    // Write IPC
    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_ipc(tmp.path(), &[batch], &arrow_schema).unwrap();

    // Read back
    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader = arrow::ipc::reader::FileReader::try_new(file, None).unwrap();
    let read_batches: Vec<_> = reader.map(|b| b.unwrap()).collect();

    assert_eq!(read_batches.len(), 1);
    assert_eq!(read_batches[0].num_rows(), 2);
    assert_eq!(read_batches[0].num_columns(), 2);

    let name_col = read_batches[0].column(0).as_string::<i32>();
    assert_eq!(name_col.value(0), "ALICE");
    assert_eq!(name_col.value(1), "BOB");

    let amt_col = read_batches[0]
        .column(1)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(amt_col.value(0), 12345);
    assert_eq!(amt_col.value(1), -999);
}

// ===================================================================
// 9. Round-trip: Arrow → Parquet (Zstd) → read back → verify
// ===================================================================

#[test]
fn parquet_zstd_roundtrip_multi_type() {
    let schema = Schema::from_fields(vec![
        make_field("ID", FieldKind::Alphanum { len: 3 }, 0, 3),
        make_field(
            "SCORE",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            3,
            4,
        ),
    ]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::Zstd,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    let mut r1 = Vec::new();
    r1.extend_from_slice(b"AAA");
    r1.extend_from_slice(&100_i32.to_be_bytes());
    builder.append_record(&r1).unwrap();

    let mut r2 = Vec::new();
    r2.extend_from_slice(b"BBB");
    r2.extend_from_slice(&(-50_i32).to_be_bytes());
    builder.append_record(&r2).unwrap();

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
    let total_rows: usize = read_batches.iter().map(|b| b.num_rows()).sum();
    assert_eq!(total_rows, 2);

    let id_col = read_batches[0].column(0).as_string::<i32>();
    assert_eq!(id_col.value(0), "AAA");
    assert_eq!(id_col.value(1), "BBB");

    let score_col = read_batches[0]
        .column(1)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(score_col.value(0), 100);
    assert_eq!(score_col.value(1), -50);
}

// ===================================================================
// 10. Edge case: empty record batch (flush with no records)
// ===================================================================

#[test]
fn empty_batch_flush_returns_none() {
    let schema = Schema::from_fields(vec![
        make_field("A", FieldKind::Alphanum { len: 5 }, 0, 5),
        make_field(
            "B",
            FieldKind::PackedDecimal {
                digits: 5,
                scale: 2,
                signed: true,
            },
            5,
            3,
        ),
        make_field(
            "C",
            FieldKind::BinaryInt {
                bits: 64,
                signed: true,
            },
            8,
            8,
        ),
    ]);
    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    assert!(builder.flush().unwrap().is_none());
}

// ===================================================================
// 11. Edge case: single record produces valid batch
// ===================================================================

#[test]
fn single_record_produces_valid_batch() {
    let schema = Schema::from_fields(vec![
        make_field("NAME", FieldKind::Alphanum { len: 6 }, 0, 6),
        make_field(
            "BAL",
            FieldKind::PackedDecimal {
                digits: 7,
                scale: 2,
                signed: true,
            },
            6,
            4,
        ),
    ]);
    let opts = ascii_opts();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    let mut record = Vec::new();
    record.extend_from_slice(b"SINGLE");
    record.extend_from_slice(&[0x01, 0x23, 0x45, 0x6C]); // packed +12345.67 (not quite, but valid packed)

    builder.append_record(&record).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.num_columns(), 2);

    let name_col = batch.column(0).as_string::<i32>();
    assert_eq!(name_col.value(0), "SINGLE");
}

// ===================================================================
// 12. Edge case: large batch (1000 records) via streaming
// ===================================================================

#[test]
fn large_batch_1000_records_streaming() {
    let schema = make_schema_with_lrecl(
        vec![
            make_field("SEQ", FieldKind::Alphanum { len: 4 }, 0, 4),
            make_field(
                "VAL",
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

    let num_records = 1000;
    let mut data = Vec::with_capacity(num_records * 8);
    for i in 0..num_records {
        let seq = format!("{i:04}");
        data.extend_from_slice(seq.as_bytes());
        data.extend_from_slice(&(i as i32).to_be_bytes());
    }

    let reader = Cursor::new(data);
    let opts = ascii_opts();
    let batches = stream_to_batches(reader, &schema, &opts).unwrap();

    let total_rows: usize = batches.iter().map(|b| b.num_rows()).sum();
    assert_eq!(total_rows, num_records);

    // Verify first and last record values
    let first_batch = &batches[0];
    let seq_col = first_batch.column(0).as_string::<i32>();
    assert_eq!(seq_col.value(0), "0000");

    let val_col = first_batch
        .column(1)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(val_col.value(0), 0);

    // Check last batch's last row
    let last_batch = batches.last().unwrap();
    let last_row = last_batch.num_rows() - 1;
    let seq_col = last_batch.column(0).as_string::<i32>();
    let val_col = last_batch
        .column(1)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    // The last record should have SEQ="0999" and VAL=999
    assert_eq!(seq_col.value(last_row), "0999");
    assert_eq!(val_col.value(last_row), 999);
}

// ===================================================================
// 13. Edge case: schema with only FILLER fields (emit_filler=false)
// ===================================================================

#[test]
fn schema_only_filler_fields_produces_empty_arrow_schema() {
    let schema = Schema::from_fields(vec![
        make_field("_filler_00000000", FieldKind::Alphanum { len: 10 }, 0, 10),
        make_field("FILLER", FieldKind::Alphanum { len: 5 }, 10, 5),
        make_field("_filler_00000015", FieldKind::Alphanum { len: 3 }, 15, 3),
    ]);

    let opts = ArrowOptions::default(); // emit_filler = false
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(arrow.fields().len(), 0);
}

// ===================================================================
// 14. Edge case: FILLER-only schema with emit_filler=true
// ===================================================================

#[test]
fn schema_only_filler_fields_emitted_when_configured() {
    let schema = Schema::from_fields(vec![
        make_field("_filler_00000000", FieldKind::Alphanum { len: 10 }, 0, 10),
        make_field("FILLER", FieldKind::Alphanum { len: 5 }, 10, 5),
    ]);

    let opts = ArrowOptions {
        emit_filler: true,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(arrow.fields().len(), 2);
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
    assert_eq!(*arrow.field(1).data_type(), DataType::Utf8);
}

// ===================================================================
// 15. Packed decimal negative value decode through batch builder
// ===================================================================

#[test]
fn packed_decimal_negative_values_decoded_correctly() {
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

    // +123.45: 0x12 0x34 0x5C (C = positive)
    builder.append_record(&[0x12, 0x34, 0x5C]).unwrap();
    // -123.45: 0x12 0x34 0x5D (D = negative)
    builder.append_record(&[0x12, 0x34, 0x5D]).unwrap();
    // +000.00: 0x00 0x00 0x0C
    builder.append_record(&[0x00, 0x00, 0x0C]).unwrap();
    // unsigned +123.45: 0x12 0x34 0x5F (F = unsigned)
    builder.append_record(&[0x12, 0x34, 0x5F]).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 4);

    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), 12345);
    assert_eq!(col.value(1), -12345);
    assert_eq!(col.value(2), 0);
    assert_eq!(col.value(3), 12345);
}

// ===================================================================
// 16. Float32 and Float64 actual values via batch builder
// ===================================================================

#[test]
fn float_values_decoded_correctly() {
    let schema = Schema::from_fields(vec![
        make_field("F32", FieldKind::FloatSingle, 0, 4),
        make_field("F64", FieldKind::FloatDouble, 4, 8),
    ]);
    let opts = ArrowOptions {
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    let f32_val: f32 = std::f32::consts::PI;
    let f64_val: f64 = std::f64::consts::E;
    let mut record = Vec::new();
    record.extend_from_slice(&f32_val.to_be_bytes());
    record.extend_from_slice(&f64_val.to_be_bytes());

    builder.append_record(&record).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let f32_col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Float32Array>()
        .unwrap();
    assert!((f32_col.value(0) - std::f32::consts::PI).abs() < 0.001);

    let f64_col = batch
        .column(1)
        .as_any()
        .downcast_ref::<Float64Array>()
        .unwrap();
    assert!((f64_col.value(0) - std::f64::consts::E).abs() < 0.000_001);
}

// ===================================================================
// 17. Streaming: empty input returns empty batch list
// ===================================================================

#[test]
fn streaming_empty_input_returns_no_batches() {
    let schema = make_schema_with_lrecl(
        vec![
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
        ],
        9,
    );

    let reader = Cursor::new(Vec::<u8>::new());
    let batches = stream_to_batches(reader, &schema, &ascii_opts()).unwrap();
    assert!(batches.is_empty());
}

// ===================================================================
// 18. Multiple auto-flush cycles with small batch_size
// ===================================================================

#[test]
fn multiple_auto_flush_cycles() {
    let schema = Schema::from_fields(vec![make_field("V", FieldKind::Alphanum { len: 2 }, 0, 2)]);
    let opts = ArrowOptions {
        batch_size: 2,
        codepage: copybook_codec::Codepage::ASCII,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // First cycle: 2 records → auto-flush
    assert!(builder.append_record(b"AA").unwrap().is_none());
    let batch1 = builder.append_record(b"BB").unwrap();
    assert!(batch1.is_some());
    assert_eq!(batch1.unwrap().num_rows(), 2);

    // Second cycle: 2 records → auto-flush
    assert!(builder.append_record(b"CC").unwrap().is_none());
    let batch2 = builder.append_record(b"DD").unwrap();
    assert!(batch2.is_some());
    assert_eq!(batch2.unwrap().num_rows(), 2);

    // Third cycle: 1 record → manual flush
    assert!(builder.append_record(b"EE").unwrap().is_none());
    let batch3 = builder.flush().unwrap();
    assert!(batch3.is_some());
    assert_eq!(batch3.unwrap().num_rows(), 1);

    // Empty flush
    assert!(builder.flush().unwrap().is_none());
}

// ===================================================================
// 19. Parquet round-trip with Gzip compression
// ===================================================================

#[test]
fn parquet_gzip_roundtrip_data_preserved() {
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
    ]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::Gzip,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    let mut record = Vec::new();
    record.extend_from_slice(b"HELLO");
    record.extend_from_slice(&[0x99, 0x99, 0x9C]); // packed +999.99

    builder.append_record(&record).unwrap();
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
    assert_eq!(read_batches[0].num_rows(), 1);

    let name_col = read_batches[0].column(0).as_string::<i32>();
    assert_eq!(name_col.value(0), "HELLO");

    let amt_col = read_batches[0]
        .column(1)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(amt_col.value(0), 99999);
}

// ===================================================================
// 20. Zoned decimal (CP037) via streaming with signed values
// ===================================================================

#[test]
fn zoned_decimal_cp037_signed_streaming() {
    // PIC S9(5)V99: 7 digits, scale=2, 7 bytes per record
    let schema = make_schema_with_lrecl(
        vec![make_field(
            "AMT",
            FieldKind::ZonedDecimal {
                digits: 7,
                scale: 2,
                signed: true,
                sign_separate: None,
            },
            0,
            7,
        )],
        7,
    );

    let data: Vec<u8> = vec![
        // +00100.00: F0 F0 F1 F0 F0 F0 C0 (positive, last nibble C)
        0xF0, 0xF0, 0xF1, 0xF0, 0xF0, 0xF0, 0xC0,
        // -00200.50: F0 F0 F2 F0 F0 F5 D0 (negative, last nibble D)
        0xF0, 0xF0, 0xF2, 0xF0, 0xF0, 0xF5, 0xD0,
    ];

    let reader = Cursor::new(data);
    let batches = stream_to_batches(reader, &schema, &ArrowOptions::default()).unwrap();
    assert_eq!(batches.len(), 1);
    assert_eq!(batches[0].num_rows(), 2);

    let col = batches[0]
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), 10_000); // +100.00 unscaled
    assert_eq!(col.value(1), -20_050); // -200.50 unscaled
}

// ===================================================================
// 21. Binary int negative values decode correctly
// ===================================================================

#[test]
fn binary_int_negative_values_decoded() {
    let schema = Schema::from_fields(vec![
        make_field(
            "I32",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            0,
            4,
        ),
        make_field(
            "I64",
            FieldKind::BinaryInt {
                bits: 64,
                signed: true,
            },
            4,
            8,
        ),
    ]);
    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    let mut record = Vec::new();
    record.extend_from_slice(&(-12345_i32).to_be_bytes());
    record.extend_from_slice(&(-9_876_543_210_i64).to_be_bytes());

    builder.append_record(&record).unwrap();
    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 1);

    let i32_col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(i32_col.value(0), -12345);

    let i64_col = batch
        .column(1)
        .as_any()
        .downcast_ref::<Int64Array>()
        .unwrap();
    assert_eq!(i64_col.value(0), -9_876_543_210);
}

// ===================================================================
// 22. Large batch Parquet round-trip (100 records)
// ===================================================================

#[test]
fn large_batch_parquet_roundtrip() {
    let schema = Schema::from_fields(vec![
        make_field("ID", FieldKind::Alphanum { len: 4 }, 0, 4),
        make_field(
            "CNT",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            4,
            4,
        ),
    ]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::None,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    let num_records = 100;
    for i in 0..num_records {
        let mut record = Vec::new();
        let id = format!("{i:04}");
        record.extend_from_slice(id.as_bytes());
        record.extend_from_slice(&(i as i32).to_be_bytes());
        builder.append_record(&record).unwrap();
    }
    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), num_records);

    // Write and read back
    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader = parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap()
        .build()
        .unwrap();
    let read_batches: Vec<_> = reader.map(|b| b.unwrap()).collect();
    let total_rows: usize = read_batches.iter().map(|b| b.num_rows()).sum();
    assert_eq!(total_rows, num_records);

    // Spot-check first and last
    let first_batch = &read_batches[0];
    let id_col = first_batch.column(0).as_string::<i32>();
    assert_eq!(id_col.value(0), "0000");

    let cnt_col = first_batch
        .column(1)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(cnt_col.value(0), 0);
}
