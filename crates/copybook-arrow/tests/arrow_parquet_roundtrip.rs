// SPDX-License-Identifier: AGPL-3.0-or-later
//! Parquet round-trip integration tests for copybook-arrow.
//!
//! Validates end-to-end: COBOL schema → Arrow RecordBatch → Parquet file → read back → verify.

#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]

use std::sync::Arc;

use arrow::array::{AsArray, Decimal128Array, Int32Array};
use arrow::datatypes::DataType;
use copybook_arrow::batch_builder::RecordBatchBuilder;
use copybook_arrow::options::{ArrowOptions, Compression};
use copybook_arrow::schema_convert::cobol_schema_to_arrow;
use copybook_arrow::write_parquet;
use copybook_core::schema::{Field, FieldKind, Schema};
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

fn ascii_opts_uncompressed() -> ArrowOptions {
    ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::None,
        ..ArrowOptions::default()
    }
}

// ===================================================================
// 1. Single batch: write to Parquet, read back, verify data values
// ===================================================================

#[test]
fn parquet_alphanum_single_batch_data_roundtrip() {
    let schema = Schema::from_fields(vec![make_field(
        "NAME",
        FieldKind::Alphanum { len: 5 },
        0,
        5,
    )]);
    let opts = ascii_opts_uncompressed();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    builder.append_record(b"ALICE").unwrap();
    builder.append_record(b"BOB  ").unwrap();
    builder.append_record(b"CAROL").unwrap();
    let batch = builder.flush().unwrap().unwrap();

    // Write to Parquet
    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    // Read back and verify values
    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader = ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap()
        .build()
        .unwrap();
    let read_batches: Vec<_> = reader.map(|b| b.unwrap()).collect();
    let total_rows: usize = read_batches.iter().map(|b| b.num_rows()).sum();
    assert_eq!(total_rows, 3);

    let col = read_batches[0].column(0).as_string::<i32>();
    assert_eq!(col.value(0), "ALICE");
    assert_eq!(col.value(1), "BOB"); // trailing spaces trimmed by codec
    assert_eq!(col.value(2), "CAROL");
}

#[test]
fn parquet_packed_decimal_data_roundtrip() {
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
    let opts = ArrowOptions {
        compression: Compression::None,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    // +123.45
    builder.append_record(&[0x12, 0x34, 0x5C]).unwrap();
    // -999.99
    builder.append_record(&[0x99, 0x99, 0x9D]).unwrap();
    // +000.00
    builder.append_record(&[0x00, 0x00, 0x0C]).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    // Read back and verify decimal values
    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader = ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap()
        .build()
        .unwrap();
    let read_batches: Vec<_> = reader.map(|b| b.unwrap()).collect();
    assert_eq!(read_batches[0].num_rows(), 3);

    let col = read_batches[0]
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), 12345); // +123.45 unscaled
    assert_eq!(col.value(1), -99999); // -999.99 unscaled
    assert_eq!(col.value(2), 0); // +000.00 unscaled
}

// ===================================================================
// 2. Multiple record batches to single Parquet file
// ===================================================================

#[test]
fn parquet_multiple_batches_data_roundtrip() {
    let schema = Schema::from_fields(vec![make_field(
        "CODE",
        FieldKind::Alphanum { len: 3 },
        0,
        3,
    )]);
    let opts = ascii_opts_uncompressed();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();

    // Build batch 1
    let mut b1 = RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    b1.append_record(b"AAA").unwrap();
    b1.append_record(b"BBB").unwrap();
    let batch1 = b1.flush().unwrap().unwrap();

    // Build batch 2
    let mut b2 = RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    b2.append_record(b"CCC").unwrap();
    let batch2 = b2.flush().unwrap().unwrap();

    // Write both batches to single Parquet file
    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch1, batch2], &arrow_schema, &opts, None).unwrap();

    // Read back and verify all rows present
    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader = ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap()
        .build()
        .unwrap();
    let read_batches: Vec<_> = reader.map(|b| b.unwrap()).collect();
    let total_rows: usize = read_batches.iter().map(|b| b.num_rows()).sum();
    assert_eq!(total_rows, 3);

    // Collect all values across batches
    let mut all_values = Vec::new();
    for rb in &read_batches {
        let col = rb.column(0).as_string::<i32>();
        for i in 0..rb.num_rows() {
            all_values.push(col.value(i).to_string());
        }
    }
    assert!(all_values.contains(&"AAA".to_string()));
    assert!(all_values.contains(&"BBB".to_string()));
    assert!(all_values.contains(&"CCC".to_string()));
}

#[test]
fn parquet_multiple_batches_mixed_types_roundtrip() {
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
    let opts = ascii_opts_uncompressed();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();

    // Batch 1: 2 records
    let mut b1 = RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    let mut r1 = Vec::new();
    r1.extend_from_slice(b"AAAA");
    r1.extend_from_slice(&10_i32.to_be_bytes());
    b1.append_record(&r1).unwrap();
    let mut r2 = Vec::new();
    r2.extend_from_slice(b"BBBB");
    r2.extend_from_slice(&20_i32.to_be_bytes());
    b1.append_record(&r2).unwrap();
    let batch1 = b1.flush().unwrap().unwrap();

    // Batch 2: 1 record
    let mut b2 = RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    let mut r3 = Vec::new();
    r3.extend_from_slice(b"CCCC");
    r3.extend_from_slice(&30_i32.to_be_bytes());
    b2.append_record(&r3).unwrap();
    let batch2 = b2.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch1, batch2], &arrow_schema, &opts, None).unwrap();

    // Read back and verify
    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader = ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap()
        .build()
        .unwrap();
    let read_batches: Vec<_> = reader.map(|b| b.unwrap()).collect();
    let total_rows: usize = read_batches.iter().map(|b| b.num_rows()).sum();
    assert_eq!(total_rows, 3);
}

// ===================================================================
// 3. Schema preservation through Parquet round-trip
// ===================================================================

#[test]
fn parquet_schema_field_names_preserved() {
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
            "STATUS",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            15,
            4,
        ),
    ]);
    let opts = ascii_opts_uncompressed();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    let mut record = Vec::new();
    record.extend_from_slice(b"CUST000001");
    record.extend_from_slice(&[0x00, 0x10, 0x00, 0x00, 0x0C]); // packed +10000.00
    record.extend_from_slice(&1_i32.to_be_bytes());
    builder.append_record(&record).unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    // Read back and verify schema field names and types are preserved
    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader_builder = ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
    let read_schema = reader_builder.schema();

    assert_eq!(read_schema.field(0).name(), "CUSTOMER-ID");
    assert_eq!(read_schema.field(1).name(), "BALANCE");
    assert_eq!(read_schema.field(2).name(), "STATUS");

    assert_eq!(*read_schema.field(0).data_type(), DataType::Utf8);
    assert_eq!(
        *read_schema.field(1).data_type(),
        DataType::Decimal128(9, 2)
    );
    assert_eq!(*read_schema.field(2).data_type(), DataType::Int32);
}

#[test]
fn parquet_mixed_types_full_data_roundtrip() {
    let schema = Schema::from_fields(vec![
        make_field("NAME", FieldKind::Alphanum { len: 8 }, 0, 8),
        make_field(
            "AMOUNT",
            FieldKind::PackedDecimal {
                digits: 5,
                scale: 2,
                signed: true,
            },
            8,
            3,
        ),
        make_field(
            "COUNT",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            11,
            4,
        ),
    ]);
    let opts = ascii_opts_uncompressed();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();

    // Record 1: NAME="WIDGET  ", AMOUNT=+123.45, COUNT=10
    let mut r1 = Vec::new();
    r1.extend_from_slice(b"WIDGET  ");
    r1.extend_from_slice(&[0x12, 0x34, 0x5C]);
    r1.extend_from_slice(&10_i32.to_be_bytes());
    builder.append_record(&r1).unwrap();

    // Record 2: NAME="GADGET  ", AMOUNT=-999.99, COUNT=42
    let mut r2 = Vec::new();
    r2.extend_from_slice(b"GADGET  ");
    r2.extend_from_slice(&[0x99, 0x99, 0x9D]);
    r2.extend_from_slice(&42_i32.to_be_bytes());
    builder.append_record(&r2).unwrap();

    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    // Read back and verify all data values
    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader = ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap()
        .build()
        .unwrap();
    let read_batches: Vec<_> = reader.map(|b| b.unwrap()).collect();
    assert_eq!(read_batches.len(), 1);
    let rb = &read_batches[0];
    assert_eq!(rb.num_rows(), 2);

    // String column
    let name_col = rb.column(0).as_string::<i32>();
    assert_eq!(name_col.value(0), "WIDGET");
    assert_eq!(name_col.value(1), "GADGET");

    // Decimal column
    let amt_col = rb
        .column(1)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(amt_col.value(0), 12345); // +123.45 unscaled
    assert_eq!(amt_col.value(1), -99999); // -999.99 unscaled

    // Int column
    let cnt_col = rb.column(2).as_any().downcast_ref::<Int32Array>().unwrap();
    assert_eq!(cnt_col.value(0), 10);
    assert_eq!(cnt_col.value(1), 42);
}

#[test]
fn parquet_metadata_preserved_through_roundtrip() {
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

    let copybook_text = "       01 REC.\n         05 V PIC X(4).";
    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(
        tmp.path(),
        &[batch],
        &arrow_schema,
        &opts,
        Some(copybook_text),
    )
    .unwrap();

    // Read back and verify metadata survives round-trip
    let file = std::fs::File::open(tmp.path()).unwrap();
    let reader_builder = ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
    let meta = reader_builder.schema().metadata();

    assert!(meta.contains_key("copybook_rs.version"));
    assert!(meta.contains_key("copybook_rs.codepage"));
    assert_eq!(
        meta.get("copybook_rs.copybook_text").map(String::as_str),
        Some(copybook_text)
    );
}
