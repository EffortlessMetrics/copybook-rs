// SPDX-License-Identifier: AGPL-3.0-or-later
//! Batch builder integration tests

#![allow(clippy::unwrap_used)]

use arrow::array::{AsArray, Decimal128Array, Float32Array};
use arrow::datatypes::DataType;
use copybook_arrow::batch_builder::RecordBatchBuilder;
use copybook_arrow::options::ArrowOptions;
use copybook_arrow::schema_convert::cobol_schema_to_arrow;
use copybook_core::schema::{Field, FieldKind, Schema};
use std::sync::Arc;

fn make_field(name: &str, kind: FieldKind, offset: u32, len: u32) -> Field {
    let mut f = Field::with_kind(5, name.to_string(), kind);
    f.path = name.to_string();
    f.offset = offset;
    f.len = len;
    f
}

#[test]
fn build_batch_from_packed_decimal_records() {
    // PIC S9(5)V99 COMP-3 -> 5 digits, scale 2, signed, 3 bytes
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

    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // 12345 positive: 0x12 0x34 0x5C
    builder.append_record(&[0x12, 0x34, 0x5C]).unwrap();
    // 67890 positive: 0x67 0x89 0x0C
    builder.append_record(&[0x67, 0x89, 0x0C]).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 2);
    assert_eq!(batch.num_columns(), 1);
    assert_eq!(
        *batch.schema().field(0).data_type(),
        DataType::Decimal128(5, 2)
    );

    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), 12345);
    assert_eq!(col.value(1), 67890);
}

#[test]
fn packed_decimal_odd_digits_ignores_trailing_bytes() {
    // 5 digits => packed payload is 3 bytes. Field len is intentionally oversized
    // to verify accumulator slicing uses packed length, not full provided slice.
    let schema = Schema::from_fields(vec![make_field(
        "AMOUNT",
        FieldKind::PackedDecimal {
            digits: 5,
            scale: 2,
            signed: true,
        },
        0,
        4,
    )]);

    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // 12345 positive in first 3 bytes + unrelated trailing byte.
    builder.append_record(&[0x12, 0x34, 0x5C, 0xFF]).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), 12345);
}

#[test]
fn build_batch_from_ibm_hex_float_records() {
    let schema = Schema::from_fields(vec![make_field("RATE", FieldKind::FloatSingle, 0, 4)]);

    let opts = ArrowOptions {
        float_format: copybook_codec::FloatFormat::IbmHex,
        ..ArrowOptions::default()
    };

    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // IBM HFP short for +1.0
    builder.append_record(&[0x41, 0x10, 0x00, 0x00]).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Float32Array>()
        .unwrap();
    assert!((col.value(0) - 1.0_f32).abs() < f32::EPSILON);
}

#[test]
fn build_batch_from_alphanum_records() {
    let schema = Schema::from_fields(vec![make_field(
        "NAME",
        FieldKind::Alphanum { len: 5 },
        0,
        5,
    )]);

    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        ..ArrowOptions::default()
    };

    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    builder.append_record(b"HELLO").unwrap();
    builder.append_record(b"WORLD").unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 2);

    let col = batch.column(0).as_string::<i32>();
    assert_eq!(col.value(0), "HELLO");
    assert_eq!(col.value(1), "WORLD");
}

#[test]
fn batch_auto_flushes_at_batch_size() {
    let schema = Schema::from_fields(vec![make_field(
        "DATA",
        FieldKind::Alphanum { len: 3 },
        0,
        3,
    )]);

    let opts = ArrowOptions {
        batch_size: 2,
        codepage: copybook_codec::Codepage::ASCII,
        ..ArrowOptions::default()
    };

    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    assert!(builder.append_record(b"AAA").unwrap().is_none());
    let batch = builder.append_record(b"BBB").unwrap();
    assert!(batch.is_some());
    assert_eq!(batch.unwrap().num_rows(), 2);

    // Builder should be reset after auto-flush
    assert!(builder.append_record(b"CCC").unwrap().is_none());
    let final_batch = builder.flush().unwrap();
    assert!(final_batch.is_some());
    assert_eq!(final_batch.unwrap().num_rows(), 1);
}

#[test]
fn empty_flush_returns_none() {
    let schema = Schema::from_fields(vec![make_field(
        "DATA",
        FieldKind::Alphanum { len: 3 },
        0,
        3,
    )]);

    let opts = ArrowOptions::default();
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    assert!(builder.flush().unwrap().is_none());
}

#[test]
fn multi_column_batch() {
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
        ..ArrowOptions::default()
    };

    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // 5 bytes alphanum + 3 bytes packed decimal
    let mut record = Vec::new();
    record.extend_from_slice(b"ALICE");
    record.extend_from_slice(&[0x12, 0x34, 0x5C]); // 12345 positive

    builder.append_record(&record).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.num_columns(), 2);

    let name_col = batch.column(0).as_string::<i32>();
    assert_eq!(name_col.value(0), "ALICE");

    let amt_col = batch
        .column(1)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(amt_col.value(0), 12345);
}
