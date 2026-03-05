// SPDX-License-Identifier: AGPL-3.0-or-later
//! Tests for the typed Arrow conversion API: schema conversion edge cases,
//! streaming, IPC/Parquet roundtrips, error handling, and builder integration.

#![allow(clippy::unwrap_used, clippy::panic)]

use std::io::Cursor;
use std::sync::Arc;

use arrow::array::{Decimal128Array, Float64Array, Int32Array};
use arrow::datatypes::DataType;
use copybook_arrow::batch_builder::RecordBatchBuilder;
use copybook_arrow::options::{ArrowOptions, Compression, EditedPicRepresentation};
use copybook_arrow::schema_convert::cobol_schema_to_arrow;
use copybook_arrow::streaming::{stream_to_batches, stream_to_batches_with_schema};
use copybook_arrow::{ArrowError, write_ipc, write_parquet};
use copybook_core::schema::{Field, FieldKind, Occurs, Schema};

// ---------------------------------------------------------------------------
// Helper
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

// ---------------------------------------------------------------------------
// 1. Empty schema
// ---------------------------------------------------------------------------

#[test]
fn empty_schema_produces_empty_arrow_schema() {
    let schema = Schema::from_fields(vec![]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 0);
}

// ---------------------------------------------------------------------------
// 2. Unsupported binary int width
// ---------------------------------------------------------------------------

#[test]
fn unsupported_binary_int_width_returns_error() {
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
        ArrowError::SchemaConversion(msg) => {
            assert!(msg.contains("24"), "expected width in message: {msg}");
        }
        other => panic!("Expected SchemaConversion, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// 3. Nested group (flatten disabled) -> Struct
// ---------------------------------------------------------------------------

#[test]
fn nested_group_without_flattening_produces_struct() {
    let mut group = Field::new(1, "GRP".to_string());
    group.path = "GRP".to_string();

    let mut child = Field::with_kind(5, "CHILD".to_string(), FieldKind::Alphanum { len: 5 });
    child.path = "GRP.CHILD".to_string();
    child.offset = 0;
    child.len = 5;
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
            assert_eq!(fields[0].name(), "CHILD");
            assert_eq!(*fields[0].data_type(), DataType::Utf8);
        }
        other => panic!("Expected Struct, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// 4. Empty group (no leaf children) is omitted from Struct output
// ---------------------------------------------------------------------------

#[test]
fn empty_group_is_omitted_when_not_flattened() {
    let mut group = Field::new(1, "EMPTY_GRP".to_string());
    group.path = "EMPTY_GRP".to_string();
    group.children = vec![]; // no children

    let schema = Schema::from_fields(vec![group]);
    let opts = ArrowOptions {
        flatten_groups: false,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(arrow.fields().len(), 0);
}

// ---------------------------------------------------------------------------
// 5. FILLER with uppercase name variant
// ---------------------------------------------------------------------------

#[test]
fn filler_named_uppercase_is_skipped() {
    let schema = Schema::from_fields(vec![
        make_field("DATA", FieldKind::Alphanum { len: 5 }, 0, 5),
        make_field("FILLER", FieldKind::Alphanum { len: 3 }, 5, 3),
    ]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 1);
    assert_eq!(arrow.field(0).name(), "DATA");
}

// ---------------------------------------------------------------------------
// 6. Streaming with packed-decimal records
// ---------------------------------------------------------------------------

#[test]
fn stream_to_batches_decodes_packed_decimal() {
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

    // Two records: 12345C, 67890C
    let data: Vec<u8> = vec![0x12, 0x34, 0x5C, 0x67, 0x89, 0x0C];
    let reader = Cursor::new(data);

    let opts = ArrowOptions {
        batch_size: 1024,
        ..ArrowOptions::default()
    };
    let batches = stream_to_batches(reader, &schema, &opts).unwrap();
    assert_eq!(batches.len(), 1);
    assert_eq!(batches[0].num_rows(), 2);

    let col = batches[0]
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), 12345);
    assert_eq!(col.value(1), 67890);
}

// ---------------------------------------------------------------------------
// 7. Streaming without lrecl_fixed returns error
// ---------------------------------------------------------------------------

#[test]
fn stream_without_lrecl_returns_error() {
    let schema = Schema::from_fields(vec![make_field("X", FieldKind::Alphanum { len: 5 }, 0, 5)]);
    // lrecl_fixed is None
    let reader = Cursor::new(Vec::<u8>::new());
    let err = stream_to_batches(reader, &schema, &ArrowOptions::default()).unwrap_err();
    match err {
        ArrowError::SchemaConversion(msg) => {
            assert!(
                msg.contains("lrecl_fixed"),
                "expected lrecl_fixed in message: {msg}"
            );
        }
        other => panic!("Expected SchemaConversion, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// 8. stream_to_batches_with_schema returns matching schema
// ---------------------------------------------------------------------------

#[test]
fn stream_to_batches_with_schema_returns_correct_schema() {
    let schema = make_schema_with_lrecl(
        vec![make_field("NAME", FieldKind::Alphanum { len: 4 }, 0, 4)],
        4,
    );
    let data = b"ABCD";
    let reader = Cursor::new(data.as_slice());

    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        ..ArrowOptions::default()
    };
    let (arrow_schema, batches) = stream_to_batches_with_schema(reader, &schema, &opts).unwrap();

    assert_eq!(arrow_schema.fields().len(), 1);
    assert_eq!(arrow_schema.field(0).name(), "NAME");
    assert_eq!(*arrow_schema.field(0).data_type(), DataType::Utf8);
    assert_eq!(batches.len(), 1);
    assert_eq!(batches[0].num_rows(), 1);
}

// ---------------------------------------------------------------------------
// 9. Streaming with empty input produces no batches
// ---------------------------------------------------------------------------

#[test]
fn stream_empty_input_produces_no_batches() {
    let schema = make_schema_with_lrecl(
        vec![make_field("X", FieldKind::Alphanum { len: 4 }, 0, 4)],
        4,
    );
    let reader = Cursor::new(Vec::<u8>::new());
    let batches = stream_to_batches(reader, &schema, &ArrowOptions::default()).unwrap();
    assert!(batches.is_empty());
}

// ---------------------------------------------------------------------------
// 10. IPC roundtrip writes valid file
// ---------------------------------------------------------------------------

#[test]
fn ipc_roundtrip_write_read() {
    let schema = make_schema_with_lrecl(
        vec![make_field("VAL", FieldKind::Alphanum { len: 3 }, 0, 3)],
        3,
    );
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        ..ArrowOptions::default()
    };

    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder.append_record(b"ABC").unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_ipc(tmp.path(), &[batch], &arrow_schema).unwrap();

    // Verify file is non-empty
    let metadata = std::fs::metadata(tmp.path()).unwrap();
    assert!(metadata.len() > 0);
}

// ---------------------------------------------------------------------------
// 11. Parquet roundtrip writes valid file
// ---------------------------------------------------------------------------

#[test]
fn parquet_roundtrip_write() {
    let schema = make_schema_with_lrecl(
        vec![make_field("VAL", FieldKind::Alphanum { len: 3 }, 0, 3)],
        3,
    );
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        compression: Compression::None,
        ..ArrowOptions::default()
    };

    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder =
        RecordBatchBuilder::new(Arc::new(arrow_schema.clone()), &schema, &opts).unwrap();
    builder.append_record(b"XYZ").unwrap();
    let batch = builder.flush().unwrap().unwrap();

    let tmp = tempfile::NamedTempFile::new().unwrap();
    write_parquet(tmp.path(), &[batch], &arrow_schema, &opts, None).unwrap();

    let metadata = std::fs::metadata(tmp.path()).unwrap();
    assert!(metadata.len() > 0);
}

// ---------------------------------------------------------------------------
// 12. Parquet with copybook embedding
// ---------------------------------------------------------------------------

#[test]
fn parquet_embeds_copybook_text_when_configured() {
    let schema = make_schema_with_lrecl(
        vec![make_field("F", FieldKind::Alphanum { len: 2 }, 0, 2)],
        2,
    );
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        embed_copybook: true,
        compression: Compression::None,
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
        Some("       01 REC.\n         05 F PIC X(2)."),
    )
    .unwrap();

    // Read back and verify metadata is embedded in the Arrow schema
    let file = std::fs::File::open(tmp.path()).unwrap();
    let builder =
        parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder::try_new(file).unwrap();
    let arrow_meta = builder.schema().metadata();

    assert!(
        arrow_meta.contains_key("copybook_rs.version"),
        "copybook_rs.version metadata missing; keys: {:?}",
        arrow_meta.keys().collect::<Vec<_>>()
    );
    assert!(
        arrow_meta.contains_key("copybook_rs.copybook_text"),
        "copybook_rs.copybook_text metadata missing; keys: {:?}",
        arrow_meta.keys().collect::<Vec<_>>()
    );
}

// ---------------------------------------------------------------------------
// 13. Record shorter than field range -> null
// ---------------------------------------------------------------------------

#[test]
fn short_record_produces_null_column() {
    // Field at offset 0, len 10, but we'll supply only 5 bytes
    let schema = Schema::from_fields(vec![make_field(
        "BIG",
        FieldKind::Alphanum { len: 10 },
        0,
        10,
    )]);
    let opts = ArrowOptions {
        codepage: copybook_codec::Codepage::ASCII,
        ..ArrowOptions::default()
    };

    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();
    builder.append_record(b"SHORT").unwrap(); // only 5 bytes
    let batch = builder.flush().unwrap().unwrap();

    assert_eq!(batch.num_rows(), 1);
    // The accumulator should have appended null because the record is too short
    assert_eq!(batch.column(0).null_count(), 1);
}

// ---------------------------------------------------------------------------
// 14. Multiple batch auto-flush and final flush
// ---------------------------------------------------------------------------

#[test]
fn streaming_respects_batch_size_boundary() {
    let schema = make_schema_with_lrecl(
        vec![make_field("D", FieldKind::Alphanum { len: 2 }, 0, 2)],
        2,
    );
    // 5 records, batch_size = 2 -> 2 full batches + 1 partial
    let data: Vec<u8> = b"AABBCCDDEE".to_vec();
    let reader = Cursor::new(data);

    let opts = ArrowOptions {
        batch_size: 2,
        codepage: copybook_codec::Codepage::ASCII,
        ..ArrowOptions::default()
    };
    let batches = stream_to_batches(reader, &schema, &opts).unwrap();
    assert_eq!(batches.len(), 3);
    assert_eq!(batches[0].num_rows(), 2);
    assert_eq!(batches[1].num_rows(), 2);
    assert_eq!(batches[2].num_rows(), 1);
}

// ---------------------------------------------------------------------------
// 15. ArrowError Display variants
// ---------------------------------------------------------------------------

#[test]
fn arrow_error_display_all_variants() {
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

// ---------------------------------------------------------------------------
// 16. Edited PIC as decimal vs string
// ---------------------------------------------------------------------------

#[test]
fn edited_pic_decimal_vs_string_schema_mapping() {
    let field = make_field(
        "TOTAL",
        FieldKind::EditedNumeric {
            pic_string: "$ZZ,ZZ9.99".to_string(),
            width: 10,
            scale: 2,
            signed: false,
        },
        0,
        10,
    );
    let schema = Schema::from_fields(vec![field]);

    // Decimal mode (default)
    let dec_opts = ArrowOptions::default();
    let arrow_dec = cobol_schema_to_arrow(&schema, &dec_opts).unwrap();
    assert!(
        matches!(arrow_dec.field(0).data_type(), DataType::Decimal128(38, 2)),
        "expected Decimal128(38,2), got {:?}",
        arrow_dec.field(0).data_type()
    );

    // String mode
    let str_opts = ArrowOptions {
        edited_pic_as: EditedPicRepresentation::String,
        ..ArrowOptions::default()
    };
    let arrow_str = cobol_schema_to_arrow(&schema, &str_opts).unwrap();
    assert_eq!(*arrow_str.field(0).data_type(), DataType::Utf8);
}

// ---------------------------------------------------------------------------
// 17. Binary int 32-bit signed decode through builder
// ---------------------------------------------------------------------------

#[test]
fn batch_builder_int32_signed() {
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

    // Big-endian 42
    builder.append_record(&[0x00, 0x00, 0x00, 0x2A]).unwrap();
    // Big-endian -1 (0xFFFFFFFF)
    builder.append_record(&[0xFF, 0xFF, 0xFF, 0xFF]).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Int32Array>()
        .unwrap();
    assert_eq!(col.value(0), 42);
    assert_eq!(col.value(1), -1);
}

// ---------------------------------------------------------------------------
// 18. Float64 decode through builder
// ---------------------------------------------------------------------------

#[test]
fn batch_builder_float64_ieee_big_endian() {
    let schema = Schema::from_fields(vec![make_field("RATE", FieldKind::FloatDouble, 0, 8)]);
    let opts = ArrowOptions {
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
        ..ArrowOptions::default()
    };
    let arrow_schema = cobol_schema_to_arrow(&schema, &opts).unwrap();
    let mut builder = RecordBatchBuilder::new(Arc::new(arrow_schema), &schema, &opts).unwrap();

    // IEEE 754 big-endian for 1.0
    let one_be = 1.0_f64.to_be_bytes();
    builder.append_record(&one_be).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Float64Array>()
        .unwrap();
    assert!((col.value(0) - 1.0).abs() < f64::EPSILON);
}

// ---------------------------------------------------------------------------
// 19. Fixed-size OCCURS wraps scalar in FixedSizeList
// ---------------------------------------------------------------------------

#[test]
fn fixed_occurs_on_packed_decimal_wraps_in_list() {
    let mut f = make_field(
        "ITEMS",
        FieldKind::PackedDecimal {
            digits: 5,
            scale: 0,
            signed: true,
        },
        0,
        3,
    );
    f.occurs = Some(Occurs::Fixed { count: 3 });

    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    match arrow.field(0).data_type() {
        DataType::FixedSizeList(inner, 3) => {
            assert_eq!(*inner.data_type(), DataType::Decimal128(5, 0));
        }
        other => panic!("Expected FixedSizeList(_, 3), got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// 20. ODO wraps in variable List
// ---------------------------------------------------------------------------

#[test]
fn odo_on_binary_int_wraps_in_list() {
    let mut f = make_field(
        "VALS",
        FieldKind::BinaryInt {
            bits: 16,
            signed: false,
        },
        0,
        2,
    );
    f.occurs = Some(Occurs::ODO {
        min: 0,
        max: 50,
        counter_path: "CNT".to_string(),
    });

    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    match arrow.field(0).data_type() {
        DataType::List(inner) => {
            assert_eq!(*inner.data_type(), DataType::UInt16);
        }
        other => panic!("Expected List, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// 21. Mixed schema: multiple field kinds in one schema
// ---------------------------------------------------------------------------

#[test]
fn mixed_schema_all_scalar_types() {
    let schema = Schema::from_fields(vec![
        make_field("NAME", FieldKind::Alphanum { len: 10 }, 0, 10),
        make_field(
            "ZONE",
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
            "PACK",
            FieldKind::PackedDecimal {
                digits: 5,
                scale: 0,
                signed: false,
            },
            17,
            3,
        ),
        make_field(
            "INT",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
            20,
            4,
        ),
        make_field("F32", FieldKind::FloatSingle, 24, 4),
        make_field("F64", FieldKind::FloatDouble, 28, 8),
    ]);

    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 6);
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
    assert_eq!(*arrow.field(1).data_type(), DataType::Decimal128(7, 2));
    assert_eq!(*arrow.field(2).data_type(), DataType::Decimal128(5, 0));
    assert_eq!(*arrow.field(3).data_type(), DataType::Int32);
    assert_eq!(*arrow.field(4).data_type(), DataType::Float32);
    assert_eq!(*arrow.field(5).data_type(), DataType::Float64);
}

// ---------------------------------------------------------------------------
// 22. ArrowOptions default values
// ---------------------------------------------------------------------------

#[test]
fn arrow_options_defaults() {
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

// ---------------------------------------------------------------------------
// 23. Negative packed decimal through builder
// ---------------------------------------------------------------------------

#[test]
fn batch_builder_negative_packed_decimal() {
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

    // -12345: 0x12 0x34 0x5D (D = negative sign nibble)
    builder.append_record(&[0x12, 0x34, 0x5D]).unwrap();

    let batch = builder.flush().unwrap().unwrap();
    let col = batch
        .column(0)
        .as_any()
        .downcast_ref::<Decimal128Array>()
        .unwrap();
    assert_eq!(col.value(0), -12345);
}
