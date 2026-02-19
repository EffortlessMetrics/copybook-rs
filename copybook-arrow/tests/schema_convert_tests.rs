//! Schema conversion tests for COBOL -> Arrow type mapping

#![allow(clippy::unwrap_used)]

use arrow::datatypes::DataType;
use copybook_arrow::options::{ArrowOptions, EditedPicRepresentation};
use copybook_arrow::schema_convert::cobol_schema_to_arrow;
use copybook_core::schema::{Field, FieldKind, Occurs, Schema};

fn make_field(name: &str, kind: FieldKind, offset: u32, len: u32) -> Field {
    let mut f = Field::with_kind(5, name.to_string(), kind);
    f.path = name.to_string();
    f.offset = offset;
    f.len = len;
    f
}

#[test]
fn alphanum_maps_to_utf8() {
    let schema = Schema::from_fields(vec![make_field(
        "NAME",
        FieldKind::Alphanum { len: 30 },
        0,
        30,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
    assert_eq!(arrow.field(0).name(), "NAME");
}

#[test]
fn zoned_decimal_maps_to_decimal128() {
    let schema = Schema::from_fields(vec![make_field(
        "PRICE",
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
fn packed_decimal_maps_to_decimal128() {
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
fn binary_int_maps_to_correct_types() {
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
fn wide_decimal_falls_back_to_utf8() {
    let schema = Schema::from_fields(vec![make_field(
        "WIDE",
        FieldKind::ZonedDecimal {
            digits: 40,
            scale: 0,
            signed: false,
            sign_separate: None,
        },
        0,
        40,
    )]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
}

#[test]
fn condition_and_renames_skipped() {
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
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 1);
    assert_eq!(arrow.field(0).name(), "DATA");
}

#[test]
fn group_flattening_promotes_children() {
    let mut group = Field::new(1, "GRP".to_string());
    group.path = "GRP".to_string();

    let mut child1 = Field::with_kind(5, "A".to_string(), FieldKind::Alphanum { len: 5 });
    child1.path = "GRP.A".to_string();
    child1.offset = 0;
    child1.len = 5;

    let mut child2 = Field::with_kind(5, "B".to_string(), FieldKind::Alphanum { len: 5 });
    child2.path = "GRP.B".to_string();
    child2.offset = 5;
    child2.len = 5;

    group.children = vec![child1, child2];

    let schema = Schema::from_fields(vec![group]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 2);
    assert_eq!(arrow.field(0).name(), "A");
    assert_eq!(arrow.field(1).name(), "B");
}

#[test]
fn filler_fields_skipped_by_default() {
    let schema = Schema::from_fields(vec![
        make_field("DATA", FieldKind::Alphanum { len: 10 }, 0, 10),
        make_field("_filler_00000010", FieldKind::Alphanum { len: 5 }, 10, 5),
    ]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(arrow.fields().len(), 1);
}

#[test]
fn edited_pic_as_decimal() {
    let schema = Schema::from_fields(vec![make_field(
        "AMT",
        FieldKind::EditedNumeric {
            pic_string: "ZZ,ZZ9.99".to_string(),
            width: 9,
            scale: 2,
            signed: false,
        },
        0,
        9,
    )]);
    let opts = ArrowOptions::default(); // Decimal is default
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Decimal128(38, 2));
}

#[test]
fn edited_pic_as_string() {
    let schema = Schema::from_fields(vec![make_field(
        "AMT",
        FieldKind::EditedNumeric {
            pic_string: "ZZ,ZZ9.99".to_string(),
            width: 9,
            scale: 2,
            signed: false,
        },
        0,
        9,
    )]);
    let opts = ArrowOptions {
        edited_pic_as: EditedPicRepresentation::String,
        ..ArrowOptions::default()
    };
    let arrow = cobol_schema_to_arrow(&schema, &opts).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Utf8);
}

#[test]
fn fixed_occurs_wraps_in_fixed_size_list() {
    let mut f = make_field("ITEMS", FieldKind::Alphanum { len: 10 }, 0, 10);
    f.occurs = Some(Occurs::Fixed { count: 5 });

    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();

    match arrow.field(0).data_type() {
        DataType::FixedSizeList(_, size) => assert_eq!(*size, 5),
        other => panic!("Expected FixedSizeList, got {other:?}"),
    }
}

#[test]
fn odo_wraps_in_list() {
    let mut f = make_field("ITEMS", FieldKind::Alphanum { len: 10 }, 0, 10);
    f.occurs = Some(Occurs::ODO {
        min: 1,
        max: 100,
        counter_path: "COUNT".to_string(),
    });

    let schema = Schema::from_fields(vec![f]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert!(matches!(arrow.field(0).data_type(), DataType::List(_)));
}

#[test]
fn float_types() {
    let schema = Schema::from_fields(vec![
        make_field("F32", FieldKind::FloatSingle, 0, 4),
        make_field("F64", FieldKind::FloatDouble, 4, 8),
    ]);
    let arrow = cobol_schema_to_arrow(&schema, &ArrowOptions::default()).unwrap();
    assert_eq!(*arrow.field(0).data_type(), DataType::Float32);
    assert_eq!(*arrow.field(1).data_type(), DataType::Float64);
}
