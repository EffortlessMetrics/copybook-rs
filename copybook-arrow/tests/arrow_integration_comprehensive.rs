// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive Arrow integration tests for copybook-rs
//!
//! This test suite validates Arrow and Parquet conversion functionality,
//! including error handling, edge cases, and integration with copybook data.

#![allow(clippy::unwrap_used)]
#![allow(clippy::expect_used)]
#![allow(deprecated)]

use arrow::array::{ArrayRef, Int64Array, StringArray};
use arrow::datatypes::{DataType, Field, Schema};
use copybook_arrow::{ArrowError, ArrowWriter, json_to_record_batch, json_to_schema};
use serde_json::json;
use std::sync::Arc;

#[test]
fn test_json_to_schema_simple() {
    let json_value = json!({
        "name": "string",
        "age": 42,
        "active": true
    });

    let result = json_to_schema(&json_value);
    assert!(result.is_ok());

    let schema = result.unwrap();
    assert_eq!(schema.fields().len(), 3);
    assert_eq!(schema.field(0).name(), "name");
    assert_eq!(schema.field(1).name(), "age");
    assert_eq!(schema.field(2).name(), "active");
}

#[test]
fn test_json_to_schema_empty_object() {
    let json_value = json!({});
    let result = json_to_schema(&json_value);
    assert!(result.is_ok());
    let schema = result.unwrap();
    assert_eq!(schema.fields().len(), 0);
}

#[test]
fn test_json_to_schema_not_object() {
    let json_value = json!("not an object");
    let result = json_to_schema(&json_value);
    assert!(result.is_err());
    match result.unwrap_err() {
        ArrowError::JsonConversion(msg) => {
            assert!(msg.contains("Expected JSON object"));
        }
        _ => panic!("Expected JsonConversion error"),
    }
}

#[test]
fn test_json_to_schema_nested() {
    let json_value = json!({
        "simple": "value",
        "nested": {
            "inner": 123
        }
    });

    let result = json_to_schema(&json_value);
    assert!(result.is_ok());

    let schema = result.unwrap();
    assert_eq!(schema.fields().len(), 2);
    assert_eq!(schema.field(0).name(), "simple");
    assert_eq!(schema.field(1).name(), "nested");
}

#[test]
fn test_json_to_record_batch_simple() {
    let schema = Schema::new(vec![
        Field::new("name", DataType::Utf8, true),
        Field::new("age", DataType::Int64, true),
    ]);

    let json_value = json!({
        "name": "Alice",
        "age": 30
    });

    let result = json_to_record_batch(&schema, &json_value);
    assert!(result.is_ok());

    let batch = result.unwrap();
    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.num_columns(), 2);
}

#[test]
fn test_json_to_record_batch_missing_field() {
    let schema = Schema::new(vec![
        Field::new("name", DataType::Utf8, true),
        Field::new("age", DataType::Int64, true),
        Field::new("city", DataType::Utf8, true),
    ]);

    let json_value = json!({
        "name": "Bob",
        "age": 25
        // city is missing
    });

    let result = json_to_record_batch(&schema, &json_value);
    assert!(result.is_ok());

    let batch = result.unwrap();
    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.num_columns(), 3);
    assert_eq!(batch.column(2).null_count(), 1);
}

#[test]
fn test_json_to_record_batch_not_object() {
    let schema = Schema::new(vec![Field::new("field", DataType::Utf8, true)]);
    let json_value = json!("not an object");

    let result = json_to_record_batch(&schema, &json_value);
    assert!(result.is_err());
    match result.unwrap_err() {
        ArrowError::JsonConversion(msg) => {
            assert!(msg.contains("Expected JSON object"));
        }
        _ => panic!("Expected JsonConversion error"),
    }
}

#[test]
fn test_arrow_writer_new() {
    let schema = Schema::new(vec![Field::new("field", DataType::Utf8, true)]);
    let writer = ArrowWriter::new(schema.clone());

    assert_eq!(writer.batch_count(), 0);
    assert_eq!(writer.schema().as_ref(), &schema);
    assert!(writer.batches().is_empty());
}

#[test]
fn test_arrow_writer_add_batch() {
    let schema = Schema::new(vec![Field::new("field", DataType::Utf8, true)]);
    let mut writer = ArrowWriter::new(schema.clone());

    let array = Arc::new(StringArray::from(vec![Some("value")]));
    let batch =
        arrow::record_batch::RecordBatch::try_new(Arc::new(schema), vec![array as ArrayRef])
            .unwrap();

    writer.add_batch(batch.clone());
    assert_eq!(writer.batch_count(), 1);
    assert_eq!(writer.batches().len(), 1);
}

#[test]
fn test_arrow_writer_add_json_record() {
    let schema = Schema::new(vec![
        Field::new("name", DataType::Utf8, true),
        Field::new("age", DataType::Int64, true),
    ]);
    let mut writer = ArrowWriter::new(schema.clone());

    let json_value = json!({
        "name": "Charlie",
        "age": 35
    });

    let result = writer.add_json_record(&json_value);
    assert!(result.is_ok());
    assert_eq!(writer.batch_count(), 1);
}

#[test]
fn test_arrow_writer_add_json_record_schema_mismatch() {
    let schema = Schema::new(vec![Field::new("name", DataType::Utf8, true)]);
    let mut writer = ArrowWriter::new(schema);

    let json_value = json!({
        "name": "Dave",
        "extra_field": 42
    });

    let result = writer.add_json_record(&json_value);
    // Should succeed - extra fields are ignored
    assert!(result.is_ok());
}

#[test]
fn test_arrow_writer_from_json_schema() {
    let json_schema = json!({
        "field1": "value",
        "field2": 123
    });

    let result = ArrowWriter::from_json_schema(&json_schema);
    assert!(result.is_ok());

    let writer = result.unwrap();
    assert_eq!(writer.batch_count(), 0);
    assert_eq!(writer.schema().fields().len(), 2);
}

#[test]
fn test_arrow_writer_from_json_schema_invalid() {
    let json_schema = json!("not an object");

    let result = ArrowWriter::from_json_schema(&json_schema);
    assert!(result.is_err());
}

#[test]
fn test_arrow_writer_multiple_batches() {
    let schema = Schema::new(vec![
        Field::new("name", DataType::Utf8, true),
        Field::new("age", DataType::Int64, true),
    ]);
    let mut writer = ArrowWriter::new(schema.clone());

    for i in 0..5 {
        let json_value = json!({
            "name": format!("Person{}", i),
            "age": 20 + i
        });
        writer.add_json_record(&json_value).unwrap();
    }

    assert_eq!(writer.batch_count(), 5);
    assert_eq!(writer.batches().len(), 5);
}

#[test]
fn test_arrow_error_display() {
    let error = ArrowError::JsonConversion("test error".to_string());
    let display = format!("{}", error);
    assert!(display.contains("JSON conversion error"));
    assert!(display.contains("test error"));

    let error = ArrowError::SchemaConversion("schema error".to_string());
    let display = format!("{}", error);
    assert!(display.contains("Schema conversion error"));

    let error = ArrowError::ParquetWrite("write error".to_string());
    let display = format!("{}", error);
    assert!(display.contains("Parquet write error"));
}

#[test]
fn test_arrow_error_from_io() {
    let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
    let arrow_error: ArrowError = io_error.into();
    assert!(matches!(arrow_error, ArrowError::Io(_)));
}

#[test]
fn test_arrow_error_from_arrow() {
    use arrow::error::ArrowError as ArrowErr;
    let arrow_err = ArrowErr::ParseError("parse error".to_string());
    let arrow_error: ArrowError = arrow_err.into();
    assert!(matches!(arrow_error, ArrowError::Arrow(_)));
}

#[test]
fn test_json_to_schema_with_special_characters() {
    let json_value = json!({
        "field-with-dash": "value1",
        "field_with_underscore": "value2",
        "field.with.dot": "value3"
    });

    let result = json_to_schema(&json_value);
    assert!(result.is_ok());

    let schema = result.unwrap();
    assert_eq!(schema.fields().len(), 3);
    assert_eq!(schema.field(0).name(), "field-with-dash");
    assert_eq!(schema.field(1).name(), "field_with_underscore");
    assert_eq!(schema.field(2).name(), "field.with.dot");
}

#[test]
fn test_json_to_record_batch_all_nulls() {
    let schema = Schema::new(vec![
        Field::new("name", DataType::Utf8, true),
        Field::new("age", DataType::Int64, true),
    ]);

    let json_value = json!({
        "name": serde_json::Value::Null,
        "age": serde_json::Value::Null
    });

    let result = json_to_record_batch(&schema, &json_value);
    assert!(result.is_ok());

    let batch = result.unwrap();
    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.column(0).null_count(), 1);
    assert_eq!(batch.column(1).null_count(), 1);
}

#[test]
fn test_json_to_record_batch_mixed_types() {
    let schema = Schema::new(vec![
        Field::new("string_field", DataType::Utf8, true),
        Field::new("int_field", DataType::Int64, true),
        Field::new("bool_field", DataType::Boolean, true),
    ]);

    let json_value = json!({
        "string_field": "hello",
        "int_field": 42,
        "bool_field": true
    });

    let result = json_to_record_batch(&schema, &json_value);
    assert!(result.is_ok());

    let batch = result.unwrap();
    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.num_columns(), 3);
}

#[test]
fn test_json_to_record_batch_empty_string() {
    let schema = Schema::new(vec![Field::new("empty_field", DataType::Utf8, true)]);

    let json_value = json!({
        "empty_field": ""
    });

    let result = json_to_record_batch(&schema, &json_value);
    assert!(result.is_ok());

    let batch = result.unwrap();
    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.column(0).null_count(), 0);

    let string_array = batch
        .column(0)
        .as_any()
        .downcast_ref::<StringArray>()
        .unwrap();
    assert_eq!(string_array.value(0), "");
}

#[test]
fn test_json_to_record_batch_zero_values() {
    let schema = Schema::new(vec![Field::new("zero_field", DataType::Int64, true)]);

    let json_value = json!({
        "zero_field": 0
    });

    let result = json_to_record_batch(&schema, &json_value);
    assert!(result.is_ok());

    let batch = result.unwrap();
    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.column(0).null_count(), 0);

    let int_array = batch
        .column(0)
        .as_any()
        .downcast_ref::<Int64Array>()
        .unwrap();
    assert_eq!(int_array.value(0), 0);
}

#[test]
fn test_json_to_record_batch_negative_values() {
    let schema = Schema::new(vec![Field::new("negative_field", DataType::Int64, true)]);

    let json_value = json!({
        "negative_field": -999
    });

    let result = json_to_record_batch(&schema, &json_value);
    assert!(result.is_ok());

    let batch = result.unwrap();
    assert_eq!(batch.num_rows(), 1);
    assert_eq!(batch.column(0).null_count(), 0);

    let int_array = batch
        .column(0)
        .as_any()
        .downcast_ref::<Int64Array>()
        .unwrap();
    assert_eq!(int_array.value(0), -999);
}

#[test]
fn test_json_to_schema_large_integers() {
    let json_value = json!({
        "max_int": i64::MAX,
        "min_int": i64::MIN
    });

    let result = json_to_schema(&json_value);
    assert!(result.is_ok());

    let schema = result.unwrap();
    assert_eq!(schema.fields().len(), 2);
    assert_eq!(schema.field(0).data_type(), &DataType::Int64);
    assert_eq!(schema.field(1).data_type(), &DataType::Int64);
}

#[test]
fn test_json_to_schema_special_float_values() {
    let json_value = json!({
        "infinity": f64::INFINITY,
        "neg_infinity": f64::NEG_INFINITY,
        "nan": f64::NAN
    });

    let result = json_to_schema(&json_value);
    assert!(result.is_ok());

    let schema = result.unwrap();
    assert_eq!(schema.fields().len(), 3);
    assert_eq!(schema.field(0).data_type(), &DataType::Null);
    assert_eq!(schema.field(1).data_type(), &DataType::Null);
    assert_eq!(schema.field(2).data_type(), &DataType::Null);
}

#[test]
fn test_arrow_writer_schema_field_order() {
    let json_schema = json!({
        "zebra": 1,
        "alpha": 2,
        "beta": 3
    });

    let result = ArrowWriter::from_json_schema(&json_schema);
    assert!(result.is_ok());

    let writer = result.unwrap();
    let fields = writer.schema().fields();
    assert_eq!(fields.len(), 3);
    // Fields should maintain insertion order
    assert_eq!(fields[0].name(), "zebra");
    assert_eq!(fields[1].name(), "alpha");
    assert_eq!(fields[2].name(), "beta");
}

#[test]
fn test_json_to_record_batch_unicode() {
    let schema = Schema::new(vec![Field::new("unicode_field", DataType::Utf8, true)]);

    let json_value = json!({
        "unicode_field": "Hello 世界 🌍"
    });

    let result = json_to_record_batch(&schema, &json_value);
    assert!(result.is_ok());

    let batch = result.unwrap();
    assert_eq!(batch.num_rows(), 1);

    let string_array = batch
        .column(0)
        .as_any()
        .downcast_ref::<StringArray>()
        .unwrap();
    assert_eq!(string_array.value(0), "Hello 世界 🌍");
}

#[test]
fn test_json_to_record_batch_empty_array() {
    let schema = Schema::new(vec![Field::new("array_field", DataType::Utf8, true)]);

    let json_value = json!({
        "array_field": []
    });

    let result = json_to_record_batch(&schema, &json_value);
    assert!(result.is_ok());

    let batch = result.unwrap();
    assert_eq!(batch.num_rows(), 1);

    let string_array = batch
        .column(0)
        .as_any()
        .downcast_ref::<StringArray>()
        .unwrap();
    assert_eq!(string_array.value(0), "[]");
}

#[test]
fn test_json_to_record_batch_empty_object() {
    let schema = Schema::new(vec![Field::new("object_field", DataType::Utf8, true)]);

    let json_value = json!({
        "object_field": {}
    });

    let result = json_to_record_batch(&schema, &json_value);
    assert!(result.is_ok());

    let batch = result.unwrap();
    assert_eq!(batch.num_rows(), 1);

    let string_array = batch
        .column(0)
        .as_any()
        .downcast_ref::<StringArray>()
        .unwrap();
    assert_eq!(string_array.value(0), "{}");
}
