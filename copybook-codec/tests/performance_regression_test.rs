#![allow(
    clippy::items_after_statements,
    clippy::too_many_lines,
    clippy::similar_names,
    clippy::cast_precision_loss,
    clippy::cast_sign_loss,
    clippy::print_literal,
    clippy::uninlined_format_args,
    clippy::bool_assert_comparison
)]

use copybook_codec::{DecodeOptions, decode_record};
use copybook_core::parse_copybook;
use std::time::Instant;

const ZONED_HEAVY_COPYBOOK: &str = r"
       01  ZONED-RECORD.
           05  FIELD-01            PIC S9(9).
           05  FIELD-02            PIC S9(9).
           05  FIELD-03            PIC S9(9).
           05  FIELD-04            PIC S9(9).
           05  FIELD-05            PIC S9(9).
";

#[test]
fn test_performance_regression_preserve_zoned_encoding() {
    let schema = parse_copybook(ZONED_HEAVY_COPYBOOK).unwrap();

    // Create test data: 5 fields × 9 bytes each = 45 bytes total
    let mut zoned_data = Vec::new();
    for _field in 0..5 {
        // Each field: S9(9) = 9 bytes of EBCDIC digits with positive sign
        for i in 0..8 {
            zoned_data.push(0xF0 + (i % 10) as u8); // EBCDIC digits F0-F9
        }
        zoned_data.push(0xF0); // Last digit with positive sign (F0 = +0)
    }

    let options_default = DecodeOptions::default();
    let options_preserve = DecodeOptions::default().with_preserve_zoned_encoding(true);

    println!(
        "preserve_zoned_encoding (default): {}",
        options_default.preserve_zoned_encoding
    );
    println!(
        "preserve_zoned_encoding (enabled): {}",
        options_preserve.preserve_zoned_encoding
    );

    const ITERATIONS: usize = 1000;

    // Test default (should be fast)
    let start = Instant::now();
    for _ in 0..ITERATIONS {
        let _result = decode_record(&schema, &zoned_data, &options_default).unwrap();
    }
    let default_time = start.elapsed();

    // Test with preserve enabled (may be slower)
    let start = Instant::now();
    for _ in 0..ITERATIONS {
        let _result = decode_record(&schema, &zoned_data, &options_preserve).unwrap();
    }
    let preserve_time = start.elapsed();

    let slowdown_factor = preserve_time.as_nanos() as f64 / default_time.as_nanos() as f64;

    println!("Default time: {:?}", default_time);
    println!("Preserve time: {:?}", preserve_time);
    println!("Slowdown factor: {:.2}x", slowdown_factor);

    // Performance assertion: preserve_zoned_encoding should add reasonable overhead
    // Allow more relaxed thresholds in CI environments
    let max_overhead = if std::env::var("CI").is_ok() { 5.0 } else { 3.0 };
    assert!(
        slowdown_factor < max_overhead,
        "Performance regression: preserve_zoned_encoding adds {:.2}x overhead (should be < {:.1}x)",
        slowdown_factor, max_overhead
    );
}

#[test]
fn test_default_behavior_unchanged() {
    let schema = parse_copybook(ZONED_HEAVY_COPYBOOK).unwrap();

    let mut zoned_data = Vec::new();
    for _field in 0..5 {
        for i in 0..8 {
            zoned_data.push(0xF0 + (i % 10) as u8);
        }
        zoned_data.push(0xF0);
    }

    // Verify that default options have preserve_zoned_encoding = false
    let options_default = DecodeOptions::default();
    assert_eq!(
        options_default.preserve_zoned_encoding, false,
        "Default DecodeOptions should have preserve_zoned_encoding = false"
    );

    // Verify that it works correctly
    let result = decode_record(&schema, &zoned_data, &options_default).unwrap();
    println!("Default decode result: {}", result);

    // Should be a JSON object with 5 fields
    assert!(result.is_object());
    let obj = result.as_object().unwrap();
    assert_eq!(obj.len(), 5);
}
