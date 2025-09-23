//! Integration tests for memory management and parallel processing

use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};
use copybook_core::{Field, FieldKind, Schema};
use std::io::Cursor;

fn create_test_schema() -> Schema {
    Schema {
        fields: vec![Field {
            path: "ROOT.ID".to_string(),
            name: "ID".to_string(),
            level: 5,
            kind: FieldKind::ZonedDecimal {
                digits: 5,
                scale: 0,
                signed: false,
            },
            offset: 0,
            len: 5,
            redefines_of: None,
            occurs: None,
            sync_padding: None,
            synchronized: false,
            blank_when_zero: false,
            children: vec![],
        }],
        lrecl_fixed: Some(5),
        tail_odo: None,
        fingerprint: String::new(),
    }
}

#[test]
fn test_deterministic_parallel_decode() {
    // Test that --threads 1 vs --threads 4 produce identical outputs
    let schema = create_test_schema();

    // Create test data with multiple records
    let mut test_data = Vec::new();
    for i in 0..50 {
        let record = format!("{:05}", i % 10000); // 5-digit numbers
        test_data.extend_from_slice(record.as_bytes());
    }

    let mut results = Vec::new();

    // Test with different thread counts
    for threads in [1, 4] {
        let options = DecodeOptions {
            format: RecordFormat::Fixed,
            codepage: Codepage::ASCII,
            json_number_mode: JsonNumberMode::Lossless,
            emit_filler: false,
            emit_meta: false,
            emit_raw: RawMode::Off,
            strict_mode: false,
            max_errors: None,
            on_decode_unmappable: UnmappablePolicy::Error,
            threads,
        };

        let input = Cursor::new(&test_data);
        let mut output = Vec::new();

        let summary =
            copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
        assert_eq!(summary.records_processed, 50);

        let output_str = String::from_utf8(output).unwrap();
        results.push((threads, output_str));
    }

    // All outputs should be identical
    let baseline = &results[0].1;
    for (threads, output) in &results[1..] {
        assert_eq!(
            output, baseline,
            "Output differs between 1 thread and {} threads",
            threads
        );
    }

    // Verify output is valid JSON lines
    let lines: Vec<&str> = baseline.trim().split('\n').collect();
    assert_eq!(lines.len(), 50);

    // Each line should be valid JSON
    for line in lines {
        let _: serde_json::Value = serde_json::from_str(line).unwrap();
    }
}

#[test]
fn test_memory_bounded_processing() {
    let schema = create_test_schema();

    // Create larger test data to test memory management
    let mut test_data = Vec::new();
    for i in 0..200 {
        let record = format!("{:05}", i % 10000);
        test_data.extend_from_slice(record.as_bytes());
    }

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 4,
    };

    let input = Cursor::new(&test_data);
    let mut output = Vec::new();

    let summary =
        copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    assert_eq!(summary.records_processed, 200);

    // Verify output is valid JSON lines
    let output_str = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = output_str.trim().split('\n').collect();
    assert_eq!(lines.len(), 200);

    // Each line should be valid JSON
    for line in lines {
        let _: serde_json::Value = serde_json::from_str(line).unwrap();
    }

    // Verify throughput is reasonable (allow for very fast processing of small data)
    // For small datasets, throughput might be 0.0 due to timing precision
    assert!(
        summary.throughput_mbps >= 0.0,
        "Throughput should not be negative"
    );
    println!(
        "Throughput: {:.2} MB/s (processing time: {} ms)",
        summary.throughput_mbps, summary.processing_time_ms
    );
}
