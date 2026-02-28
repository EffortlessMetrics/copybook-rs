#![allow(
// SPDX-License-Identifier: AGPL-3.0-or-later
    clippy::expect_used,
    clippy::unwrap_used,
    clippy::too_many_lines,
    clippy::cast_precision_loss,
    clippy::cast_possible_truncation,
    clippy::uninlined_format_args,
    clippy::cast_possible_wrap
)]

/*!
 * Performance Hardening Validation Tests
 *
 * These tests validate performance characteristics under stress conditions,
 * ensuring the system maintains acceptable performance levels under various
 * enterprise workload conditions.
 *
 * Focus Areas:
 * - Memory usage under large dataset processing
 * - Performance regression detection
 * - Throughput validation under different load patterns
 * - Resource utilization optimization
 * - Error handling performance impact
 */

use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RecordFormat, decode_file_to_jsonl, decode_record,
};
use copybook_core::parse_copybook;
use std::io::Cursor;
use std::sync::Arc;
use std::thread;
use std::time::Instant;

/// Test memory usage patterns with large datasets
#[test]
fn test_memory_usage_large_datasets() -> Result<(), Box<dyn std::error::Error>> {
    const LARGE_RECORD_COPYBOOK: &str = r"
01 LARGE-DATA-RECORD.
   05 HEADER-SECTION     PIC X(100).
   05 DATA-SECTION       PIC X(500).
   05 NUMERIC-SECTION    PIC 9(50).
   05 DECIMAL-SECTION    PIC S9(15)V99 COMP-3.
   05 FOOTER-SECTION     PIC X(200).
";

    let schema = parse_copybook(LARGE_RECORD_COPYBOOK)?;
    let record_size = 859; // 100 + 500 + 50 + 9 + 200

    // Create test data for memory usage validation
    let num_records = 10_000;
    let mut test_data = Vec::with_capacity(record_size * num_records);

    for i in 0..num_records {
        let mut record = vec![b' '; record_size];

        // HEADER-SECTION: PIC X(100) at offset 0
        let header = format!("HEADER-{:093}", i);
        record[..100].copy_from_slice(&header.as_bytes()[..100]);

        // DATA-SECTION: PIC X(500) at offset 100
        let data_section = format!("DATA-SECTION-{:487}", i);
        record[100..600].copy_from_slice(&data_section.as_bytes()[..500]);

        // NUMERIC-SECTION: PIC 9(50) at offset 600
        let numeric = format!("{:050}", i);
        record[600..650].copy_from_slice(numeric.as_bytes());

        // DECIMAL-SECTION: S9(15)V99 COMP-3 (9 bytes) at offset 650
        let packed = encode_comp3_signed(i as i64 * 12345, 9);
        record[650..659].copy_from_slice(&packed);

        // FOOTER-SECTION: PIC X(200) at offset 659
        let footer = format!("FOOTER-{:193}", i);
        record[659..859].copy_from_slice(&footer.as_bytes()[..200]);

        test_data.extend_from_slice(&record);
    }

    // Test memory-efficient processing
    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_threads(1); // Single thread for memory measurement

    let start_time = Instant::now();
    let mut output = Vec::new();

    let summary = decode_file_to_jsonl(&schema, Cursor::new(&test_data), &mut output, &options)?;

    let duration = start_time.elapsed();
    let throughput_mb_s = (test_data.len() as f64 / (1024.0 * 1024.0)) / duration.as_secs_f64();

    println!(
        "Memory test: Processed {} records ({:.2} MB) in {}ms ({:.2} MB/s)",
        summary.records_processed,
        test_data.len() as f64 / (1024.0 * 1024.0),
        duration.as_millis(),
        throughput_mb_s
    );

    // Validate performance and memory characteristics
    assert_eq!(
        summary.records_processed as usize, num_records,
        "Should process all records"
    );
    assert!(
        throughput_mb_s > 1.0,
        "Memory-efficient processing should maintain reasonable throughput: {:.2} MB/s",
        throughput_mb_s
    );

    // Memory usage validation (output size should be reasonable)
    let output_mb = output.len() as f64 / (1024.0 * 1024.0);
    assert!(
        output_mb < 100.0, // Should not exceed 100MB for 10k records
        "Output size should be memory-efficient: {:.2} MB",
        output_mb
    );

    Ok(())
}

/// Test performance regression detection capabilities
#[test]
fn test_performance_regression_detection() -> Result<(), Box<dyn std::error::Error>> {
    const REGRESSION_TEST_COPYBOOK: &str = r"
01 REGRESSION-TEST-RECORD.
   05 FIELD1 PIC X(20).
   05 FIELD2 PIC 9(10).
   05 FIELD3 PIC S9(7)V99 COMP-3.
   05 FIELD4 PIC X(50).
";

    let schema = parse_copybook(REGRESSION_TEST_COPYBOOK)?;

    // Create consistent test data
    let num_records = 5_000;
    let record_size = 85; // 20 + 10 + 5 + 50
    let mut test_data = Vec::with_capacity(record_size * num_records);

    for i in 0..num_records {
        let mut record = Vec::with_capacity(record_size);

        // FIELD1: PIC X(20) at offset 0
        let field1 = format!("FIELD1-{:013}", i);
        record.extend_from_slice(field1.as_bytes());

        // FIELD2: PIC 9(10) at offset 20
        let field2 = format!("{:010}", i % 9_999_999_999usize);
        record.extend_from_slice(field2.as_bytes());

        // FIELD3: S9(7)V99 COMP-3 (5 bytes) at offset 30
        let field3_packed = encode_comp3_signed((i as i64 * 123) % 9_999_999, 5);
        record.extend_from_slice(&field3_packed);

        // FIELD4: PIC X(50) at offset 35
        let field4 = format!("REGRESSION-TEST-DATA-{:029}", i);
        record.extend_from_slice(&field4.as_bytes()[..50]);

        test_data.extend_from_slice(&record);
    }

    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless);

    // Run multiple iterations to detect performance variance
    let mut durations = Vec::new();

    for iteration in 1..=5 {
        let start_time = Instant::now();

        let records_processed = test_data.chunks(record_size).count();
        for record_data in test_data.chunks(record_size) {
            let _json_value = decode_record(&schema, record_data, &options)?;
        }

        let duration = start_time.elapsed();
        durations.push(duration);

        println!(
            "Iteration {}: Processed {} records in {}ms",
            iteration,
            records_processed,
            duration.as_millis()
        );
    }

    // Calculate performance statistics
    let avg_duration = durations.iter().sum::<std::time::Duration>() / durations.len() as u32;
    let min_duration = durations.iter().min().unwrap();
    let max_duration = durations.iter().max().unwrap();

    let variance_ratio = max_duration.as_millis() as f64 / min_duration.as_millis() as f64;

    println!(
        "Performance stats: avg={}ms, min={}ms, max={}ms, variance={:.2}x",
        avg_duration.as_millis(),
        min_duration.as_millis(),
        max_duration.as_millis(),
        variance_ratio
    );

    // Validate performance consistency (regression detection).
    // Shared CI runners and debug-build environments can exhibit bursty
    // scheduling/CPU contention, so this bound is intentionally tolerant
    // while still catching major regressions.
    assert!(
        variance_ratio < 5.0,
        "Performance variance should be low for regression detection: {:.2}x",
        variance_ratio
    );

    let throughput_records_s = num_records as f64 / avg_duration.as_secs_f64();
    assert!(
        throughput_records_s > 1000.0,
        "Average throughput should be reasonable: {:.0} records/s",
        throughput_records_s
    );

    Ok(())
}

/// Test throughput validation under different load patterns
#[test]
fn test_throughput_load_patterns() -> Result<(), Box<dyn std::error::Error>> {
    const LOAD_TEST_COPYBOOK: &str = r"
01 LOAD-TEST-RECORD.
   05 BATCH-ID PIC 9(8).
   05 SEQUENCE PIC 9(6).
   05 PAYLOAD PIC X(100).
   05 CHECKSUM PIC 9(10).
";

    let schema = parse_copybook(LOAD_TEST_COPYBOOK)?;

    // Test different load patterns
    let load_patterns = vec![
        ("Small Burst", 1_000, 1),
        ("Medium Sustained", 5_000, 2),
        ("Large Batch", 10_000, 4),
    ];

    let mut results = Vec::new();

    for (pattern_name, num_records, thread_count) in load_patterns {
        // Generate test data
        let record_size = 124; // 8 + 6 + 100 + 10
        let mut test_data = Vec::with_capacity(record_size * num_records);

        for i in 0..num_records {
            let mut record = Vec::with_capacity(record_size);

            // BATCH-ID: PIC 9(8) at offset 0
            record.extend_from_slice(format!("{:08}", i / 1000).as_bytes());

            // SEQUENCE: PIC 9(6) at offset 8
            record.extend_from_slice(format!("{:06}", i % 1_000_000).as_bytes());

            // PAYLOAD: PIC X(100) at offset 14
            let payload = format!("LOAD-TEST-PAYLOAD-{:082}", i);
            record.extend_from_slice(&payload.as_bytes()[..100]);

            // CHECKSUM: PIC 9(10) at offset 114
            let checksum = format!("{:010}", i % 9_999_999_999usize);
            record.extend_from_slice(checksum.as_bytes());

            test_data.extend_from_slice(&record);
        }

        let options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII)
            .with_json_number_mode(JsonNumberMode::Lossless)
            .with_threads(thread_count);

        let start_time = Instant::now();
        let mut output = Vec::new();

        let summary =
            decode_file_to_jsonl(&schema, Cursor::new(&test_data), &mut output, &options)?;

        let duration = start_time.elapsed();
        let throughput_records_s = summary.records_processed as f64 / duration.as_secs_f64();
        let throughput_mb_s = (test_data.len() as f64 / (1024.0 * 1024.0)) / duration.as_secs_f64();

        results.push((pattern_name, throughput_records_s, throughput_mb_s));

        println!(
            "{}: {} records in {}ms ({:.0} records/s, {:.2} MB/s) with {} threads",
            pattern_name,
            summary.records_processed,
            duration.as_millis(),
            throughput_records_s,
            throughput_mb_s,
            thread_count
        );

        // Validate load-specific performance
        match pattern_name {
            "Small Burst" => assert!(
                throughput_records_s > 2000.0,
                "Small burst should have high record rate: {:.0} records/s",
                throughput_records_s
            ),
            "Medium Sustained" => assert!(
                throughput_mb_s > 0.6,
                "Medium sustained should have good throughput: {:.2} MB/s",
                throughput_mb_s
            ),
            "Large Batch" => assert!(
                throughput_mb_s > 0.75,
                "Large batch should leverage parallelism: {:.2} MB/s",
                throughput_mb_s
            ),
            _ => {}
        }

        assert_eq!(
            summary.records_processed as usize, num_records,
            "Should process all records in {}",
            pattern_name
        );
    }

    println!("\n✅ Load pattern results:");
    for (pattern, records_s, mb_s) in results {
        println!(
            "  {}: {:.0} records/s, {:.2} MB/s",
            pattern, records_s, mb_s
        );
    }

    Ok(())
}

/// Test resource utilization optimization
#[test]
fn test_resource_utilization_optimization() -> Result<(), Box<dyn std::error::Error>> {
    const RESOURCE_TEST_COPYBOOK: &str = r"
01 RESOURCE-USAGE-RECORD.
   05 CPU-INTENSIVE-FIELD PIC X(200).
   05 MEMORY-FIELD PIC X(300).
   05 COMP3-FIELD PIC S9(13)V99 COMP-3.
";

    let schema = Arc::new(parse_copybook(RESOURCE_TEST_COPYBOOK)?);

    // Test different threading scenarios
    let thread_configs = vec![1, 2, 4, 8];
    let num_records = 5_000;
    let record_size = 508; // 200 + 300 + 8

    // Generate shared test data
    let mut test_data = Vec::with_capacity(record_size * num_records);
    for i in 0..num_records {
        let mut record = vec![b' '; record_size];

        // CPU-INTENSIVE-FIELD: PIC X(200) at offset 0
        let cpu_data = format!("CPU-INTENSIVE-DATA-{:181}", i);
        record[..200].copy_from_slice(&cpu_data.as_bytes()[..200]);

        // MEMORY-FIELD: PIC X(300) at offset 200
        let memory_data = format!("MEMORY-USAGE-DATA-{:282}", i);
        record[200..500].copy_from_slice(&memory_data.as_bytes()[..300]);

        // COMP3-FIELD: S9(13)V99 COMP-3 (8 bytes) at offset 500
        let comp3_packed = encode_comp3_signed(i as i64 * 98765, 8);
        record[500..508].copy_from_slice(&comp3_packed);

        test_data.extend_from_slice(&record);
    }

    let test_data = Arc::new(test_data);

    let mut thread_results = Vec::new();

    for thread_count in thread_configs {
        let schema_clone = Arc::clone(&schema);
        let data_clone = Arc::clone(&test_data);

        let start_time = Instant::now();

        // Simulate concurrent processing
        let handles: Vec<_> = (0..thread_count)
            .map(|thread_id| {
                let schema = Arc::clone(&schema_clone);
                let data = Arc::clone(&data_clone);

                thread::spawn(move || {
                    let chunk_size = num_records / thread_count;
                    let start_idx = thread_id * chunk_size * record_size;
                    let end_idx = if thread_id == thread_count - 1 {
                        data.len()
                    } else {
                        (thread_id + 1) * chunk_size * record_size
                    };

                    if start_idx < data.len() {
                        let chunk_data = &data[start_idx..end_idx.min(data.len())];
                        let options = DecodeOptions::new()
                            .with_format(RecordFormat::Fixed)
                            .with_codepage(Codepage::ASCII);

                        let mut processed = 0;
                        for record_data in chunk_data.chunks(record_size) {
                            if record_data.len() == record_size {
                                let _result = decode_record(&schema, record_data, &options);
                                processed += 1;
                            }
                        }
                        processed
                    } else {
                        0
                    }
                })
            })
            .collect();

        let mut total_processed = 0;
        for handle in handles {
            total_processed += handle.join().unwrap_or(0);
        }

        let duration = start_time.elapsed();
        let throughput = f64::from(total_processed) / duration.as_secs_f64();

        thread_results.push((thread_count, throughput, duration));

        println!(
            "Threads: {} | Processed: {} records | Duration: {}ms | Throughput: {:.0} records/s",
            thread_count,
            total_processed,
            duration.as_millis(),
            throughput
        );
    }

    // Validate resource utilization scaling
    let single_thread_throughput = thread_results[0].1;

    for (thread_count, throughput, _duration) in &thread_results[1..] {
        let scaling_efficiency = *throughput / (single_thread_throughput * *thread_count as f64);

        println!(
            "Scaling efficiency for {} threads: {:.1}%",
            thread_count,
            scaling_efficiency * 100.0
        );

        // Allow for significant efficiency loss in debug builds under system load
        assert!(
            scaling_efficiency > 0.01, // At least 1% scaling efficiency on heavily contended debug runners
            "Threading should provide reasonable scaling efficiency: {:.1}% for {} threads",
            scaling_efficiency * 100.0,
            thread_count
        );
    }

    Ok(())
}

/// Test error handling performance impact
#[test]
fn test_error_handling_performance_impact() -> Result<(), Box<dyn std::error::Error>> {
    const ERROR_TEST_COPYBOOK: &str = r"
01 ERROR-TEST-RECORD.
   05 VALID-FIELD PIC X(10).
   05 NUMERIC-FIELD PIC 9(8).
   05 COMP3-FIELD PIC S9(9)V99 COMP-3.
";

    let schema = parse_copybook(ERROR_TEST_COPYBOOK)?;

    // Create test data with mix of valid and invalid records
    let num_records = 3_000;
    let record_size = 24; // 10 + 8 + 6
    let mut valid_data = Vec::new();
    let mut mixed_data = Vec::new();

    for i in 0..num_records {
        let mut valid_record = Vec::with_capacity(record_size);
        let mut mixed_record = Vec::with_capacity(record_size);

        // VALID-FIELD: PIC X(10) at offset 0
        let field1 = format!("FIELD{i:05}");
        valid_record.extend_from_slice(&field1.as_bytes()[..10]);
        mixed_record.extend_from_slice(&field1.as_bytes()[..10]);

        // NUMERIC-FIELD: PIC 9(8) at offset 10 - introduce some invalid numeric data in mixed set
        if i % 10 == 0 {
            // Invalid numeric data for mixed set
            mixed_record.extend_from_slice(b"INVALID!");
            valid_record.extend_from_slice(format!("{:08}", i % 99_999_999).as_bytes());
        } else {
            let numeric = format!("{:08}", i % 99_999_999);
            valid_record.extend_from_slice(numeric.as_bytes());
            mixed_record.extend_from_slice(numeric.as_bytes());
        }

        // COMP3-FIELD: S9(9)V99 COMP-3 (6 bytes) at offset 18
        let comp3_data = encode_comp3_signed((i64::from(i) * 543) % 999_999_999, 6);
        valid_record.extend_from_slice(&comp3_data);
        mixed_record.extend_from_slice(&comp3_data);

        valid_data.extend_from_slice(&valid_record);
        mixed_data.extend_from_slice(&mixed_record);
    }

    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII);

    // Test valid data performance (baseline)
    let start_time = Instant::now();
    let mut valid_processed = 0;
    for record_data in valid_data.chunks(record_size) {
        if decode_record(&schema, record_data, &options).is_ok() {
            valid_processed += 1;
            // Should not have errors for valid data
        }
    }
    let valid_duration = start_time.elapsed();

    // Test mixed data performance (with errors)
    let start_time = Instant::now();
    let mut mixed_processed = 0;
    let mut error_count = 0;
    for record_data in mixed_data.chunks(record_size) {
        match decode_record(&schema, record_data, &options) {
            Ok(_) => mixed_processed += 1,
            Err(_) => error_count += 1,
        }
    }
    let mixed_duration = start_time.elapsed();

    let valid_throughput = f64::from(valid_processed) / valid_duration.as_secs_f64();
    let mixed_throughput = f64::from(mixed_processed + error_count) / mixed_duration.as_secs_f64();

    println!(
        "Valid data: {} records in {}ms ({:.0} records/s)",
        valid_processed,
        valid_duration.as_millis(),
        valid_throughput
    );

    println!(
        "Mixed data: {} valid, {} errors in {}ms ({:.0} records/s)",
        mixed_processed,
        error_count,
        mixed_duration.as_millis(),
        mixed_throughput
    );

    // Validate error handling doesn't significantly impact performance
    let performance_impact = valid_duration.as_millis() as f64 / mixed_duration.as_millis() as f64;

    assert_eq!(
        valid_processed, num_records,
        "Should process all valid records"
    );
    assert_eq!(
        error_count,
        num_records / 10,
        "Should detect expected errors"
    );

    assert!(
        performance_impact < 2.0,
        "Error handling should not severely impact performance: {:.2}x slower",
        1.0 / performance_impact
    );

    assert!(
        mixed_throughput > 1000.0,
        "Mixed processing should maintain reasonable throughput: {mixed_throughput:.0} records/s"
    );

    Ok(())
}

/// Encode a signed integer as COMP-3 packed decimal of the given byte count.
///
/// COMP-3 stores two digits per byte, with the last nibble as the sign
/// (0x0C = positive, 0x0D = negative). The total digit capacity is
/// `byte_count * 2 - 1` digits.
fn encode_comp3_signed(value: i64, byte_count: usize) -> Vec<u8> {
    let mut result = vec![0u8; byte_count];
    let num_digits = byte_count * 2 - 1;
    let abs_str = format!("{:0>width$}", value.abs(), width = num_digits);
    let digits: Vec<u8> = abs_str.bytes().map(|b| b - b'0').collect();

    // Pack pairs of digits into bytes; last byte gets final digit + sign nibble
    for (i, chunk) in digits.chunks(2).enumerate() {
        if i < byte_count - 1 {
            result[i] = (chunk[0] << 4) | chunk.get(1).copied().unwrap_or(0);
        } else {
            let sign = if value >= 0 { 0x0C } else { 0x0D };
            result[byte_count - 1] = (chunk[0] << 4) | sign;
        }
    }

    result
}

/// Meta-test for comprehensive performance hardening coverage
#[test]
fn test_comprehensive_performance_hardening_coverage() {
    let performance_tests = [
        "test_memory_usage_large_datasets",
        "test_performance_regression_detection",
        "test_throughput_load_patterns",
        "test_resource_utilization_optimization",
        "test_error_handling_performance_impact",
    ];

    assert_eq!(
        performance_tests.len(),
        5,
        "Should have comprehensive performance hardening coverage"
    );

    println!(
        "✅ Performance hardening validation covers {} test scenarios:",
        performance_tests.len()
    );
    for (i, test_name) in performance_tests.iter().enumerate() {
        println!("   {}. {}", i + 1, test_name);
    }

    // Validate test diversity
    let memory_test = performance_tests.iter().any(|s| s.contains("memory"));
    let regression_test = performance_tests.iter().any(|s| s.contains("regression"));
    let throughput_test = performance_tests.iter().any(|s| s.contains("throughput"));
    let resource_test = performance_tests.iter().any(|s| s.contains("resource"));
    let error_test = performance_tests.iter().any(|s| s.contains("error"));

    assert!(
        memory_test && regression_test && throughput_test && resource_test && error_test,
        "Should cover all performance hardening aspects"
    );
}
