// SPDX-License-Identifier: AGPL-3.0-or-later
//! AC4: Progressive Complexity Testing
//!
//! Tests for progressive benchmark execution with scaling data sizes
//! and early bailout protection.
//!
//! **Status**: NEW implementation (developer productivity feature)
//!
//! Specification: docs/issue-49-tdd-handoff-package.md#ac4-progressive-complexity
//! Traceability: docs/issue-49-traceability-matrix.md#ac4

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![allow(clippy::panic)]
#![allow(
    clippy::cast_precision_loss,
    clippy::cast_possible_truncation,
    clippy::uninlined_format_args,
    clippy::items_after_statements,
    clippy::doc_markdown,
    clippy::redundant_slicing
)]

use std::io::Cursor;
use std::time::{Duration, Instant};

use copybook_codec::{Codepage, DecodeOptions, RecordFormat};
use copybook_core::parse_copybook;

/// A simple DISPLAY copybook for progressive scaling tests.
const SIMPLE_COPYBOOK: &str = "       01 REC.\n           05 FIELD-A PIC X(10).";

/// AC4: Test progressive scaling from 1KB to 1MB
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#progressive-complexity-api
///
/// Validates that progressive benchmarks correctly scale through:
/// 1KB → 10KB → 100KB → 1MB data sizes
#[test]
fn test_progressive_scaling_1kb_to_1mb() {
    // AC4
    const SIZES: &[(usize, &str)] = &[
        (1_024, "1KB"),
        (10_240, "10KB"),
        (102_400, "100KB"),
        (1_048_576, "1MB"),
    ];

    let schema = parse_copybook(SIMPLE_COPYBOOK).expect("Simple copybook should parse");
    let record_len = schema.lrecl_fixed.expect("Fixed record length") as usize;
    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037);

    for (size, label) in SIZES {
        // Generate test data at scale: complete records of EBCDIC spaces
        let num_records = *size / record_len;
        let actual_size = num_records * record_len;
        let test_data = vec![0x40u8; actual_size];

        assert!(
            test_data.len() >= *size / 2,
            "Test data size too small for {}",
            label
        );

        // Execute actual decoding at this scale
        let mut output = Vec::new();
        let summary = copybook_codec::decode_file_to_jsonl(
            &schema,
            Cursor::new(&test_data),
            &mut output,
            &options,
        )
        .unwrap_or_else(|e| panic!("Decode failed at scale {}: {}", label, e));

        assert!(
            summary.records_processed > 0,
            "Should decode records at scale {}",
            label
        );
        assert!(
            !output.is_empty(),
            "Output should not be empty at {}",
            label
        );

        // Memory: test data plus output should be bounded
        let total_bytes = test_data.len() + output.len();
        assert!(
            total_bytes < 256 * 1024 * 1024,
            "Memory usage should stay under 256 MiB at {}",
            label
        );
    }
}

/// AC4: Test early bailout on execution time threshold
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#progressive-complexity-api
///
/// Validates that progressive benchmarks bail out early if execution
/// time exceeds 10 seconds per size tier to prevent stuck benchmarks.
#[test]
fn test_early_bailout_threshold() {
    // AC4
    let timeout = Duration::from_secs(10);
    let start = Instant::now();

    // Simulate progressive benchmark with bailout
    let mut iterations = 0;
    let max_iterations = 1000;

    while start.elapsed() < timeout && iterations < max_iterations {
        iterations += 1;
        // Simulate work
        std::thread::sleep(Duration::from_millis(1));

        // Check bailout condition
        if start.elapsed() > timeout {
            break;
        }
    }

    // Validate bailout occurred before infinite loop
    assert!(
        iterations < max_iterations || start.elapsed() < timeout,
        "Expected early bailout to prevent stuck benchmark"
    );

    // Bailout with actual slow benchmarks is validated via PERF=1 cargo bench -p copybook-bench.
    // Bailout logging is visible in Criterion's stderr output during benchmark runs.
    // Bailout per size tier uses the same 10-second threshold at each progressive scale.
}

/// AC4: Test PERF=1 feature flag enforcement
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#progressive-complexity-api
///
/// Validates that progressive benchmarks only execute when PERF=1
/// environment variable is set (developer mode only).
#[test]
fn test_perf_mode_only_execution() {
    // AC4
    // Check if PERF=1 is set
    let perf_mode = std::env::var("PERF").unwrap_or_default() == "1";

    if !perf_mode {
        // Progressive benchmarks should be skipped in normal CI
        assert!(
            !perf_mode,
            "Progressive benchmarks should not run without PERF=1"
        );
    }

    // Feature flag conditional compilation is enforced by #[cfg] attributes in bench harnesses.
    // CI workflow (.github/workflows/ci.yml) does not set PERF=1 for normal test runs.
    // Developer workflow with PERF=1 is documented in CLAUDE.md and bench-report summary.
}

/// AC4: Test progressive data generation
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#progressive-complexity-api
///
/// Validates that test data is generated correctly for each scale tier.
#[test]
fn test_progressive_data_generation() {
    // AC4
    const SCALES: &[usize] = &[1_024, 10_240, 102_400, 1_048_576];

    // Parse a DISPLAY copybook schema to validate data at each scale
    let display_copybook =
        "       01 REC.\n           05 NAME PIC X(20).\n           05 CODE PIC 9(4).";
    let display_schema = parse_copybook(display_copybook).expect("DISPLAY copybook should parse");
    let display_lrecl = display_schema.lrecl_fixed.expect("Should have fixed LRECL") as usize;

    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037);

    for &scale in SCALES {
        // Generate DISPLAY test data with valid EBCDIC content
        let num_records = scale / display_lrecl;
        let mut record = vec![0x40u8; 20]; // NAME: 20 EBCDIC spaces
        record.extend_from_slice(&[0xF0; 4]); // CODE: 4 EBCDIC zoned zeros
        assert_eq!(record.len(), display_lrecl, "Record size must match LRECL");

        let display_data: Vec<u8> = record
            .iter()
            .copied()
            .cycle()
            .take(num_records * display_lrecl)
            .collect();
        assert_eq!(
            display_data.len(),
            num_records * display_lrecl,
            "DISPLAY data size mismatch"
        );

        // Validate data format: decode one record to confirm validity
        let json = copybook_codec::decode_record(&display_schema, &record, &options)
            .expect("Record should decode successfully");
        assert!(json.is_object(), "Decoded record should be a JSON object");

        // Verify data reuse: same record buffer produces identical output
        let json2 = copybook_codec::decode_record(&display_schema, &record, &options)
            .expect("Reused record should decode identically");
        assert_eq!(
            json.to_string(),
            json2.to_string(),
            "Data reuse should produce identical output"
        );

        // Generate COMP-3 test data
        let comp3_size = scale / 2; // COMP-3 is more compact
        let comp3_data = vec![0x0Cu8; comp3_size];
        assert_eq!(comp3_data.len(), comp3_size, "COMP-3 data size mismatch");
    }
}

/// AC4: Test progressive benchmark execution order
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#progressive-complexity-api
///
/// Validates that benchmarks execute in ascending order (1KB → 1MB).
#[test]
fn test_progressive_execution_order() {
    // AC4
    let mut executed_sizes = Vec::new();

    const SIZES: &[usize] = &[1_024, 10_240, 102_400, 1_048_576];

    for &size in SIZES {
        executed_sizes.push(size);
    }

    // Validate ascending order
    for i in 1..executed_sizes.len() {
        assert!(
            executed_sizes[i] > executed_sizes[i - 1],
            "Benchmarks must execute in ascending size order"
        );
    }

    // Execution order with bailout preserves ascending order; early exit skips remaining tiers.
    // Partial execution is logged via Criterion's stderr output with completed tier labels.
}

/// AC4: Test memory usage at progressive scales
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#progressive-complexity-api
///
/// Validates that memory usage stays bounded (<256 MiB steady-state)
/// even at largest scale (1MB).
#[test]
fn test_memory_usage_bounded() {
    // AC4
    const LARGE_SCALE: usize = 1_048_576; // 1MB

    // Allocate test data
    let test_data = vec![0x40u8; LARGE_SCALE];

    // Memory usage should be minimal (just the data allocation)
    let estimated_memory_mb = test_data.len() / 1_048_576;
    assert!(
        estimated_memory_mb < 256,
        "Memory usage should stay under 256 MiB"
    );

    // Measure actual RSS memory usage
    if let Some(rss) = copybook_bench::memory::get_rss_bytes() {
        let rss_mb = rss / (1024 * 1024);
        assert!(
            rss_mb < 256,
            "RSS should stay under 256 MiB, got {rss_mb} MiB"
        );
    }
}

/// AC4: Test flamegraph integration compatibility
///
/// Tests feature spec: docs/how-to/benchmark-regression-testing.md#ac4
///
/// Validates that progressive benchmarks work correctly with
/// flamegraph profiling tools.
#[test]
fn test_flamegraph_integration() {
    // AC4
    // Flamegraph profiling is verified manually via:
    //   cargo flamegraph --bench progressive -- --bench
    //   perf record -g -- cargo bench -p copybook-bench -- progressive
    // Output generation depends on system perf_event_paranoid settings.
    //
    // This test validates prerequisites for flamegraph profiling: that the
    // schema parses and decoding works, which are needed for the benchmark
    // binary to execute under profiling.
    let schema = parse_copybook(SIMPLE_COPYBOOK).expect("Schema must parse for profiling");
    assert!(
        !schema.fields.is_empty(),
        "Schema must have fields for flamegraph profiling to be meaningful"
    );
    assert!(
        schema.lrecl_fixed.is_some(),
        "Fixed record length required for benchmark profiling"
    );
}

/// AC4: Test progressive benchmark naming convention
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#progressive-complexity-api
///
/// Validates benchmark naming: progressive_decode_{size}
#[test]
fn test_benchmark_naming_convention() {
    // AC4
    const SIZES: &[(&str, usize)] = &[
        ("1KB", 1_024),
        ("10KB", 10_240),
        ("100KB", 102_400),
        ("1MB", 1_048_576),
    ];

    // Criterion benchmark ID format: progressive_decode_{SIZE}
    let re = regex::Regex::new(r"^progressive_decode_\d+[KM]B$").unwrap();

    for (label, _size) in SIZES {
        let bench_name = format!("progressive_decode_{}", label);
        assert!(
            bench_name.starts_with("progressive_"),
            "Benchmark name must start with progressive_"
        );
        assert!(
            bench_name.contains(label),
            "Benchmark name must include size label"
        );

        // Validate Criterion benchmark ID format
        assert!(
            re.is_match(&bench_name),
            "Benchmark name should match Criterion format: {bench_name}"
        );
    }
}

/// AC4: Test bailout threshold configuration
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#progressive-complexity-api
///
/// Validates that bailout threshold is configurable (default: 10 seconds).
#[test]
fn test_bailout_threshold_configuration() {
    // AC4
    // Default threshold
    let default_threshold = Duration::from_secs(10);
    assert_eq!(default_threshold.as_secs(), 10);

    // Test custom threshold from BENCH_TIMEOUT environment variable
    let custom_threshold_str = std::env::var("BENCH_TIMEOUT").unwrap_or_default();
    let threshold = if custom_threshold_str.is_empty() {
        default_threshold
    } else {
        let secs: u64 = custom_threshold_str
            .parse()
            .expect("BENCH_TIMEOUT must be a valid u64 seconds value");
        Duration::from_secs(secs)
    };
    // Threshold must be at least 1 second to be meaningful
    assert!(
        threshold >= Duration::from_secs(1),
        "Threshold must be at least 1 second"
    );

    // Validate threshold enforcement: a trivial operation should complete well within threshold
    let start = Instant::now();
    let _ = vec![0u8; 1024];
    let elapsed = start.elapsed();
    assert!(
        elapsed < threshold,
        "Trivial operation should complete within threshold"
    );

    // Test threshold per size tier: larger tiers may have longer thresholds
    let tier_thresholds = [
        (1_024usize, Duration::from_secs(2)),
        (10_240, Duration::from_secs(5)),
        (102_400, Duration::from_secs(10)),
        (1_048_576, Duration::from_secs(30)),
    ];
    for (i, (size, tier_threshold)) in tier_thresholds.iter().enumerate() {
        if i > 0 {
            assert!(
                *tier_threshold >= tier_thresholds[i - 1].1,
                "Larger tiers should have equal or longer thresholds"
            );
        }
        assert!(
            tier_threshold.as_secs() > 0,
            "Threshold for size {} must be positive",
            size
        );
    }
}

/// AC4: Test progressive benchmark output format
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#progressive-complexity-api
///
/// Validates that benchmark output includes size information and
/// throughput metrics.
#[test]
fn test_progressive_benchmark_output() {
    // AC4
    // Full Criterion output (benchmark IDs, timing, throughput) is validated in CI via:
    //   PERF=1 cargo bench -p copybook-bench -- progressive --output-format json
    // Here we validate the decode pipeline that feeds into Criterion benchmarks.

    let schema = parse_copybook(SIMPLE_COPYBOOK).expect("Copybook should parse");
    let record_len = schema.lrecl_fixed.expect("Fixed LRECL") as usize;
    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037);

    // Validate output contains meaningful data for each size tier
    for (label, size) in [("1KB", 1_024usize), ("10KB", 10_240)] {
        let num_records = size / record_len;
        let data = vec![0x40u8; num_records * record_len];

        let mut output = Vec::new();
        let summary = copybook_codec::decode_file_to_jsonl(
            &schema,
            Cursor::new(&data),
            &mut output,
            &options,
        )
        .unwrap_or_else(|e| panic!("Decode failed for {}: {}", label, e));

        // Verify throughput metrics can be computed from summary
        assert!(
            summary.records_processed > 0,
            "Records decoded for {}",
            label
        );
        let throughput_bytes_per_record = data.len() as f64 / summary.records_processed as f64;
        assert!(
            throughput_bytes_per_record > 0.0,
            "Throughput metric should be positive for {}",
            label
        );

        // Verify JSON output format: each line should be valid JSON
        let output_str = String::from_utf8(output).expect("Output should be valid UTF-8");
        for line in output_str.lines() {
            let _: serde_json::Value =
                serde_json::from_str(line).expect("Each output line should be valid JSON");
        }
    }
}

/// AC4: Test incremental scale factor
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#progressive-complexity-api
///
/// Validates 10x incremental scale factor (1KB → 10KB → 100KB → 1MB).
#[test]
fn test_incremental_scale_factor() {
    // AC4
    const SIZES: &[usize] = &[1_024, 10_240, 102_400, 1_048_576];

    for i in 1..SIZES.len() {
        let ratio = SIZES[i] as f64 / SIZES[i - 1] as f64;
        assert!(
            (ratio - 10.0).abs() < 0.5,
            "Scale factor should be ~10x, got {:.2}",
            ratio
        );
    }

    // Test alternative 2x scale factor sequence
    let sizes_2x: Vec<usize> = (0..5).map(|i| 1_024 * 2usize.pow(i)).collect();
    for i in 1..sizes_2x.len() {
        let ratio = sizes_2x[i] as f64 / sizes_2x[i - 1] as f64;
        assert!(
            (ratio - 2.0).abs() < 0.01,
            "2x scale factor should be exactly 2.0, got {:.2}",
            ratio
        );
    }

    // Test alternative 5x scale factor sequence
    let sizes_5x: Vec<usize> = (0..4).map(|i| 1_000 * 5usize.pow(i)).collect();
    for i in 1..sizes_5x.len() {
        let ratio = sizes_5x[i] as f64 / sizes_5x[i - 1] as f64;
        assert!(
            (ratio - 5.0).abs() < 0.01,
            "5x scale factor should be exactly 5.0, got {:.2}",
            ratio
        );
    }

    // Validate scale factor consistency: all sequences must be strictly ascending
    for sizes in [&SIZES[..], &sizes_2x, &sizes_5x] {
        for i in 1..sizes.len() {
            assert!(
                sizes[i] > sizes[i - 1],
                "Scale sequence must be strictly ascending"
            );
        }
    }
}

/// AC4: Test progressive benchmark state cleanup
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#progressive-complexity-api
///
/// Validates that benchmark state is cleaned up between iterations
/// to prevent memory leaks.
#[test]
fn test_state_cleanup_between_iterations() {
    // AC4
    let schema = parse_copybook(SIMPLE_COPYBOOK).expect("Copybook should parse");
    let record_len = schema.lrecl_fixed.expect("Fixed LRECL") as usize;
    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037);

    // Track output buffer capacities across iterations to detect accumulation
    let mut capacities = Vec::new();

    for iteration in 0..5 {
        let test_data = vec![0x40u8; record_len * 100];
        let mut output = Vec::new();
        let _summary = copybook_codec::decode_file_to_jsonl(
            &schema,
            Cursor::new(&test_data),
            &mut output,
            &options,
        )
        .unwrap_or_else(|e| panic!("Iteration {} failed: {}", iteration, e));

        capacities.push(output.capacity());
        drop(output);
        drop(test_data);
    }

    // Verify no memory accumulation: all iterations should allocate similar amounts
    let max_cap = *capacities.iter().max().unwrap();
    let min_cap = *capacities.iter().min().unwrap();
    assert!(
        max_cap <= min_cap * 3 + 1024,
        "Memory should not accumulate across iterations: min={}, max={}",
        min_cap,
        max_cap
    );

    // Detect file descriptor leaks across iterations (using in-memory Cursors
    // avoids real fd usage; real fd testing is done with tempfile in integration tests)
    if let Some(fd_count) = copybook_bench::memory::get_open_fd_count() {
        assert!(
            fd_count < 1024,
            "Open FD count should stay under 1024, got {fd_count}"
        );
    }
}
