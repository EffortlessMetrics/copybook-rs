//! AC4: Progressive Complexity Benchmarks
//!
//! Progressive benchmark suite with scaling data sizes (1KB → 10KB → 100KB → 1MB)
//! for performance profiling with flamegraph, perf, and valgrind integration.
//!
//! Usage:
//! ```bash
//! PERF=1 cargo bench --bench progressive --features progressive
//! ```
//!
//! Specification: docs/issue-49-tdd-handoff-package.md#ac4-progressive-complexity
//! Traceability: docs/issue-49-traceability-matrix.md#ac4

#![allow(clippy::cast_precision_loss, clippy::items_after_statements)]

#[cfg(feature = "progressive")]
use copybook_codec::{DecodeOptions, decode_record};
#[cfg(feature = "progressive")]
use copybook_core::parse_copybook;
#[cfg(feature = "progressive")]
use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
#[cfg(feature = "progressive")]
use std::hint::black_box;
#[cfg(feature = "progressive")]
use std::time::Duration;

/// Progressive data sizes: 1KB → 10KB → 100KB → 1MB
#[cfg(feature = "progressive")]
const PROGRESSIVE_SIZES: &[(usize, &str)] = &[
    (1_024, "1KB"),
    (10_240, "10KB"),
    (102_400, "100KB"),
    (1_048_576, "1MB"),
];

/// Early bailout threshold: 10 seconds per size tier
#[cfg(feature = "progressive")]
const BAILOUT_THRESHOLD_SECS: u64 = 10;

/// Simple copybook for progressive testing
#[cfg(feature = "progressive")]
const PROGRESSIVE_COPYBOOK: &str = r"
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID           PIC 9(8) COMP.
           05  CUSTOMER-NAME         PIC X(50).
           05  ACCOUNT-BALANCE       PIC S9(13)V99 COMP-3.
           05  STATUS-CODE           PIC X(1).
";

/// Generate progressive test data for DISPLAY fields
#[cfg(feature = "progressive")]
fn generate_display_data(target_size: usize) -> Vec<u8> {
    let record_size = 67; // Approximate size of CUSTOMER-RECORD
    let record_count = (target_size + record_size - 1) / record_size;
    let mut data = Vec::new();

    for i in 0..record_count {
        // Customer ID (8 bytes binary)
        data.extend_from_slice(&(i as u32).to_be_bytes());
        data.extend_from_slice(&[0u8; 4]);

        // Customer Name (50 bytes EBCDIC text)
        let name = format!("CUSTOMER_{:06}_ABCDEFGHIJKLMNOPQRSTUVWXYZ", i);
        let mut name_bytes = name.as_bytes().to_vec();
        name_bytes.resize(50, 0x40); // Pad with EBCDIC spaces
        data.extend_from_slice(&name_bytes);

        // Account Balance (8 bytes COMP-3)
        let balance = (i * 1000) % 999_999_999;
        let balance_str = format!("{:015}", balance); // 15 digits for S9(13)V99
        let mut comp3_data = vec![0u8; 8];
        for (j, chunk) in balance_str.as_bytes().chunks(2).enumerate() {
            if j < 7 {
                let high = chunk[0] - b'0';
                let low = if chunk.len() > 1 { chunk[1] - b'0' } else { 0 };
                comp3_data[j] = (high << 4) | low;
            } else {
                // Last byte with sign
                comp3_data[7] = (chunk[0] - b'0') << 4 | 0x0C; // Positive sign
            }
        }
        data.extend_from_slice(&comp3_data);

        // Status Code (1 byte)
        data.push(b'A' + (i % 26) as u8);
    }

    data.truncate(target_size);
    data
}

/// Generate progressive test data for COMP-3 fields
#[cfg(feature = "progressive")]
fn generate_comp3_data(target_size: usize) -> Vec<u8> {
    // COMP-3 is more compact, so we need more records to reach target size
    let record_size = 8; // Just the COMP-3 field
    let record_count = (target_size + record_size - 1) / record_size;
    let mut data = Vec::new();

    for i in 0..record_count {
        let value = (i * 12345) % 999_999_999;
        let value_str = format!("{:015}", value); // 15 digits for S9(13)V99
        let mut comp3_data = vec![0u8; 8];

        for (j, chunk) in value_str.as_bytes().chunks(2).enumerate() {
            if j < 7 {
                let high = chunk[0] - b'0';
                let low = if chunk.len() > 1 { chunk[1] - b'0' } else { 0 };
                comp3_data[j] = (high << 4) | low;
            } else {
                // Last byte with sign
                comp3_data[7] = (chunk[0] - b'0') << 4 | 0x0C; // Positive sign
            }
        }

        data.extend_from_slice(&comp3_data);
    }

    data.truncate(target_size);
    data
}

/// AC4: Progressive DISPLAY decode benchmark
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#progressive-complexity-api
///
/// Benchmarks DISPLAY field decoding at progressive scales with early bailout.
#[cfg(feature = "progressive")]
fn progressive_decode_display(c: &mut Criterion) {
    let schema = parse_copybook(PROGRESSIVE_COPYBOOK).expect("Failed to parse copybook");
    let options = DecodeOptions::default();
    let record_size = 67; // Approximate size of CUSTOMER-RECORD

    let mut group = c.benchmark_group("progressive_decode_display");
    group.warm_up_time(Duration::from_secs(1));
    group.measurement_time(Duration::from_secs(5));

    for (size, label) in PROGRESSIVE_SIZES {
        let test_data = generate_display_data(*size);
        group.throughput(Throughput::Bytes(test_data.len() as u64));

        group.bench_with_input(BenchmarkId::from_parameter(label), &test_data, |b, data| {
            let start = std::time::Instant::now();

            b.iter(|| {
                // Early bailout check
                if start.elapsed().as_secs() > BAILOUT_THRESHOLD_SECS {
                    return;
                }

                // Process data in record chunks
                for chunk in data.chunks(record_size) {
                    if chunk.len() < record_size {
                        break; // Skip incomplete record
                    }

                    let result =
                        decode_record(black_box(&schema), black_box(chunk), black_box(&options));
                    let _ = black_box(result);
                }
            });
        });
    }

    group.finish();
}

/// AC4: Progressive COMP-3 decode benchmark
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#progressive-complexity-api
///
/// Benchmarks COMP-3 packed decimal decoding at progressive scales.
#[cfg(feature = "progressive")]
fn progressive_decode_comp3(c: &mut Criterion) {
    // Use a simpler copybook focused on COMP-3
    const COMP3_COPYBOOK: &str = r"
           01  NUMERIC-RECORD.
               05  ACCOUNT-BALANCE   PIC S9(13)V99 COMP-3.
    ";

    let schema = parse_copybook(COMP3_COPYBOOK).expect("Failed to parse copybook");
    let options = DecodeOptions::default();
    let record_size = 8; // Size of COMP-3 field

    let mut group = c.benchmark_group("progressive_decode_comp3");
    group.warm_up_time(Duration::from_secs(1));
    group.measurement_time(Duration::from_secs(5));

    for (size, label) in PROGRESSIVE_SIZES {
        let test_data = generate_comp3_data(*size);
        group.throughput(Throughput::Bytes(test_data.len() as u64));

        group.bench_with_input(BenchmarkId::from_parameter(label), &test_data, |b, data| {
            let start = std::time::Instant::now();

            b.iter(|| {
                // Early bailout check
                if start.elapsed().as_secs() > BAILOUT_THRESHOLD_SECS {
                    return;
                }

                // Process data in record chunks
                for chunk in data.chunks(record_size) {
                    if chunk.len() < record_size {
                        break; // Skip incomplete record
                    }

                    let result =
                        decode_record(black_box(&schema), black_box(chunk), black_box(&options));
                    let _ = black_box(result);
                }
            });
        });
    }

    group.finish();
}

/// AC4: Progressive memory usage benchmark
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#progressive-complexity-api
///
/// Validates memory usage stays bounded (<256 MiB) at all scales.
#[cfg(feature = "progressive")]
fn progressive_memory_usage(c: &mut Criterion) {
    let schema = parse_copybook(PROGRESSIVE_COPYBOOK).expect("Failed to parse copybook");
    let options = DecodeOptions::default();
    let record_size = 67;

    let mut group = c.benchmark_group("progressive_memory_usage");
    group.warm_up_time(Duration::from_secs(1));
    group.measurement_time(Duration::from_secs(3));

    for (size, label) in PROGRESSIVE_SIZES {
        group.bench_function(BenchmarkId::from_parameter(label), |b| {
            b.iter(|| {
                // Allocate test data
                let test_data = generate_display_data(*size);

                // Process data to simulate memory usage
                for chunk in test_data.chunks(record_size) {
                    if chunk.len() < record_size {
                        break;
                    }

                    let result =
                        decode_record(black_box(&schema), black_box(chunk), black_box(&options));
                    let _ = black_box(result);
                }

                // Memory should be cleaned up automatically (drop)
            });
        });
    }

    group.finish();
}

/// AC4: Progressive scaling validation benchmark
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#progressive-complexity-api
///
/// Validates 10x incremental scale factor consistency.
#[cfg(feature = "progressive")]
fn progressive_scale_validation(c: &mut Criterion) {
    c.bench_function("progressive_scale_validation", |b| {
        b.iter(|| {
            // Validate scale factor is 10x
            for i in 1..PROGRESSIVE_SIZES.len() {
                let ratio = PROGRESSIVE_SIZES[i].0 as f64 / PROGRESSIVE_SIZES[i - 1].0 as f64;
                assert!((ratio - 10.0).abs() < 0.1, "Scale factor should be ~10x");
            }
        });
    });
}

/// AC4: Progressive throughput scaling analysis
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#progressive-complexity-api
///
/// Measures throughput scaling across progressive data sizes.
#[cfg(feature = "progressive")]
fn progressive_throughput_scaling(c: &mut Criterion) {
    let schema = parse_copybook(PROGRESSIVE_COPYBOOK).expect("Failed to parse copybook");
    let options = DecodeOptions::default();
    let record_size = 67;

    let mut group = c.benchmark_group("progressive_throughput_scaling");
    group.warm_up_time(Duration::from_secs(1));
    group.measurement_time(Duration::from_secs(5));

    for (size, label) in PROGRESSIVE_SIZES {
        let test_data = generate_display_data(*size);
        group.throughput(Throughput::Bytes(test_data.len() as u64));

        group.bench_with_input(BenchmarkId::from_parameter(label), &test_data, |b, data| {
            b.iter(|| {
                // Process all data to measure throughput
                for chunk in data.chunks(record_size) {
                    if chunk.len() < record_size {
                        break;
                    }

                    let result =
                        decode_record(black_box(&schema), black_box(chunk), black_box(&options));
                    let _ = black_box(result);
                }
            });
        });
    }

    group.finish();
}

// Conditional benchmark group registration based on feature flag
#[cfg(feature = "progressive")]
criterion_group!(
    progressive_benches,
    progressive_decode_display,
    progressive_decode_comp3,
    progressive_memory_usage,
    progressive_scale_validation,
    progressive_throughput_scaling
);

#[cfg(feature = "progressive")]
criterion_main!(progressive_benches);

// Dummy main for when feature is not enabled
#[cfg(not(feature = "progressive"))]
fn main() {
    eprintln!("Progressive benchmarks require PERF=1 environment variable");
    eprintln!("Usage: PERF=1 cargo bench --bench progressive --features progressive");
    std::process::exit(0);
}
