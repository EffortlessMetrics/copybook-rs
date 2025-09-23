use copybook_codec::{DecodeOptions, decode_file_to_jsonl, decode_record};
use copybook_core::parse_copybook;
use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use std::hint::black_box;
use std::io::Cursor;

const SIMPLE_COPYBOOK: &str = r"
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID         PIC 9(6).
           05  CUSTOMER-NAME       PIC X(30).
           05  ACCOUNT-BALANCE     PIC S9(7)V99 COMP-3.
           05  LAST-ACTIVITY-DATE  PIC 9(8).
           05  STATUS-CODE         PIC X(1).
";

const DISPLAY_HEAVY_COPYBOOK: &str = r"
       01  TEXT-RECORD.
           05  FIELD-01            PIC X(50).
           05  FIELD-02            PIC X(50).
           05  FIELD-03            PIC X(50).
           05  FIELD-04            PIC X(50).
           05  FIELD-05            PIC X(50).
           05  FIELD-06            PIC X(50).
           05  FIELD-07            PIC X(50).
           05  FIELD-08            PIC X(50).
           05  FIELD-09            PIC X(50).
           05  FIELD-10            PIC X(50).
";

const COMP3_HEAVY_COPYBOOK: &str = r"
       01  NUMERIC-RECORD.
           05  FIELD-01            PIC S9(9)V99 COMP-3.
           05  FIELD-02            PIC S9(9)V99 COMP-3.
           05  FIELD-03            PIC S9(9)V99 COMP-3.
           05  FIELD-04            PIC S9(9)V99 COMP-3.
           05  FIELD-05            PIC S9(9)V99 COMP-3.
           05  FIELD-06            PIC S9(9)V99 COMP-3.
           05  FIELD-07            PIC S9(9)V99 COMP-3.
           05  FIELD-08            PIC S9(9)V99 COMP-3.
           05  FIELD-09            PIC S9(9)V99 COMP-3.
           05  FIELD-10            PIC S9(9)V99 COMP-3.
";

const BINARY_HEAVY_COPYBOOK: &str = r"
       01  BINARY-RECORD.
           05  FIELD-01            PIC S9(4) COMP.
           05  FIELD-02            PIC S9(9) COMP.
           05  FIELD-03            PIC S9(18) COMP.
           05  FIELD-04            PIC S9(4) COMP.
           05  FIELD-05            PIC S9(9) COMP.
           05  FIELD-06            PIC S9(18) COMP.
           05  FIELD-07            PIC S9(4) COMP.
           05  FIELD-08            PIC S9(9) COMP.
           05  FIELD-09            PIC S9(18) COMP.
           05  FIELD-10            PIC S9(4) COMP.
";

fn generate_display_heavy_data(record_count: usize) -> Vec<u8> {
    // Generate DISPLAY-heavy test data (500 bytes per record)
    let mut data = Vec::new();
    for i in 0..record_count {
        // Generate 10 fields of 50 bytes each (EBCDIC text)
        for field in 0..10 {
            let text = format!(
                "FIELD{field:02}_{i:06}_ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
            );
            let mut field_data = text.as_bytes().to_vec();
            field_data.resize(50, 0x40); // Pad with EBCDIC spaces
            data.extend_from_slice(&field_data);
        }
    }
    data
}

fn generate_comp3_heavy_data(record_count: usize) -> Vec<u8> {
    // Generate COMP-3 heavy test data (60 bytes per record)
    let mut data = Vec::new();
    for i in 0..record_count {
        // Generate 10 packed decimal fields of 6 bytes each
        for field in 0..10 {
            let value = (i * 10 + field) % 999999999;
            // Convert to packed decimal: S9(9)V99 = 6 bytes
            let mut packed = vec![0x00; 6];

            // Simple packed decimal encoding for benchmark
            let digits = format!("{value:011}"); // 11 digits total
            let digit_bytes: Vec<u8> = digits.bytes().map(|b| b - b'0').collect();

            // Pack digits (2 per byte, sign in last nibble)
            for (i, chunk) in digit_bytes.chunks(2).enumerate() {
                if i < 5 {
                    packed[i] = (chunk[0] << 4) | chunk.get(1).unwrap_or(&0);
                } else {
                    // Last byte with sign
                    packed[5] = (chunk[0] << 4) | 0x0C; // Positive sign
                }
            }

            data.extend_from_slice(&packed);
        }
    }
    data
}

fn generate_binary_heavy_data(record_count: usize) -> Vec<u8> {
    // Generate binary-heavy test data (64 bytes per record)
    let mut data = Vec::new();
    for i in 0..record_count {
        // Generate mixed binary fields: 2-byte, 4-byte, 8-byte
        for field in 0..10 {
            let value = (i * 10 + field) as i64;
            match field % 3 {
                0 => {
                    // 16-bit binary (2 bytes)
                    data.extend_from_slice(&(value as i16).to_be_bytes());
                }
                1 => {
                    // 32-bit binary (4 bytes)
                    data.extend_from_slice(&(value as i32).to_be_bytes());
                }
                2 => {
                    // 64-bit binary (8 bytes)
                    data.extend_from_slice(&value.to_be_bytes());
                }
                _ => unreachable!(),
            }
        }
    }
    data
}

fn bench_decode_display_heavy(c: &mut Criterion) {
    let schema = parse_copybook(DISPLAY_HEAVY_COPYBOOK).expect("Failed to parse copybook");
    let options = DecodeOptions::default();

    let mut group = c.benchmark_group("decode_display_heavy");

    // Test different record counts to measure throughput scaling
    for record_count in &[100, 1000, 10000] {
        let test_data = generate_display_heavy_data(*record_count);
        let record_size = 500; // 10 fields * 50 bytes each

        group.throughput(Throughput::Bytes(test_data.len() as u64));

        group.bench_with_input(
            BenchmarkId::new("single_threaded", record_count),
            record_count,
            |b, &record_count| {
                b.iter(|| {
                    for chunk in test_data.chunks(record_size) {
                        let result = decode_record(
                            black_box(&schema),
                            black_box(chunk),
                            black_box(&options),
                        );
                        black_box(result);
                    }
                });
            },
        );

        // Benchmark streaming processor for larger datasets
        if *record_count >= 1000 {
            group.bench_with_input(
                BenchmarkId::new("streaming_processor", record_count),
                record_count,
                |b, _| {
                    b.iter(|| {
                        let input = Cursor::new(black_box(&test_data));
                        let mut output = Vec::new();
                        let result = decode_file_to_jsonl(
                            black_box(&schema),
                            input,
                            &mut output,
                            black_box(&options),
                        );
                        black_box(result);
                    });
                },
            );
        }
    }

    group.finish();
}

fn bench_decode_comp3_heavy(c: &mut Criterion) {
    let schema = parse_copybook(COMP3_HEAVY_COPYBOOK).expect("Failed to parse copybook");
    let options = DecodeOptions::default();

    let mut group = c.benchmark_group("decode_comp3_heavy");

    // Test different record counts to measure throughput scaling
    for record_count in &[100, 1000, 10000] {
        let test_data = generate_comp3_heavy_data(*record_count);
        let record_size = 60; // 10 fields * 6 bytes each

        group.throughput(Throughput::Bytes(test_data.len() as u64));

        group.bench_with_input(
            BenchmarkId::new("single_threaded", record_count),
            record_count,
            |b, &record_count| {
                b.iter(|| {
                    for chunk in test_data.chunks(record_size) {
                        let result = decode_record(
                            black_box(&schema),
                            black_box(chunk),
                            black_box(&options),
                        );
                        black_box(result);
                    }
                });
            },
        );

        // Benchmark streaming processor for larger datasets
        if *record_count >= 1000 {
            group.bench_with_input(
                BenchmarkId::new("streaming_processor", record_count),
                record_count,
                |b, _| {
                    b.iter(|| {
                        let input = Cursor::new(black_box(&test_data));
                        let mut output = Vec::new();
                        let result = decode_file_to_jsonl(
                            black_box(&schema),
                            input,
                            &mut output,
                            black_box(&options),
                        );
                        black_box(result);
                    });
                },
            );
        }
    }

    group.finish();
}

fn bench_decode_binary_heavy(c: &mut Criterion) {
    let schema = parse_copybook(BINARY_HEAVY_COPYBOOK).expect("Failed to parse copybook");
    let options = DecodeOptions::default();

    let mut group = c.benchmark_group("decode_binary_heavy");

    // Test different record counts to measure throughput scaling
    for record_count in &[100, 1000, 10000] {
        let test_data = generate_binary_heavy_data(*record_count);
        let record_size = 64; // Mixed binary fields totaling 64 bytes

        group.throughput(Throughput::Bytes(test_data.len() as u64));

        group.bench_with_input(
            BenchmarkId::new("single_threaded", record_count),
            record_count,
            |b, &record_count| {
                b.iter(|| {
                    for chunk in test_data.chunks(record_size) {
                        let result = decode_record(
                            black_box(&schema),
                            black_box(chunk),
                            black_box(&options),
                        );
                        black_box(result);
                    }
                });
            },
        );
    }

    group.finish();
}

fn bench_parse_copybook(c: &mut Criterion) {
    let mut group = c.benchmark_group("parse_copybook");

    group.bench_function("simple_copybook", |b| {
        b.iter(|| {
            let result = parse_copybook(black_box(SIMPLE_COPYBOOK));
            black_box(result);
        });
    });

    group.bench_function("comp3_heavy_copybook", |b| {
        b.iter(|| {
            let result = parse_copybook(black_box(COMP3_HEAVY_COPYBOOK));
            black_box(result);
        });
    });

    group.finish();
}

fn bench_throughput_slo_validation(c: &mut Criterion) {
    // SLO validation benchmarks - ensure we meet performance targets
    let mut group = c.benchmark_group("slo_validation");

    // Target: ≥80 MB/s for DISPLAY-heavy workloads
    let display_schema = parse_copybook(DISPLAY_HEAVY_COPYBOOK).expect("Failed to parse copybook");
    let display_data = generate_display_heavy_data(10000); // 5MB of data
    let options = DecodeOptions::default();

    group.throughput(Throughput::Bytes(display_data.len() as u64));
    group.bench_function("display_heavy_slo_80mbps", |b| {
        b.iter(|| {
            let input = Cursor::new(black_box(&display_data));
            let mut output = Vec::new();
            let result = decode_file_to_jsonl(
                black_box(&display_schema),
                input,
                &mut output,
                black_box(&options),
            );
            let _ = black_box(result);
        });
    });

    // Target: ≥40 MB/s for COMP-3-heavy workloads
    let comp3_schema = parse_copybook(COMP3_HEAVY_COPYBOOK).expect("Failed to parse copybook");
    let comp3_data = generate_comp3_heavy_data(10000); // 600KB of data

    group.throughput(Throughput::Bytes(comp3_data.len() as u64));
    group.bench_function("comp3_heavy_slo_40mbps", |b| {
        b.iter(|| {
            let input = Cursor::new(black_box(&comp3_data));
            let mut output = Vec::new();
            let result = decode_file_to_jsonl(
                black_box(&comp3_schema),
                input,
                &mut output,
                black_box(&options),
            );
            let _ = black_box(result);
        });
    });

    group.finish();
}

fn bench_parallel_scaling(c: &mut Criterion) {
    let schema = parse_copybook(DISPLAY_HEAVY_COPYBOOK).expect("Failed to parse copybook");
    let test_data = generate_display_heavy_data(10000);

    let mut group = c.benchmark_group("parallel_scaling");
    group.throughput(Throughput::Bytes(test_data.len() as u64));

    // Test scaling with different thread counts
    for thread_count in &[1, 2, 4, 8] {
        let mut options = DecodeOptions::default();
        options.threads = *thread_count;

        group.bench_with_input(
            BenchmarkId::new("threads", thread_count),
            thread_count,
            |b, _| {
                b.iter(|| {
                    let input = Cursor::new(black_box(&test_data));
                    let mut output = Vec::new();
                    let result = decode_file_to_jsonl(
                        black_box(&schema),
                        input,
                        &mut output,
                        black_box(&options),
                    );
                    let _ = black_box(result);
                });
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_decode_display_heavy,
    bench_decode_comp3_heavy,
    bench_decode_binary_heavy,
    bench_parse_copybook,
    bench_throughput_slo_validation,
    bench_parallel_scaling
);

criterion_main!(benches);
