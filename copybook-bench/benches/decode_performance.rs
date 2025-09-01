use copybook_codec::{DecodeOptions, decode_record};
use copybook_core::parse_copybook;
use criterion::{Criterion, Throughput, black_box, criterion_group, criterion_main};
use std::hint::black_box as hint_black_box;

const SIMPLE_COPYBOOK: &str = r#"
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID         PIC 9(6).
           05  CUSTOMER-NAME       PIC X(30).
           05  ACCOUNT-BALANCE     PIC S9(7)V99 COMP-3.
           05  LAST-ACTIVITY-DATE  PIC 9(8).
           05  STATUS-CODE         PIC X(1).
"#;

const COMP3_HEAVY_COPYBOOK: &str = r#"
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
"#;

fn generate_test_data(record_size: usize, count: usize) -> Vec<u8> {
    // Generate synthetic binary data for testing
    // This is a placeholder - real implementation would generate proper EBCDIC/binary data
    vec![0x40; record_size * count] // EBCDIC space character
}

fn bench_decode_display_heavy(c: &mut Criterion) {
    let schema = parse_copybook(SIMPLE_COPYBOOK).expect("Failed to parse copybook");
    let options = DecodeOptions::default();
    let test_data = generate_test_data(50, 1000); // 1000 records of 50 bytes each

    let mut group = c.benchmark_group("decode_display_heavy");
    group.throughput(Throughput::Bytes(test_data.len() as u64));

    group.bench_function("decode_1000_records", |b| {
        b.iter(|| {
            for chunk in test_data.chunks(50) {
                let _result =
                    decode_record(black_box(&schema), black_box(chunk), black_box(&options));
                hint_black_box(_result);
            }
        })
    });

    group.finish();
}

fn bench_decode_comp3_heavy(c: &mut Criterion) {
    let schema = parse_copybook(COMP3_HEAVY_COPYBOOK).expect("Failed to parse copybook");
    let options = DecodeOptions::default();
    let test_data = generate_test_data(60, 1000); // 1000 records of 60 bytes each

    let mut group = c.benchmark_group("decode_comp3_heavy");
    group.throughput(Throughput::Bytes(test_data.len() as u64));

    group.bench_function("decode_1000_records", |b| {
        b.iter(|| {
            for chunk in test_data.chunks(60) {
                let _result =
                    decode_record(black_box(&schema), black_box(chunk), black_box(&options));
                hint_black_box(_result);
            }
        })
    });

    group.finish();
}

fn bench_parse_copybook(c: &mut Criterion) {
    let mut group = c.benchmark_group("parse_copybook");

    group.bench_function("simple_copybook", |b| {
        b.iter(|| {
            let _result = parse_copybook(black_box(SIMPLE_COPYBOOK));
            hint_black_box(_result);
        })
    });

    group.bench_function("comp3_heavy_copybook", |b| {
        b.iter(|| {
            let _result = parse_copybook(black_box(COMP3_HEAVY_COPYBOOK));
            hint_black_box(_result);
        })
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_decode_display_heavy,
    bench_decode_comp3_heavy,
    bench_parse_copybook
);

criterion_main!(benches);
