use copybook_codec::Codepage;
use copybook_codec::memory::ScratchBuffers;
use copybook_codec::numeric::{
    SmallDecimal, encode_packed_decimal, encode_packed_decimal_with_scratch, encode_zoned_decimal,
    encode_zoned_decimal_with_scratch,
};
use criterion::{BenchmarkId, Criterion, black_box, criterion_group, criterion_main};

fn bench_zoned_encode(c: &mut Criterion) {
    let decimal = SmallDecimal::new(1234567890, 0, false);
    let mut group = c.benchmark_group("encode_zoned_decimal");
    group.bench_function(BenchmarkId::new("standard", 10), |b| {
        b.iter(|| {
            black_box(
                encode_zoned_decimal(
                    &decimal.to_string(),
                    10,
                    decimal.scale,
                    true,
                    Codepage::ASCII,
                )
                .unwrap(),
            )
        })
    });
    group.bench_function(BenchmarkId::new("scratch", 10), |b| {
        b.iter(|| {
            let mut scratch = ScratchBuffers::new();
            black_box(
                encode_zoned_decimal_with_scratch(
                    &decimal,
                    10,
                    true,
                    Codepage::ASCII,
                    false,
                    &mut scratch,
                )
                .unwrap(),
            )
        })
    });
    group.finish();
}

fn bench_packed_encode(c: &mut Criterion) {
    let decimal = SmallDecimal::new(1234567890, 0, false);
    let mut group = c.benchmark_group("encode_packed_decimal");
    group.bench_function(BenchmarkId::new("standard", 10), |b| {
        b.iter(|| {
            black_box(encode_packed_decimal(&decimal.to_string(), 10, decimal.scale, true).unwrap())
        })
    });
    group.bench_function(BenchmarkId::new("scratch", 10), |b| {
        b.iter(|| {
            let mut scratch = ScratchBuffers::new();
            black_box(encode_packed_decimal_with_scratch(&decimal, 10, true, &mut scratch).unwrap())
        })
    });
    group.finish();
}

criterion_group!(benches, bench_zoned_encode, bench_packed_encode);
criterion_main!(benches);
