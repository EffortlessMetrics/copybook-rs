#![allow(clippy::unwrap_used, clippy::expect_used, clippy::unreachable)]
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat,
    UnmappablePolicy, ZonedEncodingFormat, decode_record, decode_record_with_scratch,
    encode_record, memory::ScratchBuffers,
};
use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use std::hint::black_box;

fn schema_text() -> &'static str {
    r"
01 REC.
   05 A   PIC S9(9)     COMP-3.
   05 B   PIC S9(14)V9(4)  COMP-3.
"
}

fn make_opts() -> (EncodeOptions, DecodeOptions) {
    let enc = EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        use_raw: false,
        bwz_encode: false,
        strict_mode: true,
        max_errors: None,
        threads: 1,
        coerce_numbers: false,
        zoned_encoding_override: None,
    };
    let dec = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: true,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Ebcdic,
    };
    (enc, dec)
}

fn bench_comp3(c: &mut Criterion) {
    let schema = copybook_core::parse_copybook(schema_text()).unwrap();
    let (enc, dec) = make_opts();

    // Make a buffer of JSON records (hot path: encode)
    // Use values that fit within i64 limits to avoid overflow
    let json = r#"{"A":"123456789","B":"12345678901234.1234"}"#;

    let mut g = c.benchmark_group("comp3");
    g.throughput(Throughput::Bytes(json.len() as u64));

    // Quick profile for local development
    if std::env::var("QUICK_BENCH").is_ok() {
        g.measurement_time(std::time::Duration::from_millis(800));
        g.sample_size(25);
    }

    g.bench_function("encode_comp3", |b| {
        b.iter(|| {
            let _ = encode_record(
                black_box(&schema),
                black_box(&serde_json::from_str(json).unwrap()),
                black_box(&enc),
            )
            .unwrap();
        });
    });

    // Now generate one encoded record and benchmark decode
    let encoded = encode_record(&schema, &serde_json::from_str(json).unwrap(), &enc).unwrap();

    g.bench_function("decode_comp3", |b| {
        b.iter(|| {
            let _ =
                decode_record(black_box(&schema), black_box(&encoded), black_box(&dec)).unwrap();
        });
    });

    // Optimized decode with scratch buffers - CRITICAL PERFORMANCE OPTIMIZATION
    g.bench_function("decode_comp3_optimized", |b| {
        let mut scratch = ScratchBuffers::new();
        b.iter(|| {
            let _ = decode_record_with_scratch(
                black_box(&schema),
                black_box(&encoded),
                black_box(&dec),
                black_box(&mut scratch),
            )
            .unwrap();
        });
    });

    g.finish();
}

criterion_group!(benches, bench_comp3);
criterion_main!(benches);
