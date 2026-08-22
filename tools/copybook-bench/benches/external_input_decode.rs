// SPDX-License-Identifier: AGPL-3.0-or-later
//! Opt-in local telemetry target for one validated external-input manifest.
//!
//! This target reports diagnostic payload-byte throughput only. It is not a
//! canonical receipt, baseline, gate, threshold, SLO, or scheduled result.

use std::env;
use std::hint::black_box;
use std::path::PathBuf;
use std::process;

use anyhow::{Context, Result};
use copybook_bench::external_input::prepare_external_input_decode_benchmark;
use criterion::{Criterion, Throughput};

const MANIFEST_ENV: &str = "COPYBOOK_EXTERNAL_INPUT_MANIFEST";

fn main() -> Result<()> {
    let manifest = env::var_os(MANIFEST_ENV)
        .filter(|value| !value.is_empty())
        .map(PathBuf::from)
        .with_context(|| format!("{MANIFEST_ENV} must name one external-input manifest"))?;
    let mut benchmark = prepare_external_input_decode_benchmark(&manifest)
        .with_context(|| format!("failed to prepare external input {}", manifest.display()))?;
    let payload_bytes = u64::try_from(benchmark.payload_bytes())
        .context("external-input payload byte total does not fit Criterion throughput")?;

    let mut criterion = Criterion::default().configure_from_args();
    let mut group = criterion.benchmark_group("external_input_decode");
    group.throughput(Throughput::Bytes(payload_bytes));
    let benchmark_id = benchmark.benchmark_id().to_string();
    group.bench_function(benchmark_id, |bencher| {
        bencher.iter_custom(
            |iterations| match benchmark.measure_decode_iterations(iterations) {
                Ok(duration) => {
                    black_box(iterations);
                    duration
                }
                Err(error) => {
                    eprintln!("external-input decode failed during measurement: {error:#}");
                    process::exit(1);
                }
            },
        );
    });
    group.finish();
    criterion.final_summary();
    Ok(())
}
