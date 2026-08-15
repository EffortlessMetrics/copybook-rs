// SPDX-License-Identifier: AGPL-3.0-or-later
//! Opt-in local Criterion target for one validated external-input manifest.

use std::env;
use std::hint::black_box;
use std::path::PathBuf;
use std::time::Instant;

use anyhow::{Context, Result, bail};
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
    let mut failure = None;
    group.bench_function("validated_manifest", |bencher| {
        bencher.iter_custom(|iterations| {
            let start = Instant::now();
            for _ in 0..iterations {
                match benchmark.decode_pass() {
                    Ok(decoded) => {
                        black_box(decoded);
                    }
                    Err(error) => {
                        failure = Some(error);
                        break;
                    }
                }
            }
            start.elapsed()
        });
    });
    group.finish();
    criterion.final_summary();
    if let Some(error) = failure {
        bail!(error.context("external-input decode failed during measurement"));
    }
    Ok(())
}
