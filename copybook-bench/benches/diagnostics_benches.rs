//! AC5: Diagnostic Benchmarks
//!
//! Diagnostic benchmark suite for infrastructure testing including:
//! - Baseline I/O performance
//! - JSON parsing overhead
//! - File system latency
//! - Memory allocation performance
//!
//! Usage:
//! ```bash
//! cargo bench --bench diagnostics_benches
//! ```
//!
//! Specification: docs/issue-49-tdd-handoff-package.md#ac5-enhanced-diagnostics
//! Traceability: docs/issue-49-traceability-matrix.md#ac5

use copybook_bench::baseline::BaselineStore;
use copybook_bench::reporting::PerformanceReport;
use criterion::{criterion_group, criterion_main, Criterion};

/// AC5: Diagnostic benchmark for JSON parsing overhead
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#diagnostic-api-contracts
///
/// Measures JSON serialization and deserialization performance for
/// performance reports and baselines.
fn diagnostics_json_parsing(c: &mut Criterion) {
    c.bench_function("diagnostics_json_parsing", |b| {
        let mut report = PerformanceReport::new();
        report.display_gibs = Some(2.50);
        report.comp3_mibs = Some(172.0);
        report.timestamp = chrono::Utc::now().to_rfc3339();
        report.commit = "test-commit".to_string();

        b.iter(|| {
            // Serialize
            let json = serde_json::to_string(&report).expect("Failed to serialize");

            // Deserialize
            let _parsed: PerformanceReport =
                serde_json::from_str(&json).expect("Failed to deserialize");
        });
    });
}

/// AC5: Diagnostic benchmark for baseline I/O performance
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#diagnostic-api-contracts
///
/// Measures baseline save/load I/O performance to identify file system
/// bottlenecks and I/O overhead.
fn diagnostics_baseline_io(c: &mut Criterion) {
    let temp_dir = std::env::temp_dir();
    let temp_path = temp_dir.join("diagnostic_baseline_bench.json");

    c.bench_function("diagnostics_baseline_io", |b| {
        let mut store = BaselineStore::new();
        let mut report = PerformanceReport::new();
        report.display_gibs = Some(2.50);
        report.comp3_mibs = Some(172.0);
        store.promote_baseline(&report, "main", "test-commit");

        b.iter(|| {
            // Save baseline
            store.save(&temp_path).expect("Failed to save baseline");

            // Load baseline
            let _loaded = BaselineStore::load_or_create(&temp_path)
                .expect("Failed to load baseline");
        });
    });

    // Cleanup
    std::fs::remove_file(temp_path).ok();
}

/// AC5: Diagnostic benchmark for file system latency
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#diagnostic-api-contracts
///
/// Measures file system latency for small file operations to identify
/// I/O performance characteristics.
fn diagnostics_filesystem_latency(c: &mut Criterion) {
    let temp_dir = std::env::temp_dir();

    c.bench_function("diagnostics_filesystem_latency", |b| {
        b.iter(|| {
            let temp_path = temp_dir.join(format!("diagnostic_fs_{}.txt", std::time::Instant::now().elapsed().as_nanos()));

            // Write small file
            std::fs::write(&temp_path, "test data").expect("Failed to write file");

            // Read small file
            let _contents = std::fs::read_to_string(&temp_path).expect("Failed to read file");

            // Delete file
            std::fs::remove_file(temp_path).ok();
        });
    });
}

/// AC5: Diagnostic benchmark for memory allocation overhead
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#diagnostic-api-contracts
///
/// Measures memory allocation overhead for various data structure sizes
/// to identify allocation bottlenecks.
fn diagnostics_memory_allocation(c: &mut Criterion) {
    c.bench_function("diagnostics_memory_allocation", |b| {
        b.iter(|| {
            // Small allocation (1KB)
            let small = vec![0u8; 1_024];
            drop(small);

            // Medium allocation (100KB)
            let medium = vec![0u8; 102_400];
            drop(medium);

            // Large allocation (1MB)
            let large = vec![0u8; 1_048_576];
            drop(large);
        });
    });
}

/// AC5: Diagnostic benchmark for baseline regression check
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#diagnostic-api-contracts
///
/// Measures regression detection performance to validate overhead
/// is acceptable for CI integration.
fn diagnostics_regression_check(c: &mut Criterion) {
    let mut store = BaselineStore::new();

    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(100.0);
    baseline.comp3_mibs = Some(500.0);
    store.promote_baseline(&baseline, "main", "baseline");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(93.0);
    current.comp3_mibs = Some(485.0);

    c.bench_function("diagnostics_regression_check", |b| {
        b.iter(|| {
            let _regressions = store.check_regression(&current, 5.0);
        });
    });
}

/// AC5: Diagnostic benchmark for baseline store creation
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#diagnostic-api-contracts
///
/// Measures baseline store initialization overhead.
fn diagnostics_store_creation(c: &mut Criterion) {
    c.bench_function("diagnostics_store_creation", |b| {
        b.iter(|| {
            let store = BaselineStore::new();
            drop(store);
        });
    });
}

/// AC5: Diagnostic benchmark for report formatting
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#diagnostic-api-contracts
///
/// Measures performance report formatting overhead for PR comments.
fn diagnostics_report_formatting(c: &mut Criterion) {
    let mut report = PerformanceReport::new();
    report.display_gibs = Some(2.50);
    report.comp3_mibs = Some(172.0);
    report.timestamp = chrono::Utc::now().to_rfc3339();
    report.commit = "test-commit".to_string();

    c.bench_function("diagnostics_report_formatting", |b| {
        b.iter(|| {
            let _summary = report.format_pr_summary();
        });
    });
}

/// AC5: Diagnostic benchmark for baseline retention policy
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#diagnostic-api-contracts
///
/// Measures retention policy application performance (90-day cleanup).
fn diagnostics_retention_policy(c: &mut Criterion) {
    c.bench_function("diagnostics_retention_policy", |b| {
        b.iter(|| {
            let mut store = BaselineStore::new();

            // Add historical baselines
            for i in 0..100 {
                let mut report = PerformanceReport::new();
                report.display_gibs = Some(2.50);
                report.comp3_mibs = Some(172.0);
                report.timestamp = (chrono::Utc::now() - chrono::Duration::days(i)).to_rfc3339();
                store.promote_baseline(&report, "main", &format!("commit-{}", i));
            }

            // Retention policy applied automatically in promote_baseline
        });
    });
}

criterion_group!(
    diagnostics_benches,
    diagnostics_json_parsing,
    diagnostics_baseline_io,
    diagnostics_filesystem_latency,
    diagnostics_memory_allocation,
    diagnostics_regression_check,
    diagnostics_store_creation,
    diagnostics_report_formatting,
    diagnostics_retention_policy
);

criterion_main!(diagnostics_benches);