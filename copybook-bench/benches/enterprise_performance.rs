#![allow(clippy::unwrap_used, clippy::expect_used, clippy::unreachable)]
#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::too_many_lines,
    clippy::similar_names,
    clippy::shadow_unrelated,
    clippy::field_reassign_with_default,
    clippy::unnecessary_unwrap
)]

//! Enterprise Performance Benchmarks
//!
//! This benchmark suite measures the performance impact of enterprise features:
//! - Baseline mode (no enterprise features)
//! - Audit system enabled
//! - Compliance policy enabled (SOX as representative)
//! - Security monitoring enabled
//! - Combined enterprise mode (all features)
//!
//! These benchmarks help establish overhead delta targets and ensure enterprise
//! features remain performant.

use copybook_codec::{DecodeOptions, decode_file_to_jsonl, decode_record};
use copybook_core::{
    audit::context::SecurityClassification,
    audit::{
        AuditContext, AuditLogger, AuditLoggerConfig, ComplianceEngine, ComplianceProfile,
        SecurityAuditor, SecurityMonitor,
    },
    parse_copybook,
};
use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use std::hint::black_box;
use std::io::Cursor;

const ENTERPRISE_COPYBOOK: &str = r"
       01  ENTERPRISE-RECORD.
           05  TRANSACTION-ID      PIC 9(12).
           05  CUSTOMER-ID         PIC 9(8).
           05  CUSTOMER-NAME       PIC X(50).
           05  TRANSACTION-AMOUNT  PIC S9(13)V99 COMP-3.
           05  ACCOUNT-BALANCE     PIC S9(13)V99 COMP-3.
           05  TRANSACTION-DATE    PIC 9(8).
           05  STATUS-CODE         PIC X(2).
           05  SECURITY-LEVEL      PIC X(1).
           05  AUDIT-FLAG          PIC X(1).
           05  FILLER              PIC X(100).
";

fn generate_enterprise_data(record_count: usize) -> Vec<u8> {
    // Generate test data (200 bytes per record)
    let mut data = Vec::new();
    for i in 0..record_count {
        // Transaction ID (12 bytes, ASCII numeric)
        let tx_id = format!("{:012}", i);
        data.extend_from_slice(tx_id.as_bytes());

        // Customer ID (8 bytes, ASCII numeric)
        let cust_id = format!("{:08}", i % 99999999);
        data.extend_from_slice(cust_id.as_bytes());

        // Customer Name (50 bytes, EBCDIC)
        let name = format!("CUSTOMER_{:040}", i);
        let mut name_bytes = name.as_bytes().to_vec();
        name_bytes.resize(50, 0x40); // Pad with EBCDIC spaces
        data.extend_from_slice(&name_bytes);

        // Transaction Amount (8 bytes, COMP-3)
        let amount = (i * 1000) % 999999999999;
        let mut amount_packed = vec![0x00; 8];
        let amount_digits = format!("{amount:014}");
        let amount_bytes: Vec<u8> = amount_digits.bytes().map(|b| b - b'0').collect();
        for (j, chunk) in amount_bytes.chunks(2).enumerate() {
            if j < 7 {
                amount_packed[j] = (chunk[0] << 4) | chunk.get(1).unwrap_or(&0);
            } else {
                amount_packed[7] = (chunk[0] << 4) | 0x0C; // Positive sign
            }
        }
        data.extend_from_slice(&amount_packed);

        // Account Balance (8 bytes, COMP-3)
        let balance = (i * 5000) % 999999999999;
        let mut balance_packed = vec![0x00; 8];
        let balance_digits = format!("{balance:014}");
        let balance_bytes: Vec<u8> = balance_digits.bytes().map(|b| b - b'0').collect();
        for (j, chunk) in balance_bytes.chunks(2).enumerate() {
            if j < 7 {
                balance_packed[j] = (chunk[0] << 4) | chunk.get(1).unwrap_or(&0);
            } else {
                balance_packed[7] = (chunk[0] << 4) | 0x0C; // Positive sign
            }
        }
        data.extend_from_slice(&balance_packed);

        // Transaction Date (8 bytes, ASCII numeric)
        let date = format!("{:08}", 20240101 + i);
        data.extend_from_slice(date.as_bytes());

        // Status Code (2 bytes, ASCII)
        let status = if i % 10 == 0 { "RE" } else { "OK" };
        data.extend_from_slice(status.as_bytes());

        // Security Level (1 byte, ASCII)
        let sec_level = match i % 3 {
            0 => b'H', // High
            1 => b'M', // Medium
            _ => b'L', // Low
        };
        data.push(sec_level);

        // Audit Flag (1 byte, ASCII)
        data.push(if i % 5 == 0 { b'Y' } else { b'N' });

        // Filler (100 bytes, EBCDIC spaces)
        data.extend_from_slice(&[0x40; 100]);
    }
    data
}

/// Baseline benchmark without any enterprise features
fn bench_baseline(c: &mut Criterion) {
    let schema = parse_copybook(ENTERPRISE_COPYBOOK).expect("Failed to parse copybook");
    let options = DecodeOptions::default();
    let test_data = generate_enterprise_data(10000); // 2MB of data

    let mut group = c.benchmark_group("enterprise_baseline");

    group.throughput(Throughput::Bytes(test_data.len() as u64));
    group.bench_function("decode_baseline", |b| {
        b.iter(|| {
            let input = Cursor::new(black_box(&test_data));
            let mut output = Vec::new();
            let result =
                decode_file_to_jsonl(black_box(&schema), input, &mut output, black_box(&options));
            let _ = black_box(result);
        });
    });

    group.finish();
}

/// Benchmark with audit context enabled
fn bench_audit_enabled(c: &mut Criterion) {
    let schema = parse_copybook(ENTERPRISE_COPYBOOK).expect("Failed to parse copybook");
    let options = DecodeOptions::default();
    let test_data = generate_enterprise_data(10000); // 2MB of data

    // Create lightweight audit context for each iteration
    let audit_context = AuditContext::new_lightweight()
        .with_operation_id("enterprise_audit_benchmark")
        .with_security_classification(SecurityClassification::Internal)
        .with_metadata("benchmark_mode", "audit_enabled");

    let mut group = c.benchmark_group("enterprise_audit");

    group.throughput(Throughput::Bytes(test_data.len() as u64));
    group.bench_function("decode_with_audit", |b| {
        b.iter(|| {
            let _context = audit_context.create_lightweight_child_context("benchmark_iteration");
            let input = Cursor::new(black_box(&test_data));
            let mut output = Vec::new();
            let result =
                decode_file_to_jsonl(black_box(&schema), input, &mut output, black_box(&options));
            let _ = black_box(result);
        });
    });

    group.finish();
}

/// Benchmark with compliance policy enabled (SOX)
fn bench_compliance_enabled(c: &mut Criterion) {
    let schema = parse_copybook(ENTERPRISE_COPYBOOK).expect("Failed to parse copybook");
    let options = DecodeOptions::default();
    let test_data = generate_enterprise_data(10000); // 2MB of data

    // Create compliance engine with SOX profile
    let compliance_config = copybook_core::audit::compliance::ComplianceConfig::default();
    let compliance_engine =
        ComplianceEngine::new(compliance_config).with_profiles(&[ComplianceProfile::SOX]);

    let mut group = c.benchmark_group("enterprise_compliance");

    group.throughput(Throughput::Bytes(test_data.len() as u64));
    group.bench_function("decode_with_compliance", |b| {
        b.iter(|| {
            let audit_context = AuditContext::new_lightweight()
                .with_operation_id("enterprise_compliance_benchmark")
                .with_security_classification(SecurityClassification::MaterialTransaction)
                .with_compliance_profile(ComplianceProfile::SOX);

            // Note: In production, validation would be async. For benchmarking,
            // we simulate the overhead of creating the context and preparing validation.
            let _validation_prepared = compliance_engine
                .validate_processing_operation(&audit_context)
                .now_or_never();

            let input = Cursor::new(black_box(&test_data));
            let mut output = Vec::new();
            let result =
                decode_file_to_jsonl(black_box(&schema), input, &mut output, black_box(&options));
            let _ = black_box(result);
        });
    });

    group.finish();
}

/// Benchmark with security monitoring enabled
fn bench_security_enabled(c: &mut Criterion) {
    let schema = parse_copybook(ENTERPRISE_COPYBOOK).expect("Failed to parse copybook");
    let options = DecodeOptions::default();
    let test_data = generate_enterprise_data(10000); // 2MB of data

    // Create security monitor
    let security_monitor = SecurityMonitor::new();
    let security_auditor = SecurityAuditor::new();

    let mut group = c.benchmark_group("enterprise_security");

    group.throughput(Throughput::Bytes(test_data.len() as u64));
    group.bench_function("decode_with_security", |b| {
        b.iter(|| {
            let audit_context = AuditContext::new_lightweight()
                .with_operation_id("enterprise_security_benchmark")
                .with_security_classification(SecurityClassification::Confidential);

            // Simulate security validation overhead
            let _validation = security_auditor.validate_access(&audit_context);

            let input = Cursor::new(black_box(&test_data));
            let mut output = Vec::new();
            let result =
                decode_file_to_jsonl(black_box(&schema), input, &mut output, black_box(&options));
            let _ = black_box(result);
        });
    });

    group.finish();
}

/// Benchmark with all enterprise features combined
fn bench_enterprise_combined(c: &mut Criterion) {
    let schema = parse_copybook(ENTERPRISE_COPYBOOK).expect("Failed to parse copybook");
    let options = DecodeOptions::default();
    let test_data = generate_enterprise_data(10000); // 2MB of data

    // Create all enterprise components
    let compliance_config = copybook_core::audit::compliance::ComplianceConfig::default();
    let compliance_engine =
        ComplianceEngine::new(compliance_config).with_profiles(&[ComplianceProfile::SOX]);
    let security_monitor = SecurityMonitor::new();
    let security_auditor = SecurityAuditor::new();

    let mut group = c.benchmark_group("enterprise_combined");

    group.throughput(Throughput::Bytes(test_data.len() as u64));
    group.bench_function("decode_enterprise_all", |b| {
        b.iter(|| {
            // Create full enterprise context
            let audit_context = AuditContext::new_lightweight()
                .with_operation_id("enterprise_combined_benchmark")
                .with_security_classification(SecurityClassification::Confidential)
                .with_compliance_profile(ComplianceProfile::SOX);

            // Simulate all enterprise validations
            let _compliance_prepared = compliance_engine
                .validate_processing_operation(&audit_context)
                .now_or_never();
            let _security_validated = security_auditor.validate_access(&audit_context);

            let input = Cursor::new(black_box(&test_data));
            let mut output = Vec::new();
            let result =
                decode_file_to_jsonl(black_box(&schema), input, &mut output, black_box(&options));
            let _ = black_box(result);
        });
    });

    group.finish();
}

/// SLO validation benchmarks for enterprise mode
/// These establish performance targets for enterprise features
fn bench_enterprise_slo_validation(c: &mut Criterion) {
    let schema = parse_copybook(ENTERPRISE_COPYBOOK).expect("Failed to parse copybook");
    let options = DecodeOptions::default();
    let test_data = generate_enterprise_data(10000); // 2MB of data

    let mut group = c.benchmark_group("enterprise_slo_validation");

    // Baseline SLO: Should meet standard performance targets
    group.throughput(Throughput::Bytes(test_data.len() as u64));
    group.bench_function("baseline_slo", |b| {
        b.iter(|| {
            let input = Cursor::new(black_box(&test_data));
            let mut output = Vec::new();
            let result =
                decode_file_to_jsonl(black_box(&schema), input, &mut output, black_box(&options));
            let _ = black_box(result);
        });
    });

    // Audit SLO: Target < 2% overhead
    let audit_context = AuditContext::new_lightweight()
        .with_operation_id("audit_slo_benchmark")
        .with_security_classification(SecurityClassification::Internal);

    group.throughput(Throughput::Bytes(test_data.len() as u64));
    group.bench_function("audit_slo", |b| {
        b.iter(|| {
            let _context = audit_context.create_lightweight_child_context("slo_iteration");
            let input = Cursor::new(black_box(&test_data));
            let mut output = Vec::new();
            let result =
                decode_file_to_jsonl(black_box(&schema), input, &mut output, black_box(&options));
            let _ = black_box(result);
        });
    });

    // Compliance SLO: Target < 3% overhead
    let compliance_config = copybook_core::audit::compliance::ComplianceConfig::default();
    let compliance_engine =
        ComplianceEngine::new(compliance_config).with_profiles(&[ComplianceProfile::SOX]);

    group.throughput(Throughput::Bytes(test_data.len() as u64));
    group.bench_function("compliance_slo", |b| {
        b.iter(|| {
            let audit_context = AuditContext::new_lightweight()
                .with_operation_id("compliance_slo_benchmark")
                .with_security_classification(SecurityClassification::MaterialTransaction)
                .with_compliance_profile(ComplianceProfile::SOX);

            let _validation_prepared = compliance_engine
                .validate_processing_operation(&audit_context)
                .now_or_never();

            let input = Cursor::new(black_box(&test_data));
            let mut output = Vec::new();
            let result =
                decode_file_to_jsonl(black_box(&schema), input, &mut output, black_box(&options));
            let _ = black_box(result);
        });
    });

    // Security SLO: Target < 1% overhead
    let security_auditor = SecurityAuditor::new();

    group.throughput(Throughput::Bytes(test_data.len() as u64));
    group.bench_function("security_slo", |b| {
        b.iter(|| {
            let audit_context = AuditContext::new_lightweight()
                .with_operation_id("security_slo_benchmark")
                .with_security_classification(SecurityClassification::Confidential);

            let _validation = security_auditor.validate_access(&audit_context);

            let input = Cursor::new(black_box(&test_data));
            let mut output = Vec::new();
            let result =
                decode_file_to_jsonl(black_box(&schema), input, &mut output, black_box(&options));
            let _ = black_box(result);
        });
    });

    // Combined SLO: Target < 5% total overhead
    let compliance_config = copybook_core::audit::compliance::ComplianceConfig::default();
    let compliance_engine =
        ComplianceEngine::new(compliance_config).with_profiles(&[ComplianceProfile::SOX]);
    let security_auditor = SecurityAuditor::new();

    group.throughput(Throughput::Bytes(test_data.len() as u64));
    group.bench_function("combined_slo", |b| {
        b.iter(|| {
            let audit_context = AuditContext::new_lightweight()
                .with_operation_id("combined_slo_benchmark")
                .with_security_classification(SecurityClassification::Confidential)
                .with_compliance_profile(ComplianceProfile::SOX);

            let _compliance_prepared = compliance_engine
                .validate_processing_operation(&audit_context)
                .now_or_never();
            let _security_validated = security_auditor.validate_access(&audit_context);

            let input = Cursor::new(black_box(&test_data));
            let mut output = Vec::new();
            let result =
                decode_file_to_jsonl(black_box(&schema), input, &mut output, black_box(&options));
            let _ = black_box(result);
        });
    });

    group.finish();
}

criterion_group!(
    enterprise_benches,
    bench_baseline,
    bench_audit_enabled,
    bench_compliance_enabled,
    bench_security_enabled,
    bench_enterprise_combined,
    bench_enterprise_slo_validation
);

criterion_main!(enterprise_benches);
