# Performance Benchmarks Guide

This guide explains how to run, interpret, and contribute to the performance benchmarking infrastructure in copybook-rs.

## Overview

The `copybook-bench` crate provides comprehensive performance testing and validation to ensure copybook-rs meets its throughput targets:

- **DISPLAY-heavy workloads**: ≥80 MB/s
- **COMP-3-heavy workloads**: ≥40 MB/s
- **Memory usage**: <256 MiB steady-state for multi-GB files

## Getting Started

### Prerequisites

- Rust 1.89+ with cargo
- copybook-rs workspace built
- Optional: `criterion` for advanced reporting

### Running Basic Benchmarks

```bash
# Run all benchmarks
cargo bench --package copybook-bench

# Run specific benchmark suites
cargo bench --package copybook-bench -- encode_performance
cargo bench --package copybook-bench -- decode_performance

# Run SLO validation tests only
cargo bench --package copybook-bench -- slo_validation
```

### Generating Reports

```bash
# Generate HTML report with detailed metrics
cargo bench --package copybook-bench -- --output-format html

# Save benchmark results to file
cargo bench --package copybook-bench > benchmark-results.txt

# Run with performance profiling (requires PERF=1)
PERF=1 cargo bench --package copybook-bench
```

## Benchmark Suites

### 1. encode_performance

Tests numeric encoding performance with and without scratch buffer optimization.

**What it tests:**
- Zoned decimal encoding (standard vs scratch buffer implementation)
- Packed decimal encoding (standard vs scratch buffer implementation)
- Memory allocation patterns

**Key metrics:**
- Throughput (operations per second)
- Memory allocations avoided
- CPU cycle efficiency

**Interpreting results:**
```
encode_zoned_decimal/standard/10
                        time:   [45.234 ns 45.456 ns 45.678 ns]
encode_zoned_decimal/scratch/10
                        time:   [28.123 ns 28.234 ns 28.345 ns]
```

The scratch implementation shows ~38% improvement (28.234ns vs 45.456ns).

### 2. decode_performance

Comprehensive decoding benchmarks for different data types and record counts.

**What it tests:**
- DISPLAY-heavy workloads (text fields)
- COMP-3-heavy workloads (packed decimal)
- Binary-heavy workloads (COMP fields)
- Scaling with different record counts (100, 1K, 10K records)

**Key metrics:**
- Throughput in MB/s
- Records processed per second
- Memory usage patterns

**Interpreting results:**
```
decode_display_heavy/single_threaded/1000
                        time:   [12.45 ms 12.67 ms 12.89 ms]
                        thrpt:  [38.86 MiB/s 39.56 MiB/s 40.24 MiB/s]
```

This shows 39.56 MiB/s throughput for DISPLAY-heavy data.

### 3. slo_validation

Service Level Objective validation ensures performance targets are consistently met.

**What it tests:**
- DISPLAY-heavy SLO: ≥80 MB/s throughput
- COMP-3-heavy SLO: ≥40 MB/s throughput
- Memory usage under load
- Thread scaling efficiency

**Key metrics:**
- Pass/fail status for each SLO
- Actual vs target performance
- Regression detection

## Understanding Benchmark Output

### Throughput Measurements

Criterion reports throughput in multiple formats:

```
time:   [45.234 ns 45.456 ns 45.678 ns]
        [lower_bound estimate upper_bound]
        
thrpt:  [38.86 MiB/s 39.56 MiB/s 40.24 MiB/s]
        [lower_bound estimate upper_bound]
```

- **Time**: Per-operation latency (lower is better)
- **Throughput**: Data processed per second (higher is better)
- **Bounds**: 95% confidence interval

### Performance Comparison

When comparing optimizations:

```
Found 2 outliers among 100 measurements (2.00%)
  1 (1.00%) low mild
  1 (1.00%) high mild

change: [-42.456% -41.234% -39.876%] (p = 0.00 < 0.05)
        Performance has improved.
```

- **Change**: Percentage improvement/regression
- **p-value**: Statistical significance (< 0.05 is significant)

### Memory Analysis

Look for allocation patterns in verbose output:

```
allocated: 1024 bytes (8 allocations)
deallocated: 1024 bytes (8 deallocations)
reallocated: 0 bytes (0 reallocations)
```

Optimized implementations should show fewer allocations.

## Running Performance Tests

### Development Testing

For quick feedback during development:

```bash
# Test specific function optimization
cargo bench --package copybook-bench -- encode_zoned_decimal

# Compare before/after implementations
cargo bench --package copybook-bench -- "encode_zoned_decimal/(standard|scratch)"

# Quick SLO validation
cargo bench --package copybook-bench -- slo_validation --quick
```

### Continuous Integration

For CI/CD pipelines:

```bash
# Run benchmarks with machine-readable output
cargo bench --package copybook-bench --message-format=json > benchmark-results.json

# Check SLO compliance
cargo bench --package copybook-bench -- slo_validation || exit 1

# Generate performance regression report
criterion-compare baseline.json current.json --output-format markdown
```

### Performance Profiling

For detailed performance analysis:

```bash
# Profile with perf (Linux only)
PERF=1 cargo bench --package copybook-bench -- encode_performance

# Memory profiling with valgrind
valgrind --tool=massif cargo bench --package copybook-bench -- --profile-time=1

# CPU profiling with flamegraph
cargo flamegraph --bench encode_performance --package copybook-bench
```

## Writing New Benchmarks

### Basic Benchmark Structure

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};

fn bench_new_feature(c: &mut Criterion) {
    let mut group = c.benchmark_group("new_feature");
    
    // Test data preparation
    let test_data = prepare_test_data();
    
    group.bench_function(BenchmarkId::new("standard", "test"), |b| {
        b.iter(|| {
            // Code under test
            let result = process_standard(black_box(&test_data));
            black_box(result);
        })
    });
    
    group.bench_function(BenchmarkId::new("optimized", "test"), |b| {
        b.iter(|| {
            // Optimized implementation
            let result = process_optimized(black_box(&test_data));
            black_box(result);
        })
    });
    
    group.finish();
}

criterion_group!(benches, bench_new_feature);
criterion_main!(benches);
```

### SLO Validation Benchmarks

```rust
use criterion::{Criterion, Throughput};

fn bench_slo_validation(c: &mut Criterion) {
    let mut group = c.benchmark_group("slo_validation");
    
    // Large test dataset
    let test_data = generate_test_data(10_000); // 10K records
    group.throughput(Throughput::Bytes(test_data.len() as u64));
    
    group.bench_function("throughput_slo", |b| {
        b.iter(|| {
            let start = std::time::Instant::now();
            let result = process_data(black_box(&test_data));
            let elapsed = start.elapsed();
            
            // Calculate throughput
            let throughput = (test_data.len() as f64) / elapsed.as_secs_f64() / (1024.0 * 1024.0);
            
            // Assert SLO compliance
            assert!(throughput >= 80.0, "SLO missed: {:.2} MB/s < 80 MB/s", throughput);
            
            black_box(result);
        })
    });
    
    group.finish();
}
```

### Memory Usage Benchmarks

```rust
fn bench_memory_usage(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_usage");
    
    group.bench_function("allocation_pattern", |b| {
        b.iter_with_large_drop(|| {
            // Test memory allocation patterns
            let mut data = Vec::new();
            for i in 0..1000 {
                data.push(process_record(black_box(i)));
            }
            black_box(data)
        })
    });
    
    group.finish();
}
```

## Best Practices

### Benchmark Design

1. **Use black_box()**: Prevent compiler optimizations that could skew results
2. **Realistic data**: Use representative test data that matches production patterns
3. **Warm-up**: Include setup code outside the measured block
4. **Consistent environment**: Run benchmarks on consistent hardware/OS

### Data Generation

```rust
// Good: Realistic mainframe data patterns
fn generate_realistic_data() -> Vec<u8> {
    let mut data = Vec::new();
    for i in 0..10000 {
        // EBCDIC customer record with realistic field values
        let customer_id = format!("{:06}", i);
        let name = format!("CUSTOMER{:020}", i);
        let balance = (i * 12345) % 999999999;
        
        // Convert to proper EBCDIC/packed decimal encoding
        data.extend(encode_customer_record(customer_id, name, balance));
    }
    data
}

// Bad: Unrealistic patterns
fn generate_simple_data() -> Vec<u8> {
    vec![0u8; 1000000] // All zeros don't represent real mainframe data
}
```

### Statistical Significance

```rust
// Configure for statistical rigor
fn configure_benchmark(c: &mut Criterion) {
    c.configure_from_args()
        .measurement_time(Duration::from_secs(30))  // Longer measurement for stability
        .sample_size(100)                           // More samples for confidence
        .significance_level(0.05)                   // 5% significance level
        .noise_threshold(0.02);                     // 2% noise threshold
}
```

## Troubleshooting

### Common Issues

1. **Inconsistent results**: Ensure stable system load, disable frequency scaling
2. **Memory pressure**: Close other applications during benchmarking
3. **Thermal throttling**: Monitor CPU temperature during long benchmark runs
4. **Compiler variations**: Use consistent Rust version and optimization flags

### Debug Mode Issues

```bash
# Don't run benchmarks in debug mode
cargo bench --package copybook-bench --release

# Verify optimization level
RUSTFLAGS="-C opt-level=3" cargo bench --package copybook-bench
```

### System Configuration

```bash
# Linux: Disable CPU frequency scaling for consistent results
echo performance | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor

# Set CPU affinity for isolated benchmarking
taskset -c 0 cargo bench --package copybook-bench
```

## Integration with CI/CD

### GitHub Actions Example

```yaml
name: Performance Benchmarks

on:
  pull_request:
    paths:
      - 'copybook-codec/src/**'
      - 'copybook-bench/benches/**'

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
      
      - name: Run benchmarks
        run: |
          cargo bench --package copybook-bench -- slo_validation
          
      - name: Check performance regression
        run: |
          cargo bench --package copybook-bench --message-format=json > results.json
          # Process results and fail if regression > 10%
```

### Performance Monitoring

Set up alerts for performance regressions:

```bash
# Extract throughput metrics from benchmark results
grep -E "thrpt.*MB/s" benchmark-results.txt | \
  awk '{if($3 < 80) print "SLO violation: " $3 " MB/s"}'
```

## Future Enhancements

### Planned Benchmarks

- SIMD-optimized EBCDIC conversion
- Vectorized packed decimal processing
- Memory-mapped file I/O performance
- Network streaming benchmarks

### Advanced Metrics

- Cache miss rates
- Branch prediction accuracy
- Memory bandwidth utilization
- Power consumption analysis

For more detailed performance analysis, see [THROUGHPUT_OPTIMIZATIONS.md](../THROUGHPUT_OPTIMIZATIONS.md).