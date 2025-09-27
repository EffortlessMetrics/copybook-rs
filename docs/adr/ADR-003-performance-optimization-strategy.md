# ADR-003: Performance Optimization Strategy for Enterprise Audit System

## Status
Accepted

## Context
The copybook-rs Enterprise Audit System must maintain the exceptional performance characteristics that define copybook-rs (DISPLAY: 4.1+ GiB/s, COMP-3: 560+ MiB/s) while adding comprehensive audit capabilities. The audit system cannot degrade the core processing performance that gives copybook-rs its competitive advantage in enterprise mainframe data processing.

### Performance Requirements
- **Maximum Performance Impact**: <5% degradation across all processing metrics
- **DISPLAY Processing**: Maintain >3.9 GiB/s (95% of 4.1 GiB/s baseline)
- **COMP-3 Processing**: Maintain >532 MiB/s (95% of 560 MiB/s baseline)
- **Memory Efficiency**: <50 MiB additional steady-state memory
- **Audit Overhead**: <25μs per audit event creation (target: <10μs optimal)
- **Scalability**: Linear performance scaling with CPU cores

### Technical Constraints
- Zero unsafe code policy must be maintained
- Existing copybook processing APIs cannot be modified for backward compatibility
- Audit system must integrate seamlessly without disrupting hot paths
- Memory allocation patterns must align with existing optimizations
- Thread safety required for parallel processing scenarios

### Enterprise Requirements
- Support for high-volume environments (>1M records/hour)
- Real-time audit event generation without batching delays
- Configurable performance vs. audit completeness trade-offs
- Integration with enterprise performance monitoring systems
- Baseline tracking and regression detection

## Decision
We will implement a multi-layered performance optimization strategy using async-first architecture, memory pool management, intelligent sampling, and hot path preservation.

### 1. Async-First Architecture
All audit operations execute asynchronously to avoid blocking main processing threads:
- Background audit task system with dedicated worker threads
- Non-blocking audit event creation using message passing
- Lazy evaluation for expensive audit computations
- Priority queues for critical audit events

### 2. Memory Pool Management
Pre-allocated memory pools eliminate allocation overhead during processing:
- Event buffer pools with configurable sizes
- Context object pools for audit metadata
- Scratch buffer reuse for serialization operations
- Zero-copy operations where possible

### 3. Intelligent Sampling Strategy
Adaptive sampling reduces audit load while maintaining coverage:
- Configurable base sampling rates (default: 10%)
- Performance-adaptive rate adjustment
- Always-audit policies for security-critical operations
- Statistical sampling for performance monitoring

### 4. Hot Path Preservation
Critical processing paths remain unmodified for maximum performance:
- Compile-time feature gates for audit integration
- Minimal instrumentation points in core processing
- Optional audit context creation
- Zero-overhead when auditing disabled

### 5. Performance Monitoring Integration
Continuous monitoring ensures performance targets are maintained:
- Real-time performance metric collection
- Baseline comparison and regression detection
- Automated performance alert generation
- Integration with copybook-bench for baseline updates

## Architecture Implementation

### Async Task System
```rust
pub struct AuditTaskSystem {
    task_queue: Arc<SegQueue<AuditTask>>,
    priority_queue: Arc<SegQueue<AuditTask>>,
    workers: Vec<JoinHandle<()>>,
    metrics: Arc<Mutex<TaskSystemMetrics>>,
    config: TaskSystemConfig,
}

impl AuditTaskSystem {
    pub fn submit_task(&self, task: AuditTask) -> TaskSubmissionResult {
        // Check queue capacity to avoid unbounded growth
        if self.task_queue.len() > self.config.max_queue_size {
            return TaskSubmissionResult::QueueFull;
        }

        // Route based on priority
        match task.priority {
            TaskPriority::Critical | TaskPriority::High => {
                self.priority_queue.push(task);
            }
            _ => {
                self.task_queue.push(task);
            }
        }

        TaskSubmissionResult::Accepted
    }

    async fn worker_loop(
        worker_id: usize,
        task_queue: Arc<SegQueue<AuditTask>>,
        priority_queue: Arc<SegQueue<AuditTask>>,
        memory_pool: AuditMemoryPool,
    ) {
        let mut task_processor = TaskProcessor::new(worker_id);

        loop {
            // Process priority tasks first
            if let Some(task) = priority_queue.pop() {
                task_processor.process_task(task, &memory_pool).await;
                continue;
            }

            // Process regular tasks
            if let Some(task) = task_queue.pop() {
                task_processor.process_task(task, &memory_pool).await;
                continue;
            }

            // Brief yield if no tasks
            tokio::time::sleep(Duration::from_micros(100)).await;
        }
    }
}
```

### Memory Pool Optimization
```rust
pub struct AuditMemoryPool {
    event_buffers: Vec<Vec<u8>>,
    available_buffers: VecDeque<usize>,
    context_pool: Vec<AuditContext>,
    available_contexts: VecDeque<usize>,
    allocation_stats: AllocationStats,
}

impl AuditMemoryPool {
    pub fn get_event_buffer(&mut self) -> Option<Vec<u8>> {
        self.available_buffers.pop_front().map(|idx| {
            self.allocation_stats.buffers_acquired += 1;
            std::mem::take(&mut self.event_buffers[idx])
        })
    }

    pub fn return_event_buffer(&mut self, mut buffer: Vec<u8>) {
        buffer.clear(); // Retain capacity

        if let Some(available_idx) = (0..self.event_buffers.len())
            .find(|&i| self.event_buffers[i].is_empty())
        {
            self.event_buffers[available_idx] = buffer;
            self.available_buffers.push_back(available_idx);
            self.allocation_stats.buffers_returned += 1;
        }
    }

    pub fn utilization_stats(&self) -> PoolUtilization {
        PoolUtilization {
            buffer_utilization: 1.0 - (self.available_buffers.len() as f64
                                     / self.event_buffers.len() as f64),
            efficiency: self.allocation_stats.calculate_efficiency(),
        }
    }
}
```

### Intelligent Sampling
```rust
pub struct AdaptiveSampler {
    base_sample_rate: f64,
    current_rate: f64,
    performance_threshold: f64,
    performance_history: VecDeque<PerformanceSample>,
    sample_counter: AtomicU64,
}

impl AdaptiveSampler {
    pub fn should_audit(&self, context: &OperationContext) -> bool {
        // Always audit critical operations
        if context.is_critical() || context.requires_compliance() {
            return true;
        }

        // Apply sampling for performance operations
        let counter = self.sample_counter.fetch_add(1, Ordering::Relaxed);
        (counter as f64 * self.current_rate) % 1.0 < self.current_rate
    }

    pub fn update_performance_feedback(&mut self, impact: f64) {
        self.performance_history.push_back(PerformanceSample {
            impact,
            timestamp: Instant::now(),
        });

        let avg_impact = self.performance_history.iter()
            .map(|s| s.impact)
            .sum::<f64>() / self.performance_history.len() as f64;

        // Adjust sampling rate based on performance
        if avg_impact > self.performance_threshold {
            self.current_rate = (self.current_rate * 0.9).max(0.01);
        } else if avg_impact < self.performance_threshold * 0.5 {
            self.current_rate = (self.current_rate * 1.1).min(1.0);
        }
    }
}
```

### Hot Path Integration
```rust
// Ultra-lightweight audit integration for critical paths
#[inline(always)]
pub fn audit_critical_operation<F, R>(
    operation_id: &'static str,
    operation: F,
    config: &CriticalPathAuditConfig,
) -> R
where
    F: FnOnce() -> R,
{
    if config.audit_enabled && config.should_audit_operation(operation_id) {
        let start = std::time::Instant::now();
        let result = operation();
        let duration = start.elapsed();

        // Only create audit event if operation was slow
        if duration > config.audit_threshold {
            tokio::spawn(async move {
                create_performance_audit_event(operation_id, duration).await;
            });
        }

        result
    } else {
        // Zero overhead path when auditing disabled
        operation()
    }
}

// Minimal audit context for hot paths
#[derive(Debug, Clone)]
pub struct MinimalAuditContext {
    pub operation_id: &'static str,
    pub started_at: std::time::Instant,
}

impl MinimalAuditContext {
    #[inline(always)]
    pub fn new(operation_id: &'static str) -> Self {
        Self {
            operation_id,
            started_at: std::time::Instant::now(),
        }
    }
}
```

### Performance Monitoring
```rust
pub struct PerformanceMonitor {
    baseline: PerformanceBaseline,
    current_metrics: Arc<RwLock<CurrentMetrics>>,
    alert_thresholds: AlertThresholds,
    regression_detector: RegressionDetector,
}

impl PerformanceMonitor {
    pub async fn measure_operation_with_audit<F, R>(
        &self,
        operation_name: &str,
        operation: F,
    ) -> (R, PerformanceImpact)
    where
        F: FnOnce() -> R,
    {
        let start_time = Instant::now();
        let start_memory = get_memory_usage();

        let result = operation();

        let duration = start_time.elapsed();
        let memory_used = get_memory_usage() - start_memory;

        let impact = PerformanceImpact {
            operation_name: operation_name.to_string(),
            duration_us: duration.as_micros() as u64,
            memory_used_bytes: memory_used,
            measured_at: Instant::now(),
        };

        // Check for regression
        if self.is_regression(&impact) {
            self.handle_performance_regression(&impact).await;
        }

        (result, impact)
    }

    fn is_regression(&self, impact: &PerformanceImpact) -> bool {
        impact.duration_us > self.alert_thresholds.max_duration_us ||
        impact.memory_used_bytes > self.alert_thresholds.max_memory_bytes
    }

    async fn handle_performance_regression(&self, impact: &PerformanceImpact) {
        // Log regression
        warn!(
            "Performance regression detected: {} took {}μs (threshold: {}μs)",
            impact.operation_name,
            impact.duration_us,
            self.alert_thresholds.max_duration_us
        );

        // Trigger adaptive sampling reduction
        if let Some(sampler) = &self.adaptive_sampler {
            sampler.reduce_sampling_rate(0.1).await; // Reduce by 10%
        }
    }
}
```

### Integration with copybook-bench
```rust
pub struct BaselineManager {
    baselines: HashMap<String, PerformanceBaseline>,
    bench_client: BenchmarkClient,
    validation_history: Vec<BaselineValidation>,
}

impl BaselineManager {
    pub async fn update_baselines_from_benchmarks(&mut self) -> Result<()> {
        let benchmark_results = self.bench_client.get_latest_results().await?;

        for result in benchmark_results {
            let baseline = PerformanceBaseline {
                operation_name: result.benchmark_name.clone(),
                target_duration_ms: result.avg_duration_ms,
                target_throughput_mbps: result.throughput_mbps,
                target_memory_mb: result.peak_memory_mb,
                created_at: SystemTime::now(),
            };

            // Validate baseline is achievable with audit system
            if self.validate_baseline_feasibility(&baseline).await? {
                self.baselines.insert(result.benchmark_name, baseline);
            }
        }

        Ok(())
    }

    async fn validate_baseline_feasibility(
        &self,
        baseline: &PerformanceBaseline,
    ) -> Result<bool> {
        // Run test with audit enabled
        let test_result = self.run_baseline_test_with_audit(baseline).await?;

        // Check if audit overhead allows meeting baseline
        let feasible = test_result.audit_overhead_percent <= 5.0;

        Ok(feasible)
    }
}
```

## Performance Targets and Monitoring

### Target Performance Metrics
```rust
#[derive(Debug, Clone)]
pub struct PerformanceTargets {
    // Core processing targets (with audit enabled)
    pub min_display_throughput_gbps: f64,    // 3.9 GiB/s (95% of baseline)
    pub min_comp3_throughput_mbps: f64,      // 532 MiB/s (95% of baseline)
    pub max_memory_overhead_mb: u64,         // 50 MiB additional

    // Audit-specific targets
    pub max_audit_event_creation_us: u64,    // 25μs per event
    pub max_audit_overhead_percent: f64,     // 5% total overhead
    pub min_sampling_rate: f64,               // 1% minimum sampling

    // System targets
    pub max_queue_depth: usize,               // 10,000 pending tasks
    pub max_worker_threads: usize,            // 4 background workers
}

impl Default for PerformanceTargets {
    fn default() -> Self {
        Self {
            min_display_throughput_gbps: 3.9,
            min_comp3_throughput_mbps: 532.0,
            max_memory_overhead_mb: 50,
            max_audit_event_creation_us: 25,
            max_audit_overhead_percent: 5.0,
            min_sampling_rate: 0.01,
            max_queue_depth: 10_000,
            max_worker_threads: 4,
        }
    }
}
```

### Real-Time Monitoring
```rust
pub struct PerformanceMetrics {
    // Throughput metrics
    pub current_display_throughput: Gauge,
    pub current_comp3_throughput: Gauge,
    pub processing_duration: Histogram,

    // Audit metrics
    pub audit_event_creation_time: Histogram,
    pub audit_queue_depth: Gauge,
    pub audit_overhead_percent: Gauge,

    // Memory metrics
    pub memory_usage: Gauge,
    pub pool_utilization: Gauge,

    // Error metrics
    pub performance_regressions: Counter,
    pub queue_overflows: Counter,
}

impl PerformanceMetrics {
    pub fn record_processing_performance(&self, metrics: &ProcessingMetrics) {
        self.current_display_throughput.set(metrics.display_throughput);
        self.current_comp3_throughput.set(metrics.comp3_throughput);
        self.processing_duration.observe(metrics.duration_ms as f64);

        // Check against targets
        if metrics.display_throughput < 3.9 {
            self.performance_regressions.inc();
        }
    }

    pub fn record_audit_performance(&self, impact: &PerformanceImpact) {
        self.audit_event_creation_time.observe(impact.duration_us as f64);
        self.audit_overhead_percent.set(impact.overhead_percent);

        // Alert on target violations
        if impact.duration_us > 25 {
            warn!("Audit event creation exceeds target: {}μs", impact.duration_us);
        }
    }
}
```

### Automated Performance Testing
```rust
#[cfg(test)]
mod performance_tests {
    use super::*;

    #[tokio::test]
    async fn test_display_processing_with_audit_overhead() {
        let audit_config = AuditConfig::default();
        let performance_monitor = PerformanceMonitor::new();

        // Generate test data
        let test_data = generate_display_test_data(1_000_000);

        // Test without audit
        let (baseline_result, baseline_perf) = performance_monitor
            .measure_operation("display_processing_baseline", || {
                process_display_fields(&test_data)
            }).await;

        // Test with audit enabled
        let audit_context = AuditContext::new();
        let (audit_result, audit_perf) = performance_monitor
            .measure_operation("display_processing_with_audit", || {
                process_display_fields_with_audit(&test_data, &audit_context)
            }).await;

        // Validate performance requirements
        let overhead_percent = ((audit_perf.duration_us as f64
                               / baseline_perf.duration_us as f64) - 1.0) * 100.0;

        assert!(overhead_percent < 5.0,
                "Audit overhead too high: {:.2}%", overhead_percent);

        // Validate throughput requirements
        assert!(audit_result.throughput_gbps >= 3.9,
                "DISPLAY throughput below target: {:.2} GiB/s",
                audit_result.throughput_gbps);
    }

    #[tokio::test]
    async fn test_audit_event_creation_performance() {
        let memory_pool = AuditMemoryPool::new(PoolConfig::default());
        let iterations = 10_000;

        let start_time = Instant::now();

        for i in 0..iterations {
            let context = AuditContext::new()
                .with_operation_id(format!("perf_test_{}", i));

            let event = AuditEvent::new(
                AuditEventType::PerformanceMeasurement,
                context,
                test_payload(),
            );

            // Use memory pool for buffer management
            if let Some(buffer) = memory_pool.get_event_buffer() {
                // Serialize to buffer
                let _serialized = serde_json::to_writer(buffer, &event);
                memory_pool.return_event_buffer(buffer);
            }
        }

        let total_duration = start_time.elapsed();
        let avg_creation_time_us = total_duration.as_micros() / iterations as u128;

        assert!(avg_creation_time_us < 25,
                "Audit event creation too slow: {}μs per event",
                avg_creation_time_us);
    }

    #[tokio::test]
    async fn test_memory_pool_efficiency() {
        let config = PoolConfig {
            event_buffer_count: 1000,
            event_buffer_size: 4096,
        };

        let mut memory_pool = AuditMemoryPool::new(config);
        let mut buffers = Vec::new();

        // Acquire all buffers
        for _ in 0..1000 {
            if let Some(buffer) = memory_pool.get_event_buffer() {
                buffers.push(buffer);
            }
        }

        assert_eq!(buffers.len(), 1000);

        // Return all buffers
        for buffer in buffers {
            memory_pool.return_event_buffer(buffer);
        }

        // Check efficiency
        let stats = memory_pool.utilization_stats();
        assert!(stats.efficiency > 0.95,
                "Memory pool efficiency too low: {:.2}", stats.efficiency);
    }
}
```

## Configuration and Tuning

### Performance Configuration
```yaml
# Performance optimization configuration
audit_performance:
  # Task system configuration
  task_system:
    worker_count: 4                    # Background worker threads
    max_queue_size: 10000             # Maximum pending tasks
    priority_queue_size: 1000         # Priority task queue
    worker_timeout_ms: 5000           # Task processing timeout

  # Memory pool configuration
  memory_pool:
    event_buffer_count: 1000          # Pre-allocated buffers
    event_buffer_size_kb: 4           # Buffer size
    context_pool_size: 500            # Context object pool
    allocation_timeout_ms: 100        # Allocation timeout

  # Sampling configuration
  adaptive_sampling:
    enabled: true
    base_rate: 0.1                    # 10% base sampling
    min_rate: 0.01                    # 1% minimum
    max_rate: 1.0                     # 100% maximum
    performance_threshold: 0.02       # 2% impact threshold
    adjustment_factor: 0.1            # 10% rate adjustment

  # Hot path configuration
  hot_path:
    audit_enabled: true
    audit_threshold_us: 100           # Minimum duration for audit
    always_audit_operations:
      - "compliance_validation"
      - "security_event"
    never_audit_operations:
      - "metrics_collection"

  # Performance targets
  targets:
    max_overhead_percent: 5.0         # 5% maximum overhead
    max_event_creation_us: 25         # 25μs event creation
    max_memory_overhead_mb: 50        # 50MB additional memory
    min_display_throughput_gbps: 3.9  # Minimum DISPLAY throughput
    min_comp3_throughput_mbps: 532    # Minimum COMP-3 throughput

  # Monitoring configuration
  monitoring:
    enabled: true
    collection_interval_ms: 1000      # Metrics collection interval
    regression_detection: true        # Enable regression detection
    alert_threshold: 0.05             # 5% regression threshold
    baseline_update_interval: 3600    # Baseline update (1 hour)
```

### Tuning Guidelines

#### High-Volume Environments
- Increase worker thread count to match CPU cores
- Expand memory pool sizes for sustained throughput
- Enable aggressive sampling (1-5%) to reduce overhead
- Use dedicated audit processing nodes in distributed environments

#### Low-Latency Environments
- Minimize sampling rates while maintaining compliance
- Reduce queue sizes to prevent latency buildup
- Enable priority queuing for critical audit events
- Use memory mapping for large audit trail files

#### Memory-Constrained Environments
- Reduce memory pool sizes and buffer counts
- Enable compression for audit event storage
- Implement LRU eviction for audit caches
- Use streaming serialization for large events

#### High-Security Environments
- Increase sampling rates for complete audit coverage
- Enable digital signatures for all audit events
- Use dedicated HSM for key management
- Implement real-time integrity validation

## Alternatives Considered

### Alternative 1: Synchronous Audit Processing
**Approach**: Process audit events synchronously in main thread
**Rejected Because**:
- Unacceptable performance impact (>20% overhead measured)
- Blocking operations prevent pipeline optimization
- Poor scalability with increasing audit complexity
- Risk of audit failures blocking main processing

### Alternative 2: Batched Audit Processing
**Approach**: Collect audit events and process in large batches
**Rejected Because**:
- Delayed audit event availability for real-time monitoring
- Memory pressure from accumulated event batches
- Complex failure recovery for partial batch processing
- Poor compliance with real-time audit requirements

### Alternative 3: External Audit Service
**Approach**: Send audit events to external service via network
**Rejected Because**:
- Network latency and reliability concerns
- Additional infrastructure complexity
- Security risks of audit data in transit
- Performance impact of network serialization

### Alternative 4: Lock-Free Data Structures Only
**Approach**: Use only lock-free queues and structures
**Rejected Because**:
- Increased complexity for memory pool management
- Limited ecosystem support for lock-free memory pools
- Potential performance penalties on some architectures
- Debugging and maintenance complexity

## Implementation Phases

### Phase 1: Core Performance Infrastructure (Week 1-2)
- Implement async task system with worker threads
- Create memory pool management system
- Basic performance monitoring and metrics collection
- Hot path integration with minimal overhead

### Phase 2: Intelligent Sampling (Week 3-4)
- Implement adaptive sampling algorithm
- Performance feedback loop integration
- Configuration system for sampling policies
- Testing and validation of sampling accuracy

### Phase 3: Performance Monitoring (Week 5-6)
- Integration with copybook-bench for baseline management
- Real-time performance regression detection
- Automated alerting for performance violations
- Performance dashboard and metrics export

### Phase 4: Optimization and Tuning (Week 7-8)
- Performance profiling and bottleneck identification
- Memory usage optimization and leak detection
- CPU utilization analysis and thread tuning
- Load testing and scalability validation

## Success Metrics

### Primary Performance Metrics
- **DISPLAY Throughput**: Maintain >3.9 GiB/s (95% of baseline)
- **COMP-3 Throughput**: Maintain >532 MiB/s (95% of baseline)
- **Audit Overhead**: <5% total processing time impact
- **Event Creation**: <25μs average audit event creation time
- **Memory Overhead**: <50 MiB additional steady-state usage

### Secondary Performance Metrics
- **Queue Depth**: <1000 average pending audit tasks
- **Worker Utilization**: 70-90% optimal worker thread utilization
- **Memory Pool Efficiency**: >95% buffer reuse rate
- **Sampling Accuracy**: <2% deviation from target sample rates
- **Regression Detection**: <1% false positive rate

### Compliance Metrics
- **Audit Coverage**: >99% of compliance-required events captured
- **Event Integrity**: 100% cryptographic integrity validation
- **Real-time Processing**: <100ms average event-to-storage latency
- **Error Handling**: <0.1% audit event creation failure rate

## Related Decisions
- ADR-001: Audit Event Schema Design
- ADR-002: Cryptographic Integrity Implementation
- ADR-004: Multi-Framework Compliance Engine
- ADR-005: Enterprise Integration Patterns

## References
- [copybook-rs Performance Analysis](../REPORT.md)
- [Rust Performance Book](https://nnethercote.github.io/perf-book/)
- [Async Rust Performance Guide](https://ryhl.io/blog/async-what-is-blocking/)
- [Lock-Free Programming in Rust](https://doc.rust-lang.org/nomicon/)
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
