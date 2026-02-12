# Benchmark API Contracts Reference
## Issue #49 - Performance Regression Monitoring APIs

### Overview

This reference document defines the **public API contracts** for copybook-rs performance regression monitoring infrastructure (Issue #49), building on the foundation from Issue #52 machine-readable benchmark reporting.

**API Stability**: All APIs follow Rust semantic versioning (SemVer) with backward compatibility guarantees.

**Foundation**: Issue #52 (PR #67) provides `PerformanceReport`, `BaselineStore`, and `bench-report` CLI. Issue #49 extends with regression detection, CI integration, and diagnostic capabilities.

---

## Core API Modules

### 1. Performance Report API (`copybook-bench::reporting`)

#### `PerformanceReport` Struct

Machine-readable performance report for COBOL data processing throughput.

```rust
use copybook_bench::reporting::PerformanceReport;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PerformanceReport {
    /// DISPLAY throughput in GiB/s
    pub display_gibs: Option<f64>,

    /// COMP-3 throughput in MiB/s
    pub comp3_mibs: Option<f64>,

    /// ISO 8601 timestamp (e.g., "2025-09-30T12:34:56Z")
    #[serde(default)]
    pub timestamp: String,

    /// Git commit hash (short, 8 characters)
    #[serde(default)]
    pub commit: String,

    /// Overall status: "success", "warning", "failure"
    #[serde(default = "default_status")]
    pub status: String,

    /// Performance warning messages
    #[serde(default)]
    pub warnings: Vec<String>,

    /// Performance error messages
    #[serde(default)]
    pub errors: Vec<String>,
}
```

**JSON Schema**:
```json
{
  "display_gibs": 4.22,
  "comp3_mibs": 571.0,
  "timestamp": "2025-09-30T12:34:56Z",
  "commit": "abc12345",
  "status": "success",
  "warnings": [],
  "errors": []
}
```

**Field Specifications**:
- `display_gibs`: DISPLAY field decoding throughput (GiB/s). `None` if benchmark not run.
  - **Type**: `Option<f64>`
  - **Range**: `[0.0, f64::MAX]`
  - **Precision**: 2 decimal places recommended
  - **Validation**: Must be `>= 0.0` if present

- `comp3_mibs`: COMP-3 packed decimal decoding throughput (MiB/s). `None` if benchmark not run.
  - **Type**: `Option<f64>`
  - **Range**: `[0.0, f64::MAX]`
  - **Precision**: 1 decimal place recommended
  - **Validation**: Must be `>= 0.0` if present

- `timestamp`: ISO 8601 formatted timestamp (UTC timezone)
  - **Type**: `String`
  - **Format**: `YYYY-MM-DDTHH:MM:SSZ` (RFC 3339)
  - **Example**: `"2025-09-30T12:34:56Z"`
  - **Validation**: Must parse as valid ISO 8601

- `commit`: Git commit hash (short form)
  - **Type**: `String`
  - **Format**: 7-8 character hexadecimal
  - **Example**: `"abc12345"`
  - **Default**: `"unknown"` if not in git repository

- `status`: Overall performance status
  - **Type**: `String`
  - **Values**: `"success"`, `"warning"`, `"failure"`
  - **Default**: `"success"`
  - **Semantics**:
    - `"success"`: All performance targets met
    - `"warning"`: Performance close to targets (within 5%)
    - `"failure"`: Performance below targets or regression >10%

- `warnings`: Performance warning messages
  - **Type**: `Vec<String>`
  - **Examples**:
    - `"DISPLAY throughput 4.15 GiB/s close to SLO threshold"`
    - `"COMP-3 throughput 565 MiB/s within 5% of baseline"`

- `errors`: Performance error messages
  - **Type**: `Vec<String>`
  - **Examples**:
    - `"DISPLAY throughput 3.5 GiB/s below SLO 4.1 GiB/s"`
    - `"Performance regression: 12.5% slower than baseline"`

#### `PerformanceReport` Methods

**Constructor**:
```rust
impl PerformanceReport {
    /// Create new report with default values
    #[must_use]
    pub fn new() -> Self;
}
```

**SLO Validation** (Issue #52):
```rust
/// Validate performance against SLO thresholds
///
/// # Arguments
/// * `display_slo` - DISPLAY throughput SLO in GiB/s (e.g., 4.1)
/// * `comp3_slo` - COMP-3 throughput SLO in MiB/s (e.g., 560.0)
///
/// # Behavior
/// - Sets `status` to "failure" if below SLO
/// - Sets `status` to "warning" if within 5% of SLO
/// - Populates `errors` and `warnings` with details
///
/// # Example
/// ```rust
/// let mut report = PerformanceReport::new();
/// report.display_gibs = Some(4.22);
/// report.comp3_mibs = Some(571.0);
/// report.validate_slos(4.1, 560.0);
/// assert_eq!(report.status, "success");
/// ```
pub fn validate_slos(&mut self, display_slo: f64, comp3_slo: f64);
```

**PR Summary Formatting** (Issue #52):
```rust
/// Format one-liner summary for PR comments
///
/// # Returns
/// Formatted string with status icon and throughput values
///
/// # Example Output
/// ```text
/// DISPLAY: 4.22 GiB/s, COMP-3: 571 MiB/s ✅
/// ```
///
/// # Status Icons
/// - ✅ (success): All targets met
/// - ⚠️ (warning): Close to targets
/// - ❌ (failure): Below targets or regression
#[must_use]
pub fn format_pr_summary(&self) -> String;
```

**Serialization** (Issue #52):
```rust
/// Serialize report to JSON string
///
/// # Errors
/// Returns `serde_json::Error` if serialization fails
///
/// # Example
/// ```rust
/// let report = PerformanceReport::new();
/// let json = serde_json::to_string_pretty(&report)?;
/// ```
impl Serialize for PerformanceReport { /* ... */ }

/// Deserialize report from JSON string
///
/// # Errors
/// Returns `serde_json::Error` if JSON is invalid
///
/// # Example
/// ```rust
/// let json = r#"{"display_gibs": 4.22, "comp3_mibs": 571.0}"#;
/// let report: PerformanceReport = serde_json::from_str(json)?;
/// ```
impl Deserialize for PerformanceReport { /* ... */ }
```

---

### 2. Baseline Management API (`copybook-bench::baseline`)

#### `PerformanceBaseline` Struct

Historical performance baseline for regression detection.

```rust
use copybook_bench::baseline::PerformanceBaseline;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceBaseline {
    /// Branch name (typically "main")
    pub branch: String,

    /// Git commit hash (8 characters)
    pub commit: String,

    /// Baseline creation timestamp (ISO 8601)
    pub timestamp: String,

    /// DISPLAY throughput baseline in GiB/s
    pub display_gibs: Option<f64>,

    /// COMP-3 throughput baseline in MiB/s
    pub comp3_mibs: Option<f64>,

    /// Number of measurements averaged
    pub sample_count: u32,
}
```

**Field Specifications**:
- `branch`: Git branch name (e.g., `"main"`, `"develop"`)
- `commit`: Git commit hash (8 characters)
- `timestamp`: ISO 8601 formatted timestamp
- `display_gibs`: Baseline DISPLAY throughput (GiB/s)
- `comp3_mibs`: Baseline COMP-3 throughput (MiB/s)
- `sample_count`: Number of benchmark runs averaged (typically 5)

#### `BaselineStore` Struct

Baseline storage with 90-day retention policy.

```rust
use copybook_bench::baseline::BaselineStore;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BaselineStore {
    /// Current active baseline for comparison
    pub current: Option<PerformanceBaseline>,

    /// Historical baselines (90-day retention)
    pub history: Vec<PerformanceBaseline>,

    /// Last updated timestamp (ISO 8601)
    pub updated: String,
}
```

**Field Specifications**:
- `current`: Active baseline for PR comparisons (most recent main branch)
- `history`: Historical baselines retained for 90 days (audit compliance)
- `updated`: Last modification timestamp

#### `BaselineStore` Methods

**Constructor**:
```rust
impl BaselineStore {
    /// Create new empty baseline store
    #[must_use]
    pub fn new() -> Self;
}
```

**File I/O** (Issue #52):
```rust
/// Load baseline store from file or create new if missing
///
/// # Arguments
/// * `path` - Path to baseline file (e.g., `target/baselines/performance.json`)
///
/// # Returns
/// - `Ok(BaselineStore)`: Existing baseline loaded or new store created
/// - `Err(anyhow::Error)`: File exists but cannot be read/parsed
///
/// # Example
/// ```rust
/// use std::path::PathBuf;
/// let path = PathBuf::from("target/baselines/performance.json");
/// let store = BaselineStore::load_or_create(&path)?;
/// ```
pub fn load_or_create<P: AsRef<Path>>(path: P) -> anyhow::Result<Self>;

/// Save baseline store to file
///
/// # Arguments
/// * `path` - Path to baseline file
///
/// # Errors
/// Returns error if file cannot be created or written
///
/// # Behavior
/// - Creates parent directories if needed
/// - Writes pretty-printed JSON
///
/// # Example
/// ```rust
/// let path = PathBuf::from("target/baselines/performance.json");
/// store.save(&path)?;
/// ```
pub fn save<P: AsRef<Path>>(&self, path: P) -> anyhow::Result<()>;
```

**Baseline Promotion** (Issue #52):
```rust
/// Promote report to baseline (main branch merge)
///
/// # Arguments
/// * `report` - Performance report to promote
/// * `branch` - Branch name (typically "main")
/// * `commit` - Git commit hash
///
/// # Behavior
/// - Archives current baseline to history
/// - Creates new baseline from report
/// - Updates `updated` timestamp
/// - Applies 90-day retention policy
///
/// # Example
/// ```rust
/// let mut store = BaselineStore::new();
/// let report = PerformanceReport::new();
/// store.promote_baseline(&report, "main", "abc12345");
/// ```
pub fn promote_baseline(
    &mut self,
    report: &PerformanceReport,
    branch: &str,
    commit: &str
);
```

**Regression Detection** (Issue #49 AC1):
```rust
/// Check for performance regression against baseline
///
/// # Arguments
/// * `report` - Current performance report
/// * `threshold` - Regression threshold percentage (e.g., 5.0 for 5%)
///
/// # Returns
/// Vector of regression messages (empty if no regressions)
///
/// # Regression Calculation
/// ```text
/// regression_pct = (baseline - current) / baseline * 100.0
/// ```
///
/// # Example
/// ```rust
/// let store = BaselineStore::load_or_create("baseline.json")?;
/// let report = PerformanceReport::new();
/// let regressions = store.check_regression(&report, 5.0);
///
/// if regressions.is_empty() {
///     println!("✅ No regressions detected");
/// } else {
///     for msg in regressions {
///         println!("❌ {}", msg);
///     }
/// }
/// ```
///
/// # Regression Messages
/// - `"DISPLAY regression: 12.5% slower than baseline (3.5 vs 4.0 GiB/s)"`
/// - `"COMP-3 regression: 8.2% slower than baseline (520 vs 567 MiB/s)"`
#[must_use]
pub fn check_regression(
    &self,
    report: &PerformanceReport,
    threshold: f64
) -> Vec<String>;
```

**Summary Reporting** (Issue #52):
```rust
/// Get baseline summary for display
///
/// # Returns
/// Formatted baseline summary string
///
/// # Example Output
/// ```text
/// Baseline (abc12345): DISPLAY 4.22 GiB/s COMP-3 571 MiB/s [main]
/// ```
///
/// # Returns "No baseline established" if `current` is `None`
#[must_use]
pub fn summary(&self) -> String;
```

---

### 3. Command-Line Interface (`bench-report`)

#### CLI Tool Overview

```bash
copybook-rs benchmark reporting tool for Issue #52

USAGE:
    bench-report <COMMAND> [OPTIONS]

COMMANDS:
    validate <perf.json>           Validate performance report JSON
    baseline promote <perf.json>   Promote report to main baseline
    baseline show                  Show current baseline
    compare <perf.json>            Compare against baseline
    summary                        Show baseline and SLO status
    help                          Show this help message
```

#### Command Specifications

**`validate` Command**:
```bash
# Validate performance report JSON structure and SLOs
bench-report validate perf.json

# Output:
# ✅ Valid performance report
#    Status: success
#    DISPLAY: 4.22 GiB/s
#    COMP-3: 571 MiB/s

# Exit Code:
# 0: Validation successful
# 1: Validation failed (invalid JSON or SLO failure)
```

**Contract**:
- **Input**: Path to `perf.json` file
- **Validation**: JSON schema, SLO thresholds (4.1 GiB/s DISPLAY, 560 MiB/s COMP-3)
- **Output**: Success/failure message with details
- **Exit Code**: 0 (success), 1 (failure)

**`baseline promote` Command**:
```bash
# Promote performance report to main baseline
bench-report baseline promote perf.json

# Output:
# ✅ Promoted baseline: Baseline (abc12345): DISPLAY 4.22 GiB/s COMP-3 571 MiB/s [main]

# Exit Code:
# 0: Promotion successful
# 1: Promotion failed (I/O error, invalid report)
```

**Contract**:
- **Input**: Path to `perf.json` file
- **Behavior**: Archives current baseline, promotes new baseline, saves to file
- **Output**: Confirmation message with baseline summary
- **Exit Code**: 0 (success), 1 (failure)
- **Side Effects**: Writes to `target/baselines/performance.json`

**`baseline show` Command**:
```bash
# Show current active baseline
bench-report baseline show

# Output:
# 📊 Baseline (abc12345): DISPLAY 4.22 GiB/s COMP-3 571 MiB/s [main]
#    Baseline file: target/baselines/performance.json
#    History entries: 15

# Exit Code:
# 0: Always succeeds (shows "No baseline established" if missing)
```

**Contract**:
- **Input**: None
- **Behavior**: Reads baseline file, displays current baseline
- **Output**: Baseline summary or "No baseline established"
- **Exit Code**: 0 (always)

**`compare` Command** (Issue #49 AC1):
```bash
# Compare performance report against baseline
bench-report compare perf.json

# Output (No Regression):
# 📊 Performance Comparison
#    Baseline (abc12345): DISPLAY 4.22 GiB/s COMP-3 571 MiB/s [main]
#    Current: DISPLAY: 4.18 GiB/s, COMP-3: 565 MiB/s ✅
# ✅ No performance regressions detected

# Output (Regression):
# 📊 Performance Comparison
#    Baseline (abc12345): DISPLAY 4.22 GiB/s COMP-3 571 MiB/s [main]
#    Current: DISPLAY: 3.50 GiB/s, COMP-3: 500 MiB/s ❌
# ❌ Performance regressions detected:
#    DISPLAY regression: 17.06% slower than baseline (3.50 vs 4.22 GiB/s)
#    COMP-3 regression: 12.43% slower than baseline (500 vs 571 MiB/s)

# Exit Code:
# 0: No regressions or baseline missing (NEUTRAL)
# 1: Regressions detected (>5% threshold)
```

**Contract**:
- **Input**: Path to `perf.json` file
- **Behavior**: Compares against baseline using 5% threshold
- **Output**: Comparison summary with delta percentages
- **Exit Code**: 0 (pass/neutral), 1 (regression detected)
- **Threshold**: 5% WARNING, 10% FAILURE (exit code 1 for both)

**`summary` Command**:
```bash
# Show performance summary and SLO status
bench-report summary

# Output:
# copybook-rs Performance Summary
# ==============================
#
# 📊 Baseline (abc12345): DISPLAY 4.22 GiB/s COMP-3 571 MiB/s [main]
#
# 🎯 SLO Targets:
#    DISPLAY: ≥4.1 GiB/s
#    COMP-3:  ≥560 MiB/s
#
# 📈 Performance History: 15 entries
#    Baseline file: target/baselines/performance.json

# Exit Code:
# 0: Always succeeds
```

**Contract**:
- **Input**: None
- **Behavior**: Displays baseline, SLO targets, history count
- **Output**: Multi-line summary report
- **Exit Code**: 0 (always)

---

## Regression Detection Algorithm

### Threshold Calculation (Issue #49 AC1)

**Formula**:
```rust
let regression_pct = (baseline_value - current_value) / baseline_value * 100.0;
```

**Status Determination**:
```rust
if regression_pct > 10.0 {
    Status::FAILURE  // >10% regression
} else if regression_pct > 5.0 {
    Status::WARNING  // 5-10% regression
} else {
    Status::PASS     // <5% variance
}
```

**Missing Baseline Handling**:
```rust
if baseline.is_none() {
    return Status::NEUTRAL;  // No comparison possible
}
```

### Threshold Policy

| Regression | Status | PR Gate | PR Comment | CI Exit Code |
|-----------|--------|---------|-----------|--------------|
| <5% | PASS | ✅ Pass | ✅ No regressions | 0 |
| 5-10% | WARNING | ⚠️ Pass | ⚠️ Investigation recommended | 0 |
| >10% | FAILURE | ❌ Block | ❌ Regressions detected | 1 |
| No baseline | NEUTRAL | ✅ Pass | ℹ️ No baseline for comparison | 0 |

### Example Calculations

**Scenario 1: No Regression**
```rust
baseline_display = 4.22 GiB/s
current_display = 4.18 GiB/s
regression_pct = (4.22 - 4.18) / 4.22 * 100.0 = 0.95%
status = PASS  // <5%
```

**Scenario 2: WARNING Regression**
```rust
baseline_display = 4.22 GiB/s
current_display = 4.00 GiB/s
regression_pct = (4.22 - 4.00) / 4.22 * 100.0 = 5.21%
status = WARNING  // 5-10%
```

**Scenario 3: FAILURE Regression**
```rust
baseline_display = 4.22 GiB/s
current_display = 3.50 GiB/s
regression_pct = (4.22 - 3.50) / 4.22 * 100.0 = 17.06%
status = FAILURE  // >10%
```

---

## CI Integration Contracts

### GitHub Actions Workflow Outputs

**Artifact Uploads** (Issue #49 AC3):
```yaml
- name: Upload Performance Artifacts
  uses: actions/upload-artifact@v4
  with:
    name: perf-${{ github.sha }}
    path: |
      perf.json
      target/criterion/**/*.json
    retention-days: 90  # Enterprise audit compliance
```

**Artifact Structure**:
```
perf-abc12345.zip
├── perf.json                           # Machine-readable performance report
└── target/criterion/
    ├── decode_display_heavy/
    │   ├── base/
    │   │   ├── estimates.json
    │   │   └── benchmark.json
    │   └── new/
    │       ├── estimates.json
    │       └── benchmark.json
    └── decode_comp3_heavy/
        ├── base/
        │   ├── estimates.json
        │   └── benchmark.json
        └── new/
            ├── estimates.json
            └── benchmark.json
```

**Baseline Artifact** (Main Branch):
```yaml
- name: Upload Baseline
  if: github.ref == 'refs/heads/main'
  uses: actions/upload-artifact@v4
  with:
    name: baseline-main
    path: target/baselines/performance.json
    retention-days: 90
```

### PR Comment Contract (Issue #49 AC3)

**Comment Format** (No Regression):
```markdown
## Performance Benchmark Results

📊 Performance Comparison
   Baseline (abc12345): DISPLAY 4.22 GiB/s COMP-3 571 MiB/s [main]
   Current: DISPLAY: 4.18 GiB/s, COMP-3: 565 MiB/s ✅

✅ No performance regressions detected
   DISPLAY: -0.95% (within acceptable variance)
   COMP-3: -1.05% (within acceptable variance)

📦 Artifacts: [perf-def67890.zip](https://github.com/EffortlessMetrics/copybook-rs/actions/runs/...)
```

**Comment Format** (Regression):
```markdown
## Performance Benchmark Results

📊 Performance Comparison
   Baseline (abc12345): DISPLAY 4.22 GiB/s COMP-3 571 MiB/s [main]
   Current: DISPLAY: 3.50 GiB/s, COMP-3: 500 MiB/s ❌

❌ Performance regressions detected:
   DISPLAY regression: 17.06% slower than baseline (3.50 vs 4.22 GiB/s)
   COMP-3 regression: 12.43% slower than baseline (500 vs 571 MiB/s)

⚠️ This PR is blocked due to >10% performance regression.
   Please investigate and address performance degradation before merging.

📦 Artifacts: [perf-def67890.zip](https://github.com/EffortlessMetrics/copybook-rs/actions/runs/...)
```

**Comment Format** (No Baseline):
```markdown
## Performance Benchmark Results

📊 Performance Comparison
   No baseline established
   Current: DISPLAY: 4.22 GiB/s, COMP-3: 571 MiB/s ✅

ℹ️ Cannot detect regressions without baseline
   This is expected for first-time benchmarks

📦 Artifacts: [perf-def67890.zip](https://github.com/EffortlessMetrics/copybook-rs/actions/runs/...)
```

**Comment Behavior**:
- **Single Comment**: Update existing comment rather than creating new
- **Comment Identification**: Search for "Performance Benchmark Results" header
- **Update Timing**: On every benchmark run (PR commits)
- **Permissions Required**: `pull-requests: write`

---

## Performance Targets and SLOs

### Enterprise Performance Requirements

**Floor Targets** (Minimum Production):
```rust
const DISPLAY_FLOOR_GIBS: f64 = 0.078125;  // 80 MB/s converted to GiB/s
const COMP3_FLOOR_MIBS: f64 = 40.0;         // 40 MB/s converted to MiB/s
```

**Aspirational Targets** (Hardware Dependent):
```rust
const DISPLAY_ASPIRATIONAL_GIBS: f64 = 4.1;   // ~4.1 GiB/s
const COMP3_ASPIRATIONAL_MIBS: f64 = 560.0;   // ~560 MiB/s
```

**Current Performance Baseline** (Requires AC2 Reconciliation):
```rust
// CLAUDE.md (needs verification)
const DISPLAY_CURRENT_GIBS: f64 = 2.33;    // 2.33 GiB/s
const COMP3_CURRENT_MIBS: f64 = 172.0;     // 168-176 MiB/s

// REPORT.md (appears incorrect - likely measurement error)
// const DISPLAY_REPORTED: 66-95 MiB/s
// const COMP3_REPORTED: 18-25 MiB/s
```

**Safety Margins**:
```rust
// Floor targets (minimum production requirements)
DISPLAY: 2.33 GiB/s / 0.078125 GiB/s = 29.8x margin
COMP-3: 172 MiB/s / 40 MiB/s = 4.3x margin

// Aspirational targets (hardware dependent)
DISPLAY: 4.1 GiB/s target (not yet achieved consistently)
COMP-3: 560 MiB/s target (not yet achieved consistently)
```

### SLO Validation API

**Default SLO Validation**:
```rust
// Uses aspirational targets by default
report.validate_slos(4.1, 560.0);
```

**Custom SLO Validation**:
```rust
// Use floor targets for conservative validation
report.validate_slos(0.078125, 40.0);  // Floor targets

// Use custom targets
report.validate_slos(3.0, 400.0);  // Custom targets
```

---

## Diagnostic API Contracts (Issue #49 AC5)

### Health Check API

```rust
// Extended bench-report CLI command
bench-report health-check

// Output:
// 🏥 Copybook Benchmark Health Check
// ✅ Rust version: 1.92.0
// ✅ Available memory: 28 GB
// ⚠️ CPU governor: powersave (consider 'performance' for benchmarks)
// ✅ Baseline exists: target/baselines/performance.json
// ✅ Baseline integrity: valid JSON, 15 history entries
// ✅ Criterion infrastructure: target/criterion exists
// ✅ Output directories: writeable
// ✅ Minimal benchmark: 1.23 ms execution time
// ✅ All health checks passed
```

**Health Check Components**:
- **Rust Version**: Validate MSRV compliance (1.92+)
- **Available Memory**: Check sufficient memory (4GB+ recommended)
- **CPU Governor**: Recommend 'performance' mode for benchmarks
- **Baseline Validation**: Check baseline file exists and is valid
- **Infrastructure Validation**: Verify Criterion directories exist
- **Smoke Test**: Run minimal benchmark to validate execution

### Verbose Logging API

```bash
# Enable verbose diagnostic output
bench-report --verbose validate perf.json

# Output:
# 🔍 Validating performance report: perf.json
# ├─ Reading file: perf.json (142 bytes)
# ├─ Parsing JSON schema
# │  ├─ display_gibs: 4.22 (valid float)
# │  ├─ comp3_mibs: 571.0 (valid float)
# │  ├─ timestamp: 2025-09-30T12:34:56Z (valid ISO 8601)
# │  └─ commit: abc12345 (valid hash)
# ├─ Validating against SLOs
# │  ├─ DISPLAY: 4.22 GiB/s >= 4.1 GiB/s SLO ✅
# │  └─ COMP-3: 571.0 MiB/s >= 560 MiB/s SLO ✅
# └─ Status: success ✅
```

**Verbose Mode Features**:
- **File Operations**: Display file paths, sizes, read/write operations
- **JSON Parsing**: Show field-by-field parsing validation
- **SLO Validation**: Display comparison calculations step-by-step
- **Regression Detection**: Show threshold calculations with formulas

---

## Error Handling Contracts

### Error Taxonomy

**Benchmark Error Codes** (Following copybook-rs patterns):
```rust
#[derive(Debug, thiserror::Error)]
pub enum BenchmarkError {
    #[error("BENCH001: Performance validation failed: {0}")]
    ValidationFailure(String),

    #[error("BENCH002: Baseline not found: {0}")]
    BaselineNotFound(String),

    #[error("BENCH003: Performance regression detected: {0}")]
    RegressionDetected(String),

    #[error("BENCH004: Health check failed: {0}")]
    HealthCheckFailed(String),

    #[error("BENCH005: Artifact upload failed: {0}")]
    ArtifactUploadFailed(String),

    #[error("BENCH006: JSON parsing failed: {0}")]
    JsonParsingFailed(#[from] serde_json::Error),

    #[error("BENCH007: I/O error: {0}")]
    IoError(#[from] std::io::Error),
}
```

### Error Handling Examples

**Validation Failure**:
```rust
// Code
let report = PerformanceReport::new();
report.display_gibs = Some(3.0);  // Below SLO
report.validate_slos(4.1, 560.0);

// Result
assert_eq!(report.status, "failure");
assert!(report.errors.contains(&"DISPLAY throughput 3.0 GiB/s below SLO 4.1 GiB/s".to_string()));
```

**Missing Baseline**:
```rust
// Code
let store = BaselineStore::load_or_create("nonexistent.json")?;

// Result
// Returns Ok(BaselineStore::new()) - graceful degradation
// No error thrown, empty baseline store created
```

**Regression Detection**:
```rust
// Code
let regressions = store.check_regression(&report, 5.0);

// Result (>10% regression)
// Vec<String> containing:
// - "DISPLAY regression: 17.06% slower than baseline (3.50 vs 4.22 GiB/s)"
// - "COMP-3 regression: 12.43% slower than baseline (500 vs 571 MiB/s)"
```

---

## Backward Compatibility Guarantees

### Issue #52 API Stability

All Issue #52 APIs remain **unchanged and backward compatible**:
- `PerformanceReport`: No modifications to existing fields or methods
- `BaselineStore`: New methods added, existing methods unchanged
- `bench-report` CLI: New commands added, existing commands unchanged

### Semantic Versioning

```toml
[package]
name = "copybook-bench"
version = "0.1.0"  # Pre-1.0: breaking changes allowed with minor version bump
```

**API Stability Commitment**:
- **0.x.y**: Pre-1.0 development, breaking changes with minor version bump
- **1.x.y**: Stable API, breaking changes only with major version bump
- **Deprecation Policy**: 2 minor versions warning before removal

---

## Testing Contracts

### API Testing Requirements

**Unit Tests** (Required):
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_performance_report_creation() {  // AC1
        let report = PerformanceReport::new();
        assert_eq!(report.status, "success");
    }

    #[test]
    fn test_regression_detection() {  // AC1
        let mut store = BaselineStore::new();
        let baseline = create_report(4.0, 600.0);
        store.promote_baseline(&baseline, "main", "abc123");

        let current = create_report(3.0, 500.0);  // 25% regression
        let regressions = store.check_regression(&current, 5.0);
        assert_eq!(regressions.len(), 2);
    }

    #[test]
    fn test_missing_baseline_neutral() {  // AC1
        let store = BaselineStore::new();
        let report = create_report(4.0, 600.0);
        let regressions = store.check_regression(&report, 5.0);
        assert!(regressions.is_empty());  // NEUTRAL
    }
}
```

**Integration Tests** (Required):
```rust
// tests/cli_integration.rs
#[test]
fn test_bench_report_validate() {  // AC1
    let output = Command::new("bench-report")
        .args(&["validate", "perf.json"])
        .output()
        .expect("Failed to execute bench-report");

    assert!(output.status.success());
    assert!(String::from_utf8_lossy(&output.stdout).contains("✅ Valid performance report"));
}

#[test]
fn test_bench_report_compare() {  // AC1
    let output = Command::new("bench-report")
        .args(&["compare", "perf.json"])
        .output()
        .expect("Failed to execute bench-report");

    assert!(output.status.success());
    assert!(String::from_utf8_lossy(&output.stdout).contains("Performance Comparison"));
}
```

### TDD Test Tags

All tests must include `// AC:ID` tags for traceability:
```rust
#[test]
fn test_regression_warning_threshold() {  // AC1: Regression threshold validation
    // Test implementation
}

#[test]
fn test_baseline_promotion() {  // AC2: Baseline reconciliation
    // Test implementation
}

#[test]
fn test_pr_comment_generation() {  // AC3: CI integration
    // Test implementation
}
```

---

## Appendix: API Evolution Roadmap

### Issue #52 Foundation (Implemented)
- ✅ `PerformanceReport` JSON schema
- ✅ `BaselineStore` with 90-day retention
- ✅ `bench-report` CLI basic commands
- ✅ GitHub Actions artifact upload

### Issue #49 Extensions (This Specification)
- ⬜ `check_regression()` with thresholds (AC1)
- ⬜ Baseline reconciliation procedures (AC2)
- ⬜ CI integration with PR comments (AC3)
- ⬜ Progressive complexity benchmarks (AC4)
- ⬜ Enhanced diagnostics (AC5)

### Future Enhancements (Post-Issue #49)
- Historical trend analysis API
- Performance degradation prediction
- Multi-branch baseline comparison
- Custom threshold configuration
- Long-term artifact retention (365 days)

---

**API Reference Version**: 1.0
**Last Updated**: 2025-09-30
**Related Issues**: #52 (foundation), #49 (extensions)
**Related Documents**:
- `docs/explanation/performance-regression-monitoring.md`
- `docs/how-to/benchmark-regression-testing.md`