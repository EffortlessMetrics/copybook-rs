use std::collections::HashMap;
use std::time::{Duration, SystemTime};

pub struct PerformanceRegressionDetector {
    pub(crate) baseline_repository: BaselineRepository,
    pub(crate) statistical_analyzer: StatisticalRegressionAnalyzer,
    pub(crate) ci_integrator: CiIntegrator,
    pub(crate) alert_system: AlertSystem,
}

/// Repository for managing performance baselines
#[derive(Debug, Clone)]
pub struct BaselineRepository {
    pub(crate) storage_backend: StorageBackend,
    pub(crate) baseline_metadata: HashMap<String, BaselineMetadata>,
    pub(crate) retention_policy: BaselineRetentionPolicy,
}

/// Storage backend for baselines
#[derive(Debug, Clone)]
pub enum StorageBackend {
    FileSystem {
        root_path: String,
    },
    Database {
        connection_string: String,
    },
    CloudStorage {
        bucket_name: String,
        region: String,
    },
    GitRepository {
        repository_url: String,
        branch: String,
    },
}

/// Baseline metadata with environment context
#[derive(Debug, Clone)]
pub struct BaselineMetadata {
    pub baseline_id: String,
    pub creation_timestamp: SystemTime,
    pub git_commit_hash: Option<String>,
    pub environment_info: EnvironmentInfo,
    pub performance_metrics: PerformanceMetrics,
    pub statistical_properties: StatisticalProperties,
    pub validation_status: BaselineValidationStatus,
}

/// Environment information for baseline context
#[derive(Debug, Clone)]
pub struct EnvironmentInfo {
    pub rust_version: String,
    pub target_triple: String,
    pub cpu_info: CpuInfo,
    pub memory_info: MemoryInfo,
    pub build_configuration: BuildConfiguration,
}

/// CPU information
#[derive(Debug, Clone)]
pub struct CpuInfo {
    pub model: String,
    pub cores: usize,
    pub frequency_mhz: u32,
    pub cache_size_kb: u32,
}

/// Memory information
#[derive(Debug, Clone)]
pub struct MemoryInfo {
    pub total_gb: f64,
    pub available_gb: f64,
    pub memory_type: String,
}

/// Build configuration details
#[derive(Debug, Clone)]
pub struct BuildConfiguration {
    pub optimization_level: OptimizationLevel,
    pub debug_info: bool,
    pub target_cpu: String,
    pub features_enabled: Vec<String>,
}

/// Optimization levels
#[derive(Debug, Clone)]
pub enum OptimizationLevel {
    Debug,
    Release,
    ReleaseWithDebugInfo,
}

/// Performance metrics for regression analysis
#[derive(Debug, Clone)]
pub struct PerformanceMetrics {
    pub display_throughput: ThroughputMetrics,
    pub comp3_throughput: ThroughputMetrics,
    pub memory_usage: MemoryUsageMetrics,
    pub latency_metrics: LatencyMetrics,
}

/// Throughput metrics
#[derive(Debug, Clone)]
pub struct ThroughputMetrics {
    pub mean: f64,
    pub median: f64,
    pub percentile_95: f64,
    pub percentile_99: f64,
    pub std_deviation: f64,
    pub min: f64,
    pub max: f64,
}

/// Memory usage metrics
#[derive(Debug, Clone)]
pub struct MemoryUsageMetrics {
    pub peak_memory_mb: f64,
    pub average_memory_mb: f64,
    pub steady_state_memory_mb: f64,
    pub memory_variance: f64,
}

/// Latency metrics
#[derive(Debug, Clone)]
pub struct LatencyMetrics {
    pub p50_ms: f64,
    pub p95_ms: f64,
    pub p99_ms: f64,
    pub p999_ms: f64,
    pub max_latency_ms: f64,
}

/// Statistical properties of metrics
#[derive(Debug, Clone)]
pub struct StatisticalProperties {
    pub sample_size: usize,
    pub confidence_interval_95: ConfidenceInterval,
    pub statistical_significance: bool,
    pub normality_test_passed: bool,
}

/// Confidence interval
#[derive(Debug, Clone)]
pub struct ConfidenceInterval {
    pub lower_bound: f64,
    pub upper_bound: f64,
    pub confidence_level: f64,
}

/// Baseline validation status
#[derive(Debug, Clone)]
pub enum BaselineValidationStatus {
    Valid,
    UnderReview,
    Invalid { reason: String },
    Superseded { new_baseline_id: String },
}

/// Baseline retention policy
#[derive(Debug, Clone)]
pub struct BaselineRetentionPolicy {
    pub max_baselines_per_branch: usize,
    pub retention_days: u32,
    pub archive_after_days: u32,
}

/// Statistical regression analyzer
pub struct StatisticalRegressionAnalyzer {
    pub(crate) variance_tolerance: f64,
    pub(crate) significance_level: f64,
    pub(crate) minimum_sample_size: usize,
    pub(crate) outlier_detection: OutlierDetectionConfig,
}

/// Outlier detection configuration
#[derive(Debug, Clone)]
pub struct OutlierDetectionConfig {
    pub method: OutlierDetectionMethod,
    pub threshold: f64,
    pub action: OutlierAction,
}

/// Outlier detection methods
#[derive(Debug, Clone)]
pub enum OutlierDetectionMethod {
    IQR,
    ZScore,
    ModifiedZScore,
    IsolationForest,
}

/// Actions for outliers
#[derive(Debug, Clone)]
pub enum OutlierAction {
    Remove,
    Flag,
    Transform,
    Ignore,
}

/// Regression analysis result
#[derive(Debug)]
pub struct RegressionAnalysis {
    pub status: RegressionStatus,
    pub metrics_comparison: MetricsComparison,
    pub statistical_tests: StatisticalTestResults,
    pub confidence_score: f64,
    pub recommendations: Vec<RecommendationAction>,
}

/// Regression status
#[derive(Debug)]
pub enum RegressionStatus {
    NoRegression,
    MinorRegression { severity: RegressionSeverity },
    MajorRegression { severity: RegressionSeverity },
    CriticalRegression { severity: RegressionSeverity },
    Improvement { magnitude: f64 },
}

/// Regression severity
#[derive(Debug, Clone)]
pub enum RegressionSeverity {
    Low,
    Medium,
    High,
    Critical,
}

/// Metrics comparison
#[derive(Debug)]
pub struct MetricsComparison {
    pub throughput_changes: Vec<MetricChange>,
    pub memory_changes: Vec<MetricChange>,
    pub latency_changes: Vec<MetricChange>,
    pub overall_change_percent: f64,
}

/// Individual metric change
#[derive(Debug)]
pub struct MetricChange {
    pub metric_name: String,
    pub baseline_value: f64,
    pub current_value: f64,
    pub change_percent: f64,
    pub change_direction: ChangeDirection,
    pub statistical_significance: bool,
}

/// Direction of change
#[derive(Debug)]
pub enum ChangeDirection {
    Improvement,
    Degradation,
    Neutral,
}

/// Statistical test results
#[derive(Debug)]
pub struct StatisticalTestResults {
    pub t_test_result: TTestResult,
    pub mann_whitney_u_result: MannWhitneyResult,
    pub effect_size: EffectSize,
    pub power_analysis: PowerAnalysisResult,
}

/// T-test result
#[derive(Debug)]
pub struct TTestResult {
    pub statistic: f64,
    pub p_value: f64,
    pub degrees_of_freedom: usize,
    pub significant: bool,
}

/// Mann-Whitney U test result
#[derive(Debug)]
pub struct MannWhitneyResult {
    pub u_statistic: f64,
    pub p_value: f64,
    pub significant: bool,
}

/// Effect size measures
#[derive(Debug)]
pub struct EffectSize {
    pub cohens_d: f64,
    pub glass_delta: f64,
    pub hedges_g: f64,
    pub interpretation: EffectSizeInterpretation,
}

/// Effect size interpretation
#[derive(Debug)]
pub enum EffectSizeInterpretation {
    Negligible,
    Small,
    Medium,
    Large,
    VeryLarge,
}

/// Power analysis result
#[derive(Debug)]
pub struct PowerAnalysisResult {
    pub power: f64,
    pub required_sample_size: usize,
    pub adequate_power: bool,
}

/// Recommendation actions
#[derive(Debug)]
pub enum RecommendationAction {
    AcceptRegression { reason: String },
    RejectChanges { reason: String },
    InvestigateRegression { focus_areas: Vec<String> },
    IncreaseBaseline { new_baseline_justification: String },
    OptimizePerformance { suggested_areas: Vec<String> },
}

/// CI integrator for automated checks
pub struct CiIntegrator {
    pub(crate) performance_gates: Vec<PerformanceGate>,
    pub(crate) notification_config: NotificationConfig,
    pub(crate) integration_config: CiIntegrationConfig,
}

/// Performance gate configuration
#[derive(Debug)]
pub struct PerformanceGate {
    pub gate_id: String,
    pub metric_type: GateMetricType,
    pub threshold: GateThreshold,
    pub action: GateAction,
}

/// Gate metric types
#[derive(Debug)]
pub enum GateMetricType {
    DisplayThroughput,
    Comp3Throughput,
    MemoryUsage,
    Latency,
    Overall,
}

/// Gate threshold
#[derive(Debug)]
pub struct GateThreshold {
    pub max_regression_percent: f64,
    pub confidence_level: f64,
    pub require_statistical_significance: bool,
}

/// Gate actions
#[derive(Debug)]
pub enum GateAction {
    Block,
    Warn,
    Notify,
    Skip,
}

/// Notification configuration
#[derive(Debug)]
pub struct NotificationConfig {
    pub slack_webhook: Option<String>,
    pub email_recipients: Vec<String>,
    pub github_integration: Option<GitHubIntegrationConfig>,
}

/// GitHub integration configuration
#[derive(Debug)]
pub struct GitHubIntegrationConfig {
    pub token: String,
    pub repository: String,
    pub create_issues: bool,
    pub pr_comments: bool,
}

/// CI integration configuration
#[derive(Debug)]
pub struct CiIntegrationConfig {
    pub platforms: Vec<CiPlatform>,
    pub artifact_storage: ArtifactStorageConfig,
    pub reporting_config: ReportingConfig,
}

/// CI platforms
#[derive(Debug)]
pub enum CiPlatform {
    GitHubActions,
    GitLabCI,
    JenkinsCI,
    BuildKite,
    TeamCity,
}

/// Artifact storage configuration
#[derive(Debug)]
pub struct ArtifactStorageConfig {
    pub store_raw_data: bool,
    pub store_analysis_reports: bool,
    pub retention_days: u32,
}

/// Reporting configuration
#[derive(Debug)]
pub struct ReportingConfig {
    pub generate_html_reports: bool,
    pub generate_json_reports: bool,
    pub include_trend_analysis: bool,
}

/// CI check result
#[derive(Debug)]
pub struct CiCheckResult {
    pub overall_status: CiStatus,
    pub gate_results: Vec<GateResult>,
    pub analysis_summary: AnalysisSummary,
    pub recommendations: Vec<RecommendationAction>,
}

/// CI status
#[derive(Debug)]
pub enum CiStatus {
    Passed,
    Failed { reason: String },
    Warning { message: String },
    Skipped { reason: String },
}

/// Gate result
#[derive(Debug)]
pub struct GateResult {
    pub gate_id: String,
    pub status: GateStatus,
    pub measured_value: f64,
    pub threshold_value: f64,
    pub message: String,
}

/// Gate status
#[derive(Debug)]
pub enum GateStatus {
    Passed,
    Failed,
    Warning,
    Skipped,
}

/// Analysis summary
#[derive(Debug)]
pub struct AnalysisSummary {
    pub total_metrics_analyzed: usize,
    pub regressions_detected: usize,
    pub improvements_detected: usize,
    pub confidence_score: f64,
    pub analysis_duration: Duration,
}

/// Alert system for notifications
pub struct AlertSystem {
    pub(crate) alert_policies: Vec<AlertPolicy>,
    pub(crate) escalation_manager: EscalationManager,
    pub(crate) notification_channels: Vec<NotificationChannel>,
}

/// Alert policy
#[derive(Debug, Clone)]
pub struct AlertPolicy {
    pub policy_id: String,
    pub trigger_conditions: Vec<AlertTrigger>,
    pub severity: AlertSeverity,
    pub escalation_policy_id: Option<String>,
}

/// Alert triggers
#[derive(Debug, Clone)]
pub enum AlertTrigger {
    RegressionDetected { min_severity: RegressionSeverity },
    ThresholdExceeded { metric: String, threshold: f64 },
    RepeatedRegressions { count: u32 },
    CriticalMetricFailure,
}

/// Alert severity
#[derive(Debug, Clone)]
pub enum AlertSeverity {
    Info,
    Warning,
    Error,
    Critical,
}

/// Escalation manager
pub struct EscalationManager {
    pub(crate) escalation_policies: HashMap<String, EscalationPolicy>,
    pub(crate) active_escalations: HashMap<String, ActiveEscalation>,
}

/// Escalation policy
#[derive(Debug)]
pub struct EscalationPolicy {
    pub policy_id: String,
    pub escalation_steps: Vec<EscalationStep>,
    pub max_escalation_level: u32,
}

/// Escalation step
#[derive(Debug)]
pub struct EscalationStep {
    pub step_number: u32,
    pub delay: Duration,
    pub notification_targets: Vec<String>,
    pub required_acknowledgment: bool,
}

/// Active escalation
#[derive(Debug)]
pub struct ActiveEscalation {
    pub escalation_id: String,
    pub policy_id: String,
    pub current_step: u32,
    pub start_time: SystemTime,
    pub acknowledged: bool,
}

/// Notification channels
#[derive(Debug)]
pub enum NotificationChannel {
    Email {
        recipients: Vec<String>,
    },
    Slack {
        webhook_url: String,
        channel: String,
    },
    Teams {
        webhook_url: String,
    },
    PagerDuty {
        integration_key: String,
    },
    Custom {
        name: String,
        config: HashMap<String, String>,
    },
}

/// Alert result
#[derive(Debug)]
pub struct AlertResult {
    pub alert_id: String,
    pub policy_id: String,
    pub severity: AlertSeverity,
    pub message: String,
    pub notifications_sent: Vec<NotificationResult>,
    pub escalation_triggered: bool,
}

/// Notification result
#[derive(Debug)]
pub struct NotificationResult {
    pub channel: String,
    pub success: bool,
    pub error_message: Option<String>,
    pub delivery_time: Duration,
}
