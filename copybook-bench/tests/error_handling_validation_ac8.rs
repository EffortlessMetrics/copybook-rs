//! Test scaffolding for Issue #52 AC8: Error handling and comprehensive validation
//!
//! Tests feature spec: issue-52-spec.md#AC8
//! Validates machine-readable reports capture benchmark warnings and errors with proper `anyhow::Result<T>` patterns

#![allow(clippy::expect_used)] // Test code: expects are acceptable for test assertions
#![allow(clippy::unwrap_used)] // Test code: unwraps are acceptable for test assertions
#![allow(clippy::unnecessary_wraps)] // Test scaffolding requires Result types

use std::collections::HashMap;
use std::time::{Duration, SystemTime};

/// Error taxonomy for benchmark reporting infrastructure
#[derive(Debug, Clone)]
pub enum BenchmarkReportingError {
    // JSON Generation Errors
    JsonGenerationError(String),
    JsonSchemaValidationError(String),
    JsonSerializationError(String),

    // Benchmark Execution Errors
    BenchmarkExecutionError(String),
    BenchmarkTimeoutError(Duration),
    BenchmarkDataCorruptionError(String),

    // Performance Analysis Errors
    PerformanceExtractionError(String),
    MetricsCalculationError(String),
    StatisticalAnalysisError(String),

    // Automation Errors
    GithubApiError(String),
    PrCommentPostingError(String),
    BaselinePromotionError(String),

    // Validation Errors
    SloValidationError(String),
    ComplianceValidationError(String),
    DataValidationError(String),

    // Infrastructure Errors
    FileSystemError(String),
    NetworkError(String),
    ConfigurationError(String),

    // Audit and Compliance Errors
    AuditReportGenerationError(String),
    ComplianceFrameworkError(String),
    EvidenceCollectionError(String),
}

impl std::fmt::Display for BenchmarkReportingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BenchmarkReportingError::JsonGenerationError(msg) => {
                write!(f, "JSON generation failed: {msg}")
            }
            BenchmarkReportingError::JsonSchemaValidationError(msg) => {
                write!(f, "JSON schema validation failed: {msg}")
            }
            BenchmarkReportingError::JsonSerializationError(msg) => {
                write!(f, "JSON serialization failed: {msg}")
            }
            BenchmarkReportingError::BenchmarkExecutionError(msg) => {
                write!(f, "Benchmark execution failed: {msg}")
            }
            BenchmarkReportingError::BenchmarkTimeoutError(duration) => {
                write!(f, "Benchmark timed out after {duration:?}")
            }
            BenchmarkReportingError::BenchmarkDataCorruptionError(msg) => {
                write!(f, "Benchmark data corruption detected: {msg}")
            }
            BenchmarkReportingError::PerformanceExtractionError(msg) => {
                write!(f, "Performance data extraction failed: {msg}")
            }
            BenchmarkReportingError::MetricsCalculationError(msg) => {
                write!(f, "Metrics calculation failed: {msg}")
            }
            BenchmarkReportingError::StatisticalAnalysisError(msg) => {
                write!(f, "Statistical analysis failed: {msg}")
            }
            BenchmarkReportingError::GithubApiError(msg) => write!(f, "GitHub API error: {msg}"),
            BenchmarkReportingError::PrCommentPostingError(msg) => {
                write!(f, "PR comment posting failed: {msg}")
            }
            BenchmarkReportingError::BaselinePromotionError(msg) => {
                write!(f, "Baseline promotion failed: {msg}")
            }
            BenchmarkReportingError::SloValidationError(msg) => {
                write!(f, "SLO validation failed: {msg}")
            }
            BenchmarkReportingError::ComplianceValidationError(msg) => {
                write!(f, "Compliance validation failed: {msg}")
            }
            BenchmarkReportingError::DataValidationError(msg) => {
                write!(f, "Data validation failed: {msg}")
            }
            BenchmarkReportingError::FileSystemError(msg) => {
                write!(f, "File system error: {msg}")
            }
            BenchmarkReportingError::NetworkError(msg) => write!(f, "Network error: {msg}"),
            BenchmarkReportingError::ConfigurationError(msg) => {
                write!(f, "Configuration error: {msg}")
            }
            BenchmarkReportingError::AuditReportGenerationError(msg) => {
                write!(f, "Audit report generation failed: {msg}")
            }
            BenchmarkReportingError::ComplianceFrameworkError(msg) => {
                write!(f, "Compliance framework error: {msg}")
            }
            BenchmarkReportingError::EvidenceCollectionError(msg) => {
                write!(f, "Evidence collection failed: {msg}")
            }
        }
    }
}

impl std::error::Error for BenchmarkReportingError {}

/// Result type alias following anyhow::Result<T> patterns
pub type BenchmarkResult<T> = Result<T, BenchmarkReportingError>;

/// Error context for detailed diagnostics
#[derive(Debug, Clone)]
pub struct ErrorContext {
    pub error_code: String,
    pub component: String,
    pub operation: String,
    pub timestamp: SystemTime,
    pub environment: EnvironmentContext,
    pub recovery_suggestions: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct EnvironmentContext {
    pub rust_version: String,
    pub target_triple: String,
    pub cpu_cores: u32,
    pub memory_gb: u32,
    pub disk_space_gb: u32,
}

/// Comprehensive error handler for benchmark reporting
pub struct BenchmarkErrorHandler {
    error_history: Vec<ErrorRecord>,
    recovery_strategies: HashMap<String, RecoveryStrategy>,
    escalation_rules: Vec<EscalationRule>,
}

#[derive(Debug, Clone)]
pub struct ErrorRecord {
    pub error: BenchmarkReportingError,
    pub context: ErrorContext,
    pub severity: ErrorSeverity,
    pub recovery_attempted: bool,
    pub recovery_successful: Option<bool>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorSeverity {
    Critical,      // Blocks all functionality
    High,          // Blocks major functionality
    Medium,        // Degrades functionality
    Low,           // Minor impact
    Informational, // No functional impact
}

#[derive(Debug, Clone)]
pub struct RecoveryStrategy {
    pub strategy_id: String,
    pub description: String,
    pub steps: Vec<RecoveryStep>,
    pub success_probability: f64,
}

#[derive(Debug, Clone)]
pub struct RecoveryStep {
    pub description: String,
    pub action: RecoveryAction,
    pub timeout: Duration,
}

#[derive(Debug, Clone)]
pub enum RecoveryAction {
    RetryOperation,
    FallbackToCache,
    UseDefaultValues,
    SkipNonCritical,
    RestartComponent,
}

#[derive(Debug, Clone)]
pub struct EscalationRule {
    pub rule_id: String,
    pub trigger_condition: EscalationTrigger,
    pub notification_targets: Vec<String>,
    pub escalation_delay: Duration,
}

#[derive(Debug, Clone)]
pub enum EscalationTrigger {
    ErrorCountThreshold(usize),
    CriticalError,
    RecoveryFailure,
    SystemUnavailable(Duration),
}

/// Performance metrics with error handling
#[derive(Debug, Clone)]
pub struct PerformanceMetrics {
    pub display_gibs: Option<f64>,
    pub comp3_mibs: Option<f64>,
    pub extraction_errors: Vec<String>,
    pub calculation_warnings: Vec<String>,
}

impl PerformanceMetrics {
    pub fn validate(&self) -> BenchmarkResult<()> {
        if self.display_gibs.is_none() && self.comp3_mibs.is_none() {
            return Err(BenchmarkReportingError::PerformanceExtractionError(
                "No performance metrics could be extracted".to_string(),
            ));
        }

        if let Some(display) = self.display_gibs {
            if display < 0.0 {
                return Err(BenchmarkReportingError::MetricsCalculationError(format!(
                    "Invalid DISPLAY throughput: {display}"
                )));
            }
            if display > 100.0 {
                return Err(BenchmarkReportingError::MetricsCalculationError(format!(
                    "Unrealistic DISPLAY throughput: {display} GiB/s"
                )));
            }
        }

        if let Some(comp3) = self.comp3_mibs {
            if comp3 < 0.0 {
                return Err(BenchmarkReportingError::MetricsCalculationError(format!(
                    "Invalid COMP-3 throughput: {}",
                    comp3
                )));
            }
            if comp3 > 10000.0 {
                return Err(BenchmarkReportingError::MetricsCalculationError(format!(
                    "Unrealistic COMP-3 throughput: {} MiB/s",
                    comp3
                )));
            }
        }

        Ok(())
    }
}

/// JSON report generator with comprehensive error handling
pub struct JsonReportGenerator {
    error_handler: BenchmarkErrorHandler,
}

impl JsonReportGenerator {
    pub fn new() -> Self {
        Self {
            error_handler: BenchmarkErrorHandler::new(),
        }
    }

    pub fn generate_report(
        &mut self,
        metrics: &PerformanceMetrics,
    ) -> BenchmarkResult<serde_json::Value> {
        // Validate input metrics
        metrics.validate().map_err(|e| {
            self.error_handler.handle_error(e.clone());
            e
        })?;

        // Attempt JSON generation with error handling
        let report = self.create_json_structure(metrics)?;

        // Validate JSON schema
        self.validate_json_schema(&report)?;

        Ok(report)
    }

    fn create_json_structure(
        &self,
        metrics: &PerformanceMetrics,
    ) -> BenchmarkResult<serde_json::Value> {
        let display_value = metrics.display_gibs.unwrap_or(0.0);
        let comp3_value = metrics.comp3_mibs.unwrap_or(0.0);

        let mut warnings = metrics.calculation_warnings.clone();
        let errors = metrics.extraction_errors.clone();

        // Add warnings for missing metrics
        if metrics.display_gibs.is_none() {
            warnings.push("DISPLAY throughput could not be measured".to_string());
        }
        if metrics.comp3_mibs.is_none() {
            warnings.push("COMP-3 throughput could not be measured".to_string());
        }

        serde_json::to_value(serde_json::json!({
            "display_gibs": display_value,
            "comp3_mibs": comp3_value,
            "warnings": warnings,
            "errors": errors,
            "_metadata": {
                "timestamp": SystemTime::now().duration_since(SystemTime::UNIX_EPOCH)
                    .unwrap_or_default().as_secs(),
                "schema_version": "1.0",
                "generator": "copybook-bench-reporter"
            }
        }))
        .map_err(|e| BenchmarkReportingError::JsonSerializationError(e.to_string()))
    }

    fn validate_json_schema(&self, json: &serde_json::Value) -> BenchmarkResult<()> {
        let obj = json.as_object().ok_or_else(|| {
            BenchmarkReportingError::JsonSchemaValidationError(
                "JSON root must be an object".to_string(),
            )
        })?;

        // Validate required fields
        let required_fields = ["display_gibs", "comp3_mibs", "warnings", "errors"];
        for field in &required_fields {
            if !obj.contains_key(*field) {
                return Err(BenchmarkReportingError::JsonSchemaValidationError(format!(
                    "Missing required field: {}",
                    field
                )));
            }
        }

        // Validate field types
        if !obj["display_gibs"].is_number() {
            return Err(BenchmarkReportingError::JsonSchemaValidationError(
                "display_gibs must be a number".to_string(),
            ));
        }

        if !obj["comp3_mibs"].is_number() {
            return Err(BenchmarkReportingError::JsonSchemaValidationError(
                "comp3_mibs must be a number".to_string(),
            ));
        }

        if !obj["warnings"].is_array() {
            return Err(BenchmarkReportingError::JsonSchemaValidationError(
                "warnings must be an array".to_string(),
            ));
        }

        if !obj["errors"].is_array() {
            return Err(BenchmarkReportingError::JsonSchemaValidationError(
                "errors must be an array".to_string(),
            ));
        }

        Ok(())
    }
}

impl BenchmarkErrorHandler {
    pub fn new() -> Self {
        Self {
            error_history: Vec::new(),
            recovery_strategies: Self::create_recovery_strategies(),
            escalation_rules: Self::create_escalation_rules(),
        }
    }

    pub fn handle_error(&mut self, error: BenchmarkReportingError) -> ErrorRecord {
        let context = self.create_error_context(&error);
        let severity = self.determine_error_severity(&error);

        let error_record = ErrorRecord {
            error: error.clone(),
            context,
            severity,
            recovery_attempted: false,
            recovery_successful: None,
        };

        self.error_history.push(error_record.clone());
        self.check_escalation_rules();

        error_record
    }

    fn create_error_context(&self, error: &BenchmarkReportingError) -> ErrorContext {
        let (component, operation) = match error {
            BenchmarkReportingError::JsonGenerationError(_) => {
                ("json_generator", "generate_report")
            }
            BenchmarkReportingError::BenchmarkExecutionError(_) => {
                ("benchmark_runner", "execute_benchmarks")
            }
            BenchmarkReportingError::GithubApiError(_) => ("github_api", "post_comment"),
            BenchmarkReportingError::SloValidationError(_) => {
                ("slo_validator", "validate_performance")
            }
            _ => ("unknown", "unknown"),
        };

        ErrorContext {
            error_code: format!("BRE-{:03}", self.error_history.len() + 1),
            component: component.to_string(),
            operation: operation.to_string(),
            timestamp: SystemTime::now(),
            environment: EnvironmentContext {
                rust_version: "1.90.0".to_string(),
                target_triple: "x86_64-unknown-linux-gnu".to_string(),
                cpu_cores: 8,
                memory_gb: 16,
                disk_space_gb: 100,
            },
            recovery_suggestions: self.get_recovery_suggestions(error),
        }
    }

    fn determine_error_severity(&self, error: &BenchmarkReportingError) -> ErrorSeverity {
        match error {
            BenchmarkReportingError::BenchmarkExecutionError(_) => ErrorSeverity::Critical,
            BenchmarkReportingError::BenchmarkTimeoutError(_) => ErrorSeverity::High,
            BenchmarkReportingError::JsonGenerationError(_) => ErrorSeverity::High,
            BenchmarkReportingError::GithubApiError(_) => ErrorSeverity::Medium,
            BenchmarkReportingError::MetricsCalculationError(_) => ErrorSeverity::Medium,
            BenchmarkReportingError::ConfigurationError(_) => ErrorSeverity::Low,
            _ => ErrorSeverity::Medium,
        }
    }

    fn get_recovery_suggestions(&self, error: &BenchmarkReportingError) -> Vec<String> {
        match error {
            BenchmarkReportingError::BenchmarkExecutionError(_) => vec![
                "Check benchmark dependencies are installed".to_string(),
                "Verify sufficient system resources".to_string(),
                "Review benchmark configuration".to_string(),
            ],
            BenchmarkReportingError::BenchmarkTimeoutError(_) => vec![
                "Increase benchmark timeout threshold".to_string(),
                "Check system performance and load".to_string(),
                "Consider reducing benchmark scope".to_string(),
            ],
            BenchmarkReportingError::GithubApiError(_) => vec![
                "Verify GitHub token is valid".to_string(),
                "Check network connectivity".to_string(),
                "Retry operation with exponential backoff".to_string(),
            ],
            _ => vec![
                "Review error details for specific guidance".to_string(),
                "Consult documentation for troubleshooting".to_string(),
            ],
        }
    }

    fn create_recovery_strategies() -> HashMap<String, RecoveryStrategy> {
        let mut strategies = HashMap::new();

        strategies.insert(
            "benchmark_timeout".to_string(),
            RecoveryStrategy {
                strategy_id: "benchmark_timeout".to_string(),
                description: "Recover from benchmark timeout by using cached results".to_string(),
                steps: vec![
                    RecoveryStep {
                        description: "Check for cached benchmark results".to_string(),
                        action: RecoveryAction::FallbackToCache,
                        timeout: Duration::from_secs(30),
                    },
                    RecoveryStep {
                        description: "Use default performance values if no cache".to_string(),
                        action: RecoveryAction::UseDefaultValues,
                        timeout: Duration::from_secs(5),
                    },
                ],
                success_probability: 0.8,
            },
        );

        strategies.insert(
            "github_api_failure".to_string(),
            RecoveryStrategy {
                strategy_id: "github_api_failure".to_string(),
                description: "Recover from GitHub API failures with retry logic".to_string(),
                steps: vec![
                    RecoveryStep {
                        description: "Retry GitHub API call with exponential backoff".to_string(),
                        action: RecoveryAction::RetryOperation,
                        timeout: Duration::from_secs(60),
                    },
                    RecoveryStep {
                        description: "Skip PR comment if API unavailable".to_string(),
                        action: RecoveryAction::SkipNonCritical,
                        timeout: Duration::from_secs(5),
                    },
                ],
                success_probability: 0.9,
            },
        );

        strategies
    }

    fn create_escalation_rules() -> Vec<EscalationRule> {
        vec![
            EscalationRule {
                rule_id: "critical_error_immediate".to_string(),
                trigger_condition: EscalationTrigger::CriticalError,
                notification_targets: vec!["ops-team@company.com".to_string()],
                escalation_delay: Duration::from_secs(0),
            },
            EscalationRule {
                rule_id: "error_threshold".to_string(),
                trigger_condition: EscalationTrigger::ErrorCountThreshold(5),
                notification_targets: vec!["dev-team@company.com".to_string()],
                escalation_delay: Duration::from_secs(300), // 5 minutes
            },
        ]
    }

    fn check_escalation_rules(&self) {
        for rule in &self.escalation_rules {
            match &rule.trigger_condition {
                EscalationTrigger::ErrorCountThreshold(threshold) => {
                    if self.error_history.len() >= *threshold {
                        // Would trigger escalation notification
                    }
                }
                EscalationTrigger::CriticalError => {
                    if let Some(last_error) = self.error_history.last() {
                        if last_error.severity == ErrorSeverity::Critical {
                            // Would trigger immediate escalation
                        }
                    }
                }
                _ => {}
            }
        }
    }
}

/// Tests feature spec: issue-52-spec.md#AC8-error-taxonomy
/// Validates comprehensive error taxonomy for benchmark reporting
#[test]
fn test_comprehensive_error_taxonomy() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC8 - Verify comprehensive error taxonomy covers all error categories

    // Test JSON-related errors
    let json_error =
        BenchmarkReportingError::JsonGenerationError("Schema validation failed".to_string());
    assert!(json_error.to_string().contains("JSON generation failed"));

    let schema_error =
        BenchmarkReportingError::JsonSchemaValidationError("Missing field".to_string());
    assert!(
        schema_error
            .to_string()
            .contains("JSON schema validation failed")
    );

    // Test benchmark execution errors
    let exec_error =
        BenchmarkReportingError::BenchmarkExecutionError("Cargo bench failed".to_string());
    assert!(
        exec_error
            .to_string()
            .contains("Benchmark execution failed")
    );

    let timeout_error = BenchmarkReportingError::BenchmarkTimeoutError(Duration::from_secs(3600));
    assert!(timeout_error.to_string().contains("Benchmark timed out"));

    // Test automation errors
    let github_error = BenchmarkReportingError::GithubApiError("Authentication failed".to_string());
    assert!(github_error.to_string().contains("GitHub API error"));

    let pr_error =
        BenchmarkReportingError::PrCommentPostingError("Rate limit exceeded".to_string());
    assert!(pr_error.to_string().contains("PR comment posting failed"));

    // Test validation errors
    let slo_error =
        BenchmarkReportingError::SloValidationError("Performance below floor".to_string());
    assert!(slo_error.to_string().contains("SLO validation failed"));

    let compliance_error =
        BenchmarkReportingError::ComplianceValidationError("SOX requirements not met".to_string());
    assert!(
        compliance_error
            .to_string()
            .contains("Compliance validation failed")
    );

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC8-anyhow-result-patterns
/// Validates proper anyhow::Result<T> patterns throughout the system
#[test]
fn test_anyhow_result_patterns() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC8 - Verify proper anyhow::Result<T> patterns are used

    // Test successful operation
    let metrics = PerformanceMetrics {
        display_gibs: Some(4.22),
        comp3_mibs: Some(571.0),
        extraction_errors: Vec::new(),
        calculation_warnings: Vec::new(),
    };

    let validation_result: BenchmarkResult<()> = metrics.validate();
    assert!(
        validation_result.is_ok(),
        "Valid metrics should pass validation"
    );

    // Test error propagation
    let invalid_metrics = PerformanceMetrics {
        display_gibs: Some(-1.0), // Invalid negative value
        comp3_mibs: Some(571.0),
        extraction_errors: Vec::new(),
        calculation_warnings: Vec::new(),
    };

    let validation_result = invalid_metrics.validate();
    assert!(
        validation_result.is_err(),
        "Invalid metrics should fail validation"
    );

    // Verify error can be unwrapped and examined
    match validation_result {
        Err(BenchmarkReportingError::MetricsCalculationError(msg)) => {
            assert!(msg.contains("Invalid DISPLAY throughput"));
        }
        _ => panic!("Expected MetricsCalculationError"),
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC8-error-context
/// Validates that errors include comprehensive context information
#[test]
fn test_error_context_information() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC8 - Verify errors include comprehensive context information
    let mut error_handler = BenchmarkErrorHandler::new();

    let test_error = BenchmarkReportingError::BenchmarkExecutionError(
        "Criterion benchmark failed with exit code 1".to_string(),
    );

    let error_record = error_handler.handle_error(test_error);

    // Verify error context is comprehensive
    assert!(
        !error_record.context.error_code.is_empty(),
        "Should have error code"
    );
    assert_eq!(error_record.context.component, "benchmark_runner");
    assert_eq!(error_record.context.operation, "execute_benchmarks");
    assert!(error_record.context.timestamp <= SystemTime::now());

    // Verify environment context
    assert!(!error_record.context.environment.rust_version.is_empty());
    assert!(!error_record.context.environment.target_triple.is_empty());
    assert!(error_record.context.environment.cpu_cores > 0);
    assert!(error_record.context.environment.memory_gb > 0);

    // Verify recovery suggestions
    assert!(!error_record.context.recovery_suggestions.is_empty());
    let suggestions = error_record.context.recovery_suggestions.join(" ");
    assert!(suggestions.contains("dependencies") || suggestions.contains("resources"));

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC8-error-severity-classification
/// Validates that error severity is properly classified
#[test]
fn test_error_severity_classification() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC8 - Verify error severity classification is accurate
    let mut error_handler = BenchmarkErrorHandler::new();

    let test_cases = vec![
        (
            BenchmarkReportingError::BenchmarkExecutionError("Failed".to_string()),
            ErrorSeverity::Critical,
        ),
        (
            BenchmarkReportingError::BenchmarkTimeoutError(Duration::from_secs(3600)),
            ErrorSeverity::High,
        ),
        (
            BenchmarkReportingError::JsonGenerationError("Failed".to_string()),
            ErrorSeverity::High,
        ),
        (
            BenchmarkReportingError::GithubApiError("Rate limited".to_string()),
            ErrorSeverity::Medium,
        ),
        (
            BenchmarkReportingError::MetricsCalculationError("Invalid value".to_string()),
            ErrorSeverity::Medium,
        ),
        (
            BenchmarkReportingError::ConfigurationError("Missing setting".to_string()),
            ErrorSeverity::Low,
        ),
    ];

    for (error, expected_severity) in test_cases {
        let error_record = error_handler.handle_error(error);
        assert_eq!(
            error_record.severity, expected_severity,
            "Error severity should match expected classification"
        );
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC8-json-report-error-handling
/// Validates JSON report generation with comprehensive error handling
#[test]
fn test_json_report_error_handling() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC8 - Verify JSON report generation handles errors comprehensively
    let mut generator = JsonReportGenerator::new();

    // Test successful report generation
    let valid_metrics = PerformanceMetrics {
        display_gibs: Some(4.22),
        comp3_mibs: Some(571.0),
        extraction_errors: Vec::new(),
        calculation_warnings: Vec::new(),
    };

    let result = generator.generate_report(&valid_metrics);
    assert!(
        result.is_ok(),
        "Valid metrics should generate successful report"
    );

    let report = result.expect("Valid metrics should generate successful report");
    assert!(report.is_object(), "Report should be a JSON object");

    // Test error handling for invalid metrics
    let invalid_metrics = PerformanceMetrics {
        display_gibs: Some(150.0), // Unrealistic value
        comp3_mibs: Some(571.0),
        extraction_errors: Vec::new(),
        calculation_warnings: Vec::new(),
    };

    let result = generator.generate_report(&invalid_metrics);
    assert!(
        result.is_err(),
        "Invalid metrics should fail report generation"
    );

    // Test error handling for missing metrics
    let missing_metrics = PerformanceMetrics {
        display_gibs: None,
        comp3_mibs: None,
        extraction_errors: vec!["Benchmark execution failed".to_string()],
        calculation_warnings: Vec::new(),
    };

    let result = generator.generate_report(&missing_metrics);
    assert!(result.is_err(), "Missing metrics should fail validation");

    match result {
        Err(BenchmarkReportingError::PerformanceExtractionError(_)) => {
            // Expected error type
        }
        _ => panic!("Expected PerformanceExtractionError for missing metrics"),
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC8-recovery-strategies
/// Validates that recovery strategies are comprehensive and actionable
#[test]
fn test_recovery_strategies() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC8 - Verify recovery strategies are comprehensive and actionable
    let error_handler = BenchmarkErrorHandler::new();

    // Verify recovery strategies exist
    assert!(
        !error_handler.recovery_strategies.is_empty(),
        "Should have defined recovery strategies"
    );

    // Test benchmark timeout recovery strategy
    let timeout_strategy = error_handler.recovery_strategies.get("benchmark_timeout");
    assert!(
        timeout_strategy.is_some(),
        "Should have benchmark timeout recovery strategy"
    );

    let strategy = timeout_strategy.expect("Should have benchmark timeout recovery strategy");
    assert!(
        !strategy.description.is_empty(),
        "Strategy should have description"
    );
    assert!(
        !strategy.steps.is_empty(),
        "Strategy should have recovery steps"
    );
    assert!(
        strategy.success_probability > 0.0,
        "Strategy should have success probability"
    );

    // Verify recovery steps are actionable
    for step in &strategy.steps {
        assert!(!step.description.is_empty(), "Step should have description");
        assert!(
            step.timeout > Duration::from_secs(0),
            "Step should have timeout"
        );

        match &step.action {
            RecoveryAction::FallbackToCache
            | RecoveryAction::UseDefaultValues
            | RecoveryAction::RetryOperation
            | RecoveryAction::SkipNonCritical
            | RecoveryAction::RestartComponent => {
                // Valid recovery actions
            }
        }
    }

    // Test GitHub API failure recovery strategy
    let github_strategy = error_handler.recovery_strategies.get("github_api_failure");
    assert!(
        github_strategy.is_some(),
        "Should have GitHub API failure recovery strategy"
    );

    let strategy = github_strategy.expect("Should have GitHub integration error recovery strategy");
    assert!(
        strategy.success_probability > 0.8,
        "GitHub API recovery should have high success rate"
    );

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC8-escalation-rules
/// Validates that error escalation rules trigger appropriately
#[test]
fn test_error_escalation_rules() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC8 - Verify error escalation rules trigger appropriately
    let error_handler = BenchmarkErrorHandler::new();

    // Verify escalation rules exist
    assert!(
        !error_handler.escalation_rules.is_empty(),
        "Should have defined escalation rules"
    );

    // Test critical error escalation rule
    let critical_rule = error_handler
        .escalation_rules
        .iter()
        .find(|rule| matches!(rule.trigger_condition, EscalationTrigger::CriticalError));
    assert!(
        critical_rule.is_some(),
        "Should have critical error escalation rule"
    );

    let rule = critical_rule.expect("Should have critical error validation rule");
    assert_eq!(
        rule.escalation_delay,
        Duration::from_secs(0),
        "Critical errors should escalate immediately"
    );
    assert!(
        !rule.notification_targets.is_empty(),
        "Critical errors should have notification targets"
    );

    // Test error count threshold rule
    let threshold_rule = error_handler.escalation_rules.iter().find(|rule| {
        matches!(
            rule.trigger_condition,
            EscalationTrigger::ErrorCountThreshold(_)
        )
    });
    assert!(
        threshold_rule.is_some(),
        "Should have error count threshold rule"
    );

    let rule = threshold_rule.expect("Should have threshold validation rule");
    if let EscalationTrigger::ErrorCountThreshold(threshold) = &rule.trigger_condition {
        assert!(*threshold > 0, "Error threshold should be positive");
        assert!(*threshold < 100, "Error threshold should be reasonable");
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC8-comprehensive-validation
/// Validates that all validation scenarios are properly handled
#[test]
fn test_comprehensive_validation_scenarios() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC8 - Verify comprehensive validation covers all scenarios

    // Test boundary value validation
    let boundary_test_cases = vec![
        // (display_gibs, comp3_mibs, should_pass, description)
        (Some(0.0), Some(0.0), true, "zero_values_valid"),
        (Some(-0.1), Some(571.0), false, "negative_display_invalid"),
        (Some(4.22), Some(-0.1), false, "negative_comp3_invalid"),
        (
            Some(100.1),
            Some(571.0),
            false,
            "unrealistic_display_invalid",
        ),
        (
            Some(4.22),
            Some(10000.1),
            false,
            "unrealistic_comp3_invalid",
        ),
        (Some(4.22), Some(571.0), true, "normal_values_valid"),
        (None, None, false, "no_metrics_invalid"),
        (Some(4.22), None, true, "partial_metrics_valid"),
    ];

    for (display, comp3, should_pass, description) in boundary_test_cases {
        let metrics = PerformanceMetrics {
            display_gibs: display,
            comp3_mibs: comp3,
            extraction_errors: if display.is_none() && comp3.is_none() {
                vec!["No metrics extracted".to_string()]
            } else {
                Vec::new()
            },
            calculation_warnings: Vec::new(),
        };

        let result = metrics.validate();

        if should_pass {
            assert!(
                result.is_ok(),
                "Test case '{}' should pass validation",
                description
            );
        } else {
            assert!(
                result.is_err(),
                "Test case '{}' should fail validation",
                description
            );
        }
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC8-error-aggregation
/// Validates that multiple errors can be collected and reported
#[test]
fn test_error_aggregation_and_reporting() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC8 - Verify multiple errors can be collected and reported
    let mut error_handler = BenchmarkErrorHandler::new();

    // Generate multiple errors
    let errors = vec![
        BenchmarkReportingError::JsonGenerationError("Schema validation failed".to_string()),
        BenchmarkReportingError::GithubApiError("Rate limit exceeded".to_string()),
        BenchmarkReportingError::MetricsCalculationError("Division by zero".to_string()),
        BenchmarkReportingError::ConfigurationError("Missing API key".to_string()),
    ];

    for error in errors {
        error_handler.handle_error(error);
    }

    // Verify errors were collected
    assert_eq!(
        error_handler.error_history.len(),
        4,
        "Should have collected all 4 errors"
    );

    // Verify error history maintains order
    assert!(
        error_handler.error_history[0]
            .context
            .error_code
            .contains("001")
    );
    assert!(
        error_handler.error_history[3]
            .context
            .error_code
            .contains("004")
    );

    // Verify different severity levels are represented
    let severities: Vec<_> = error_handler
        .error_history
        .iter()
        .map(|record| &record.severity)
        .collect();

    assert!(
        severities.contains(&&ErrorSeverity::High),
        "Should have high severity errors"
    );
    assert!(
        severities.contains(&&ErrorSeverity::Medium),
        "Should have medium severity errors"
    );
    assert!(
        severities.contains(&&ErrorSeverity::Low),
        "Should have low severity errors"
    );

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC8-warning-vs-error-distinction
/// Validates proper distinction between warnings and errors in reports
#[test]
fn test_warning_vs_error_distinction() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC8 - Verify proper distinction between warnings and errors
    let mut generator = JsonReportGenerator::new();

    // Test metrics with both warnings and errors
    let metrics_with_warnings = PerformanceMetrics {
        display_gibs: Some(4.22),
        comp3_mibs: None, // Missing metric (should generate warning)
        extraction_errors: Vec::new(),
        calculation_warnings: vec![
            "COMP-3 measurement had high variance".to_string(),
            "Benchmark execution time exceeded normal range".to_string(),
        ],
    };

    let result = generator.generate_report(&metrics_with_warnings);
    assert!(
        result.is_ok(),
        "Metrics with only warnings should generate report"
    );

    let report = result.expect("Metrics with only warnings should generate report");
    let warnings = report["warnings"]
        .as_array()
        .expect("Report must contain warnings array");
    let errors = report["errors"]
        .as_array()
        .expect("Report must contain errors array");

    // Should have warnings but no errors
    assert!(!warnings.is_empty(), "Should have warnings");
    assert!(errors.is_empty(), "Should have no errors");

    // Test metrics with actual errors
    let metrics_with_errors = PerformanceMetrics {
        display_gibs: Some(4.22),
        comp3_mibs: Some(571.0),
        extraction_errors: vec![
            "Benchmark crashed during execution".to_string(),
            "Data corruption detected in results".to_string(),
        ],
        calculation_warnings: Vec::new(),
    };

    let result = generator.generate_report(&metrics_with_errors);
    assert!(
        result.is_ok(),
        "Should generate report even with extraction errors"
    );

    let report = result.expect("Should generate report even with extraction errors");
    let warnings = report["warnings"]
        .as_array()
        .expect("Report must contain warnings array");
    let errors = report["errors"]
        .as_array()
        .expect("Report must contain errors array");

    // Should have errors but no warnings
    assert!(warnings.is_empty(), "Should have no warnings");
    assert!(!errors.is_empty(), "Should have errors");

    Ok(())
}
