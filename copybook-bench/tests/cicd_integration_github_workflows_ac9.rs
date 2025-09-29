//! Test scaffolding for Issue #52 AC9: CI/CD integration with existing GitHub workflows
//!
//! Tests feature spec: issue-52-spec.md#AC9
//! Validates integration with existing .github/workflows/benchmark.yml

#![allow(clippy::unwrap_used)] // Test code: unwraps are acceptable for test assertions
#![allow(clippy::too_many_lines)] // Test scaffolding files can be long
#![allow(clippy::unnecessary_wraps)] // Test scaffolding requires Result types
#![allow(clippy::panic)] // Test code: panics are acceptable for test failure cases
#![allow(clippy::match_same_arms)] // Test code: matching multiple error types is clearer
#![allow(clippy::doc_markdown)] // Test code: documentation style can be more relaxed
#![allow(clippy::cast_possible_truncation)] // Test code: casting for test data is acceptable
#![allow(clippy::unused_self)] // Test code: self methods can be non-consuming for clarity
#![allow(clippy::map_unwrap_or)] // Test code: map unwrap_or patterns are acceptable

// HashMap removed - not used in this test file
use std::path::PathBuf;
use std::time::{Duration, SystemTime};

/// GitHub Actions workflow context for CI/CD integration
#[derive(Debug, Clone)]
pub struct GitHubWorkflowContext {
    pub workflow_id: String,
    pub run_id: String,
    pub job_id: String,
    pub step_id: String,
    pub runner_os: String,
    pub runner_arch: String,
    pub event_name: String,
    pub ref_name: String,
    pub sha: String,
    pub actor: String,
    pub repository: String,
}

impl GitHubWorkflowContext {
    /// Creates a `GitHubWorkflowContext` from environment variables.
    ///
    /// # Errors
    ///
    /// Returns `CicdIntegrationError::EnvironmentVariableError` if required
    /// environment variables are missing.
    pub fn from_environment() -> Result<Self, CicdIntegrationError> {
        let get_env = |key: &str| -> Result<String, CicdIntegrationError> {
            std::env::var(key).map_err(|_| {
                CicdIntegrationError::EnvironmentVariableError(format!(
                    "Missing required environment variable: {key}"
                ))
            })
        };

        Ok(Self {
            workflow_id: get_env("GITHUB_WORKFLOW")?,
            run_id: get_env("GITHUB_RUN_ID")?,
            job_id: get_env("GITHUB_JOB")?,
            step_id: std::env::var("GITHUB_STEP_ID").unwrap_or_default(),
            runner_os: get_env("RUNNER_OS")?,
            runner_arch: get_env("RUNNER_ARCH")?,
            event_name: get_env("GITHUB_EVENT_NAME")?,
            ref_name: get_env("GITHUB_REF_NAME")?,
            sha: get_env("GITHUB_SHA")?,
            actor: get_env("GITHUB_ACTOR")?,
            repository: get_env("GITHUB_REPOSITORY")?,
        })
    }

    #[must_use]
    pub fn mock_pull_request() -> Self {
        Self {
            workflow_id: "benchmark.yml".to_string(),
            run_id: "12345678".to_string(),
            job_id: "benchmark-job".to_string(),
            step_id: "benchmark-step".to_string(),
            runner_os: "Linux".to_string(),
            runner_arch: "X64".to_string(),
            event_name: "pull_request".to_string(),
            ref_name: "feature/benchmark-reporting".to_string(),
            sha: "a1b2c3d4e5f6789012345678901234567890abcd".to_string(),
            actor: "github-actions[bot]".to_string(),
            repository: "copybook-rs/copybook-rs".to_string(),
        }
    }

    #[must_use]
    pub fn mock_push_to_main() -> Self {
        Self {
            workflow_id: "benchmark.yml".to_string(),
            run_id: "12345679".to_string(),
            job_id: "benchmark-job".to_string(),
            step_id: "benchmark-step".to_string(),
            runner_os: "Linux".to_string(),
            runner_arch: "X64".to_string(),
            event_name: "push".to_string(),
            ref_name: "main".to_string(),
            sha: "b2c3d4e5f6789012345678901234567890abcdef".to_string(),
            actor: "developer".to_string(),
            repository: "copybook-rs/copybook-rs".to_string(),
        }
    }
}

/// CI/CD pipeline integration orchestrator
#[derive(Debug, Clone)]
pub struct CicdPipelineIntegrator {
    workflow_context: GitHubWorkflowContext,
    benchmark_config: BenchmarkConfiguration,
    reporting_config: ReportingConfiguration,
}

#[derive(Debug, Clone)]
pub struct BenchmarkConfiguration {
    pub perf_mode_enabled: bool,
    pub timeout_seconds: u32,
    pub parallel_jobs: u32,
    pub output_format: String,
    pub baseline_comparison: bool,
}

#[derive(Debug, Clone)]
#[allow(clippy::struct_excessive_bools)]
pub struct ReportingConfiguration {
    pub json_output_enabled: bool,
    pub pr_comments_enabled: bool,
    pub baseline_promotion_enabled: bool,
    pub audit_reports_enabled: bool,
    pub output_directory: PathBuf,
}

impl CicdPipelineIntegrator {
    #[must_use]
    pub fn new(workflow_context: GitHubWorkflowContext) -> Self {
        Self {
            workflow_context,
            benchmark_config: BenchmarkConfiguration::default(),
            reporting_config: ReportingConfiguration::default(),
        }
    }

    /// Executes the complete benchmark pipeline.
    ///
    /// # Errors
    ///
    /// Returns `CicdIntegrationError` if any pipeline step fails,
    /// including environment validation, benchmark execution,
    /// or report generation.
    pub fn execute_benchmark_pipeline(
        &self,
    ) -> Result<PipelineExecutionResult, CicdIntegrationError> {
        let mut execution_steps = Vec::new();
        let start_time = SystemTime::now();

        // Step 1: Environment validation
        let _env_validation = Self::validate_environment()?;
        execution_steps.push(PipelineStep {
            step_name: "environment_validation".to_string(),
            status: StepStatus::Completed,
            duration: Duration::from_millis(100),
            output: Some("Environment validation passed".to_string()),
        });

        // Step 2: Benchmark execution
        let benchmark_result = self.execute_benchmarks()?;
        execution_steps.push(PipelineStep {
            step_name: "benchmark_execution".to_string(),
            status: StepStatus::Completed,
            duration: Duration::from_secs(120),
            output: Some(format!(
                "Benchmarks completed: {} results",
                benchmark_result.metrics.len()
            )),
        });

        // Step 3: JSON report generation
        if self.reporting_config.json_output_enabled {
            let json_report = self.generate_json_report(&benchmark_result)?;
            execution_steps.push(PipelineStep {
                step_name: "json_report_generation".to_string(),
                status: StepStatus::Completed,
                duration: Duration::from_millis(500),
                output: Some(format!(
                    "JSON report generated: {} bytes",
                    json_report.len()
                )),
            });
        }

        // Step 4: PR comment posting (for pull_request events)
        if self.workflow_context.event_name == "pull_request"
            && self.reporting_config.pr_comments_enabled
        {
            let _pr_comment = self.post_pr_comment(&benchmark_result)?;
            execution_steps.push(PipelineStep {
                step_name: "pr_comment_posting".to_string(),
                status: StepStatus::Completed,
                duration: Duration::from_millis(1000),
                output: Some("PR comment posted successfully".to_string()),
            });
        }

        // Step 5: Baseline promotion (for push to main)
        if self.workflow_context.event_name == "push"
            && self.workflow_context.ref_name == "main"
            && self.reporting_config.baseline_promotion_enabled
        {
            let _baseline_promotion = self.promote_baseline(&benchmark_result)?;
            execution_steps.push(PipelineStep {
                step_name: "baseline_promotion".to_string(),
                status: StepStatus::Completed,
                duration: Duration::from_millis(800),
                output: Some("Baseline promoted successfully".to_string()),
            });
        }

        // Step 6: Audit report generation (if enabled)
        if self.reporting_config.audit_reports_enabled {
            let _audit_report = self.generate_audit_report(&benchmark_result)?;
            execution_steps.push(PipelineStep {
                step_name: "audit_report_generation".to_string(),
                status: StepStatus::Completed,
                duration: Duration::from_secs(5),
                output: Some("Audit report generated".to_string()),
            });
        }

        let total_duration = start_time.elapsed().unwrap_or_default();

        Ok(PipelineExecutionResult {
            workflow_context: self.workflow_context.clone(),
            execution_steps,
            total_duration,
            success: true,
            artifacts: self.collect_artifacts()?,
        })
    }

    fn validate_environment() -> Result<EnvironmentValidationResult, CicdIntegrationError> {
        // Validate required environment variables
        let required_vars = vec![
            "GITHUB_WORKSPACE",
            "GITHUB_TOKEN",
            "RUNNER_OS",
            "RUNNER_ARCH",
        ];

        let mut missing_vars = Vec::new();
        for var in required_vars {
            if std::env::var(var).is_err() {
                missing_vars.push(var.to_string());
            }
        }

        if !missing_vars.is_empty() {
            return Err(CicdIntegrationError::EnvironmentValidationError(format!(
                "Missing environment variables: {}",
                missing_vars.join(", ")
            )));
        }

        // Validate workspace structure
        let workspace = std::env::var("GITHUB_WORKSPACE").unwrap_or_default();
        let workspace_path = PathBuf::from(workspace);

        if !workspace_path.exists() {
            return Err(CicdIntegrationError::WorkspaceValidationError(
                "GitHub workspace directory does not exist".to_string(),
            ));
        }

        // Validate copybook-bench exists
        let copybook_bench_path = workspace_path.join("copybook-bench");
        if !copybook_bench_path.exists() {
            return Err(CicdIntegrationError::WorkspaceValidationError(
                "copybook-bench directory not found in workspace".to_string(),
            ));
        }

        Ok(EnvironmentValidationResult {
            workspace_path,
            required_vars_present: true,
            copybook_bench_available: true,
            system_resources: SystemResources {
                cpu_cores: num_cpus::get() as u32,
                memory_gb: 16,      // Mock value for testing
                disk_space_gb: 100, // Mock value for testing
            },
        })
    }

    fn execute_benchmarks(&self) -> Result<BenchmarkExecutionResult, CicdIntegrationError> {
        // Mock benchmark execution for test scaffolding
        let metrics = vec![
            PerformanceMetric {
                metric_name: "decode_display_heavy".to_string(),
                throughput_gibs: 4.22,
                throughput_mibs: 0.0, // Not applicable for DISPLAY-heavy benchmark
                sample_count: 100,
                confidence_interval: (4.20, 4.24),
            },
            PerformanceMetric {
                metric_name: "decode_comp3_heavy".to_string(),
                throughput_gibs: 0.0, // Not applicable for COMP-3-heavy benchmark
                throughput_mibs: 571.0,
                sample_count: 100,
                confidence_interval: (568.0, 574.0),
            },
        ];

        Ok(BenchmarkExecutionResult {
            metrics,
            execution_time: Duration::from_secs(120),
            exit_code: 0,
            stdout_log: "Benchmark execution completed successfully".to_string(),
            stderr_log: String::new(),
        })
    }

    fn generate_json_report(
        &self,
        benchmark_result: &BenchmarkExecutionResult,
    ) -> Result<String, CicdIntegrationError> {
        let display_metric = benchmark_result
            .metrics
            .iter()
            .find(|m| m.metric_name.contains("display"))
            .map(|m| m.throughput_gibs)
            .unwrap_or(0.0);

        let comp3_metric = benchmark_result
            .metrics
            .iter()
            .find(|m| m.metric_name.contains("comp3"))
            .map(|m| m.throughput_mibs)
            .unwrap_or(0.0);

        let report = serde_json::json!({
            "display_gibs": display_metric,
            "comp3_mibs": comp3_metric,
            "warnings": [],
            "errors": [],
            "_metadata": {
                "workflow_id": self.workflow_context.workflow_id,
                "run_id": self.workflow_context.run_id,
                "sha": self.workflow_context.sha,
                "timestamp": SystemTime::now().duration_since(SystemTime::UNIX_EPOCH)
                    .unwrap_or_default().as_secs()
            }
        });

        serde_json::to_string_pretty(&report)
            .map_err(|e| CicdIntegrationError::JsonGenerationError(e.to_string()))
    }

    fn post_pr_comment(
        &self,
        benchmark_result: &BenchmarkExecutionResult,
    ) -> Result<PrCommentResult, CicdIntegrationError> {
        // Extract PR number from context
        let pr_number = self.extract_pr_number()?;

        let display_gibs = benchmark_result
            .metrics
            .iter()
            .find(|m| m.metric_name.contains("display"))
            .map(|m| m.throughput_gibs)
            .unwrap_or(0.0);

        let comp3_mibs = benchmark_result
            .metrics
            .iter()
            .find(|m| m.metric_name.contains("comp3"))
            .map(|m| m.throughput_mibs)
            .unwrap_or(0.0);

        let comment_body = format!(
            "## 🚀 Performance Report\n\nDISPLAY: {:.2} GiB/s, COMP-3: {:.0} MiB/s [✅ PASSED]\n\n_Generated by workflow run: {}_",
            display_gibs, comp3_mibs, self.workflow_context.run_id
        );

        Ok(PrCommentResult {
            pr_number,
            comment_body,
            comment_id: "comment_123456".to_string(),
            posted_at: SystemTime::now(),
        })
    }

    fn promote_baseline(
        &self,
        benchmark_result: &BenchmarkExecutionResult,
    ) -> Result<BaselinePromotionResult, CicdIntegrationError> {
        let baseline_id = format!("main-{}", &self.workflow_context.sha[..8]);

        Ok(BaselinePromotionResult {
            baseline_id,
            promoted_at: SystemTime::now(),
            git_commit: self.workflow_context.sha.clone(),
            performance_metrics: benchmark_result.metrics.clone(),
        })
    }

    fn generate_audit_report(
        &self,
        _benchmark_result: &BenchmarkExecutionResult,
    ) -> Result<AuditReportResult, CicdIntegrationError> {
        Ok(AuditReportResult {
            audit_id: {
                let run_id = self.workflow_context.run_id.clone();
                format!("AUDIT-{run_id}")
            },
            generated_at: SystemTime::now(),
            report_size_bytes: 50000, // Mock size
            compliance_score: 0.95,
        })
    }

    fn extract_pr_number(&self) -> Result<u32, CicdIntegrationError> {
        // Mock PR number extraction for test scaffolding
        if self.workflow_context.event_name == "pull_request" {
            Ok(123) // Mock PR number
        } else {
            Err(CicdIntegrationError::PrNumberExtractionError(
                "Not a pull request event".to_string(),
            ))
        }
    }

    fn collect_artifacts(&self) -> Result<Vec<Artifact>, CicdIntegrationError> {
        let mut artifacts = Vec::new();

        if self.reporting_config.json_output_enabled {
            artifacts.push(Artifact {
                name: "performance-report.json".to_string(),
                path: self.reporting_config.output_directory.join("perf.json"),
                size_bytes: 2048,
                artifact_type: ArtifactType::JsonReport,
            });
        }

        if self.reporting_config.audit_reports_enabled {
            artifacts.push(Artifact {
                name: "audit-report.html".to_string(),
                path: self.reporting_config.output_directory.join("audit.html"),
                size_bytes: 50000,
                artifact_type: ArtifactType::AuditReport,
            });
        }

        Ok(artifacts)
    }
}

impl Default for BenchmarkConfiguration {
    fn default() -> Self {
        Self {
            perf_mode_enabled: true,
            timeout_seconds: 3600, // 1 hour
            parallel_jobs: 4,
            output_format: "json".to_string(),
            baseline_comparison: true,
        }
    }
}

impl Default for ReportingConfiguration {
    fn default() -> Self {
        Self {
            json_output_enabled: true,
            pr_comments_enabled: true,
            baseline_promotion_enabled: true,
            audit_reports_enabled: true,
            output_directory: PathBuf::from("scripts/bench"),
        }
    }
}

/// Supporting data structures for CI/CD integration
#[derive(Debug, Clone)]
pub struct EnvironmentValidationResult {
    pub workspace_path: PathBuf,
    pub required_vars_present: bool,
    pub copybook_bench_available: bool,
    pub system_resources: SystemResources,
}

#[derive(Debug, Clone)]
pub struct SystemResources {
    pub cpu_cores: u32,
    pub memory_gb: u32,
    pub disk_space_gb: u32,
}

#[derive(Debug, Clone)]
pub struct BenchmarkExecutionResult {
    pub metrics: Vec<PerformanceMetric>,
    pub execution_time: Duration,
    pub exit_code: i32,
    pub stdout_log: String,
    pub stderr_log: String,
}

#[derive(Debug, Clone)]
pub struct PerformanceMetric {
    pub metric_name: String,
    pub throughput_gibs: f64,
    pub throughput_mibs: f64,
    pub sample_count: usize,
    pub confidence_interval: (f64, f64),
}

#[derive(Debug, Clone)]
pub struct PipelineExecutionResult {
    pub workflow_context: GitHubWorkflowContext,
    pub execution_steps: Vec<PipelineStep>,
    pub total_duration: Duration,
    pub success: bool,
    pub artifacts: Vec<Artifact>,
}

#[derive(Debug, Clone)]
pub struct PipelineStep {
    pub step_name: String,
    pub status: StepStatus,
    pub duration: Duration,
    pub output: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StepStatus {
    Pending,
    Running,
    Completed,
    Failed,
    Skipped,
}

#[derive(Debug, Clone)]
pub struct PrCommentResult {
    pub pr_number: u32,
    pub comment_body: String,
    pub comment_id: String,
    pub posted_at: SystemTime,
}

#[derive(Debug, Clone)]
pub struct BaselinePromotionResult {
    pub baseline_id: String,
    pub promoted_at: SystemTime,
    pub git_commit: String,
    pub performance_metrics: Vec<PerformanceMetric>,
}

#[derive(Debug, Clone)]
pub struct AuditReportResult {
    pub audit_id: String,
    pub generated_at: SystemTime,
    pub report_size_bytes: u64,
    pub compliance_score: f64,
}

#[derive(Debug, Clone)]
pub struct Artifact {
    pub name: String,
    pub path: PathBuf,
    pub size_bytes: u64,
    pub artifact_type: ArtifactType,
}

#[derive(Debug, Clone)]
pub enum ArtifactType {
    JsonReport,
    AuditReport,
    BenchmarkLogs,
    ComplianceReport,
}

/// Error types for CI/CD integration
#[derive(Debug)]
pub enum CicdIntegrationError {
    EnvironmentVariableError(String),
    EnvironmentValidationError(String),
    WorkspaceValidationError(String),
    BenchmarkExecutionError(String),
    JsonGenerationError(String),
    PrCommentPostingError(String),
    BaselinePromotionError(String),
    AuditReportGenerationError(String),
    PrNumberExtractionError(String),
    ArtifactCollectionError(String),
}

impl std::fmt::Display for CicdIntegrationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CicdIntegrationError::EnvironmentVariableError(msg) => {
                write!(f, "Environment variable error: {msg}")
            }
            CicdIntegrationError::EnvironmentValidationError(msg) => {
                write!(f, "Environment validation error: {msg}")
            }
            CicdIntegrationError::WorkspaceValidationError(msg) => {
                write!(f, "Workspace validation error: {msg}")
            }
            CicdIntegrationError::BenchmarkExecutionError(msg) => {
                write!(f, "Benchmark execution error: {msg}")
            }
            CicdIntegrationError::JsonGenerationError(msg) => {
                write!(f, "JSON generation error: {msg}")
            }
            CicdIntegrationError::PrCommentPostingError(msg) => {
                write!(f, "PR comment posting error: {msg}")
            }
            CicdIntegrationError::BaselinePromotionError(msg) => {
                write!(f, "Baseline promotion error: {msg}")
            }
            CicdIntegrationError::AuditReportGenerationError(msg) => {
                write!(f, "Audit report generation error: {msg}")
            }
            CicdIntegrationError::PrNumberExtractionError(msg) => {
                write!(f, "PR number extraction error: {msg}")
            }
            CicdIntegrationError::ArtifactCollectionError(msg) => {
                write!(f, "Artifact collection error: {msg}")
            }
        }
    }
}

impl std::error::Error for CicdIntegrationError {}

/// Tests feature spec: issue-52-spec.md#AC9-github-workflow-context
/// Validates GitHub workflow context extraction and validation
#[test]
fn test_github_workflow_context_extraction() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC9 - Verify GitHub workflow context extraction

    // Test mock pull request context
    let pr_context = GitHubWorkflowContext::mock_pull_request();
    assert_eq!(pr_context.workflow_id, "benchmark.yml");
    assert_eq!(pr_context.event_name, "pull_request");
    assert_eq!(pr_context.ref_name, "feature/benchmark-reporting");
    assert!(!pr_context.sha.is_empty());
    assert!(!pr_context.repository.is_empty());

    // Test mock push to main context
    let main_context = GitHubWorkflowContext::mock_push_to_main();
    assert_eq!(main_context.event_name, "push");
    assert_eq!(main_context.ref_name, "main");
    assert_ne!(main_context.sha, pr_context.sha);

    // Test environment variable extraction (will fail without actual environment)
    // This tests the error handling path
    let env_result = GitHubWorkflowContext::from_environment();
    assert!(
        env_result.is_err(),
        "Should fail without proper GitHub Actions environment"
    );

    // Verify error handling
    match env_result {
        Err(CicdIntegrationError::EnvironmentVariableError(msg)) => {
            assert!(msg.contains("GITHUB_WORKFLOW"));
        }
        _ => panic!("Expected EnvironmentVariableError"),
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC9-pipeline-execution-pr
/// Validates pipeline execution for pull request events
#[test]
fn test_pipeline_execution_pull_request() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC9 - Verify pipeline execution for pull request events

    // Use mock context without modifying global environment
    let pr_context = GitHubWorkflowContext::mock_pull_request();
    let integrator = CicdPipelineIntegrator::new(pr_context);

    // Execute pipeline
    let result = integrator.execute_benchmark_pipeline();

    // Note: This will fail because we don't have the actual workspace structure
    // But it tests the error handling path
    assert!(
        result.is_err(),
        "Should fail without proper workspace setup"
    );

    // Verify error type
    match result {
        Err(CicdIntegrationError::WorkspaceValidationError(_)) => {
            // Expected error type for missing workspace
        }
        Err(CicdIntegrationError::EnvironmentValidationError(_)) => {
            // Also acceptable for environment validation failure
        }
        _ => panic!("Expected workspace or environment validation error"),
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC9-pipeline-execution-main
/// Validates pipeline execution for push to main events
#[test]
fn test_pipeline_execution_push_to_main() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC9 - Verify pipeline execution for push to main events

    let main_context = GitHubWorkflowContext::mock_push_to_main();
    let integrator = CicdPipelineIntegrator::new(main_context);

    // Verify workflow context is configured for main push
    assert_eq!(integrator.workflow_context.event_name, "push");
    assert_eq!(integrator.workflow_context.ref_name, "main");

    // Verify configurations are appropriate for main branch
    assert!(
        integrator.benchmark_config.perf_mode_enabled,
        "PERF mode should be enabled for main branch benchmarks"
    );
    assert!(
        integrator.reporting_config.baseline_promotion_enabled,
        "Baseline promotion should be enabled for main branch"
    );
    assert!(
        integrator.reporting_config.audit_reports_enabled,
        "Audit reports should be enabled for main branch"
    );

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC9-environment-validation
/// Validates CI/CD environment validation
#[test]
fn test_cicd_environment_validation() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC9 - Verify CI/CD environment validation

    let context = GitHubWorkflowContext::mock_pull_request();
    let _integrator = CicdPipelineIntegrator::new(context);

    // Test environment validation with mock setup
    // Note: Using mock context which should handle environment validation internally
    // without needing to modify global environment variables

    // Test initial validation (may pass or fail depending on actual environment)
    let validation_result = CicdPipelineIntegrator::validate_environment();

    // The validation logic should be tested through mock contexts
    // rather than manipulating global environment state

    // Will still fail due to workspace not existing, but tests environment variable validation
    match validation_result {
        Err(CicdIntegrationError::WorkspaceValidationError(_)) => {
            // Expected - workspace doesn't exist
        }
        Err(CicdIntegrationError::EnvironmentValidationError(_)) => {
            // Also acceptable if other environment issues
        }
        Ok(_) => panic!("Should not succeed without proper workspace"),
        Err(e) => panic!("Unexpected error type: {e:?}"),
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC9-benchmark-execution-integration
/// Validates benchmark execution integration with CI/CD pipeline
#[test]
fn test_benchmark_execution_integration() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC9 - Verify benchmark execution integration

    let context = GitHubWorkflowContext::mock_pull_request();
    let integrator = CicdPipelineIntegrator::new(context);

    // Test benchmark execution (mocked)
    let benchmark_result = integrator.execute_benchmarks()?;

    // Verify benchmark results structure
    assert!(
        !benchmark_result.metrics.is_empty(),
        "Should have performance metrics"
    );
    assert_eq!(
        benchmark_result.exit_code, 0,
        "Should have successful exit code"
    );
    assert!(
        benchmark_result.execution_time > Duration::from_secs(0),
        "Should have execution time"
    );

    // Verify metrics contain expected benchmarks
    let display_metric = benchmark_result
        .metrics
        .iter()
        .find(|m| m.metric_name.contains("display"));
    assert!(
        display_metric.is_some(),
        "Should have DISPLAY benchmark metric"
    );

    let comp3_metric = benchmark_result
        .metrics
        .iter()
        .find(|m| m.metric_name.contains("comp3"));
    assert!(
        comp3_metric.is_some(),
        "Should have COMP-3 benchmark metric"
    );

    // Verify performance values are reasonable
    let display = display_metric.unwrap();
    assert!(
        display.throughput_gibs > 4.0,
        "DISPLAY throughput should exceed 4 GiB/s"
    );
    assert!(display.sample_count > 50, "Should have sufficient samples");

    let comp3 = comp3_metric.unwrap();
    assert!(
        comp3.throughput_mibs > 500.0,
        "COMP-3 throughput should exceed 500 MiB/s"
    );

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC9-json-report-generation-integration
/// Validates JSON report generation in CI/CD context
#[test]
fn test_json_report_generation_integration() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC9 - Verify JSON report generation integration

    let context = GitHubWorkflowContext::mock_pull_request();
    let integrator = CicdPipelineIntegrator::new(context);

    // Mock benchmark result
    let benchmark_result = integrator.execute_benchmarks()?;

    // Generate JSON report
    let json_report_string = integrator.generate_json_report(&benchmark_result)?;

    // Verify JSON structure
    let json_report: serde_json::Value = serde_json::from_str(&json_report_string)?;
    assert!(json_report.is_object(), "Report should be JSON object");

    let obj = json_report.as_object().unwrap();
    assert!(
        obj.contains_key("display_gibs"),
        "Should contain display_gibs"
    );
    assert!(obj.contains_key("comp3_mibs"), "Should contain comp3_mibs");
    assert!(
        obj.contains_key("warnings"),
        "Should contain warnings array"
    );
    assert!(obj.contains_key("errors"), "Should contain errors array");

    // Verify metadata includes workflow context
    assert!(obj.contains_key("_metadata"), "Should contain metadata");
    let metadata = obj["_metadata"].as_object().unwrap();
    assert!(
        metadata.contains_key("workflow_id"),
        "Metadata should include workflow_id"
    );
    assert!(
        metadata.contains_key("run_id"),
        "Metadata should include run_id"
    );
    assert!(
        metadata.contains_key("sha"),
        "Metadata should include git SHA"
    );

    // Verify performance values
    let display_gibs = obj["display_gibs"].as_f64().unwrap();
    let comp3_mibs = obj["comp3_mibs"].as_f64().unwrap();

    assert!(display_gibs > 4.0, "DISPLAY should exceed 4 GiB/s");
    assert!(comp3_mibs > 500.0, "COMP-3 should exceed 500 MiB/s");

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC9-pr-comment-integration
/// Validates PR comment posting integration
#[test]
fn test_pr_comment_posting_integration() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC9 - Verify PR comment posting integration

    let pr_context = GitHubWorkflowContext::mock_pull_request();
    let integrator = CicdPipelineIntegrator::new(pr_context);

    // Mock benchmark result
    let benchmark_result = integrator.execute_benchmarks()?;

    // Test PR comment generation
    let pr_comment_result = integrator.post_pr_comment(&benchmark_result)?;

    // Verify PR comment structure
    assert!(
        pr_comment_result.pr_number > 0,
        "Should have valid PR number"
    );
    assert!(
        !pr_comment_result.comment_body.is_empty(),
        "Should have comment body"
    );
    assert!(
        !pr_comment_result.comment_id.is_empty(),
        "Should have comment ID"
    );

    // Verify comment content
    let comment_body = &pr_comment_result.comment_body;
    assert!(
        comment_body.contains("Performance Report"),
        "Should have performance report header"
    );
    assert!(
        comment_body.contains("DISPLAY:"),
        "Should show DISPLAY metrics"
    );
    assert!(
        comment_body.contains("COMP-3:"),
        "Should show COMP-3 metrics"
    );
    assert!(comment_body.contains("GiB/s"), "Should have GiB/s units");
    assert!(comment_body.contains("MiB/s"), "Should have MiB/s units");
    assert!(comment_body.contains("PASSED"), "Should show PASSED status");

    // Verify workflow context is included
    assert!(
        comment_body.contains(&integrator.workflow_context.run_id),
        "Should include workflow run ID for traceability"
    );

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC9-baseline-promotion-integration
/// Validates baseline promotion integration for main branch
#[test]
fn test_baseline_promotion_integration() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC9 - Verify baseline promotion integration

    let main_context = GitHubWorkflowContext::mock_push_to_main();
    let integrator = CicdPipelineIntegrator::new(main_context);

    // Mock benchmark result
    let benchmark_result = integrator.execute_benchmarks()?;

    // Test baseline promotion
    let promotion_result = integrator.promote_baseline(&benchmark_result)?;

    // Verify baseline promotion structure
    assert!(
        !promotion_result.baseline_id.is_empty(),
        "Should have baseline ID"
    );
    assert!(
        promotion_result.baseline_id.starts_with("main-"),
        "Should have main- prefix"
    );
    assert!(
        promotion_result
            .baseline_id
            .contains(&integrator.workflow_context.sha[..8]),
        "Should include commit SHA prefix"
    );

    // Verify promotion metadata
    assert_eq!(
        promotion_result.git_commit, integrator.workflow_context.sha,
        "Should record correct git commit"
    );
    assert!(
        !promotion_result.performance_metrics.is_empty(),
        "Should include performance metrics"
    );

    // Verify timestamp is recent
    let now = SystemTime::now();
    let promotion_age = now
        .duration_since(promotion_result.promoted_at)
        .unwrap_or_default();
    assert!(
        promotion_age < Duration::from_secs(10),
        "Promotion timestamp should be recent"
    );

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC9-artifact-collection
/// Validates artifact collection for CI/CD pipeline
#[test]
fn test_artifact_collection() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC9 - Verify artifact collection

    let context = GitHubWorkflowContext::mock_pull_request();
    let integrator = CicdPipelineIntegrator::new(context);

    // Collect artifacts
    let artifacts = integrator.collect_artifacts()?;

    // Verify artifacts are collected
    assert!(!artifacts.is_empty(), "Should have artifacts to collect");

    // Verify JSON report artifact
    let json_artifact = artifacts
        .iter()
        .find(|a| matches!(a.artifact_type, ArtifactType::JsonReport));
    assert!(json_artifact.is_some(), "Should have JSON report artifact");

    let json_artifact = json_artifact.unwrap();
    assert_eq!(json_artifact.name, "performance-report.json");
    assert!(json_artifact.size_bytes > 0, "Artifact should have size");
    assert!(json_artifact.path.to_string_lossy().contains("perf.json"));

    // Verify audit report artifact
    let audit_artifact = artifacts
        .iter()
        .find(|a| matches!(a.artifact_type, ArtifactType::AuditReport));
    assert!(
        audit_artifact.is_some(),
        "Should have audit report artifact"
    );

    let audit_artifact = audit_artifact.unwrap();
    assert_eq!(audit_artifact.name, "audit-report.html");
    assert!(
        audit_artifact.size_bytes > 0,
        "Audit artifact should have size"
    );

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC9-workflow-step-orchestration
/// Validates proper orchestration of workflow steps
#[test]
fn test_workflow_step_orchestration() {
    // AC:AC9 - Verify workflow step orchestration

    // Test orchestration for different event types
    let test_scenarios = vec![
        ("pull_request", GitHubWorkflowContext::mock_pull_request()),
        ("push_to_main", GitHubWorkflowContext::mock_push_to_main()),
    ];

    for (scenario_name, context) in test_scenarios {
        let integrator = CicdPipelineIntegrator::new(context.clone());

        // Note: Full pipeline execution will fail without proper environment
        // But we can test the orchestration logic

        // Verify step configuration based on event type
        if context.event_name == "pull_request" {
            assert!(
                integrator.reporting_config.pr_comments_enabled,
                "PR comments should be enabled for pull_request events in {scenario_name}"
            );
        }

        if context.event_name == "push" && context.ref_name == "main" {
            assert!(
                integrator.reporting_config.baseline_promotion_enabled,
                "Baseline promotion should be enabled for push to main in {scenario_name}"
            );
        }

        // Verify configuration consistency
        assert!(
            integrator.benchmark_config.perf_mode_enabled,
            "PERF mode should be enabled for {scenario_name} scenario"
        );
        assert!(
            integrator.reporting_config.json_output_enabled,
            "JSON output should be enabled for {scenario_name} scenario"
        );
    }
}
