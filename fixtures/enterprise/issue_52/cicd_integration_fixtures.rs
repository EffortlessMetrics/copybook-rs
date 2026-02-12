//! CI/CD Integration Test Fixtures for Issue #52 Machine-Readable Benchmark Reporting
//!
//! Provides test data for GitHub Actions workflows, PR automation, and CI/CD pipeline integration
//! Supports validation of Issue #52 performance monitoring and baseline promotion workflows

use serde_json::{Value, Map};
use std::collections::HashMap;

/// GitHub Actions workflow execution context
#[derive(Debug, Clone)]
pub struct GitHubWorkflowContext {
    pub workflow_name: String,
    pub run_id: u64,
    pub run_number: u32,
    pub job_name: String,
    pub step_name: String,
    pub event_name: String,
    pub ref_name: String,
    pub sha: String,
    pub actor: String,
    pub repository: String,
    pub metadata: HashMap<String, Value>,
}

impl GitHubWorkflowContext {
    pub fn new(workflow_name: &str, event_name: &str) -> Self {
        Self {
            workflow_name: workflow_name.to_string(),
            run_id: 1234567890,
            run_number: 42,
            job_name: "performance-validation".to_string(),
            step_name: "benchmark-execution".to_string(),
            event_name: event_name.to_string(),
            ref_name: "refs/heads/main".to_string(),
            sha: "abc123def456ghi789".to_string(),
            actor: "performance-bot".to_string(),
            repository: "user/copybook-rs".to_string(),
            metadata: HashMap::new(),
        }
    }

    pub fn with_metadata(mut self, key: &str, value: Value) -> Self {
        self.metadata.insert(key.to_string(), value);
        self
    }

    pub fn to_json(&self) -> serde_json::Result<Value> {
        let mut map = Map::new();
        map.insert("workflow_name".to_string(), Value::String(self.workflow_name.clone()));
        map.insert("run_id".to_string(), Value::Number(serde_json::Number::from(self.run_id)));
        map.insert("run_number".to_string(), Value::Number(serde_json::Number::from(self.run_number)));
        map.insert("job_name".to_string(), Value::String(self.job_name.clone()));
        map.insert("step_name".to_string(), Value::String(self.step_name.clone()));
        map.insert("event_name".to_string(), Value::String(self.event_name.clone()));
        map.insert("ref_name".to_string(), Value::String(self.ref_name.clone()));
        map.insert("sha".to_string(), Value::String(self.sha.clone()));
        map.insert("actor".to_string(), Value::String(self.actor.clone()));
        map.insert("repository".to_string(), Value::String(self.repository.clone()));

        for (key, value) in &self.metadata {
            map.insert(key.clone(), value.clone());
        }

        Ok(Value::Object(map))
    }
}

/// GitHub Actions workflow execution result
#[derive(Debug, Clone)]
pub struct WorkflowExecutionResult {
    pub status: String,
    pub conclusion: String,
    pub start_time: String,
    pub end_time: String,
    pub duration_seconds: u32,
    pub context: GitHubWorkflowContext,
    pub performance_data: HashMap<String, Value>,
    pub artifacts: Vec<String>,
    pub errors: Vec<String>,
}

impl WorkflowExecutionResult {
    pub fn success(context: GitHubWorkflowContext) -> Self {
        Self {
            status: "completed".to_string(),
            conclusion: "success".to_string(),
            start_time: "2024-09-25T10:00:00Z".to_string(),
            end_time: "2024-09-25T10:15:30Z".to_string(),
            duration_seconds: 930,
            context,
            performance_data: HashMap::new(),
            artifacts: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn failure(context: GitHubWorkflowContext, error: &str) -> Self {
        Self {
            status: "completed".to_string(),
            conclusion: "failure".to_string(),
            start_time: "2024-09-25T10:00:00Z".to_string(),
            end_time: "2024-09-25T10:30:00Z".to_string(),
            duration_seconds: 1800,
            context,
            performance_data: HashMap::new(),
            artifacts: Vec::new(),
            errors: vec![error.to_string()],
        }
    }

    pub fn with_performance_data(mut self, key: &str, value: Value) -> Self {
        self.performance_data.insert(key.to_string(), value);
        self
    }

    pub fn with_artifact(mut self, artifact: &str) -> Self {
        self.artifacts.push(artifact.to_string());
        self
    }

    pub fn to_json(&self) -> serde_json::Result<Value> {
        serde_json::to_value(self)
    }
}

/// Create performance validation workflow fixtures
pub fn create_performance_validation_workflows() -> HashMap<String, WorkflowExecutionResult> {
    let mut workflows = HashMap::new();

    // Successful performance validation on main branch
    let main_context = GitHubWorkflowContext::new("Performance Validation", "push")
        .with_metadata("branch", Value::String("main".to_string()))
        .with_metadata("is_main_branch", Value::Bool(true))
        .with_metadata("baseline_check_required", Value::Bool(true));

    let main_success = WorkflowExecutionResult::success(main_context)
        .with_performance_data("display_gibs", Value::Number(serde_json::Number::from_f64(4.22).unwrap()))
        .with_performance_data("comp3_mibs", Value::Number(serde_json::Number::from_f64(571.0).unwrap()))
        .with_performance_data("enterprise_compliant", Value::Bool(true))
        .with_performance_data("baseline_promotion_eligible", Value::Bool(true))
        .with_artifact("scripts/bench/perf.json")
        .with_artifact("reports/performance/performance_report.html")
        .with_artifact("reports/audit/audit_report.json");

    workflows.insert("main_branch_success".to_string(), main_success);

    // Pull request performance validation
    let pr_context = GitHubWorkflowContext::new("Performance Validation", "pull_request")
        .with_metadata("branch", Value::String("feature/performance-improvement".to_string()))
        .with_metadata("is_main_branch", Value::Bool(false))
        .with_metadata("pr_number", Value::Number(serde_json::Number::from(123)))
        .with_metadata("baseline_check_required", Value::Bool(true))
        .with_metadata("pr_comment_required", Value::Bool(true));

    let pr_success = WorkflowExecutionResult::success(pr_context)
        .with_performance_data("display_gibs", Value::Number(serde_json::Number::from_f64(4.28).unwrap()))
        .with_performance_data("comp3_mibs", Value::Number(serde_json::Number::from_f64(578.0).unwrap()))
        .with_performance_data("baseline_comparison", Value::String("improvement".to_string()))
        .with_performance_data("pr_comment_posted", Value::Bool(true))
        .with_artifact("scripts/bench/perf.json")
        .with_artifact("reports/performance/pr_performance_comparison.html");

    workflows.insert("pr_success".to_string(), pr_success);

    // Performance regression detected
    let regression_context = GitHubWorkflowContext::new("Performance Validation", "pull_request")
        .with_metadata("branch", Value::String("feature/refactoring".to_string()))
        .with_metadata("pr_number", Value::Number(serde_json::Number::from(124)))
        .with_metadata("regression_detected", Value::Bool(true));

    let regression_result = WorkflowExecutionResult::success(regression_context)
        .with_performance_data("display_gibs", Value::Number(serde_json::Number::from_f64(3.45).unwrap()))
        .with_performance_data("comp3_mibs", Value::Number(serde_json::Number::from_f64(485.0).unwrap()))
        .with_performance_data("baseline_comparison", Value::String("regression".to_string()))
        .with_performance_data("regression_magnitude_percent", Value::Number(serde_json::Number::from_f64(18.2).unwrap()))
        .with_performance_data("pr_labels_added", Value::Array(vec![
            Value::String("performance-regression".to_string()),
            Value::String("needs-investigation".to_string())
        ]))
        .with_artifact("scripts/bench/perf.json")
        .with_artifact("reports/performance/regression_analysis.html");

    workflows.insert("regression_detected".to_string(), regression_result);

    // Benchmark execution timeout
    let timeout_context = GitHubWorkflowContext::new("Performance Validation", "push")
        .with_metadata("branch", Value::String("main".to_string()))
        .with_metadata("timeout_occurred", Value::Bool(true));

    let timeout_failure = WorkflowExecutionResult::failure(
        timeout_context,
        "Benchmark execution timeout exceeded 30 minutes"
    );

    workflows.insert("benchmark_timeout".to_string(), timeout_failure);

    workflows
}

/// Create baseline promotion workflow fixtures
pub fn create_baseline_promotion_workflows() -> HashMap<String, WorkflowExecutionResult> {
    let mut workflows = HashMap::new();

    // Successful baseline promotion
    let promotion_context = GitHubWorkflowContext::new("Baseline Promotion", "workflow_dispatch")
        .with_metadata("triggered_by", Value::String("performance-team".to_string()))
        .with_metadata("promotion_type", Value::String("manual".to_string()))
        .with_metadata("new_baseline_version", Value::String("v0.3.2".to_string()));

    let promotion_success = WorkflowExecutionResult::success(promotion_context)
        .with_performance_data("new_baseline_display", Value::Number(serde_json::Number::from_f64(4.22).unwrap()))
        .with_performance_data("new_baseline_comp3", Value::Number(serde_json::Number::from_f64(571.0).unwrap()))
        .with_performance_data("previous_baseline_display", Value::Number(serde_json::Number::from_f64(4.18).unwrap()))
        .with_performance_data("previous_baseline_comp3", Value::Number(serde_json::Number::from_f64(568.0).unwrap()))
        .with_performance_data("improvement_display_percent", Value::Number(serde_json::Number::from_f64(0.96).unwrap()))
        .with_performance_data("improvement_comp3_percent", Value::Number(serde_json::Number::from_f64(0.53).unwrap()))
        .with_artifact("baselines/performance_baseline_v0.3.2.json")
        .with_artifact("baselines/archive/performance_baseline_v0.3.1_archived.json")
        .with_artifact("reports/baseline/promotion_report.html");

    workflows.insert("promotion_success".to_string(), promotion_success);

    // Automatic baseline promotion (scheduled)
    let scheduled_context = GitHubWorkflowContext::new("Baseline Promotion", "schedule")
        .with_metadata("cron_schedule", Value::String("0 6 * * 1".to_string())) // Weekly Monday 6 AM
        .with_metadata("promotion_type", Value::String("automatic".to_string()))
        .with_metadata("stability_validated", Value::Bool(true));

    let scheduled_success = WorkflowExecutionResult::success(scheduled_context)
        .with_performance_data("weekly_average_display", Value::Number(serde_json::Number::from_f64(4.19).unwrap()))
        .with_performance_data("weekly_average_comp3", Value::Number(serde_json::Number::from_f64(572.5).unwrap()))
        .with_performance_data("stability_score", Value::Number(serde_json::Number::from_f64(98.7).unwrap()))
        .with_performance_data("variance_within_tolerance", Value::Bool(true))
        .with_artifact("baselines/performance_baseline_weekly.json")
        .with_artifact("reports/baseline/weekly_stability_report.html");

    workflows.insert("scheduled_promotion".to_string(), scheduled_success);

    // Baseline promotion rejected
    let rejection_context = GitHubWorkflowContext::new("Baseline Promotion", "workflow_dispatch")
        .with_metadata("promotion_rejected", Value::Bool(true))
        .with_metadata("rejection_reason", Value::String("insufficient_improvement".to_string()));

    let rejection_result = WorkflowExecutionResult::success(rejection_context)
        .with_performance_data("current_display", Value::Number(serde_json::Number::from_f64(4.15).unwrap()))
        .with_performance_data("current_comp3", Value::Number(serde_json::Number::from_f64(565.0).unwrap()))
        .with_performance_data("baseline_display", Value::Number(serde_json::Number::from_f64(4.22).unwrap()))
        .with_performance_data("baseline_comp3", Value::Number(serde_json::Number::from_f64(571.0).unwrap()))
        .with_performance_data("improvement_required_percent", Value::Number(serde_json::Number::from_f64(1.0).unwrap()))
        .with_artifact("reports/baseline/promotion_rejection_report.html");

    workflows.insert("promotion_rejected".to_string(), rejection_result);

    workflows
}

/// Create GitHub PR comment automation fixtures
pub fn create_pr_comment_fixtures() -> HashMap<String, Value> {
    let mut comments = HashMap::new();

    // Successful performance PR comment
    comments.insert("performance_success_comment".to_string(), serde_json::json!({
        "comment_type": "performance_validation",
        "pr_number": 123,
        "status": "success",
        "markdown_content": "## 🚀 Performance Validation Results\n\n### Performance Metrics\n- **DISPLAY Throughput**: 4.22 GiB/s (52x enterprise floor)\n- **COMP-3 Throughput**: 571 MiB/s (14x enterprise floor)\n- **Enterprise Compliance**: ✅ Fully compliant\n\n### Safety Margins\n- DISPLAY: 52x above 80 MB/s regulatory floor\n- COMP-3: 14x above 40 MB/s regulatory floor\n\n### Recommendation\n✅ **Ready for merge** - Excellent performance maintains enterprise standards\n\n---\n*Generated by copybook-rs Issue #52 benchmark reporting*",
        "performance_data": {
            "display_gibs": 4.22,
            "comp3_mibs": 571.0,
            "enterprise_compliant": true,
            "safety_margin_display": 52,
            "safety_margin_comp3": 14
        },
        "github_metadata": {
            "comment_id": 987654321,
            "comment_url": "https://github.com/user/copybook-rs/pull/123#issuecomment-987654321",
            "posted_at": "2024-09-25T10:30:00Z"
        }
    }));

    // Performance regression PR comment
    comments.insert("performance_regression_comment".to_string(), serde_json::json!({
        "comment_type": "performance_regression",
        "pr_number": 124,
        "status": "regression",
        "markdown_content": "## ⚠️ Performance Regression Detected\n\n### Current Performance\n- **DISPLAY Throughput**: 3.45 GiB/s (18.2% below baseline)\n- **COMP-3 Throughput**: 485 MiB/s (15.1% below baseline)\n- **Enterprise Compliance**: ✅ Still above floors\n\n### Regression Analysis\n- Baseline DISPLAY: 4.22 GiB/s → Current: 3.45 GiB/s\n- Baseline COMP-3: 571 MiB/s → Current: 485 MiB/s\n- Regression exceeds 5% threshold\n\n### Recommendation\n❌ **Investigation required** - Performance regression needs analysis\n\n**Labels Added**: performance-regression, needs-investigation\n**Reviewers Requested**: @performance-team\n\n---\n*Generated by copybook-rs Issue #52 benchmark reporting*",
        "performance_data": {
            "display_gibs": 3.45,
            "comp3_mibs": 485.0,
            "regression_display_percent": 18.2,
            "regression_comp3_percent": 15.1,
            "threshold_exceeded": true
        },
        "github_actions": {
            "labels_added": ["performance-regression", "needs-investigation"],
            "reviewers_requested": ["performance-team"],
            "api_calls_made": 5
        }
    }));

    // Baseline promotion notification
    comments.insert("baseline_promotion_comment".to_string(), serde_json::json!({
        "comment_type": "baseline_promotion",
        "issue_number": 52,
        "status": "promoted",
        "markdown_content": "## 📈 Performance Baseline Promoted\n\n### New Baseline v0.3.2\n- **DISPLAY Baseline**: 4.18 GiB/s → 4.22 GiB/s (+0.96%)\n- **COMP-3 Baseline**: 568 MiB/s → 571 MiB/s (+0.53%)\n- **Stability Score**: 98.7%\n\n### Enterprise Impact\n- Safety margins maintained above regulatory floors\n- All compliance requirements continue to be exceeded\n- Production deployment approved\n\n### Promotion Details\n- **Commit**: abc123def456\n- **Version**: v0.3.2\n- **Validation**: ✅ Passed all stability checks\n\n---\n*Baseline promoted by copybook-rs Issue #52 automation*",
        "baseline_data": {
            "new_version": "v0.3.2",
            "display_improvement_percent": 0.96,
            "comp3_improvement_percent": 0.53,
            "stability_score": 98.7,
            "validation_passed": true
        }
    }));

    comments
}

/// Create CI/CD pipeline artifact fixtures
pub fn create_cicd_artifact_fixtures() -> HashMap<String, Value> {
    let mut artifacts = HashMap::new();

    // Performance report artifact
    artifacts.insert("performance_report_artifact".to_string(), serde_json::json!({
        "artifact_name": "performance-report",
        "artifact_type": "html_report",
        "file_path": "reports/performance/performance_report.html",
        "size_bytes": 87432,
        "retention_days": 30,
        "download_url": "https://github.com/user/copybook-rs/actions/runs/1234567890/artifacts/987654321",
        "upload_timestamp": "2024-09-25T10:30:00Z",
        "metadata": {
            "report_type": "comprehensive_performance",
            "display_gibs": 4.22,
            "comp3_mibs": 571.0,
            "enterprise_compliant": true,
            "charts_included": true,
            "historical_comparison": true
        }
    }));

    // JSON performance data artifact
    artifacts.insert("perf_json_artifact".to_string(), serde_json::json!({
        "artifact_name": "perf-json",
        "artifact_type": "machine_readable",
        "file_path": "scripts/bench/perf.json",
        "size_bytes": 2847,
        "retention_days": 90,
        "download_url": "https://github.com/user/copybook-rs/actions/runs/1234567890/artifacts/987654322",
        "upload_timestamp": "2024-09-25T10:30:00Z",
        "metadata": {
            "schema_version": "v1.0",
            "validation_passed": true,
            "machine_readable": true,
            "api_consumable": true
        }
    }));

    // Audit report artifact
    artifacts.insert("audit_report_artifact".to_string(), serde_json::json!({
        "artifact_name": "audit-report",
        "artifact_type": "compliance_report",
        "file_path": "reports/audit/audit_report.json",
        "size_bytes": 45231,
        "retention_days": 365,
        "download_url": "https://github.com/user/copybook-rs/actions/runs/1234567890/artifacts/987654323",
        "upload_timestamp": "2024-09-25T10:30:00Z",
        "metadata": {
            "compliance_profiles": ["SOX", "HIPAA", "PCI_DSS", "GDPR", "ISO_27001"],
            "audit_events": 156,
            "violations_detected": 0,
            "overall_compliance": "full"
        }
    }));

    // Baseline comparison artifact
    artifacts.insert("baseline_comparison_artifact".to_string(), serde_json::json!({
        "artifact_name": "baseline-comparison",
        "artifact_type": "analysis_report",
        "file_path": "reports/baseline/baseline_comparison.html",
        "size_bytes": 32156,
        "retention_days": 30,
        "download_url": "https://github.com/user/copybook-rs/actions/runs/1234567890/artifacts/987654324",
        "upload_timestamp": "2024-09-25T10:30:00Z",
        "metadata": {
            "comparison_type": "performance_regression_analysis",
            "baseline_version": "v0.3.1",
            "current_commit": "abc123def456",
            "recommendation": "investigation_required"
        }
    }));

    artifacts
}

/// Create GitHub Actions workflow environment fixtures
pub fn create_workflow_environment_fixtures() -> HashMap<String, Value> {
    let mut environments = HashMap::new();

    // Standard CI environment
    environments.insert("ci_environment".to_string(), serde_json::json!({
        "github_actions": true,
        "runner_os": "Linux",
        "runner_arch": "X64",
        "runner_name": "GitHub Actions 1",
        "runner_environment": "github-hosted",
        "workflow_ref": "user/copybook-rs/.github/workflows/performance.yml@refs/heads/main",
        "workflow_sha": "abc123def456ghi789",
        "job_status": "success",
        "step_summary_enabled": true,
        "artifact_retention_days": 30,
        "environment_variables": {
            "RUST_VERSION": "1.92.0",
            "CARGO_INCREMENTAL": "0",
            "RUSTFLAGS": "-D warnings",
            "PERF": "1"
        },
        "secrets_available": ["GITHUB_TOKEN", "PERFORMANCE_MONITORING_TOKEN"],
        "permissions": {
            "contents": "read",
            "issues": "write",
            "pull_requests": "write",
            "actions": "read"
        }
    }));

    // Performance monitoring environment
    environments.insert("performance_environment".to_string(), serde_json::json!({
        "dedicated_runner": true,
        "runner_specs": {
            "cpu_cores": 8,
            "memory_gb": 16,
            "storage_type": "ssd",
            "network_speed": "1gbps"
        },
        "performance_isolation": true,
        "benchmark_timeout_minutes": 30,
        "measurement_iterations": 5,
        "warmup_iterations": 2,
        "environment_consistency": {
            "temperature_controlled": true,
            "load_balanced": false,
            "interference_minimized": true
        },
        "monitoring_enabled": {
            "cpu_usage": true,
            "memory_usage": true,
            "disk_io": true,
            "network_io": true
        }
    }));

    environments
}

/// Create comprehensive CI/CD integration fixtures
pub fn create_comprehensive_cicd_fixtures() -> HashMap<String, HashMap<String, Value>> {
    let mut all_fixtures = HashMap::new();

    all_fixtures.insert("performance_workflows".to_string(),
        create_performance_validation_workflows().into_iter()
            .map(|(k, v)| (k, v.to_json().unwrap_or(Value::Null)))
            .collect()
    );

    all_fixtures.insert("baseline_workflows".to_string(),
        create_baseline_promotion_workflows().into_iter()
            .map(|(k, v)| (k, v.to_json().unwrap_or(Value::Null)))
            .collect()
    );

    all_fixtures.insert("pr_comments".to_string(), create_pr_comment_fixtures());
    all_fixtures.insert("artifacts".to_string(), create_cicd_artifact_fixtures());
    all_fixtures.insert("environments".to_string(), create_workflow_environment_fixtures());

    all_fixtures
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_github_workflow_context() {
        let context = GitHubWorkflowContext::new("Test Workflow", "push")
            .with_metadata("test_key", Value::String("test_value".to_string()));

        let json = context.to_json().unwrap();
        assert!(json.is_object());

        let obj = json.as_object().unwrap();
        assert_eq!(obj["workflow_name"], "Test Workflow");
        assert_eq!(obj["event_name"], "push");
        assert_eq!(obj["test_key"], "test_value");
    }

    #[test]
    fn test_performance_validation_workflows() {
        let workflows = create_performance_validation_workflows();
        assert!(workflows.contains_key("main_branch_success"));
        assert!(workflows.contains_key("pr_success"));
        assert!(workflows.contains_key("regression_detected"));
        assert!(workflows.contains_key("benchmark_timeout"));

        // Verify success workflow
        let main_success = &workflows["main_branch_success"];
        assert_eq!(main_success.status, "completed");
        assert_eq!(main_success.conclusion, "success");
        assert!(main_success.performance_data.contains_key("display_gibs"));
    }

    #[test]
    fn test_baseline_promotion_workflows() {
        let workflows = create_baseline_promotion_workflows();
        assert!(workflows.contains_key("promotion_success"));
        assert!(workflows.contains_key("scheduled_promotion"));
        assert!(workflows.contains_key("promotion_rejected"));

        // Verify promotion success
        let promotion = &workflows["promotion_success"];
        assert_eq!(promotion.conclusion, "success");
        assert!(promotion.performance_data.contains_key("new_baseline_display"));
    }

    #[test]
    fn test_pr_comment_fixtures() {
        let comments = create_pr_comment_fixtures();
        assert!(comments.contains_key("performance_success_comment"));
        assert!(comments.contains_key("performance_regression_comment"));
        assert!(comments.contains_key("baseline_promotion_comment"));

        // Verify comment structure
        let success_comment = &comments["performance_success_comment"];
        assert!(success_comment["markdown_content"].is_string());
        assert!(success_comment["performance_data"].is_object());
    }

    #[test]
    fn test_cicd_artifact_fixtures() {
        let artifacts = create_cicd_artifact_fixtures();
        assert!(artifacts.contains_key("performance_report_artifact"));
        assert!(artifacts.contains_key("perf_json_artifact"));
        assert!(artifacts.contains_key("audit_report_artifact"));

        // Verify artifact structure
        let perf_artifact = &artifacts["perf_json_artifact"];
        assert_eq!(perf_artifact["artifact_type"], "machine_readable");
        assert!(perf_artifact["download_url"].is_string());
    }

    #[test]
    fn test_comprehensive_fixtures() {
        let all_fixtures = create_comprehensive_cicd_fixtures();
        assert!(!all_fixtures.is_empty());

        let expected_categories = vec![
            "performance_workflows", "baseline_workflows",
            "pr_comments", "artifacts", "environments"
        ];

        for category in expected_categories {
            assert!(all_fixtures.contains_key(category));
            assert!(!all_fixtures[category].is_empty());
        }
    }
}
