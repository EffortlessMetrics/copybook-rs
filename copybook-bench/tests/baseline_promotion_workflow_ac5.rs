//! Test scaffolding for Issue #52 AC5: Baseline promotion workflow for main branch merges
//!
//! Tests feature spec: issue-52-spec.md#AC5
//! Validates baseline promotion workflow that updates performance baselines automatically

#![allow(clippy::unwrap_used)]
#![allow(clippy::unnecessary_wraps)]
#![allow(clippy::uninlined_format_args)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::float_cmp)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::items_after_statements)]
#![allow(clippy::useless_vec)]
#![allow(clippy::needless_pass_by_value)]

use serde_json::Value;
use std::collections::HashMap;
use std::time::SystemTime;

/// Performance baseline entry for promotion workflow
#[derive(Debug, Clone)]
pub struct PerformanceBaseline {
    pub baseline_id: String,
    pub timestamp: SystemTime,
    pub git_commit: String,
    pub pr_number: u32,
    pub display_gibs: f64,
    pub comp3_mibs: f64,
    pub promotion_metadata: PromotionMetadata,
    pub validation_status: BaselineValidationStatus,
}

#[derive(Debug, Clone)]
pub struct PromotionMetadata {
    pub promoted_by: String,
    pub promotion_timestamp: SystemTime,
    pub validation_passed: bool,
    pub safety_margins: SafetyMargins,
}

#[derive(Debug, Clone)]
pub struct SafetyMargins {
    pub display_safety_factor: f64, // Current: ~56x safety margin
    pub comp3_safety_factor: f64,   // Current: ~14x safety margin
}

#[derive(Debug, Clone, PartialEq)]
pub enum BaselineValidationStatus {
    Valid,
    Invalid(String),
    Pending,
}

impl PerformanceBaseline {
    #[must_use]
    pub fn new_enterprise_baseline(pr_number: u32, git_commit: String) -> Self {
        Self {
            baseline_id: format!("main-{}", &git_commit[..8]),
            timestamp: SystemTime::now(),
            git_commit,
            pr_number,
            display_gibs: 4.22,
            comp3_mibs: 571.0,
            promotion_metadata: PromotionMetadata {
                promoted_by: "automated_merge".to_string(),
                promotion_timestamp: SystemTime::now(),
                validation_passed: true,
                safety_margins: SafetyMargins {
                    display_safety_factor: 56.6, // 4.22 GiB/s = 4531 MB/s, 4531/80 = 56.6x
                    comp3_safety_factor: 14.3,   // 571/40 = 14.3x
                },
            },
            validation_status: BaselineValidationStatus::Valid,
        }
    }

    /// Serializes the performance baseline to JSON.
    ///
    /// # Errors
    ///
    /// Returns `serde_json::Error` if serialization fails.
    pub fn to_json(&self) -> serde_json::Result<Value> {
        Ok(serde_json::json!({
            "baseline_id": self.baseline_id,
            "timestamp": self.timestamp.duration_since(SystemTime::UNIX_EPOCH)
                .unwrap_or_default().as_secs(),
            "git_commit": self.git_commit,
            "pr_number": self.pr_number,
            "performance_data": {
                "display_gibs": self.display_gibs,
                "comp3_mibs": self.comp3_mibs,
                "warnings": Vec::<String>::new(),
                "errors": Vec::<String>::new()
            },
            "promotion_metadata": {
                "promoted_by": self.promotion_metadata.promoted_by,
                "promotion_timestamp": self.promotion_metadata.promotion_timestamp
                    .duration_since(SystemTime::UNIX_EPOCH).unwrap_or_default().as_secs(),
                "validation_passed": self.promotion_metadata.validation_passed,
                "safety_margins": {
                    "display_safety_factor": self.promotion_metadata.safety_margins.display_safety_factor,
                    "comp3_safety_factor": self.promotion_metadata.safety_margins.comp3_safety_factor
                }
            }
        }))
    }
}

/// Baseline manager for promotion workflows
pub struct BaselineManager {
    baselines: HashMap<String, PerformanceBaseline>,
    current_baseline_id: Option<String>,
    max_baselines_retained: usize,
}

impl Default for BaselineManager {
    fn default() -> Self {
        Self {
            baselines: HashMap::new(),
            current_baseline_id: None,
            max_baselines_retained: 10,
        }
    }
}

impl BaselineManager {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn promote_baseline(
        &mut self,
        pr_number: u32,
        git_commit: String,
        performance_data: &PerformanceData,
    ) -> Result<String, BaselinePromotionError> {
        // Validate promotion criteria
        self.validate_promotion_criteria(performance_data)?;

        // Create new baseline
        let baseline = PerformanceBaseline {
            baseline_id: format!("main-{}", &git_commit[..8]),
            timestamp: SystemTime::now(),
            git_commit: git_commit.clone(),
            pr_number,
            display_gibs: performance_data.display_gibs,
            comp3_mibs: performance_data.comp3_mibs,
            promotion_metadata: PromotionMetadata {
                promoted_by: "automated_merge".to_string(),
                promotion_timestamp: SystemTime::now(),
                validation_passed: true,
                safety_margins: SafetyMargins {
                    display_safety_factor: (performance_data.display_gibs * 1073.74) / 80.0,
                    comp3_safety_factor: performance_data.comp3_mibs / 40.0,
                },
            },
            validation_status: BaselineValidationStatus::Valid,
        };

        let baseline_id = baseline.baseline_id.clone();

        // Store baseline
        self.baselines.insert(baseline_id.clone(), baseline);

        // Update current baseline pointer
        self.current_baseline_id = Some(baseline_id.clone());

        // Cleanup old baselines
        self.cleanup_old_baselines();

        Ok(baseline_id)
    }

    pub fn validate_promotion_criteria(
        &self,
        performance_data: &PerformanceData,
    ) -> Result<(), BaselinePromotionError> {
        // Must have no errors
        if !performance_data.errors.is_empty() {
            return Err(BaselinePromotionError::ValidationFailed(format!(
                "Cannot promote baseline with {} errors",
                performance_data.errors.len()
            )));
        }

        // Must meet minimum performance floors
        let display_mbps = performance_data.display_gibs * 1073.74;
        let display_margin = display_mbps / 80.0; // vs 80 MB/s floor
        let comp3_margin = performance_data.comp3_mibs / 40.0; // vs 40 MB/s floor

        if display_margin < 1.0 {
            return Err(BaselinePromotionError::PerformanceFloor(format!(
                "DISPLAY throughput {:.2} GiB/s below 80 MB/s floor",
                performance_data.display_gibs
            )));
        }

        if comp3_margin < 1.0 {
            return Err(BaselinePromotionError::PerformanceFloor(format!(
                "COMP-3 throughput {:.1} MiB/s below 40 MB/s floor",
                performance_data.comp3_mibs
            )));
        }

        Ok(())
    }

    #[must_use]
    pub fn get_current_baseline(&self) -> Option<&PerformanceBaseline> {
        self.current_baseline_id
            .as_ref()
            .and_then(|id| self.baselines.get(id))
    }

    #[must_use]
    pub fn get_baseline_history(&self) -> Vec<&PerformanceBaseline> {
        let mut baselines: Vec<_> = self.baselines.values().collect();
        baselines.sort_by(|a, b| b.timestamp.cmp(&a.timestamp));
        baselines
    }

    fn cleanup_old_baselines(&mut self) {
        if self.baselines.len() <= self.max_baselines_retained {
            return;
        }

        // Sort baselines by timestamp (newest first)
        let mut baseline_items: Vec<_> = self.baselines.iter().collect();
        baseline_items.sort_by(|a, b| b.1.timestamp.cmp(&a.1.timestamp));

        // Keep only the newest baselines
        let baselines_to_keep: Vec<_> = baseline_items
            .into_iter()
            .take(self.max_baselines_retained)
            .map(|(id, _)| id.clone())
            .collect();

        self.baselines
            .retain(|id, _| baselines_to_keep.contains(id));
    }
}

#[derive(Debug, Clone)]
pub struct PerformanceData {
    pub display_gibs: f64,
    pub comp3_mibs: f64,
    pub warnings: Vec<String>,
    pub errors: Vec<String>,
}

impl PerformanceData {
    #[must_use]
    pub fn new_valid() -> Self {
        Self {
            display_gibs: 4.22,
            comp3_mibs: 571.0,
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    #[must_use]
    pub fn new_invalid() -> Self {
        Self {
            display_gibs: 0.05, // Below 80 MB/s floor
            comp3_mibs: 25.0,   // Below 40 MB/s floor
            warnings: Vec::new(),
            errors: vec![
                "DISPLAY throughput below floor".to_string(),
                "COMP-3 throughput below floor".to_string(),
            ],
        }
    }
}

#[derive(Debug)]
pub enum BaselinePromotionError {
    ValidationFailed(String),
    PerformanceFloor(String),
    GitCommitInvalid(String),
    StorageError(String),
}

impl std::fmt::Display for BaselinePromotionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BaselinePromotionError::ValidationFailed(msg) => {
                write!(f, "Validation failed: {}", msg)
            }
            BaselinePromotionError::PerformanceFloor(msg) => {
                write!(f, "Performance floor violation: {}", msg)
            }
            BaselinePromotionError::GitCommitInvalid(msg) => {
                write!(f, "Invalid git commit: {}", msg)
            }
            BaselinePromotionError::StorageError(msg) => write!(f, "Storage error: {}", msg),
        }
    }
}

impl std::error::Error for BaselinePromotionError {}

/// Tests feature spec: issue-52-spec.md#AC5-promotion-workflow
/// Validates that baseline promotion workflow works for valid performance data
#[test]
fn test_baseline_promotion_workflow_success() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC5 - Verify baseline promotion workflow for successful merge
    let mut manager = BaselineManager::new();
    let performance_data = PerformanceData::new_valid();

    let pr_number = 123;
    let git_commit = "a1b2c3d4e5f6789012345678901234567890abcd".to_string();

    // Promote baseline
    let baseline_id = manager.promote_baseline(pr_number, git_commit.clone(), &performance_data)?;

    // Validate baseline was created
    assert_eq!(baseline_id, format!("main-{}", &git_commit[..8]));
    assert!(manager.baselines.contains_key(&baseline_id));

    // Validate current baseline was updated
    let current = manager.get_current_baseline();
    assert!(current.is_some());
    assert_eq!(current.unwrap().baseline_id, baseline_id);
    assert_eq!(current.unwrap().pr_number, pr_number);

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC5-promotion-validation
/// Validates that promotion validation prevents invalid baselines
#[test]
fn test_baseline_promotion_validation_failures() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC5 - Verify promotion validation prevents invalid baselines
    let mut manager = BaselineManager::new();
    let invalid_data = PerformanceData::new_invalid();

    let pr_number = 123;
    let git_commit = "a1b2c3d4e5f6789012345678901234567890abcd".to_string();

    // Attempt to promote invalid baseline
    let result = manager.promote_baseline(pr_number, git_commit, &invalid_data);
    assert!(
        result.is_err(),
        "Should fail to promote baseline with errors"
    );

    // Verify no baseline was created
    assert!(
        manager.baselines.is_empty(),
        "No baseline should be created on validation failure"
    );
    assert!(
        manager.get_current_baseline().is_none(),
        "Current baseline should remain unchanged"
    );

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC5-performance-floor-validation
/// Validates that performance floor requirements are enforced
#[test]
fn test_performance_floor_validation() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC5 - Verify performance floor requirements are enforced
    let manager = BaselineManager::new();

    // Test data below floors
    let below_floor_data = PerformanceData {
        display_gibs: 0.05, // ~53 MB/s, below 80 MB/s floor
        comp3_mibs: 25.0,   // below 40 MB/s floor
        warnings: Vec::new(),
        errors: Vec::new(),
    };

    let result = manager.validate_promotion_criteria(&below_floor_data);
    assert!(
        result.is_err(),
        "Should fail validation with performance below floors"
    );

    // Test data above floors
    let above_floor_data = PerformanceData {
        display_gibs: 1.0, // ~1073 MB/s, above 80 MB/s floor
        comp3_mibs: 100.0, // above 40 MB/s floor
        warnings: Vec::new(),
        errors: Vec::new(),
    };

    let result = manager.validate_promotion_criteria(&above_floor_data);
    assert!(
        result.is_ok(),
        "Should pass validation with performance above floors"
    );

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC5-safety-margin-calculation
/// Validates that safety margin calculations are correct for baselines
#[test]
fn test_safety_margin_calculation_in_promotion() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC5 - Verify safety margin calculations in baseline promotion
    let mut manager = BaselineManager::new();
    let performance_data = PerformanceData::new_valid();

    let pr_number = 123;
    let git_commit = "a1b2c3d4e5f6789012345678901234567890abcd".to_string();

    let baseline_id = manager.promote_baseline(pr_number, git_commit, &performance_data)?;
    let baseline = manager.baselines.get(&baseline_id).unwrap();

    // Verify safety margin calculations
    let expected_display_margin = (performance_data.display_gibs * 1073.74) / 80.0;
    let expected_comp3_margin = performance_data.comp3_mibs / 40.0;

    assert!(
        (baseline
            .promotion_metadata
            .safety_margins
            .display_safety_factor
            - expected_display_margin)
            .abs()
            < 0.1,
        "DISPLAY safety margin should be {:.1}, got {:.1}",
        expected_display_margin,
        baseline
            .promotion_metadata
            .safety_margins
            .display_safety_factor
    );

    assert!(
        (baseline
            .promotion_metadata
            .safety_margins
            .comp3_safety_factor
            - expected_comp3_margin)
            .abs()
            < 0.1,
        "COMP-3 safety margin should be {:.1}, got {:.1}",
        expected_comp3_margin,
        baseline
            .promotion_metadata
            .safety_margins
            .comp3_safety_factor
    );

    // Current performance should show substantial safety margins
    assert!(
        baseline
            .promotion_metadata
            .safety_margins
            .display_safety_factor
            > 50.0,
        "DISPLAY should have substantial safety margin"
    );
    assert!(
        baseline
            .promotion_metadata
            .safety_margins
            .comp3_safety_factor
            > 10.0,
        "COMP-3 should have substantial safety margin"
    );

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC5-baseline-cleanup
/// Validates that old baselines are cleaned up properly
#[test]
fn test_baseline_cleanup_retention() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC5 - Verify old baseline cleanup maintains retention policy
    let mut manager = BaselineManager::new();
    let performance_data = PerformanceData::new_valid();

    // Promote more baselines than the retention limit
    for i in 1..=15 {
        let pr_number = i as u32;
        let git_commit = format!("{:08x}{:032}", i, 0); // Ensure unique first 8 chars
        manager.promote_baseline(pr_number, git_commit, &performance_data)?;
    }

    // Should only retain the newest 10 baselines
    assert_eq!(
        manager.baselines.len(),
        manager.max_baselines_retained,
        "Should retain exactly {} baselines",
        manager.max_baselines_retained
    );

    // Current baseline should be the most recent
    let current = manager.get_current_baseline().unwrap();
    assert_eq!(
        current.pr_number, 15,
        "Current baseline should be the most recent"
    );

    // Verify history is sorted by newest first
    let history = manager.get_baseline_history();
    assert_eq!(history.len(), manager.max_baselines_retained);
    assert_eq!(
        history[0].pr_number, 15,
        "History should start with newest baseline"
    );
    assert_eq!(
        history[history.len() - 1].pr_number,
        6,
        "History should end with oldest retained baseline"
    );

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC5-json-serialization
/// Validates that baselines can be serialized to JSON for storage
#[test]
fn test_baseline_json_serialization() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC5 - Verify baseline JSON serialization for storage
    let git_commit = "a1b2c3d4e5f6789012345678901234567890abcd".to_string();
    let baseline = PerformanceBaseline::new_enterprise_baseline(123, git_commit.clone());

    let json_value = baseline.to_json()?;
    let _json_string = serde_json::to_string_pretty(&json_value)?;

    // Verify JSON structure
    assert!(json_value.is_object(), "Baseline JSON must be an object");
    let obj = json_value.as_object().unwrap();

    // Validate required fields
    assert!(
        obj.contains_key("baseline_id"),
        "JSON must contain baseline_id"
    );
    assert!(
        obj.contains_key("git_commit"),
        "JSON must contain git_commit"
    );
    assert!(obj.contains_key("pr_number"), "JSON must contain pr_number");
    assert!(
        obj.contains_key("performance_data"),
        "JSON must contain performance_data"
    );
    assert!(
        obj.contains_key("promotion_metadata"),
        "JSON must contain promotion_metadata"
    );

    // Verify values
    assert_eq!(
        obj["baseline_id"].as_str().unwrap(),
        format!("main-{}", &git_commit[..8])
    );
    assert_eq!(obj["git_commit"].as_str().unwrap(), git_commit);
    assert_eq!(obj["pr_number"].as_u64().unwrap(), 123);

    // Verify performance data structure
    let perf_data = obj["performance_data"].as_object().unwrap();
    assert!(perf_data.contains_key("display_gibs"));
    assert!(perf_data.contains_key("comp3_mibs"));
    assert!(perf_data.contains_key("warnings"));
    assert!(perf_data.contains_key("errors"));

    // Verify promotion metadata structure
    let metadata = obj["promotion_metadata"].as_object().unwrap();
    assert!(metadata.contains_key("promoted_by"));
    assert!(metadata.contains_key("validation_passed"));
    assert!(metadata.contains_key("safety_margins"));

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC5-automated-merge-trigger
/// Validates that automated merge triggers work correctly
#[test]
fn test_automated_merge_trigger_simulation() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC5 - Verify automated merge trigger simulation
    let mut manager = BaselineManager::new();

    // Simulate merge event data
    #[allow(dead_code)]
    struct MergeEvent {
        pr_number: u32,
        git_commit: String,
        branch: String,
        merged_by: String,
    }

    let merge_event = MergeEvent {
        pr_number: 123,
        git_commit: "a1b2c3d4e5f6789012345678901234567890abcd".to_string(),
        branch: "main".to_string(),
        merged_by: "github-actions[bot]".to_string(),
    };

    // Only trigger promotion for main branch merges
    if merge_event.branch == "main" {
        let performance_data = PerformanceData::new_valid();
        let baseline_id = manager.promote_baseline(
            merge_event.pr_number,
            merge_event.git_commit.clone(),
            &performance_data,
        )?;

        // Verify baseline was created with correct metadata
        let baseline = manager.baselines.get(&baseline_id).unwrap();
        assert_eq!(baseline.pr_number, merge_event.pr_number);
        assert_eq!(baseline.git_commit, merge_event.git_commit);
        assert_eq!(baseline.promotion_metadata.promoted_by, "automated_merge");
        assert!(baseline.promotion_metadata.validation_passed);
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC5-baseline-versioning
/// Validates that baseline versioning follows correct patterns
#[test]
fn test_baseline_versioning_patterns() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC5 - Verify baseline versioning follows main-{commit8} pattern
    let mut manager = BaselineManager::new();
    let performance_data = PerformanceData::new_valid();

    let test_commits = vec![
        "1234567890abcdef1234567890abcdef12345678",
        "abcdef1234567890abcdef1234567890abcdef12",
        "fedcba0987654321fedcba0987654321fedcba09",
    ];

    for (i, commit) in test_commits.iter().enumerate() {
        let pr_number = (i + 1) as u32;
        let baseline_id =
            manager.promote_baseline(pr_number, commit.to_string(), &performance_data)?;

        // Verify baseline ID follows pattern
        let expected_id = format!("main-{}", &commit[..8]);
        assert_eq!(
            baseline_id, expected_id,
            "Baseline ID should follow main-{{commit8}} pattern"
        );

        // Verify baseline can be retrieved
        let baseline = manager.baselines.get(&baseline_id);
        assert!(baseline.is_some(), "Baseline should be retrievable by ID");
        assert_eq!(
            baseline.unwrap().git_commit,
            *commit,
            "Full commit hash should be preserved"
        );
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC5-promotion-metadata
/// Validates that promotion metadata is comprehensive and accurate
#[test]
fn test_promotion_metadata_completeness() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC5 - Verify promotion metadata is comprehensive
    let mut manager = BaselineManager::new();
    let performance_data = PerformanceData::new_valid();

    let pr_number = 123;
    let git_commit = "a1b2c3d4e5f6789012345678901234567890abcd".to_string();

    let start_time = SystemTime::now();
    let baseline_id = manager.promote_baseline(pr_number, git_commit, &performance_data)?;
    let end_time = SystemTime::now();

    let baseline = manager.baselines.get(&baseline_id).unwrap();
    let metadata = &baseline.promotion_metadata;

    // Verify metadata completeness
    assert_eq!(metadata.promoted_by, "automated_merge");
    assert!(metadata.validation_passed);

    // Verify promotion timestamp is reasonable
    assert!(metadata.promotion_timestamp >= start_time);
    assert!(metadata.promotion_timestamp <= end_time);

    // Verify safety margins are calculated
    assert!(metadata.safety_margins.display_safety_factor > 0.0);
    assert!(metadata.safety_margins.comp3_safety_factor > 0.0);

    // Verify safety margins meet enterprise requirements
    assert!(
        metadata.safety_margins.display_safety_factor > 1.0,
        "DISPLAY safety margin must exceed 1.0 for promotion"
    );
    assert!(
        metadata.safety_margins.comp3_safety_factor > 1.0,
        "COMP-3 safety margin must exceed 1.0 for promotion"
    );

    Ok(())
}
