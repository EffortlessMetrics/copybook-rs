// SPDX-License-Identifier: AGPL-3.0-or-later
//! Data Lineage Tracking System
//!
//! Comprehensive data transformation and lineage tracking for copybook-rs
//! enterprise mainframe data processing operations.

use serde::{Deserialize, Serialize};

use super::{AuditError, AuditResult};

/// Data lineage tracker for field-level transformation tracking
pub struct LineageTracker {
    source_system: String,
    target_system: String,
}

impl LineageTracker {
    pub fn new() -> Self {
        Self {
            source_system: "unknown".to_string(),
            target_system: "unknown".to_string(),
        }
    }

    #[must_use]
    pub fn with_source_system(mut self, system_id: impl Into<String>) -> Self {
        self.source_system = system_id.into();
        self
    }

    #[must_use]
    pub fn with_target_system(mut self, system_id: impl Into<String>) -> Self {
        self.target_system = system_id.into();
        self
    }

    /// Track a data transformation from source to target field
    ///
    /// Creates a lineage record that documents the transformation between fields.
    /// Quality score must be between 0.0 and 1.0 (inclusive).
    ///
    /// # Arguments
    ///
    /// * `source` - Source field lineage information
    /// * `target` - Target field lineage information
    /// * `transformation` - Type of transformation applied
    /// * `quality_score` - Data quality score (0.0-1.0)
    ///
    /// # Errors
    ///
    /// Returns `AuditError::Configuration` if quality_score is outside valid range
    pub fn track_transformation(
        &self,
        source: FieldLineage,
        target: FieldLineage,
        transformation: TransformationType,
        quality_score: f64,
    ) -> AuditResult<LineageRecord> {
        // Validate quality score is within valid range
        if !(0.0..=1.0).contains(&quality_score) {
            return Err(AuditError::Configuration {
                message: format!(
                    "Quality score must be between 0.0 and 1.0, got: {}",
                    quality_score
                ),
            });
        }

        Ok(LineageRecord {
            source_field: source,
            target_field: target,
            transformation,
            quality_score,
            processed_at: chrono::Utc::now().to_rfc3339(),
        })
    }

    /// Validate lineage chain integrity
    ///
    /// Verifies that a chain of lineage records is valid by checking:
    /// - No gaps in the chain (each record's target matches next record's source)
    /// - Consistent transformation types across the chain
    ///
    /// # Arguments
    ///
    /// * `records` - Slice of lineage records to validate
    ///
    /// # Errors
    ///
    /// Returns `AuditError::AuditTrailIntegrity` if chain validation fails
    pub fn validate_lineage_chain(&self, records: &[LineageRecord]) -> AuditResult<bool> {
        if records.is_empty() {
            return Ok(true);
        }

        // Validate chain continuity
        for i in 1..records.len() {
            let prev = &records[i - 1];
            let current = &records[i];

            // Check if target of previous record matches source of current record
            if prev.target_field.field_path != current.source_field.field_path {
                return Err(AuditError::AuditTrailIntegrity {
                    message: format!(
                        "Lineage chain gap detected: record {} target '{}' does not match record {} source '{}'",
                        i - 1,
                        prev.target_field.field_path,
                        i,
                        current.source_field.field_path
                    ),
                    expected_hash: prev.target_field.field_path.clone(),
                    actual_hash: current.source_field.field_path.clone(),
                });
            }

            // Verify system continuity
            if prev.target_field.system_id != current.source_field.system_id {
                return Err(AuditError::AuditTrailIntegrity {
                    message: format!(
                        "Lineage chain system mismatch: record {} target system '{}' does not match record {} source system '{}'",
                        i - 1,
                        prev.target_field.system_id,
                        i,
                        current.source_field.system_id
                    ),
                    expected_hash: prev.target_field.system_id.clone(),
                    actual_hash: current.source_field.system_id.clone(),
                });
            }
        }

        Ok(true)
    }
}

impl Default for LineageTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// Field-level lineage information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldLineage {
    pub field_path: String,
    pub system_id: String,
    pub schema_version: String,
    pub data_type: String,
}

/// Data transformation types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TransformationType {
    DirectMapping,
    TypeConversion,
    Aggregation,
    Calculation,
    Lookup,
    Filter,
}

/// Lineage record for tracking data transformations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LineageRecord {
    pub source_field: FieldLineage,
    pub target_field: FieldLineage,
    pub transformation: TransformationType,
    pub quality_score: f64,
    pub processed_at: String,
}

/// Impact analysis for schema changes
pub struct ImpactAnalyzer;

impl ImpactAnalyzer {
    pub fn new() -> Self {
        Self
    }

    /// Assess the downstream impact of changes to a specific field
    ///
    /// Analyzes how many systems and transformations are affected by changes
    /// to the specified field path.
    ///
    /// # Arguments
    ///
    /// * `field_path` - The field path to analyze
    /// * `lineage_records` - All lineage records to search through
    ///
    /// # Returns
    ///
    /// An `ImpactAssessment` containing affected systems, risk level, and impact estimate
    pub fn assess_field_impact(
        &self,
        field_path: &str,
        lineage_records: &[LineageRecord],
    ) -> ImpactAssessment {
        let mut affected_systems = std::collections::HashSet::new();

        // Find all records that reference this field as source or target
        for record in lineage_records {
            if record.source_field.field_path == field_path {
                affected_systems.insert(record.target_field.system_id.clone());
            }
            if record.target_field.field_path == field_path {
                affected_systems.insert(record.source_field.system_id.clone());
            }
        }

        let affected_systems: Vec<String> = affected_systems.into_iter().collect();
        let affected_count = affected_systems.len();

        // Determine risk level based on number of affected systems
        let risk_level = match affected_count {
            0 => RiskLevel::Low,
            1..=2 => RiskLevel::Low,
            3..=5 => RiskLevel::Medium,
            6..=10 => RiskLevel::High,
            _ => RiskLevel::Critical,
        };

        let estimated_impact = if affected_count == 0 {
            "No downstream systems affected".to_string()
        } else {
            format!(
                "{} downstream system(s) affected by changes to field '{}'",
                affected_count, field_path
            )
        };

        ImpactAssessment {
            affected_systems,
            risk_level,
            estimated_impact,
        }
    }

    /// Estimate remediation effort based on impact assessment
    ///
    /// Provides a human-readable effort estimate based on the risk level
    /// and number of affected systems.
    ///
    /// # Arguments
    ///
    /// * `assessment` - The impact assessment to analyze
    ///
    /// # Returns
    ///
    /// A string describing the estimated remediation effort
    pub fn estimate_remediation_effort(&self, assessment: &ImpactAssessment) -> String {
        let system_count = assessment.affected_systems.len();

        match assessment.risk_level {
            RiskLevel::Low => {
                if system_count == 0 {
                    "Minimal effort - no downstream impact".to_string()
                } else {
                    format!(
                        "Low effort - {} system(s) require validation (1-2 days)",
                        system_count
                    )
                }
            }
            RiskLevel::Medium => {
                format!(
                    "Medium effort - {} systems require updates and testing (1-2 weeks)",
                    system_count
                )
            }
            RiskLevel::High => {
                format!(
                    "High effort - {} systems require coordinated updates and regression testing (2-4 weeks)",
                    system_count
                )
            }
            RiskLevel::Critical => {
                format!(
                    "Critical effort - {} systems require extensive coordination, phased rollout, and comprehensive testing (1-3 months)",
                    system_count
                )
            }
        }
    }

    /// Generate a human-readable impact report
    ///
    /// Creates a formatted report summarizing the impact assessment with
    /// affected systems, risk level, and estimated impact.
    ///
    /// # Arguments
    ///
    /// * `assessment` - The impact assessment to report on
    ///
    /// # Returns
    ///
    /// A formatted string containing the impact report
    pub fn generate_impact_report(&self, assessment: &ImpactAssessment) -> String {
        let mut report = String::new();

        report.push_str("=== Data Lineage Impact Assessment ===\n\n");

        report.push_str(&format!("Risk Level: {:?}\n", assessment.risk_level));
        report.push_str(&format!("Impact: {}\n\n", assessment.estimated_impact));

        if assessment.affected_systems.is_empty() {
            report.push_str("Affected Systems: None\n");
        } else {
            report.push_str(&format!(
                "Affected Systems ({}):\n",
                assessment.affected_systems.len()
            ));
            for (i, system) in assessment.affected_systems.iter().enumerate() {
                report.push_str(&format!("  {}. {}\n", i + 1, system));
            }
        }

        report.push_str("\nRemediation Effort Estimate:\n");
        report.push_str(&format!(
            "  {}\n",
            self.estimate_remediation_effort(assessment)
        ));

        report
    }
}

impl Default for ImpactAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

/// Impact assessment results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImpactAssessment {
    pub affected_systems: Vec<String>,
    pub risk_level: RiskLevel,
    pub estimated_impact: String,
}

/// Risk levels for impact assessment
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum RiskLevel {
    Low,
    Medium,
    High,
    Critical,
}

#[cfg(test)]
#[allow(clippy::expect_used)]
mod tests {
    use super::*;

    fn create_test_field_lineage(field_path: &str, system_id: &str) -> FieldLineage {
        FieldLineage {
            field_path: field_path.to_string(),
            system_id: system_id.to_string(),
            schema_version: "1.0.0".to_string(),
            data_type: "string".to_string(),
        }
    }

    #[test]
    fn test_track_transformation_valid_quality_score() {
        let tracker = LineageTracker::new()
            .with_source_system("system_a")
            .with_target_system("system_b");

        let source = create_test_field_lineage("field_a", "system_a");
        let target = create_test_field_lineage("field_b", "system_b");

        // Test valid quality scores at boundaries and middle
        let result = tracker.track_transformation(
            source.clone(),
            target.clone(),
            TransformationType::DirectMapping,
            0.0,
        );
        assert!(result.is_ok());

        let result = tracker.track_transformation(
            source.clone(),
            target.clone(),
            TransformationType::TypeConversion,
            1.0,
        );
        assert!(result.is_ok());

        let result =
            tracker.track_transformation(source, target, TransformationType::Calculation, 0.5);
        assert!(result.is_ok());
    }

    #[test]
    fn test_track_transformation_invalid_quality_score() {
        let tracker = LineageTracker::new();

        let source = create_test_field_lineage("field_a", "system_a");
        let target = create_test_field_lineage("field_b", "system_b");

        // Test invalid quality scores
        let result = tracker.track_transformation(
            source.clone(),
            target.clone(),
            TransformationType::DirectMapping,
            -0.1,
        );
        assert!(result.is_err());

        let result =
            tracker.track_transformation(source, target, TransformationType::DirectMapping, 1.1);
        assert!(result.is_err());
    }

    #[test]
    fn test_track_transformation_creates_record() {
        let tracker = LineageTracker::new()
            .with_source_system("source_sys")
            .with_target_system("target_sys");

        let source = create_test_field_lineage("customer.id", "source_sys");
        let target = create_test_field_lineage("user.id", "target_sys");

        let record = tracker
            .track_transformation(
                source.clone(),
                target.clone(),
                TransformationType::DirectMapping,
                0.95,
            )
            .expect("Should create lineage record");

        assert_eq!(record.source_field.field_path, "customer.id");
        assert_eq!(record.target_field.field_path, "user.id");
        assert_eq!(record.quality_score, 0.95);
        assert!(!record.processed_at.is_empty());
    }

    #[test]
    fn test_validate_lineage_chain_empty() {
        let tracker = LineageTracker::new();
        let records: Vec<LineageRecord> = vec![];

        let result = tracker
            .validate_lineage_chain(&records)
            .expect("Empty chain should be valid");
        assert!(result);
    }

    #[test]
    fn test_validate_lineage_chain_valid() {
        let tracker = LineageTracker::new();

        // Create a valid chain: A -> B -> C
        let field_a = create_test_field_lineage("field_a", "system_1");
        let field_b = create_test_field_lineage("field_b", "system_2");
        let field_c = create_test_field_lineage("field_c", "system_3");

        let record1 = tracker
            .track_transformation(
                field_a,
                field_b.clone(),
                TransformationType::DirectMapping,
                0.9,
            )
            .expect("Should create first record");

        let record2 = tracker
            .track_transformation(field_b, field_c, TransformationType::TypeConversion, 0.8)
            .expect("Should create second record");

        let records = vec![record1, record2];

        let result = tracker
            .validate_lineage_chain(&records)
            .expect("Valid chain should pass validation");
        assert!(result);
    }

    #[test]
    fn test_validate_lineage_chain_gap() {
        let tracker = LineageTracker::new();

        // Create a chain with gap: A -> B, C -> D (missing B -> C)
        let field_a = create_test_field_lineage("field_a", "system_1");
        let field_b = create_test_field_lineage("field_b", "system_2");
        let field_c = create_test_field_lineage("field_c", "system_3");
        let field_d = create_test_field_lineage("field_d", "system_4");

        let record1 = tracker
            .track_transformation(field_a, field_b, TransformationType::DirectMapping, 0.9)
            .expect("Should create first record");

        let record2 = tracker
            .track_transformation(field_c, field_d, TransformationType::DirectMapping, 0.9)
            .expect("Should create second record");

        let records = vec![record1, record2];

        let result = tracker.validate_lineage_chain(&records);
        assert!(result.is_err());

        if let Err(AuditError::AuditTrailIntegrity { message, .. }) = result {
            assert!(message.contains("Lineage chain gap detected"));
        } else {
            panic!("Expected AuditTrailIntegrity error");
        }
    }

    #[test]
    fn test_validate_lineage_chain_system_mismatch() {
        let tracker = LineageTracker::new();

        // Create chain with system mismatch
        let field_a = create_test_field_lineage("field_a", "system_1");
        let field_b = create_test_field_lineage("field_b", "system_2");
        let mut field_b_wrong_system = create_test_field_lineage("field_b", "system_3");
        field_b_wrong_system.system_id = "wrong_system".to_string();
        let field_c = create_test_field_lineage("field_c", "system_4");

        let record1 = tracker
            .track_transformation(field_a, field_b, TransformationType::DirectMapping, 0.9)
            .expect("Should create first record");

        let record2 = tracker
            .track_transformation(
                field_b_wrong_system,
                field_c,
                TransformationType::DirectMapping,
                0.9,
            )
            .expect("Should create second record");

        let records = vec![record1, record2];

        let result = tracker.validate_lineage_chain(&records);
        assert!(result.is_err());

        if let Err(AuditError::AuditTrailIntegrity { message, .. }) = result {
            assert!(message.contains("system mismatch"));
        } else {
            panic!("Expected AuditTrailIntegrity error for system mismatch");
        }
    }

    #[test]
    fn test_assess_field_impact_no_records() {
        let analyzer = ImpactAnalyzer::new();
        let records: Vec<LineageRecord> = vec![];

        let assessment = analyzer.assess_field_impact("test_field", &records);

        assert_eq!(assessment.risk_level, RiskLevel::Low);
        assert!(assessment.affected_systems.is_empty());
        assert!(
            assessment
                .estimated_impact
                .contains("No downstream systems")
        );
    }

    #[test]
    fn test_assess_field_impact_low_risk() {
        let tracker = LineageTracker::new();
        let analyzer = ImpactAnalyzer::new();

        // Create records affecting 2 systems (low risk)
        let field_a = create_test_field_lineage("target_field", "system_1");
        let field_b = create_test_field_lineage("field_b", "system_2");

        let record = tracker
            .track_transformation(field_a, field_b, TransformationType::DirectMapping, 0.9)
            .expect("Should create record");

        let assessment = analyzer.assess_field_impact("target_field", &[record]);

        assert_eq!(assessment.risk_level, RiskLevel::Low);
        assert_eq!(assessment.affected_systems.len(), 1);
    }

    #[test]
    fn test_assess_field_impact_medium_risk() {
        let tracker = LineageTracker::new();
        let analyzer = ImpactAnalyzer::new();

        // Create records affecting 4 systems (medium risk)
        let source = create_test_field_lineage("target_field", "system_1");
        let mut records = vec![];

        for i in 2..=5 {
            let target = create_test_field_lineage(&format!("field_{i}"), &format!("system_{i}"));
            let record = tracker
                .track_transformation(
                    source.clone(),
                    target,
                    TransformationType::DirectMapping,
                    0.9,
                )
                .expect("Should create record");
            records.push(record);
        }

        let assessment = analyzer.assess_field_impact("target_field", &records);

        assert_eq!(assessment.risk_level, RiskLevel::Medium);
        assert_eq!(assessment.affected_systems.len(), 4);
    }

    #[test]
    fn test_assess_field_impact_high_risk() {
        let tracker = LineageTracker::new();
        let analyzer = ImpactAnalyzer::new();

        // Create records affecting 8 systems (high risk)
        let source = create_test_field_lineage("target_field", "system_1");
        let mut records = vec![];

        for i in 2..=9 {
            let target = create_test_field_lineage(&format!("field_{i}"), &format!("system_{i}"));
            let record = tracker
                .track_transformation(
                    source.clone(),
                    target,
                    TransformationType::DirectMapping,
                    0.9,
                )
                .expect("Should create record");
            records.push(record);
        }

        let assessment = analyzer.assess_field_impact("target_field", &records);

        assert_eq!(assessment.risk_level, RiskLevel::High);
        assert_eq!(assessment.affected_systems.len(), 8);
    }

    #[test]
    fn test_assess_field_impact_critical_risk() {
        let tracker = LineageTracker::new();
        let analyzer = ImpactAnalyzer::new();

        // Create records affecting 12 systems (critical risk)
        let source = create_test_field_lineage("target_field", "system_1");
        let mut records = vec![];

        for i in 2..=13 {
            let target = create_test_field_lineage(&format!("field_{i}"), &format!("system_{i}"));
            let record = tracker
                .track_transformation(
                    source.clone(),
                    target,
                    TransformationType::DirectMapping,
                    0.9,
                )
                .expect("Should create record");
            records.push(record);
        }

        let assessment = analyzer.assess_field_impact("target_field", &records);

        assert_eq!(assessment.risk_level, RiskLevel::Critical);
        assert_eq!(assessment.affected_systems.len(), 12);
    }

    #[test]
    fn test_estimate_remediation_effort() {
        let analyzer = ImpactAnalyzer::new();

        // Test low risk with no systems
        let assessment = ImpactAssessment {
            affected_systems: vec![],
            risk_level: RiskLevel::Low,
            estimated_impact: "test".to_string(),
        };
        let effort = analyzer.estimate_remediation_effort(&assessment);
        assert!(effort.contains("Minimal effort"));

        // Test low risk with systems
        let assessment = ImpactAssessment {
            affected_systems: vec!["system1".to_string()],
            risk_level: RiskLevel::Low,
            estimated_impact: "test".to_string(),
        };
        let effort = analyzer.estimate_remediation_effort(&assessment);
        assert!(effort.contains("1-2 days"));

        // Test medium risk
        let assessment = ImpactAssessment {
            affected_systems: vec!["system1".to_string(), "system2".to_string()],
            risk_level: RiskLevel::Medium,
            estimated_impact: "test".to_string(),
        };
        let effort = analyzer.estimate_remediation_effort(&assessment);
        assert!(effort.contains("1-2 weeks"));

        // Test high risk
        let assessment = ImpactAssessment {
            affected_systems: vec![
                "system1".to_string(),
                "system2".to_string(),
                "system3".to_string(),
            ],
            risk_level: RiskLevel::High,
            estimated_impact: "test".to_string(),
        };
        let effort = analyzer.estimate_remediation_effort(&assessment);
        assert!(effort.contains("2-4 weeks"));

        // Test critical risk
        let assessment = ImpactAssessment {
            affected_systems: (1..=12).map(|i| format!("system{i}")).collect(),
            risk_level: RiskLevel::Critical,
            estimated_impact: "test".to_string(),
        };
        let effort = analyzer.estimate_remediation_effort(&assessment);
        assert!(effort.contains("1-3 months"));
    }

    #[test]
    fn test_generate_impact_report() {
        let analyzer = ImpactAnalyzer::new();

        let assessment = ImpactAssessment {
            affected_systems: vec!["system_a".to_string(), "system_b".to_string()],
            risk_level: RiskLevel::Medium,
            estimated_impact: "Test impact description".to_string(),
        };

        let report = analyzer.generate_impact_report(&assessment);

        assert!(report.contains("Data Lineage Impact Assessment"));
        assert!(report.contains("Risk Level: Medium"));
        assert!(report.contains("Test impact description"));
        assert!(report.contains("system_a"));
        assert!(report.contains("system_b"));
        assert!(report.contains("Remediation Effort Estimate"));
        assert!(report.contains("Affected Systems (2)"));
    }

    #[test]
    fn test_generate_impact_report_no_systems() {
        let analyzer = ImpactAnalyzer::new();

        let assessment = ImpactAssessment {
            affected_systems: vec![],
            risk_level: RiskLevel::Low,
            estimated_impact: "No impact".to_string(),
        };

        let report = analyzer.generate_impact_report(&assessment);

        assert!(report.contains("Affected Systems: None"));
        assert!(report.contains("Risk Level: Low"));
    }
}
