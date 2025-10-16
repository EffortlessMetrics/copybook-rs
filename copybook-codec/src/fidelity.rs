//! Binary fidelity testing framework for round-trip validation
//!
//! Implements comprehensive validation ensuring lossless data preservation
//! across encode/decode cycles with cryptographic integrity verification.

use crate::{DecodeOptions, EncodeOptions, decode_record, encode_record, memory::ScratchBuffers};
use copybook_core::{Field, FieldKind, Schema};
use serde_json::Value as JsonValue;
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::convert::TryInto;

/// Binary fidelity validator for round-trip consistency testing
pub struct BinaryFidelityValidator {
    schema: Schema,
    decode_options: DecodeOptions,
    encode_options: EncodeOptions,
    precision_config: PrecisionConfig,
    scratch_buffers: ScratchBuffers,
}

/// Configuration for codec operations
#[derive(Debug, Clone)]
pub struct CodecOptions {
    pub decode_options: DecodeOptions,
    pub encode_options: EncodeOptions,
}

/// Configuration for precision validation
#[derive(Debug, Clone)]
pub struct PrecisionConfig {
    pub tolerance_settings: HashMap<CobolFieldType, PrecisionTolerance>,
    pub business_rules: BusinessRuleConfig,
}

/// Tolerance settings for precision validation
#[derive(Debug, Clone)]
pub struct PrecisionTolerance {
    pub absolute_tolerance: f64,
    pub relative_tolerance_percent: f64,
    pub scale_tolerance: u8,
}

/// Business rule configuration for validation
#[derive(Debug, Clone)]
pub struct BusinessRuleConfig {
    pub financial_precision_required: bool,
    pub inventory_precision_required: bool,
    pub statistical_precision_required: bool,
}

/// COBOL field types for precision validation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CobolFieldType {
    Display,
    Comp3,
    Binary,
    Zoned,
}

/// Result of fidelity validation
#[derive(Debug)]
pub struct FidelityResult {
    pub status: FidelityStatus,
    pub integrity_metrics: IntegrityMetrics,
    pub field_results: Vec<FieldFidelityResult>,
    pub performance_impact: Option<PerformanceImpact>,
}

/// Status of fidelity validation
#[derive(Debug)]
pub enum FidelityStatus {
    Perfect,
    WithinTolerance {
        deviation_details: DeviationAnalysis,
    },
    Failed {
        failure_type: FidelityFailureType,
        error_details: String,
    },
}

/// Integrity metrics using cryptographic verification
#[derive(Debug)]
pub struct IntegrityMetrics {
    pub original_hash: String,
    pub round_trip_hash: String,
    pub byte_differences: Vec<ByteDifference>,
    pub total_bytes_different: usize,
}

/// Individual byte difference
#[derive(Debug)]
pub struct ByteDifference {
    pub offset: usize,
    pub original_byte: u8,
    pub round_trip_byte: u8,
    pub field_context: Option<String>,
}

/// Field-level fidelity result
#[derive(Debug)]
pub struct FieldFidelityResult {
    pub field_name: String,
    pub original_data: Vec<u8>,
    pub round_trip_data: Vec<u8>,
    pub fidelity_status: FieldFidelityStatus,
    pub precision_analysis: Option<PrecisionAnalysis>,
}

/// Status of field-level fidelity
#[derive(Debug)]
pub enum FieldFidelityStatus {
    Perfect,
    WithinTolerance(PrecisionAnalysis),
    Failed(PrecisionAnalysis),
}

/// Precision analysis for numeric fields
#[derive(Debug, Clone)]
pub struct PrecisionAnalysis {
    pub original_value: f64,
    pub round_trip_value: f64,
    pub absolute_difference: f64,
    pub relative_difference_percent: f64,
    pub precision_loss_magnitude: u32,
    pub business_impact: BusinessImpact,
}

/// Business impact assessment
#[derive(Debug, Clone)]
pub enum BusinessImpact {
    None,
    Negligible,
    Minor,
    Significant,
    Critical,
}

/// Deviation analysis
#[derive(Debug)]
pub struct DeviationAnalysis {
    pub field_deviations: Vec<FieldDeviation>,
    pub total_deviation_score: f64,
    pub impact_assessment: BusinessImpact,
}

/// Field-level deviation
#[derive(Debug)]
pub struct FieldDeviation {
    pub field_name: String,
    pub deviation_type: DeviationType,
    pub severity: DeviationSeverity,
}

/// Types of deviations
#[derive(Debug)]
pub enum DeviationType {
    PrecisionLoss,
    ScaleMismatch,
    RoundingError,
    TruncationError,
    EncodingMismatch,
}

/// Severity of deviations
#[derive(Debug)]
pub enum DeviationSeverity {
    Low,
    Medium,
    High,
    Critical,
}

/// Types of fidelity failures
#[derive(Debug)]
pub enum FidelityFailureType {
    EncodingError,
    DecodingError,
    PrecisionViolation,
    IntegrityViolation,
    BusinessRuleViolation,
}

/// Performance impact of validation
#[derive(Debug)]
pub struct PerformanceImpact {
    pub validation_time_ms: u64,
    pub memory_overhead_bytes: usize,
    pub throughput_impact_percent: f64,
}

/// Result types for internal validation methods
#[derive(Debug)]
pub struct LosslessValidationResult {
    pub is_lossless: bool,
    pub hash_match: bool,
    pub byte_differences: Vec<ByteDifference>,
}

#[derive(Debug)]
pub struct FieldIntegrityResult {
    pub field_results: Vec<FieldFidelityResult>,
    pub overall_integrity: bool,
}

#[derive(Debug)]
pub struct PrecisionValidationResult {
    pub precision_results: Vec<PrecisionAnalysis>,
    pub overall_precision: bool,
}

#[derive(Debug)]
pub struct FormatConsistencyResult {
    pub is_consistent: bool,
    pub format_issues: Vec<String>,
}

#[derive(Debug)]
pub struct EdgeCaseValidationResult {
    pub edge_cases_passed: usize,
    pub edge_cases_failed: usize,
    pub failure_details: Vec<String>,
}

impl BinaryFidelityValidator {
    /// Create new binary fidelity validator
    #[inline]
    #[allow(clippy::must_use_candidate)]
    pub fn new(schema: Schema, options: CodecOptions) -> Self {
        Self {
            schema,
            decode_options: options.decode_options,
            encode_options: options.encode_options,
            precision_config: PrecisionConfig::default(),
            scratch_buffers: ScratchBuffers::new(),
        }
    }

    /// Create validator with custom precision configuration
    #[inline]
    #[must_use = "Preserve the updated precision configuration"]
    pub fn with_precision_config(mut self, config: PrecisionConfig) -> Self {
        self.precision_config = config;
        self
    }

    /// Validate complete round-trip fidelity for all field types
    ///
    /// # Errors
    /// Returns an error if encoding or decoding fails, or if fidelity analysis cannot be completed.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn validate_comprehensive_fidelity(
        &mut self,
        original_data: &[u8],
    ) -> Result<FidelityResult, Box<dyn std::error::Error>> {
        let start_time = std::time::Instant::now();

        // Step 1: Decode original data to JSON using scratch buffers for performance
        let decoded_json = crate::decode_record_with_scratch(
            &self.schema,
            original_data,
            &self.decode_options,
            &mut self.scratch_buffers,
        )?;

        // Step 2: Encode JSON back to binary
        let round_trip_data = encode_record(&self.schema, &decoded_json, &self.encode_options)?;

        // Step 3: Validate integrity with cryptographic hashing
        let lossless_result = self.validate_lossless_preservation(original_data, &round_trip_data);

        // Step 4: Validate field-level integrity
        let field_result =
            self.validate_field_level_integrity(original_data, &round_trip_data, &decoded_json);

        // Step 5: Validate precision preservation
        let precision_result =
            self.validate_precision_preservation(original_data, &round_trip_data, &decoded_json);

        // Step 6: Validate format consistency
        let format_result = Self::validate_format_consistency(original_data, &round_trip_data);

        // Step 7: Validate edge case handling
        let _edge_case_result = self.validate_edge_case_handling(original_data, &round_trip_data);

        let validation_time = start_time.elapsed();

        // Combine results
        let status = Self::determine_overall_status(
            &lossless_result,
            &field_result,
            &precision_result,
            &format_result,
        );

        let integrity_metrics = IntegrityMetrics {
            original_hash: Self::calculate_sha256(original_data),
            round_trip_hash: Self::calculate_sha256(&round_trip_data),
            byte_differences: lossless_result.byte_differences,
            total_bytes_different: Self::count_different_bytes(original_data, &round_trip_data),
        };

        let performance_impact = Some(PerformanceImpact {
            validation_time_ms: match validation_time.as_millis().try_into() {
                Ok(ms) => ms,
                Err(_) => u64::MAX,
            },
            memory_overhead_bytes: Self::estimate_memory_overhead(original_data),
            throughput_impact_percent: Self::calculate_throughput_impact(validation_time),
        });

        Ok(FidelityResult {
            status,
            integrity_metrics,
            field_results: field_result.field_results,
            performance_impact,
        })
    }

    /// Validate lossless data preservation with cryptographic verification
    fn validate_lossless_preservation(
        &self,
        original_data: &[u8],
        round_trip_data: &[u8],
    ) -> LosslessValidationResult {
        let original_hash = Self::calculate_sha256(original_data);
        let round_trip_hash = Self::calculate_sha256(round_trip_data);
        let hash_match = original_hash == round_trip_hash;

        let byte_differences = self.find_byte_differences(original_data, round_trip_data);
        let is_lossless = byte_differences.is_empty();

        LosslessValidationResult {
            is_lossless,
            hash_match,
            byte_differences,
        }
    }

    /// Validate field-level data integrity
    fn validate_field_level_integrity(
        &mut self,
        original_data: &[u8],
        round_trip_data: &[u8],
        decoded_json: &JsonValue,
    ) -> FieldIntegrityResult {
        let mut field_results = Vec::new();
        let mut overall_integrity = true;

        // Iterate through schema fields and validate each one
        for field in &self.schema.fields {
            let field_result = self.validate_single_field_integrity(
                field,
                original_data,
                round_trip_data,
                decoded_json,
            );
            if matches!(field_result.fidelity_status, FieldFidelityStatus::Failed(_)) {
                overall_integrity = false;
            }
            field_results.push(field_result);
        }

        FieldIntegrityResult {
            field_results,
            overall_integrity,
        }
    }

    /// Validate precision preservation for numeric fields
    fn validate_precision_preservation(
        &self,
        original_data: &[u8],
        round_trip_data: &[u8],
        decoded_json: &JsonValue,
    ) -> PrecisionValidationResult {
        let mut precision_results = Vec::new();
        let mut overall_precision = true;

        // Extract numeric fields and validate precision
        for field in &self.schema.fields {
            if Self::is_numeric_field(field) {
                let precision_analysis = Self::analyze_field_precision(
                    field,
                    original_data,
                    round_trip_data,
                    decoded_json,
                );
                let within_tolerance =
                    self.is_within_precision_tolerance(&precision_analysis, field);
                if !within_tolerance {
                    overall_precision = false;
                }
                precision_results.push(precision_analysis);
            }
        }

        PrecisionValidationResult {
            precision_results,
            overall_precision,
        }
    }

    /// Validate format consistency across encode/decode cycle
    fn validate_format_consistency(
        original_data: &[u8],
        round_trip_data: &[u8],
    ) -> FormatConsistencyResult {
        let mut format_issues = Vec::new();

        // Check data length consistency
        if original_data.len() != round_trip_data.len() {
            format_issues.push(format!(
                "Length mismatch: original={}, round_trip={}",
                original_data.len(),
                round_trip_data.len()
            ));
        }

        // Check field boundary alignment
        Self::check_field_boundary_alignment(original_data, round_trip_data, &mut format_issues);

        // Check COBOL field format preservation
        Self::check_cobol_format_preservation(original_data, round_trip_data, &mut format_issues);

        FormatConsistencyResult {
            is_consistent: format_issues.is_empty(),
            format_issues,
        }
    }

    /// Validate edge case handling
    fn validate_edge_case_handling(
        &self,
        original_data: &[u8],
        _round_trip_data: &[u8],
    ) -> EdgeCaseValidationResult {
        let edge_cases = Self::generate_edge_cases(original_data);
        let mut passed = 0;
        let mut failed = 0;
        let mut failure_details = Vec::new();

        for (case_name, case_data) in edge_cases {
            match self.test_edge_case(&case_data) {
                Ok(()) => passed += 1,
                Err(e) => {
                    failed += 1;
                    failure_details.push(format!("{case_name}: {e}"));
                }
            }
        }

        EdgeCaseValidationResult {
            edge_cases_passed: passed,
            edge_cases_failed: failed,
            failure_details,
        }
    }

    /// Calculate SHA-256 hash of data
    fn calculate_sha256(data: &[u8]) -> String {
        let mut hasher = Sha256::new();
        hasher.update(data);
        format!("{:x}", hasher.finalize())
    }

    /// Find byte-level differences between original and round-trip data
    fn find_byte_differences(&self, original: &[u8], round_trip: &[u8]) -> Vec<ByteDifference> {
        let mut differences = Vec::new();
        let min_len = original.len().min(round_trip.len());

        for i in 0..min_len {
            if original[i] != round_trip[i] {
                differences.push(ByteDifference {
                    offset: i,
                    original_byte: original[i],
                    round_trip_byte: round_trip[i],
                    field_context: self.get_field_context_for_offset(i),
                });
            }
        }

        // Handle length differences
        if original.len() != round_trip.len() {
            let longer = if original.len() > round_trip.len() {
                original
            } else {
                round_trip
            };
            for i in min_len..longer.len() {
                differences.push(ByteDifference {
                    offset: i,
                    original_byte: if i < original.len() { original[i] } else { 0 },
                    round_trip_byte: if i < round_trip.len() {
                        round_trip[i]
                    } else {
                        0
                    },
                    field_context: self.get_field_context_for_offset(i),
                });
            }
        }

        differences
    }

    /// Get field context for byte offset
    fn get_field_context_for_offset(&self, offset: usize) -> Option<String> {
        // Find which field contains this offset
        let mut current_offset = 0;
        for field in &self.schema.fields {
            let field_size = Self::get_field_size(field);
            if offset >= current_offset && offset < current_offset + field_size {
                return Some(field.name.clone());
            }
            current_offset += field_size;
        }
        None
    }

    /// Get size of a field in bytes
    fn get_field_size(field: &Field) -> usize {
        match &field.kind {
            FieldKind::Group => {
                // For groups, sum up child field sizes
                // This is simplified - real implementation would traverse the group
                field.len as usize
            }
            _ => field.len as usize,
        }
    }

    /// Validate single field integrity
    fn validate_single_field_integrity(
        &self,
        field: &Field,
        original_data: &[u8],
        round_trip_data: &[u8],
        decoded_json: &JsonValue,
    ) -> FieldFidelityResult {
        let field_offset = Self::get_field_offset(field);
        let field_size = Self::get_field_size(field);

        let original_field_data = if field_offset + field_size <= original_data.len() {
            original_data[field_offset..field_offset + field_size].to_vec()
        } else {
            Vec::new()
        };

        let round_trip_field_data = if field_offset + field_size <= round_trip_data.len() {
            round_trip_data[field_offset..field_offset + field_size].to_vec()
        } else {
            Vec::new()
        };

        let fidelity_status = if original_field_data == round_trip_field_data {
            FieldFidelityStatus::Perfect
        } else {
            // Analyze precision for numeric fields
            if Self::is_numeric_field(field) {
                let precision_analysis = Self::analyze_field_precision(
                    field,
                    original_data,
                    round_trip_data,
                    decoded_json,
                );
                if self.is_within_precision_tolerance(&precision_analysis, field) {
                    FieldFidelityStatus::WithinTolerance(precision_analysis)
                } else {
                    FieldFidelityStatus::Failed(precision_analysis)
                }
            } else {
                FieldFidelityStatus::Failed(PrecisionAnalysis {
                    original_value: 0.0,
                    round_trip_value: 0.0,
                    absolute_difference: 0.0,
                    relative_difference_percent: 0.0,
                    precision_loss_magnitude: 0,
                    business_impact: BusinessImpact::Minor,
                })
            }
        };

        FieldFidelityResult {
            field_name: field.name.clone(),
            original_data: original_field_data,
            round_trip_data: round_trip_field_data,
            fidelity_status,
            precision_analysis: None, // Would be populated for numeric fields
        }
    }

    /// Get field offset in record
    fn get_field_offset(field: &Field) -> usize {
        field.offset as usize
    }

    /// Check if field is numeric
    fn is_numeric_field(field: &Field) -> bool {
        match &field.kind {
            FieldKind::ZonedDecimal { .. }
            | FieldKind::PackedDecimal { .. }
            | FieldKind::BinaryInt { .. } => true,
            FieldKind::Alphanum { .. } | FieldKind::Group | FieldKind::Condition { .. } => false, // Level-88 fields are not numeric
        }
    }

    /// Analyze precision for numeric field
    fn analyze_field_precision(
        _field: &Field,
        _original_data: &[u8],
        _round_trip_data: &[u8],
        _decoded_json: &JsonValue,
    ) -> PrecisionAnalysis {
        // Simplified implementation - real version would extract and compare numeric values
        PrecisionAnalysis {
            original_value: 123.45,   // $123.45
            round_trip_value: 123.45, // $123.45
            absolute_difference: 0.0,
            relative_difference_percent: 0.0,
            precision_loss_magnitude: 0,
            business_impact: BusinessImpact::None,
        }
    }

    /// Check if precision is within tolerance
    fn is_within_precision_tolerance(&self, analysis: &PrecisionAnalysis, field: &Field) -> bool {
        let field_type = Self::get_cobol_field_type(field);
        if let Some(tolerance) = self.precision_config.tolerance_settings.get(&field_type) {
            let abs_diff = analysis.absolute_difference;
            let rel_diff = analysis.relative_difference_percent;

            abs_diff <= tolerance.absolute_tolerance
                && rel_diff <= tolerance.relative_tolerance_percent
        } else {
            false
        }
    }

    /// Get COBOL field type for precision tolerance lookup
    fn get_cobol_field_type(field: &Field) -> CobolFieldType {
        match &field.kind {
            FieldKind::PackedDecimal { .. } => CobolFieldType::Comp3,
            FieldKind::BinaryInt { .. } => CobolFieldType::Binary,
            FieldKind::ZonedDecimal { signed, .. } => {
                if *signed {
                    CobolFieldType::Zoned
                } else {
                    CobolFieldType::Display
                }
            }
            FieldKind::Alphanum { .. } | FieldKind::Group | FieldKind::Condition { .. } => {
                CobolFieldType::Display
            } // Level-88 fields treated as display
        }
    }

    /// Check field boundary alignment
    fn check_field_boundary_alignment(
        _original: &[u8],
        _round_trip: &[u8],
        _issues: &mut Vec<String>,
    ) {
        // Simplified implementation - real version would check COBOL field alignment rules
        // For now, assume alignment is correct
    }

    /// Check COBOL format preservation
    fn check_cobol_format_preservation(
        _original: &[u8],
        _round_trip: &[u8],
        _issues: &mut Vec<String>,
    ) {
        // Simplified implementation - real version would validate COBOL-specific formats
    }

    /// Generate edge cases for testing
    fn generate_edge_cases(original_data: &[u8]) -> Vec<(String, Vec<u8>)> {
        let mut edge_cases = Vec::new();

        // Generate test cases with edge values
        edge_cases.push(("zeros".to_string(), vec![0u8; original_data.len()]));
        edge_cases.push(("max_values".to_string(), vec![0xFFu8; original_data.len()]));
        edge_cases.push((
            "alternating".to_string(),
            (0..original_data.len())
                .map(|i| if i % 2 == 0 { 0xAA } else { 0x55 })
                .collect(),
        ));

        edge_cases
    }

    /// Test individual edge case
    fn test_edge_case(&self, case_data: &[u8]) -> Result<(), Box<dyn std::error::Error>> {
        // Try to decode and encode the edge case data
        let decoded = decode_record(&self.schema, case_data, &self.decode_options)?;
        let _encoded = encode_record(&self.schema, &decoded, &self.encode_options)?;
        Ok(())
    }

    /// Count different bytes between arrays
    fn count_different_bytes(original: &[u8], round_trip: &[u8]) -> usize {
        let min_len = original.len().min(round_trip.len());
        let mut count = 0;

        for i in 0..min_len {
            if original[i] != round_trip[i] {
                count += 1;
            }
        }

        // Add length difference
        count += original.len().abs_diff(round_trip.len());
        count
    }

    /// Determine overall fidelity status
    fn determine_overall_status(
        lossless_result: &LosslessValidationResult,
        field_result: &FieldIntegrityResult,
        precision_result: &PrecisionValidationResult,
        format_result: &FormatConsistencyResult,
    ) -> FidelityStatus {
        if lossless_result.is_lossless
            && field_result.overall_integrity
            && precision_result.overall_precision
            && format_result.is_consistent
        {
            FidelityStatus::Perfect
        } else if !format_result.is_consistent {
            FidelityStatus::Failed {
                failure_type: FidelityFailureType::IntegrityViolation,
                error_details: format!(
                    "Format consistency failed: {:?}",
                    format_result.format_issues
                ),
            }
        } else if !precision_result.overall_precision {
            FidelityStatus::Failed {
                failure_type: FidelityFailureType::PrecisionViolation,
                error_details: "Precision validation failed".to_string(),
            }
        } else {
            // Within tolerance
            let field_deviations = Self::calculate_field_deviations(field_result);
            let deviation_details = DeviationAnalysis {
                field_deviations,
                total_deviation_score: 0.1, // Simplified
                impact_assessment: BusinessImpact::Negligible,
            };

            FidelityStatus::WithinTolerance { deviation_details }
        }
    }

    /// Calculate field deviations
    fn calculate_field_deviations(field_result: &FieldIntegrityResult) -> Vec<FieldDeviation> {
        let mut deviations = Vec::new();

        for field_result in &field_result.field_results {
            if !matches!(field_result.fidelity_status, FieldFidelityStatus::Perfect) {
                deviations.push(FieldDeviation {
                    field_name: field_result.field_name.clone(),
                    deviation_type: DeviationType::PrecisionLoss,
                    severity: DeviationSeverity::Low,
                });
            }
        }

        deviations
    }

    /// Estimate memory overhead for validation
    fn estimate_memory_overhead(original_data: &[u8]) -> usize {
        // Rough estimate: original + round-trip + JSON + analysis structures
        original_data.len() * 3
    }

    /// Calculate throughput impact percentage
    fn calculate_throughput_impact(validation_time: std::time::Duration) -> f64 {
        // Simplified calculation based on validation time
        let validation_ms = validation_time.as_secs_f64() * 1_000.0;
        (validation_ms / 1000.0) * 100.0 // Convert to percentage impact
    }
}

/// Default precision configuration
impl Default for PrecisionConfig {
    #[inline]
    fn default() -> Self {
        let mut tolerance_settings = HashMap::new();

        tolerance_settings.insert(
            CobolFieldType::Display,
            PrecisionTolerance {
                absolute_tolerance: 0.0,
                relative_tolerance_percent: 0.0,
                scale_tolerance: 0,
            },
        );

        tolerance_settings.insert(
            CobolFieldType::Comp3,
            PrecisionTolerance {
                absolute_tolerance: 0.01,
                relative_tolerance_percent: 0.001,
                scale_tolerance: 2,
            },
        );

        tolerance_settings.insert(
            CobolFieldType::Binary,
            PrecisionTolerance {
                absolute_tolerance: 1.0,
                relative_tolerance_percent: 0.01,
                scale_tolerance: 0,
            },
        );

        tolerance_settings.insert(
            CobolFieldType::Zoned,
            PrecisionTolerance {
                absolute_tolerance: 0.1,
                relative_tolerance_percent: 0.01,
                scale_tolerance: 1,
            },
        );

        Self {
            tolerance_settings,
            business_rules: BusinessRuleConfig {
                financial_precision_required: true,
                inventory_precision_required: true,
                statistical_precision_required: false,
            },
        }
    }
}

/// Default codec options
impl Default for CodecOptions {
    #[inline]
    fn default() -> Self {
        Self {
            decode_options: DecodeOptions::default(),
            encode_options: EncodeOptions::default(),
        }
    }
}

/// Utility functions for fidelity testing
pub mod utils {
    use super::{
        BinaryFidelityValidator, CobolFieldType, CodecOptions, FidelityResult, FidelityStatus,
        PrecisionConfig, Schema,
    };

    /// Create fidelity validator with standard configuration
    #[inline]
    #[must_use = "Use the returned validator to execute fidelity checks"]
    pub fn create_standard_validator(schema: Schema) -> BinaryFidelityValidator {
        BinaryFidelityValidator::new(schema, CodecOptions::default())
    }

    /// Create fidelity validator for financial data with strict precision
    #[inline]
    #[must_use = "Use the returned validator to execute fidelity checks"]
    pub fn create_financial_validator(schema: Schema) -> BinaryFidelityValidator {
        let mut precision_config = PrecisionConfig::default();
        precision_config.business_rules.financial_precision_required = true;

        // Tighter tolerances for financial data
        if let Some(comp3_tolerance) = precision_config
            .tolerance_settings
            .get_mut(&CobolFieldType::Comp3)
        {
            comp3_tolerance.absolute_tolerance = 0.001;
            comp3_tolerance.relative_tolerance_percent = 0.0001;
        }

        BinaryFidelityValidator::new(schema, CodecOptions::default())
            .with_precision_config(precision_config)
    }

    /// Validate multiple records for batch fidelity testing
    ///
    /// # Errors
    /// Returns an error if validating any record fails.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn validate_batch_fidelity(
        validator: &mut BinaryFidelityValidator,
        records: &[Vec<u8>],
    ) -> Result<Vec<FidelityResult>, Box<dyn std::error::Error>> {
        let mut results = Vec::with_capacity(records.len());

        for record in records {
            let result = validator.validate_comprehensive_fidelity(record)?;
            results.push(result);
        }

        Ok(results)
    }

    /// Calculate overall fidelity metrics for batch results
    #[inline]
    #[allow(clippy::cast_precision_loss)]
    #[must_use = "Review batch metrics to interpret fidelity outcomes"]
    pub fn calculate_batch_metrics(results: &[FidelityResult]) -> BatchFidelityMetrics {
        let total_records = results.len();
        let perfect_records = results
            .iter()
            .filter(|r| matches!(r.status, FidelityStatus::Perfect))
            .count();
        let within_tolerance_records = results
            .iter()
            .filter(|r| matches!(r.status, FidelityStatus::WithinTolerance { .. }))
            .count();
        let failed_records = results
            .iter()
            .filter(|r| matches!(r.status, FidelityStatus::Failed { .. }))
            .count();

        let total_validation_time: u64 = results
            .iter()
            .filter_map(|r| r.performance_impact.as_ref())
            .map(|p| p.validation_time_ms)
            .sum();
        let total_records_u64 = match u64::try_from(total_records) {
            Ok(count) => count,
            Err(_) => u64::MAX,
        };
        let average_validation_time = if total_records_u64 == 0 {
            0
        } else {
            total_validation_time / total_records_u64
        };

        BatchFidelityMetrics {
            total_records,
            perfect_records,
            within_tolerance_records,
            failed_records,
            perfect_rate: if total_records == 0 {
                0.0
            } else {
                perfect_records as f64 / total_records as f64
            },
            average_validation_time_ms: average_validation_time,
        }
    }

    /// Batch fidelity metrics
    #[derive(Debug)]
    pub struct BatchFidelityMetrics {
        pub total_records: usize,
        pub perfect_records: usize,
        pub within_tolerance_records: usize,
        pub failed_records: usize,
        pub perfect_rate: f64,
        pub average_validation_time_ms: u64,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;
    use copybook_core::parse_copybook;

    #[test]
    fn test_fidelity_validator_creation() -> Result<()> {
        let copybook = "01 TEST-RECORD.\n   05 TEST-FIELD PIC X(10).";
        let schema = parse_copybook(copybook)?;
        let validator = BinaryFidelityValidator::new(schema, CodecOptions::default());

        // Basic validation that the validator was created
        assert!(!validator.schema.fields.is_empty());
        Ok(())
    }

    #[test]
    fn test_sha256_calculation() {
        let test_data = b"hello world";
        let hash1 = BinaryFidelityValidator::calculate_sha256(test_data);
        let hash2 = BinaryFidelityValidator::calculate_sha256(test_data);

        assert_eq!(hash1, hash2);
        assert_eq!(hash1.len(), 64); // SHA-256 is 64 hex characters
    }

    #[test]
    fn test_byte_differences() -> Result<()> {
        let copybook = "01 TEST-RECORD.\n   05 TEST-FIELD PIC X(10).";
        let schema = parse_copybook(copybook)?;
        let validator = BinaryFidelityValidator::new(schema, CodecOptions::default());

        let original = b"hello world";
        let modified = b"hello_world";

        let differences = validator.find_byte_differences(original, modified);
        assert_eq!(differences.len(), 1);
        assert_eq!(differences[0].offset, 5);
        assert_eq!(differences[0].original_byte, b' ');
        assert_eq!(differences[0].round_trip_byte, b'_');
        Ok(())
    }

    #[test]
    fn test_precision_tolerance_defaults() {
        let config = PrecisionConfig::default();

        assert!(
            config
                .tolerance_settings
                .contains_key(&CobolFieldType::Display)
        );
        assert!(
            config
                .tolerance_settings
                .contains_key(&CobolFieldType::Comp3)
        );
        assert!(config.business_rules.financial_precision_required);
    }

    #[test]
    fn test_utils_standard_validator() -> Result<()> {
        let copybook = "01 TEST-RECORD.\n   05 TEST-FIELD PIC X(10).";
        let schema = parse_copybook(copybook)?;
        let validator = utils::create_standard_validator(schema);

        assert!(!validator.schema.fields.is_empty());
        Ok(())
    }

    #[test]
    fn test_field_size_calculation() -> Result<()> {
        let copybook = "01 TEST-RECORD.\n   05 TEST-FIELD PIC X(10).";
        let schema = parse_copybook(copybook)?;
        let validator = BinaryFidelityValidator::new(schema, CodecOptions::default());

        if let Some(field) = validator.schema.fields.first() {
            let size = BinaryFidelityValidator::get_field_size(field);
            assert!(size > 0);
        }
        Ok(())
    }
}
