# ADR-003: Binary Fidelity Validation for Round-trip Consistency Testing

## Status
Accepted

## Context
copybook-rs processes enterprise mainframe data through encoding/decoding cycles that must preserve data integrity with absolute precision. Current testing validates basic round-trip functionality, but lacks comprehensive fidelity validation across all COBOL field types, edge cases, and enterprise data patterns.

The enterprise deployment requirements demand:
- Lossless data preservation across encode/decode cycles for all COBOL field types
- Validation of precision preservation for COMP-3 packed decimal fields
- Round-trip consistency testing with real-world mainframe data patterns
- Comprehensive edge case validation (boundary values, precision limits, format variations)
- Cryptographic integrity verification for large-scale data processing

Current gaps include:
- Limited field-type-specific fidelity testing
- Insufficient validation of precision preservation in numeric conversions
- Lack of comprehensive edge case coverage
- Missing enterprise-scale fidelity validation under stress conditions

## Decision
We will implement a comprehensive binary fidelity validation framework that ensures absolute data integrity across all encode/decode operations through systematic round-trip testing, precision validation, and cryptographic integrity verification.

### Key Architectural Decisions:

#### 1. Field-Type-Specific Fidelity Testing
- **Approach**: Comprehensive validation for each COBOL field type with type-specific fidelity requirements
- **Rationale**: Different field types have different fidelity requirements (lossless vs precision-aware)
- **Implementation**: Per-field-type validation with specialized test patterns and verification methods

#### 2. Cryptographic Integrity Verification
- **Approach**: SHA-256 hash validation for complete data integrity verification
- **Rationale**: Provides absolute certainty of data preservation with cryptographic guarantees
- **Implementation**: Hash-based comparison with byte-level difference analysis for failures

#### 3. Precision-Aware Validation for Numeric Types
- **Approach**: Specialized validation for COMP-3 and numeric fields with precision requirements
- **Rationale**: Numeric fields may have acceptable precision boundaries while maintaining business accuracy
- **Implementation**: Configurable precision tolerance with business rule validation

#### 4. Enterprise Pattern Validation
- **Approach**: Real-world data pattern testing with mainframe-generated data samples
- **Rationale**: Ensures fidelity validation covers actual enterprise deployment scenarios
- **Implementation**: Integration with enterprise fixture generation for authentic data testing

## Architecture

### Binary Fidelity Validation Framework

```rust
// /tests/enterprise/binary_fidelity.rs
pub struct BinaryFidelityValidator {
    schema: Schema,
    options: CodecOptions,
    precision_config: PrecisionConfig,
}

impl BinaryFidelityValidator {
    /// Validate complete round-trip fidelity for all field types
    pub fn validate_comprehensive_fidelity(&self, original_data: &[u8]) -> FidelityResult {
        let fidelity_tests = vec![
            self.validate_lossless_preservation(original_data),
            self.validate_field_level_integrity(original_data),
            self.validate_precision_preservation(original_data),
            self.validate_format_consistency(original_data),
            self.validate_edge_case_handling(original_data),
        ];

        self.aggregate_fidelity_results(fidelity_tests)
    }

    /// Validate lossless data preservation with cryptographic verification
    fn validate_lossless_preservation(&self, original_data: &[u8]) -> LosslessValidationResult {
        // Decode original data to JSON
        let decoded_json = decode_record(&self.schema, original_data, &self.options)
            .map_err(|e| FidelityError::DecodeFailure(e.to_string()))?;

        // Encode JSON back to binary
        let encoded_data = encode_record(&self.schema, &decoded_json, &self.options.into())
            .map_err(|e| FidelityError::EncodeFailure(e.to_string()))?;

        // Cryptographic integrity verification
        let original_hash = self.calculate_sha256(original_data);
        let round_trip_hash = self.calculate_sha256(&encoded_data);

        if original_hash == round_trip_hash {
            LosslessValidationResult::Perfect
        } else {
            // Detailed byte-level analysis for debugging
            let byte_differences = self.analyze_byte_differences(original_data, &encoded_data);
            LosslessValidationResult::Failed {
                original_hash,
                round_trip_hash,
                byte_differences,
            }
        }
    }
}
```

### Field-Type-Specific Validation

```rust
// /tests/enterprise/field_fidelity.rs
pub struct FieldFidelityTester {
    field_validators: HashMap<CobolFieldType, Box<dyn FieldValidator>>,
}

impl FieldFidelityTester {
    pub fn new() -> Self {
        let mut validators: HashMap<CobolFieldType, Box<dyn FieldValidator>> = HashMap::new();

        // Register field-type-specific validators
        validators.insert(CobolFieldType::Display, Box::new(DisplayValidator::new()));
        validators.insert(CobolFieldType::Comp3, Box::new(Comp3Validator::new()));
        validators.insert(CobolFieldType::Binary, Box::new(BinaryValidator::new()));
        validators.insert(CobolFieldType::Zoned, Box::new(ZonedValidator::new()));

        Self { field_validators: validators }
    }

    pub fn validate_field_fidelity(&self, field: &Field, original_data: &[u8]) -> FieldFidelityResult {
        let validator = self.field_validators.get(&field.field_type)
            .ok_or_else(|| FidelityError::UnsupportedFieldType(field.field_type))?;

        validator.validate_field_fidelity(field, original_data)
    }
}

/// Specialized validator for COMP-3 packed decimal fields
pub struct Comp3Validator {
    precision_tolerance: f64,
    scale_tolerance: u8,
}

impl FieldValidator for Comp3Validator {
    fn validate_field_fidelity(&self, field: &Field, original_data: &[u8]) -> FieldFidelityResult {
        // Extract COMP-3 field data
        let field_data = self.extract_field_data(field, original_data)?;

        // Decode to decimal representation
        let decoded_decimal = self.decode_comp3_field(&field_data)?;

        // Encode back to COMP-3
        let encoded_data = self.encode_comp3_field(decoded_decimal, field.precision, field.scale)?;

        // Precision-aware comparison
        let fidelity_result = if field_data == encoded_data {
            FieldFidelityStatus::Perfect
        } else {
            // Analyze precision differences
            let precision_analysis = self.analyze_precision_differences(&field_data, &encoded_data)?;
            if precision_analysis.within_tolerance(self.precision_tolerance, self.scale_tolerance) {
                FieldFidelityStatus::WithinTolerance(precision_analysis)
            } else {
                FieldFidelityStatus::Failed(precision_analysis)
            }
        };

        FieldFidelityResult {
            field_name: field.name.clone(),
            original_data: field_data,
            round_trip_data: encoded_data,
            fidelity_status: fidelity_result,
            precision_analysis: Some(precision_analysis),
        }
    }
}
```

### Precision Analysis Framework

```rust
// /tests/enterprise/precision_analysis.rs
pub struct PrecisionAnalyzer {
    business_rules: BusinessRuleConfig,
}

impl PrecisionAnalyzer {
    pub fn analyze_numeric_precision(&self, field: &Field, original: &[u8], round_trip: &[u8]) -> PrecisionAnalysis {
        match field.field_type {
            CobolFieldType::Comp3 => self.analyze_comp3_precision(field, original, round_trip),
            CobolFieldType::Zoned => self.analyze_zoned_precision(field, original, round_trip),
            CobolFieldType::Display if field.is_numeric() => self.analyze_display_numeric_precision(field, original, round_trip),
            _ => PrecisionAnalysis::NotApplicable,
        }
    }

    fn analyze_comp3_precision(&self, field: &Field, original: &[u8], round_trip: &[u8]) -> PrecisionAnalysis {
        // Decode both values to high-precision decimal
        let original_decimal = self.decode_comp3_to_decimal(original, field.precision, field.scale)?;
        let round_trip_decimal = self.decode_comp3_to_decimal(round_trip, field.precision, field.scale)?;

        // Calculate precision differences
        let absolute_difference = (original_decimal - round_trip_decimal).abs();
        let relative_difference = if original_decimal != Decimal::ZERO {
            absolute_difference / original_decimal.abs()
        } else {
            Decimal::ZERO
        };

        // Analyze business impact
        let business_impact = self.assess_business_impact(field, absolute_difference, relative_difference);

        PrecisionAnalysis {
            original_value: original_decimal,
            round_trip_value: round_trip_decimal,
            absolute_difference,
            relative_difference_percent: relative_difference * Decimal::from(100),
            precision_loss_magnitude: self.calculate_precision_loss_magnitude(absolute_difference, field.scale),
            business_impact,
            within_tolerance: self.is_within_business_tolerance(&business_impact),
        }
    }
}
```

### Enterprise Pattern Testing

```rust
// /tests/enterprise/pattern_fidelity.rs
pub struct EnterprisePatternFidelityTester {
    enterprise_fixtures: EnterpriseFixtureSuite,
    fidelity_validator: BinaryFidelityValidator,
}

impl EnterprisePatternFidelityTester {
    pub fn test_all_enterprise_patterns(&self) -> EnterprisePatternFidelityResult {
        let mut pattern_results = Vec::new();

        for fixture in &self.enterprise_fixtures.fixtures {
            let pattern_result = self.test_enterprise_pattern(fixture);
            pattern_results.push(pattern_result);
        }

        self.aggregate_enterprise_results(pattern_results)
    }

    fn test_enterprise_pattern(&self, fixture: &EnterpriseFixture) -> PatternFidelityResult {
        let mut sample_results = Vec::new();

        for data_sample in &fixture.data_samples {
            // Validate fidelity for this data sample
            let fidelity_result = self.fidelity_validator.validate_comprehensive_fidelity(&data_sample.binary_data);

            // Additional enterprise-specific validation
            let enterprise_validation = self.validate_enterprise_characteristics(fixture, data_sample, &fidelity_result);

            sample_results.push(SampleFidelityResult {
                sample_id: data_sample.sample_id.clone(),
                fidelity_result,
                enterprise_validation,
            });
        }

        PatternFidelityResult {
            pattern_type: fixture.pattern_info.pattern_type,
            sample_results,
            overall_status: self.determine_pattern_status(&sample_results),
            performance_impact: self.measure_fidelity_performance_impact(fixture),
        }
    }
}
```

## Implementation Strategy

### Phase 1: Core Fidelity Framework (AC3 Foundation)
**Timeline**: Day 1-2
**Focus**: Establish comprehensive round-trip validation infrastructure

**Technical Implementation**:
1. **Core Validation Engine**:
   - Create `/tests/enterprise/binary_fidelity.rs` with comprehensive validation framework
   - Implement cryptographic integrity verification with SHA-256 hashing
   - Add byte-level difference analysis for failure debugging

2. **Field-Type-Specific Validators**:
   - Implement specialized validators for each COBOL field type
   - Create precision-aware validation for COMP-3 packed decimal fields
   - Add comprehensive edge case validation patterns

**Validation Commands**:
```bash
# Core fidelity framework testing
cargo test --test binary_fidelity_core --features fidelity-validation
cargo test binary_round_trip_comprehensive -- --nocapture
```

### Phase 2: Precision Analysis Implementation
**Timeline**: Day 2-3
**Focus**: Implement sophisticated precision analysis for numeric field types

**Technical Implementation**:
1. **Precision Analysis Engine**:
   - Create comprehensive decimal precision analysis for COMP-3 fields
   - Implement business rule validation for precision tolerance
   - Add statistical analysis of precision preservation

2. **Business Rule Integration**:
   - Define acceptable precision boundaries for different field types
   - Implement configurable tolerance settings
   - Create business impact assessment framework

**Validation Commands**:
```bash
# Precision analysis validation
cargo test --test precision_analysis_comprehensive --features precision-validation
cargo test comp3_precision_preservation -- --nocapture
```

### Phase 3: Enterprise Pattern Integration
**Timeline**: Day 3-4
**Focus**: Integrate fidelity validation with real-world enterprise data patterns

**Technical Implementation**:
1. **Enterprise Pattern Testing**:
   - Integrate fidelity validation with enterprise fixture generation
   - Create comprehensive testing across all enterprise patterns
   - Add performance impact analysis for fidelity validation

2. **Stress Testing Integration**:
   - Validate fidelity preservation under enterprise-scale loads
   - Test fidelity with multi-GB datasets while maintaining memory constraints
   - Ensure deterministic fidelity validation across parallel processing

**Validation Commands**:
```bash
# Enterprise pattern fidelity testing
cargo test --test enterprise_pattern_fidelity --features enterprise-fidelity
cargo run --bin copybook -- verify-fidelity fixtures/enterprise/copybooks/financial.cpy fixtures/enterprise/data/financial_1gb.bin
```

## Validation Framework

### Comprehensive Test Cases

#### DISPLAY Field Fidelity Tests
```rust
#[test] // AC:3
fn test_display_field_fidelity() {
    let test_cases = vec![
        DisplayTestCase::ascii_characters(),
        DisplayTestCase::ebcdic_conversion(),
        DisplayTestCase::boundary_values(),
        DisplayTestCase::unicode_handling(),
    ];

    for test_case in test_cases {
        let fidelity_result = fidelity_validator.validate_display_fidelity(&test_case);
        assert_eq!(fidelity_result.status, FidelityStatus::Perfect,
                   "DISPLAY field fidelity must be lossless: {:?}", test_case);
    }
}
```

#### COMP-3 Precision Fidelity Tests
```rust
#[test] // AC:3
fn test_comp3_precision_fidelity() {
    let precision_tests = vec![
        Comp3TestCase::maximum_precision(),
        Comp3TestCase::decimal_boundaries(),
        Comp3TestCase::signed_values(),
        Comp3TestCase::zero_values(),
    ];

    for test_case in precision_tests {
        let fidelity_result = fidelity_validator.validate_comp3_fidelity(&test_case);

        match fidelity_result.status {
            FidelityStatus::Perfect => {
                // Perfect preservation - no further validation needed
            }
            FidelityStatus::WithinTolerance { deviation_magnitude, .. } => {
                // Validate business tolerance acceptance
                assert!(deviation_magnitude <= test_case.business_tolerance,
                        "COMP-3 precision deviation exceeds business tolerance");
            }
            FidelityStatus::Failed { .. } => {
                panic!("COMP-3 fidelity failed for test case: {:?}", test_case);
            }
        }
    }
}
```

#### Enterprise Scale Fidelity Tests
```rust
#[test] // AC:3
#[ignore] // Run only in enterprise test suite
fn test_enterprise_scale_fidelity() {
    let large_scale_tests = vec![
        ("customer_records_1gb.bin", 1_000_000_000),
        ("financial_data_500mb.bin", 500_000_000),
        ("inventory_mixed_2gb.bin", 2_000_000_000),
    ];

    for (filename, expected_size) in large_scale_tests {
        let file_path = format!("fixtures/enterprise/data/{}", filename);
        let binary_data = std::fs::read(&file_path)
            .expect("Enterprise test data should be available");

        assert_eq!(binary_data.len(), expected_size,
                   "Enterprise test file size mismatch");

        // Validate fidelity under memory constraints
        let memory_monitor = MemoryMonitor::new();
        let fidelity_result = fidelity_validator.validate_comprehensive_fidelity(&binary_data);
        let peak_memory_mb = memory_monitor.peak_usage_mb();

        assert_eq!(fidelity_result.status, FidelityStatus::Perfect,
                   "Enterprise scale fidelity must be lossless");
        assert!(peak_memory_mb < 256,
                "Fidelity validation must respect memory constraints");
    }
}
```

## Error Handling and Diagnostics

### Fidelity Failure Analysis

```rust
pub struct FidelityFailureAnalyzer {
    diagnostic_config: DiagnosticConfig,
}

impl FidelityFailureAnalyzer {
    pub fn analyze_fidelity_failure(&self, failure: &FidelityResult) -> FidelityDiagnostic {
        match &failure.status {
            FidelityStatus::Failed { failure_type, error_details } => {
                let root_cause = self.identify_root_cause(failure_type, error_details);
                let byte_analysis = self.analyze_byte_differences(&failure.integrity_metrics);
                let field_analysis = self.analyze_field_level_issues(&failure.field_results);

                FidelityDiagnostic {
                    root_cause,
                    byte_analysis,
                    field_analysis,
                    recommended_fixes: self.generate_fix_recommendations(&root_cause),
                }
            }
            _ => FidelityDiagnostic::no_failure(),
        }
    }
}
```

### Performance Impact Monitoring

```rust
pub struct FidelityPerformanceMonitor {
    baseline_performance: PerformanceBaseline,
}

impl FidelityPerformanceMonitor {
    pub fn measure_fidelity_impact(&self, test_case: &FidelityTestCase) -> PerformanceImpact {
        // Measure performance without fidelity validation
        let baseline_time = self.measure_baseline_processing(test_case);

        // Measure performance with comprehensive fidelity validation
        let fidelity_time = self.measure_fidelity_processing(test_case);

        PerformanceImpact {
            baseline_duration: baseline_time,
            fidelity_duration: fidelity_time,
            overhead_percent: ((fidelity_time - baseline_time) as f64 / baseline_time as f64) * 100.0,
            throughput_impact: self.calculate_throughput_impact(baseline_time, fidelity_time),
            acceptable: self.is_overhead_acceptable(overhead_percent),
        }
    }
}
```

## Success Criteria

### Quantitative Success Metrics
- **Lossless Preservation**: 100% data integrity for all DISPLAY and binary field types
- **Precision Preservation**: <0.01% precision loss for COMP-3 fields within business tolerance
- **Coverage Completeness**: 100% field type coverage with comprehensive edge case testing
- **Performance Overhead**: <10% processing overhead for fidelity validation
- **Enterprise Scale Validation**: Successful fidelity validation for multi-GB datasets

### Qualitative Success Metrics
- **Production Confidence**: Cryptographic guarantee of data integrity preservation
- **Business Validation**: Precision preservation within acceptable business tolerance
- **Diagnostic Capability**: Comprehensive failure analysis and debugging support
- **Enterprise Readiness**: Validated fidelity across real-world mainframe data patterns

This binary fidelity validation framework ensures absolute data integrity preservation while providing comprehensive diagnostic capabilities for enterprise deployment confidence.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
