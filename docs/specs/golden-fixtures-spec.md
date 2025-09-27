# Golden Fixtures Architecture Specification

## Document Information

- **Document Type**: Technical Specification
- **Component**: copybook-rs Test Suite Enhancement
- **Issue**: #53 - Golden Fixtures for Level-88 after ODO, Child-inside-ODO, Sibling-after-ODO Structural Validation
- **Status**: Implementation Ready
- **Version**: 1.0
- **Date**: 2025-09-25

## Executive Summary

This specification defines the enhanced golden fixture architecture for copybook-rs to provide comprehensive structural validation of COBOL copybook scenarios involving ODO (Occurs Depending On), Level-88 condition values, and REDEFINES clauses. The system ensures production-grade validation of mainframe data structures while maintaining enterprise performance targets.

## Architecture Overview

### Golden Fixture System Components

```
/fixtures/
├── enterprise/              # Enhanced enterprise patterns (new)
│   ├── copybooks/          # Real-world COBOL structures
│   ├── data/               # Corresponding binary samples
│   ├── golden/             # Expected JSON outputs with metadata
│   └── metadata/           # Structural validation metadata
├── copybooks/              # Existing simple patterns
├── data/                   # Existing binary samples
├── golden/                 # Existing expected outputs
└── schemas/                # JSON meta-schemas
```

### Core Design Principles

1. **Structural Validation Completeness**: Comprehensive coverage of ODO, Level-88, and REDEFINES interactions
2. **Enterprise Authenticity**: Real-world mainframe data patterns and constraints
3. **Performance Integration**: Built-in performance regression detection
4. **Production Readiness**: Zero-compromise on existing production stability
5. **Deterministic Behavior**: Reproducible results across environments and versions

## Golden Fixture Categories

### Category 1: ODO Structural Constraints

**Purpose**: Validate OCCURS DEPENDING ON placement rules and interactions

#### Fixture Types:
- **ODO with Level-88 Following**: Validates non-storage fields after ODO arrays
- **ODO with Child Elements**: Validates internal structure of variable arrays
- **ODO with Sibling Storage**: Validates rejection of storage fields after ODO
- **ODO Tail Validation**: Ensures ODO arrays are properly terminated

**Error Codes Covered**:
- `CBKP021_ODO_NOT_TAIL`: ODO array not at tail position
- `CBKS121_COUNTER_NOT_FOUND`: ODO counter field validation
- `CBKS301_ODO_CLIPPED`: ODO bounds enforcement
- `CBKS302_ODO_RAISED`: ODO minimum value validation

### Category 2: Level-88 Conditional Values

**Purpose**: Validate Level-88 condition name placement and semantics

#### Fixture Types:
- **Level-88 After ODO**: Validates conditional values following variable arrays
- **Level-88 Within Groups**: Validates nested conditional structures
- **Level-88 with REDEFINES**: Validates conditional values on redefined storage
- **Level-88 Value Validation**: Ensures proper condition value processing

### Category 3: REDEFINES Interactions

**Purpose**: Validate REDEFINES clause interactions with ODO and Level-88

#### Fixture Types:
- **REDEFINES with ODO**: Validates redefinition of variable-length structures
- **REDEFINES with Level-88**: Validates conditions on redefined storage
- **Nested REDEFINES**: Validates complex redefinition hierarchies
- **REDEFINES Layout Validation**: Ensures proper memory layout

### Category 4: Complex Structural Scenarios

**Purpose**: Validate enterprise-grade structural combinations

#### Fixture Types:
- **Multi-level ODO Hierarchies**: Nested variable-length structures
- **ODO with REDEFINES and Level-88**: Complete structural interaction matrix
- **Enterprise Record Layouts**: Real-world mainframe record structures
- **Performance Stress Scenarios**: Large-scale structural validation

## Technical Architecture

### Fixture Metadata Schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "Golden Fixture Metadata",
  "type": "object",
  "properties": {
    "fixture_id": {
      "type": "string",
      "pattern": "^[a-z0-9_]+$",
      "description": "Unique fixture identifier"
    },
    "category": {
      "type": "string",
      "enum": ["odo_structural", "level_88", "redefines", "complex_scenarios"]
    },
    "structural_elements": {
      "type": "array",
      "items": {
        "type": "string",
        "enum": ["ODO", "LEVEL_88", "REDEFINES", "NESTED_GROUPS"]
      }
    },
    "validation_rules": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "rule_id": {"type": "string"},
          "description": {"type": "string"},
          "expected_result": {"type": "string", "enum": ["PASS", "FAIL"]},
          "error_code": {"type": "string", "pattern": "^CBK[PSKRCDEF][0-9]{3}_[A-Z_]+$"}
        }
      }
    },
    "performance_criteria": {
      "type": "object",
      "properties": {
        "min_throughput_mbps": {"type": "number"},
        "max_memory_mb": {"type": "number"},
        "max_variance_percent": {"type": "number", "maximum": 5.0}
      }
    },
    "enterprise_context": {
      "type": "object",
      "properties": {
        "industry_domain": {"type": "string"},
        "record_type": {"type": "string"},
        "complexity_level": {"type": "string", "enum": ["BASIC", "INTERMEDIATE", "ADVANCED", "ENTERPRISE"]}
      }
    }
  },
  "required": ["fixture_id", "category", "structural_elements", "validation_rules"]
}
```

### Fixture Generation API

```rust
// Enhanced fixture generation architecture
pub struct GoldenFixtureGenerator {
    config: FixtureConfig,
    rng: StdRng,
    metadata_store: MetadataStore,
}

pub struct FixtureConfig {
    pub enterprise_patterns: bool,
    pub performance_validation: bool,
    pub structural_complexity: StructuralComplexity,
    pub target_coverage: CoverageTarget,
}

pub enum StructuralComplexity {
    Basic,      // Simple ODO, Level-88, REDEFINES
    Intermediate, // Nested structures, multiple interactions
    Advanced,   // Complex hierarchies, edge cases
    Enterprise, // Production-grade patterns, stress scenarios
}

impl GoldenFixtureGenerator {
    pub fn generate_odo_fixtures(&mut self) -> Result<Vec<OdoFixture>> {
        // Generate ODO structural validation fixtures
    }

    pub fn generate_level88_fixtures(&mut self) -> Result<Vec<Level88Fixture>> {
        // Generate Level-88 conditional value fixtures
    }

    pub fn generate_redefines_fixtures(&mut self) -> Result<Vec<RedefinesFixture>> {
        // Generate REDEFINES interaction fixtures
    }

    pub fn generate_complex_scenarios(&mut self) -> Result<Vec<ComplexFixture>> {
        // Generate enterprise-grade complex scenarios
    }
}
```

### Validation Framework

```rust
pub struct StructuralValidator {
    config: ValidationConfig,
    error_taxonomy: ErrorTaxonomy,
    performance_monitor: PerformanceMonitor,
}

pub struct ValidationConfig {
    pub strict_mode: bool,
    pub performance_validation: bool,
    pub enterprise_compliance: bool,
    pub regression_detection: bool,
}

impl StructuralValidator {
    pub fn validate_fixture(&self, fixture: &GoldenFixture) -> ValidationResult {
        // Comprehensive structural validation
    }

    pub fn validate_performance(&self, fixture: &GoldenFixture) -> PerformanceResult {
        // Performance regression validation
    }

    pub fn validate_enterprise_compliance(&self, fixture: &GoldenFixture) -> ComplianceResult {
        // Enterprise mainframe compatibility validation
    }
}
```

## Performance Requirements

### Throughput Targets

| Fixture Category | Min Throughput | Performance Variance |
|------------------|----------------|---------------------|
| ODO Structural | 80 MB/s | <3% |
| Level-88 | 100 MB/s | <2% |
| REDEFINES | 75 MB/s | <4% |
| Complex Scenarios | 60 MB/s | <5% |

### Memory Constraints

- **Steady State**: <256 MiB for processing multi-GB fixture sets
- **Peak Memory**: <512 MiB during fixture generation
- **Memory Growth**: <2% per 1000 fixtures processed

### Regression Detection

```rust
pub struct PerformanceBaseline {
    pub fixture_id: String,
    pub throughput_baseline: f64,      // MB/s
    pub memory_baseline: u64,          // bytes
    pub variance_threshold: f64,       // percentage
    pub timestamp: DateTime<Utc>,
}

impl PerformanceMonitor {
    pub fn establish_baseline(&mut self, results: &[BenchmarkResult]) -> PerformanceBaseline {
        // Establish performance baseline for regression detection
    }

    pub fn detect_regression(&self, current: &BenchmarkResult, baseline: &PerformanceBaseline) -> RegressionReport {
        // Detect performance regressions against established baseline
    }
}
```

## Integration Requirements

### Build System Integration

```bash
# Enhanced build targets for golden fixture validation
cargo test --test golden_fixtures_comprehensive    # Run all golden fixtures
cargo test --test golden_fixtures_odo             # ODO-specific fixtures
cargo test --test golden_fixtures_level88         # Level-88 fixtures
cargo test --test golden_fixtures_redefines       # REDEFINES fixtures
cargo test --test golden_fixtures_enterprise      # Enterprise scenarios

# Performance validation integration
PERF=1 cargo bench --package copybook-bench -- golden_fixtures
GOLDEN_FIXTURES=1 cargo test --workspace          # Include golden fixture validation

# Fixture generation
cargo run --bin copybook-gen -- generate-golden-fixtures --enterprise --output fixtures/enterprise/
```

### CI/CD Pipeline Requirements

1. **Fixture Generation**: Automated generation of enterprise fixture sets
2. **Validation Suite**: Comprehensive structural and performance validation
3. **Baseline Management**: Automated performance baseline updates
4. **Regression Detection**: Automated detection of structural or performance regressions
5. **Artifact Storage**: Efficient storage and retrieval of fixture artifacts

### Error Reporting Integration

All golden fixtures integrate with the existing error taxonomy:

```rust
impl GoldenFixture {
    pub fn expected_error(&self) -> Option<ErrorCode> {
        // Return expected error code for negative test cases
    }

    pub fn validate_error(&self, actual: &Error) -> ValidationResult {
        // Validate actual error against expected error code and context
    }
}
```

## Quality Assurance

### Validation Criteria

1. **Structural Correctness**: All fixtures validate expected COBOL structural semantics
2. **Performance Compliance**: All fixtures meet or exceed performance targets
3. **Error Handling**: Negative fixtures generate expected error codes with proper context
4. **Deterministic Behavior**: Fixtures produce identical results across runs and environments
5. **Enterprise Compatibility**: Fixtures represent authentic mainframe data patterns

### Testing Strategy

```rust
#[cfg(test)]
mod golden_fixture_tests {
    #[test]
    fn validate_all_golden_fixtures() {
        // Comprehensive validation of all golden fixtures
    }

    #[test]
    fn validate_performance_baselines() {
        // Validate all fixtures meet performance targets
    }

    #[test]
    fn validate_error_taxonomy_coverage() {
        // Ensure all relevant error codes are covered by fixtures
    }

    #[test]
    fn validate_enterprise_patterns() {
        // Validate enterprise fixture authenticity
    }
}
```

## Implementation Phases

### Phase 1: Core Infrastructure (Days 1-2)
- Implement enhanced fixture metadata schema
- Create GoldenFixtureGenerator framework
- Establish StructuralValidator architecture
- Integrate with existing error taxonomy

### Phase 2: ODO Fixture Suite (Days 3-4)
- Generate comprehensive ODO structural fixtures
- Implement ODO-specific validation rules
- Establish performance baselines for ODO processing
- Validate against existing ODO error codes

### Phase 3: Level-88 and REDEFINES (Days 5-6)
- Generate Level-88 conditional value fixtures
- Implement REDEFINES interaction fixtures
- Create complex structural scenario fixtures
- Validate enterprise compatibility

### Phase 4: Integration and Validation (Days 7-8)
- Integrate with CI/CD pipeline
- Establish automated regression detection
- Validate complete fixture suite
- Document enterprise deployment readiness

## Success Metrics

### Quantitative Metrics

- **Fixture Coverage**: 50+ golden fixtures covering all structural scenarios
- **Performance Preservation**: <2% variance from baseline performance
- **Error Coverage**: 100% coverage of relevant structural error codes
- **Enterprise Patterns**: 20+ authentic mainframe data structure patterns

### Qualitative Metrics

- **Production Confidence**: Enhanced validation of enterprise deployment scenarios
- **Maintainability**: Seamless integration with existing copybook-rs architecture
- **Developer Experience**: Clear patterns for extending golden fixture coverage
- **Documentation Quality**: Comprehensive specification and validation procedures

This specification establishes the foundation for production-ready golden fixture enhancement while maintaining copybook-rs enterprise performance standards and zero-compromise production stability.