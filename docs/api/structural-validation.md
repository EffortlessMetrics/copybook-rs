# Structural Validation API Contracts

## Document Information

- **Document Type**: API Contract Specification
- **Component**: copybook-rs Structural Validation API
- **Issue**: #53 - Golden Fixtures Enhancement
- **Status**: Implementation Ready
- **Version**: 1.0
- **Date**: 2025-09-25

## Executive Summary

This specification defines the API contracts for structural validation in copybook-rs, including parsing interfaces, validation rules, error handling, and performance requirements for COBOL structural elements including ODO, Level-88, and REDEFINES clauses.

## Core API Contracts

### Primary Parsing Interface

```rust
/// Parse copybook with structural validation
///
/// # Arguments
/// * `copybook_text` - COBOL copybook source text
/// * `options` - Optional parsing configuration
///
/// # Returns
/// * `Result<Schema, Error>` - Parsed schema with validated structure
///
/// # Errors
/// * `CBKP021_ODO_NOT_TAIL` - ODO array not at tail position
/// * `CBKP001_SYNTAX` - General syntax errors
/// * `CBKP011_UNSUPPORTED_CLAUSE` - Unsupported COBOL features
///
/// # Performance Contract
/// * Parse time: <100ms for enterprise copybooks (<10KB)
/// * Memory usage: <64 MiB peak for complex structures
/// * Deterministic results across platforms and runs
pub fn parse_copybook(copybook_text: &str) -> Result<Schema, Error>;

/// Parse copybook with explicit validation options
pub fn parse_copybook_with_options(
    copybook_text: &str,
    options: &ParseOptions
) -> Result<Schema, Error>;
```

### Enhanced Parse Options

```rust
/// Comprehensive parsing configuration for structural validation
#[derive(Debug, Clone, PartialEq)]
pub struct ParseOptions {
    /// Enable strict structural validation (default: true)
    pub strict_structural_validation: bool,

    /// Allow COBOL-2002 inline comments (*>) (default: true)
    pub allow_inline_comments: bool,

    /// Maximum record size limit in bytes (default: 16_777_216)
    pub max_record_size: usize,

    /// Maximum ODO array size (default: 1_000_000)
    pub max_odo_elements: usize,

    /// Maximum nesting depth (default: 50)
    pub max_nesting_depth: usize,

    /// Enable enterprise compatibility mode (default: true)
    pub enterprise_mode: bool,

    /// Performance validation mode (default: false)
    pub performance_validation: bool,
}

impl Default for ParseOptions {
    fn default() -> Self {
        Self {
            strict_structural_validation: true,
            allow_inline_comments: true,
            max_record_size: 16_777_216,
            max_odo_elements: 1_000_000,
            max_nesting_depth: 50,
            enterprise_mode: true,
            performance_validation: false,
        }
    }
}
```

### Schema API Contract

```rust
/// Parsed COBOL schema with validated structural elements
#[derive(Debug, Clone)]
pub struct Schema {
    /// Schema metadata including validation info
    metadata: SchemaMetadata,

    /// Hierarchical field structure
    root_field: Field,

    /// ODO validation information
    odo_info: OdoValidationInfo,

    /// Level-88 condition mappings
    level88_mappings: Level88Mappings,

    /// REDEFINES structure information
    redefines_info: RedefinesInfo,
}

impl Schema {
    /// Get all fields in traversal order with structural validation
    ///
    /// # Performance Contract
    /// * O(n) complexity where n is field count
    /// * <1ms execution time for typical schemas
    /// * Zero allocations for read-only access
    pub fn all_fields(&self) -> &[Field];

    /// Find field by path with ODO context resolution
    ///
    /// # Arguments
    /// * `path` - Dot-separated field path (e.g., "ROOT.CUSTOMER.ORDERS")
    ///
    /// # Returns
    /// * `Option<&Field>` - Field reference if found
    ///
    /// # Performance Contract
    /// * O(log n) average case with field indexing
    /// * <100μs for typical field lookups
    pub fn find_field(&self, path: &str) -> Option<&Field>;

    /// Get ODO validation information for runtime processing
    pub fn odo_info(&self) -> &OdoValidationInfo;

    /// Get Level-88 condition mappings
    pub fn level88_mappings(&self) -> &Level88Mappings;

    /// Get REDEFINES structure information
    pub fn redefines_info(&self) -> &RedefinesInfo;

    /// Validate schema against structural constraints
    ///
    /// # Returns
    /// * `Vec<ValidationResult>` - All validation results
    ///
    /// # Performance Contract
    /// * <50ms validation time for complex schemas
    /// * Linear complexity with field count
    pub fn validate_structure(&self) -> Vec<ValidationResult>;
}
```

### Structural Validation Information

```rust
/// ODO validation and runtime information
#[derive(Debug, Clone)]
pub struct OdoValidationInfo {
    /// Map of ODO field paths to their validation metadata
    pub odo_fields: HashMap<String, OdoFieldInfo>,

    /// Counter field relationships
    pub counter_mappings: HashMap<String, String>,

    /// ODO tail validation results
    pub tail_validation: Vec<TailValidationResult>,
}

#[derive(Debug, Clone)]
pub struct OdoFieldInfo {
    /// Field path of the ODO array
    pub field_path: String,

    /// Path to the counter field
    pub counter_path: String,

    /// Minimum occurrence count
    pub min_occurs: u32,

    /// Maximum occurrence count
    pub max_occurs: u32,

    /// Whether this ODO is at tail position (valid)
    pub is_tail_valid: bool,

    /// Computed maximum memory consumption
    pub max_memory_bytes: usize,
}

/// Level-88 condition name information
#[derive(Debug, Clone)]
pub struct Level88Mappings {
    /// Map of condition names to their parent fields
    pub condition_parents: HashMap<String, String>,

    /// Map of parent fields to their conditions
    pub parent_conditions: HashMap<String, Vec<String>>,

    /// Condition value mappings
    pub condition_values: HashMap<String, Level88Values>,
}

#[derive(Debug, Clone)]
pub struct Level88Values {
    /// Single values (VALUE 'A')
    pub single_values: Vec<String>,

    /// Range values (VALUE 1 THRU 10)
    pub range_values: Vec<(String, String)>,

    /// Multiple values (VALUE 'A', 'B', 'C')
    pub multiple_values: Vec<Vec<String>>,
}

/// REDEFINES structure information
#[derive(Debug, Clone)]
pub struct RedefinesInfo {
    /// Map of redefining fields to their redefined targets
    pub redefines_mappings: HashMap<String, String>,

    /// Storage size validation for redefines
    pub size_validations: HashMap<String, SizeValidation>,

    /// Level-88 conditions on redefined storage
    pub redefines_conditions: HashMap<String, Vec<String>>,
}

#[derive(Debug, Clone)]
pub struct SizeValidation {
    /// Original field size in bytes
    pub original_size: usize,

    /// Redefining field size in bytes
    pub redefining_size: usize,

    /// Whether the redefinition is valid
    pub is_valid: bool,

    /// Size difference (positive = excess, negative = underflow)
    pub size_difference: i64,
}
```

### Validation API Contract

```rust
/// Structural validation interface
pub trait StructuralValidator {
    /// Validate ODO structural constraints
    ///
    /// # Arguments
    /// * `schema` - Schema to validate
    ///
    /// # Returns
    /// * `Vec<OdoValidationResult>` - All ODO validation results
    ///
    /// # Performance Contract
    /// * <10ms for complex schemas with multiple ODO fields
    /// * Linear complexity with ODO count
    fn validate_odo_constraints(&self, schema: &Schema) -> Vec<OdoValidationResult>;

    /// Validate Level-88 placement and values
    fn validate_level88_constraints(&self, schema: &Schema) -> Vec<Level88ValidationResult>;

    /// Validate REDEFINES structural correctness
    fn validate_redefines_constraints(&self, schema: &Schema) -> Vec<RedefinesValidationResult>;

    /// Validate nested structure constraints
    fn validate_nesting_constraints(&self, schema: &Schema) -> Vec<NestingValidationResult>;
}

/// Default structural validator implementation
pub struct DefaultStructuralValidator {
    config: StructuralValidationConfig,
}

impl DefaultStructuralValidator {
    /// Create validator with default enterprise-grade configuration
    pub fn new() -> Self;

    /// Create validator with custom configuration
    pub fn with_config(config: StructuralValidationConfig) -> Self;
}

#[derive(Debug, Clone)]
pub struct StructuralValidationConfig {
    /// Enable strict validation mode (default: true)
    pub strict_mode: bool,

    /// Maximum record size allowed (default: 16 MiB)
    pub max_record_size: usize,

    /// Performance validation enabled (default: false)
    pub performance_validation: bool,

    /// Enterprise compatibility checks (default: true)
    pub enterprise_compatibility: bool,
}
```

### Validation Result Types

```rust
/// ODO-specific validation results
#[derive(Debug, Clone, PartialEq)]
pub struct OdoValidationResult {
    /// Field path of the ODO array
    pub field_path: String,

    /// Validation outcome
    pub result: ValidationOutcome,

    /// Specific ODO validation issues
    pub issues: Vec<OdoValidationIssue>,

    /// Performance metrics for this ODO field
    pub performance: Option<OdoPerformanceMetrics>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValidationOutcome {
    /// Validation passed completely
    Valid,

    /// Validation passed with warnings
    ValidWithWarnings,

    /// Validation failed with errors
    Invalid,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OdoValidationIssue {
    /// Error code for programmatic handling
    pub error_code: ErrorCode,

    /// Human-readable issue description
    pub description: String,

    /// Severity level of the issue
    pub severity: ValidationSeverity,

    /// Context information for debugging
    pub context: ValidationContext,

    /// Suggested remediation steps
    pub remediation: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValidationSeverity {
    /// Critical error preventing processing
    Error,

    /// Warning about potential issues
    Warning,

    /// Informational message
    Info,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValidationContext {
    /// Line number in source copybook
    pub line_number: Option<u32>,

    /// Column number in source copybook
    pub column_number: Option<u32>,

    /// Parent group field path
    pub parent_group: Option<String>,

    /// Counter field path (for ODO)
    pub counter_field: Option<String>,

    /// Additional context key-value pairs
    pub additional: HashMap<String, String>,
}
```

### Runtime Validation API

```rust
/// Runtime ODO bounds validation during data processing
pub trait OdoBoundsValidator {
    /// Validate ODO counter value and return clamped value
    ///
    /// # Arguments
    /// * `field_path` - Path to the ODO field
    /// * `counter_value` - Runtime counter value
    /// * `mode` - Validation mode (strict/lenient)
    ///
    /// # Returns
    /// * `Result<(u32, Vec<ValidationWarning>)>` - Clamped value and warnings
    ///
    /// # Errors
    /// * `CBKS301_ODO_CLIPPED` - Counter exceeded maximum (warning in lenient)
    /// * `CBKS302_ODO_RAISED` - Counter below minimum (warning in lenient)
    ///
    /// # Performance Contract
    /// * <1μs per validation call
    /// * Zero allocations in happy path
    fn validate_odo_bounds(
        &self,
        field_path: &str,
        counter_value: u32,
        mode: ValidationMode,
    ) -> Result<(u32, Vec<ValidationWarning>), Error>;

    /// Pre-compute validation information for hot path optimization
    fn precompute_bounds_info(&mut self, schema: &Schema) -> Result<(), Error>;
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValidationMode {
    /// Strict mode - fail on bounds violations
    Strict,

    /// Lenient mode - clamp values and warn
    Lenient,
}

#[derive(Debug, Clone)]
pub struct ValidationWarning {
    pub error_code: ErrorCode,
    pub message: String,
    pub context: ValidationContext,
}
```

### Performance Monitoring API

```rust
/// Performance validation and monitoring for structural elements
pub trait StructuralPerformanceMonitor {
    /// Measure parsing performance for structural validation
    ///
    /// # Returns
    /// * `StructuralPerformanceMetrics` - Detailed timing and memory metrics
    fn measure_parsing_performance(&self, copybook: &str) -> StructuralPerformanceMetrics;

    /// Measure validation performance
    fn measure_validation_performance(&self, schema: &Schema) -> ValidationPerformanceMetrics;

    /// Establish performance baseline for regression detection
    fn establish_baseline(&mut self, metrics: &[StructuralPerformanceMetrics]) -> PerformanceBaseline;

    /// Detect performance regressions
    fn detect_regression(
        &self,
        current: &StructuralPerformanceMetrics,
        baseline: &PerformanceBaseline,
    ) -> RegressionReport;
}

#[derive(Debug, Clone)]
pub struct StructuralPerformanceMetrics {
    /// Total parsing time in microseconds
    pub parse_time_us: u64,

    /// Structural validation time in microseconds
    pub validation_time_us: u64,

    /// Peak memory usage in bytes
    pub peak_memory_bytes: usize,

    /// Number of fields processed
    pub field_count: usize,

    /// Number of ODO fields processed
    pub odo_field_count: usize,

    /// Number of Level-88 conditions processed
    pub level88_count: usize,

    /// Performance per field type
    pub field_type_metrics: HashMap<String, FieldTypeMetrics>,
}

#[derive(Debug, Clone)]
pub struct FieldTypeMetrics {
    /// Count of this field type
    pub count: usize,

    /// Average processing time per field in nanoseconds
    pub avg_time_ns: u64,

    /// Memory overhead per field in bytes
    pub memory_overhead_bytes: usize,
}
```

### Error Handling API Contract

```rust
/// Enhanced error information for structural validation
#[derive(Debug, Clone, PartialEq)]
pub struct StructuralError {
    /// Base error information
    pub base: Error,

    /// Structural validation specific context
    pub structural_context: StructuralContext,

    /// Related validation results
    pub related_validations: Vec<ValidationResult>,

    /// Performance impact information
    pub performance_impact: Option<PerformanceImpact>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructuralContext {
    /// Type of structural element involved
    pub element_type: StructuralElementType,

    /// Hierarchy information
    pub hierarchy_path: Vec<String>,

    /// Sibling elements that may be affected
    pub affected_siblings: Vec<String>,

    /// Parent group constraints
    pub parent_constraints: Option<GroupConstraints>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StructuralElementType {
    OdoArray,
    Level88Condition,
    RedefinesClause,
    GroupField,
    ElementaryField,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GroupConstraints {
    /// Whether this group contains ODO fields
    pub has_odo: bool,

    /// Maximum size of the group
    pub max_size_bytes: usize,

    /// Nesting level
    pub nesting_level: u32,
}
```

## Usage Examples

### Basic Structural Validation

```rust
use copybook_core::{parse_copybook, ParseOptions, ErrorCode};

// Parse with default structural validation
let schema = parse_copybook(copybook_text)?;

// Parse with custom options
let options = ParseOptions {
    strict_structural_validation: true,
    max_odo_elements: 500_000,
    enterprise_mode: true,
    ..Default::default()
};

let schema = parse_copybook_with_options(copybook_text, &options)?;

// Access structural information
let odo_info = schema.odo_info();
for (field_path, info) in &odo_info.odo_fields {
    println!("ODO field: {} (tail_valid: {})", field_path, info.is_tail_valid);
}
```

### Runtime ODO Validation

```rust
use copybook_core::{OdoBoundsValidator, ValidationMode};

let validator = schema.create_odo_validator()?;

// Validate counter value during processing
let counter_value = 150;
match validator.validate_odo_bounds("ROOT.ORDERS", counter_value, ValidationMode::Lenient) {
    Ok((clamped_value, warnings)) => {
        if !warnings.is_empty() {
            for warning in warnings {
                eprintln!("ODO Warning: {}", warning.message);
            }
        }
        // Use clamped_value for processing
    }
    Err(e) => {
        // Handle validation error
        match e.code {
            ErrorCode::CBKS301_ODO_CLIPPED => {
                // Handle clipping in strict mode
            }
            _ => return Err(e),
        }
    }
}
```

### Performance Monitoring Integration

```rust
use copybook_core::StructuralPerformanceMonitor;

let monitor = DefaultStructuralPerformanceMonitor::new();

// Measure parsing performance
let metrics = monitor.measure_parsing_performance(copybook_text);
println!("Parse time: {}μs", metrics.parse_time_us);
println!("Validation time: {}μs", metrics.validation_time_us);

// Establish baseline for regression detection
let baseline = monitor.establish_baseline(&[metrics]);

// Later, check for regressions
let current_metrics = monitor.measure_parsing_performance(copybook_text);
let regression_report = monitor.detect_regression(&current_metrics, &baseline);

if regression_report.has_regression() {
    eprintln!("Performance regression detected: {:?}", regression_report);
}
```

## API Stability Guarantees

### Semantic Versioning Contract

- **Major Version**: Breaking changes to public API contracts
- **Minor Version**: New functionality, backward-compatible additions
- **Patch Version**: Bug fixes, performance improvements, no API changes

### Deprecation Policy

- **Deprecation Notice**: Minimum 6 months before removal
- **Migration Path**: Clear upgrade documentation provided
- **Legacy Support**: Previous major version supported for 12 months

### Performance SLA

- **Parse Performance**: <100ms for enterprise copybooks (<10KB)
- **Validation Performance**: <50ms for complex structural validation
- **Memory Usage**: <256 MiB steady state for large schemas
- **Regression Tolerance**: <5% performance variance between releases

This API specification establishes the comprehensive contracts for structural validation in copybook-rs, ensuring enterprise-grade reliability and performance while maintaining clear, documented interfaces for all structural validation operations.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
