<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Structural Validation Error Code Specifications

## Document Information

- **Document Type**: Error Taxonomy Specification
- **Component**: copybook-rs Structural Validation Error Handling
- **Issue**: #53 - Golden Fixtures Enhancement
- **Status**: Implementation Ready
- **Version**: 1.0
- **Date**: 2025-09-25

## Executive Summary

This specification defines the comprehensive error code taxonomy for structural validation in copybook-rs, including ODO constraints, Level-88 validation, REDEFINES interactions, and enterprise production error handling patterns. These error codes provide stable, programmatic interfaces for error handling and troubleshooting in mainframe data processing environments.

## Error Code Architecture

### Structural Error Categories

The copybook-rs error taxonomy uses structured error codes following the pattern `CBK[Category][Number]_[Description]`:

- **CBKP**: Parse-time structural errors (copybook analysis)
- **CBKS**: Schema validation and runtime structural errors
- **CBKR**: Record processing structural errors
- **CBKV**: Validation-specific errors (new category)

### Error Severity Classifications

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StructuralErrorSeverity {
    /// Critical structural violation preventing all processing
    Critical,

    /// Error that prevents specific feature but allows other processing
    Error,

    /// Warning about potential issues that may cause problems
    Warning,

    /// Informational message about structural characteristics
    Info,
}
```

## ODO (Occurs Depending On) Error Codes

### CBKP021_ODO_NOT_TAIL

**Category**: Parse Error
**Severity**: Critical
**Description**: ODO array is not at tail position within its containing group

**When Triggered**: During copybook parsing when an ODO (OCCURS DEPENDING ON) array has storage siblings following it within the same group level.

**Context Information**:
```rust
pub struct OdoNotTailContext {
    /// Path to the ODO field that violates tail constraint
    pub odo_field_path: String,

    /// Path to the containing group
    pub containing_group: String,

    /// List of storage siblings that follow the ODO field
    pub violating_siblings: Vec<String>,

    /// Line number in copybook where ODO is defined
    pub odo_line_number: u32,

    /// Line number where first violating sibling appears
    pub violation_line_number: u32,
}
```

**Example Scenarios**:
```cobol
*> INVALID: Storage field after ODO
01 RECORD-1.
   05 COUNT-FIELD     PIC 9(3).
   05 ITEMS OCCURS 1 TO 5 TIMES DEPENDING ON COUNT-FIELD.
      10 ITEM-NAME    PIC X(10).
   05 TRAILER-FIELD   PIC X(5).    *> Triggers CBKP021_ODO_NOT_TAIL

*> VALID: Non-storage Level-88 after ODO
01 RECORD-2.
   05 COUNT-FIELD     PIC 9(3).
   05 ITEMS OCCURS 1 TO 5 TIMES DEPENDING ON COUNT-FIELD.
      10 ITEM-NAME    PIC X(10).
   88 STATUS-ACTIVE   VALUE 'Y'.   *> Valid: Level-88 is non-storage
```

**Remediation Guidelines**:
1. Move ODO array to the end of its containing group
2. Move violating storage fields to a different group level
3. Convert storage siblings to non-storage elements (Level-88) where appropriate

**Error Message Template**:
```
Error: CBKP021_ODO_NOT_TAIL
ODO field: {odo_field_path}
Group: {containing_group}
Violating siblings: {violating_siblings}
Location: line {odo_line_number}
First violation: line {violation_line_number}

ODO arrays must be the last storage element in their containing group.
Remediation: Move {odo_field_path} to the end of {containing_group},
or move the following fields to a different group: {violating_siblings}
```

### CBKS121_COUNTER_NOT_FOUND

**Category**: Schema Error
**Severity**: Critical
**Description**: ODO counter field is not found or not accessible

**When Triggered**: During schema validation when an ODO array references a counter field that doesn't exist or isn't accessible according to COBOL scoping rules.

**Context Information**:
```rust
pub struct CounterNotFoundContext {
    /// Path to the ODO field with missing counter
    pub odo_field_path: String,

    /// Specified counter field path
    pub counter_field_path: String,

    /// Reason why counter is not found
    pub not_found_reason: CounterNotFoundReason,

    /// List of candidate counter fields in scope
    pub candidate_counters: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CounterNotFoundReason {
    /// Counter field does not exist anywhere in the schema
    DoesNotExist,

    /// Counter field exists but is not in scope for the ODO field
    OutOfScope,

    /// Counter field is defined after the ODO field (invalid order)
    InvalidOrder,

    /// Counter field is within the ODO array itself (circular dependency)
    CircularReference,
}
```

**Example Scenarios**:
```cobol
*> INVALID: Counter field doesn't exist
01 RECORD-1.
   05 ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON MISSING-COUNT.
      10 ITEM-DATA    PIC X(20).    *> Triggers CBKS121_COUNTER_NOT_FOUND

*> INVALID: Counter field after ODO
01 RECORD-2.
   05 ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-DATA    PIC X(20).
   05 ITEM-COUNT      PIC 9(3).     *> Triggers CBKS121_COUNTER_NOT_FOUND

*> VALID: Counter field precedes ODO
01 RECORD-3.
   05 ITEM-COUNT      PIC 9(3).
   05 ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-DATA    PIC X(20).    *> Valid
```

### CBKS301_ODO_CLIPPED and CBKS302_ODO_RAISED

**Category**: Schema Warning
**Severity**: Warning (lenient mode), Error (strict mode)
**Description**: ODO counter value is outside valid bounds

**When Triggered**: During runtime processing when ODO counter values exceed maximum (`CBKS301`) or fall below minimum (`CBKS302`) bounds.

**Context Information**:
```rust
pub struct OdoBoundsContext {
    /// Record number being processed
    pub record_number: u64,

    /// Path to the ODO field
    pub odo_field_path: String,

    /// Actual counter value from data
    pub actual_count: u32,

    /// Minimum allowed count
    pub min_count: u32,

    /// Maximum allowed count
    pub max_count: u32,

    /// Value used after clamping
    pub clamped_count: u32,

    /// Processing mode when violation occurred
    pub validation_mode: ValidationMode,
}
```

## Level-88 Condition Value Error Codes

### CBKV088_INVALID_CONDITION_VALUE

**Category**: Validation Error
**Severity**: Error
**Description**: Level-88 condition has invalid VALUE clause

**When Triggered**: When Level-88 VALUE clauses contain invalid literals or ranges that are incompatible with the parent data item.

**Context Information**:
```rust
pub struct InvalidConditionValueContext {
    /// Path to the Level-88 condition name
    pub condition_path: String,

    /// Path to the parent data item
    pub parent_field_path: String,

    /// Invalid value specification
    pub invalid_value: String,

    /// Parent field's PIC clause
    pub parent_pic: String,

    /// Reason for invalidity
    pub invalidity_reason: ConditionValueInvalidReason,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConditionValueInvalidReason {
    /// Value too long for parent field
    ValueTooLong,

    /// Value type incompatible with parent field
    TypeMismatch,

    /// Invalid range specification (THRU clause)
    InvalidRange,

    /// Numeric value for alphanumeric field or vice versa
    CategoryMismatch,
}
```

**Example Scenarios**:
```cobol
*> INVALID: Value too long for parent field
05 STATUS-CODE       PIC X(1).
   88 LONG-STATUS    VALUE 'ACTIVE'.  *> Triggers CBKV088_INVALID_CONDITION_VALUE

*> INVALID: Numeric value for alpha field
05 NAME-FIELD        PIC X(20).
   88 NUMERIC-NAME   VALUE 12345.     *> Triggers CBKV088_INVALID_CONDITION_VALUE

*> VALID: Proper value specification
05 STATUS-CODE       PIC X(1).
   88 ACTIVE-STATUS  VALUE 'A'.       *> Valid
   88 INACTIVE-STATUS VALUE 'I'.      *> Valid
```

### CBKV089_CONDITION_AFTER_ODO_VALID

**Category**: Validation Info
**Severity**: Info
**Description**: Level-88 condition after ODO is valid (informational)

**When Triggered**: During validation when Level-88 conditions are found after ODO arrays, confirming they are valid non-storage elements.

**Context Information**:
```rust
pub struct ConditionAfterOdoContext {
    /// Path to the Level-88 condition
    pub condition_path: String,

    /// Path to the preceding ODO field
    pub odo_field_path: String,

    /// Confirmation that this is valid placement
    pub placement_valid: bool,

    /// Additional Level-88 conditions in same context
    pub related_conditions: Vec<String>,
}
```

## REDEFINES Interaction Error Codes

### CBKV090_REDEFINES_SIZE_MISMATCH

**Category**: Validation Error
**Severity**: Warning (size smaller), Error (size larger)
**Description**: REDEFINES clause has size mismatch with original definition

**When Triggered**: When a REDEFINES clause specifies storage that is larger than the original field being redefined.

**Context Information**:
```rust
pub struct RedefinesSizeMismatchContext {
    /// Path to the redefining field
    pub redefining_field_path: String,

    /// Path to the original field being redefined
    pub original_field_path: String,

    /// Size of original field in bytes
    pub original_size: usize,

    /// Size of redefining field in bytes
    pub redefining_size: usize,

    /// Size difference (positive = excess, negative = smaller)
    pub size_difference: i64,

    /// Whether this is a critical mismatch
    pub is_critical: bool,
}
```

### CBKV091_REDEFINES_WITH_ODO

**Category**: Validation Warning
**Severity**: Warning
**Description**: REDEFINES applied to variable-length (ODO) structure

**When Triggered**: When REDEFINES clause attempts to redefine storage that contains ODO arrays, creating ambiguous sizing.

**Context Information**:
```rust
pub struct RedefinesWithOdoContext {
    /// Path to the REDEFINES field
    pub redefines_field_path: String,

    /// Path to the original field containing ODO
    pub original_field_path: String,

    /// List of ODO fields within the redefined area
    pub contained_odo_fields: Vec<String>,

    /// Maximum possible size of the redefined area
    pub max_redefined_size: usize,

    /// Minimum possible size of the redefined area
    pub min_redefined_size: usize,
}
```

## Enterprise Production Error Codes

### CBKS141_RECORD_TOO_LARGE

**Category**: Schema Error
**Severity**: Critical
**Description**: Computed record size exceeds implementation limits

**When Triggered**: When the maximum possible record size (accounting for maximum ODO expansions) exceeds configured limits.

**Context Information**:
```rust
pub struct RecordTooLargeContext {
    /// Computed maximum record size in bytes
    pub computed_size: usize,

    /// Implementation maximum limit in bytes
    pub size_limit: usize,

    /// List of ODO fields contributing to size
    pub contributing_odo_fields: Vec<OdoSizeContribution>,

    /// Recommended size limit for this record
    pub recommended_limit: usize,
}

pub struct OdoSizeContribution {
    /// Path to ODO field
    pub field_path: String,

    /// Maximum element count
    pub max_count: u32,

    /// Size per element in bytes
    pub element_size: usize,

    /// Total contribution to record size
    pub total_contribution: usize,
}
```

### CBKV092_NESTING_DEPTH_EXCEEDED

**Category**: Validation Error
**Severity**: Error
**Description**: Structure nesting depth exceeds implementation limits

**When Triggered**: When the depth of nested groups exceeds the configured maximum nesting depth.

**Context Information**:
```rust
pub struct NestingDepthExceededContext {
    /// Current nesting depth
    pub current_depth: u32,

    /// Maximum allowed nesting depth
    pub max_depth: u32,

    /// Path to the deepest nested field
    pub deepest_field_path: String,

    /// Hierarchy of nested groups
    pub nesting_hierarchy: Vec<String>,
}
```

## Performance-Related Error Codes

### CBKV093_PERFORMANCE_WARNING

**Category**: Validation Warning
**Severity**: Warning
**Description**: Structure may impact processing performance

**When Triggered**: When structural characteristics may lead to performance degradation in data processing.

**Context Information**:
```rust
pub struct PerformanceWarningContext {
    /// Type of performance concern
    pub performance_concern: PerformanceConcernType,

    /// Estimated performance impact
    pub impact_severity: PerformanceImpact,

    /// Fields contributing to performance concern
    pub contributing_fields: Vec<String>,

    /// Recommended optimizations
    pub optimizations: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PerformanceConcernType {
    /// Large ODO arrays may cause memory pressure
    LargeOdoArrays,

    /// Deep nesting may cause stack pressure
    DeepNesting,

    /// Many REDEFINES may cause lookup overhead
    ExcessiveRedefines,

    /// Complex Level-88 conditions may cause evaluation overhead
    ComplexConditions,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PerformanceImpact {
    /// Minimal impact, optimization suggested
    Low,

    /// Moderate impact, optimization recommended
    Medium,

    /// Significant impact, optimization strongly recommended
    High,

    /// Critical impact, may prevent production use
    Critical,
}
```

## Error Handling Implementation

### Error Code Registry

```rust
/// Registry of all structural validation error codes
pub struct StructuralErrorCodeRegistry {
    /// Map of error codes to their metadata
    error_metadata: HashMap<ErrorCode, ErrorMetadata>,
}

#[derive(Debug, Clone)]
pub struct ErrorMetadata {
    /// Human-readable description
    pub description: String,

    /// Error category
    pub category: ErrorCategory,

    /// Default severity
    pub default_severity: StructuralErrorSeverity,

    /// Whether error severity can be configured
    pub configurable_severity: bool,

    /// Example remediation steps
    pub remediation_examples: Vec<String>,

    /// Related error codes that may also occur
    pub related_errors: Vec<ErrorCode>,
}

impl StructuralErrorCodeRegistry {
    /// Get comprehensive error information
    pub fn get_error_info(&self, code: ErrorCode) -> Option<&ErrorMetadata>;

    /// Get all errors in a specific category
    pub fn get_category_errors(&self, category: ErrorCategory) -> Vec<ErrorCode>;

    /// Generate error documentation
    pub fn generate_documentation(&self) -> String;
}
```

### Error Context Enrichment

```rust
/// Enrich errors with comprehensive context information
pub trait ErrorContextEnricher {
    /// Add structural context to base error
    fn enrich_error(&self, base_error: Error, schema: &Schema) -> StructuralError;

    /// Add performance context to error
    fn add_performance_context(&self, error: &mut StructuralError, metrics: &PerformanceMetrics);

    /// Add remediation suggestions
    fn add_remediation(&self, error: &mut StructuralError, remediation_db: &RemediationDatabase);
}

/// Database of error remediation patterns
pub struct RemediationDatabase {
    /// Map of error codes to remediation strategies
    remediations: HashMap<ErrorCode, Vec<RemediationStrategy>>,
}

#[derive(Debug, Clone)]
pub struct RemediationStrategy {
    /// Brief description of the strategy
    pub description: String,

    /// Detailed steps to resolve the issue
    pub steps: Vec<String>,

    /// Applicability conditions
    pub conditions: Vec<String>,

    /// Expected outcome after applying remediation
    pub expected_outcome: String,
}
```

## Error Reporting Integration

### Structured Error Output

```rust
/// Structured error output for programmatic consumption
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructuralErrorReport {
    /// Report metadata
    pub metadata: ReportMetadata,

    /// Summary statistics
    pub summary: ErrorSummary,

    /// Detailed error list
    pub errors: Vec<DetailedError>,

    /// Performance analysis
    pub performance_analysis: Option<PerformanceAnalysis>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorSummary {
    /// Total error count by severity
    pub error_counts: HashMap<StructuralErrorSeverity, usize>,

    /// Error count by category
    pub category_counts: HashMap<ErrorCategory, usize>,

    /// Most common error codes
    pub common_errors: Vec<(ErrorCode, usize)>,

    /// Overall validation status
    pub validation_status: ValidationStatus,
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum ValidationStatus {
    /// All validation passed
    Valid,

    /// Validation passed with warnings
    ValidWithWarnings,

    /// Validation failed with recoverable errors
    InvalidRecoverable,

    /// Validation failed with critical errors
    InvalidCritical,
}
```

### CLI Error Reporting

```rust
/// Enhanced CLI error reporting for structural validation
pub struct StructuralErrorReporter {
    config: ReportingConfig,
    formatter: Box<dyn ErrorFormatter>,
}

pub trait ErrorFormatter {
    /// Format single error for display
    fn format_error(&self, error: &StructuralError) -> String;

    /// Format error summary
    fn format_summary(&self, summary: &ErrorSummary) -> String;

    /// Format remediation suggestions
    fn format_remediation(&self, error: &StructuralError) -> String;
}

#[derive(Debug, Clone)]
pub struct ReportingConfig {
    /// Include performance analysis in reports
    pub include_performance: bool,

    /// Include remediation suggestions
    pub include_remediation: bool,

    /// Maximum number of errors to display
    pub max_errors_displayed: Option<usize>,

    /// Color output for terminal display
    pub use_colors: bool,
}
```

This comprehensive error code specification establishes a robust foundation for structural validation error handling in copybook-rs, providing clear programmatic interfaces, detailed context information, and actionable remediation guidance for enterprise mainframe data processing scenarios.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
