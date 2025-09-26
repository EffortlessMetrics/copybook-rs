# Enterprise Audit System API Contracts and Schemas
## Issue #60 - Comprehensive API Specification

### Overview

This document defines comprehensive API contracts, data schemas, and integration interfaces for the copybook-rs Enterprise Audit System. All APIs are designed for high performance, enterprise integration, and regulatory compliance.

### Core Data Schemas

#### Audit Event Schema

**Primary audit event structure with cryptographic integrity**:

```rust
/// Core audit event with cryptographic integrity
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AuditEvent {
    /// Unique audit event identifier (format: audit-{timestamp}-{random_hex})
    pub event_id: String,

    /// ISO 8601 timestamp with nanosecond precision
    pub timestamp: String,

    /// Classification of audit event
    pub event_type: AuditEventType,

    /// Operation context and metadata
    pub context: AuditContext,

    /// Event-specific payload data
    pub payload: AuditPayload,

    /// SHA-256 integrity hash for tamper detection
    pub integrity_hash: String,

    /// Previous event hash for chain validation
    pub previous_hash: Option<String>,

    /// Optional digital signature for high-security environments
    pub digital_signature: Option<String>,

    /// Event severity level for monitoring and alerting
    pub severity: EventSeverity,
}

impl AuditEvent {
    /// Create new audit event with automatic integrity hash
    pub fn new(
        event_type: AuditEventType,
        context: AuditContext,
        payload: AuditPayload,
    ) -> Self { /* ... */ }

    /// Create event with previous hash for chain integrity
    pub fn with_previous_hash(mut self, previous_hash: String) -> Self { /* ... */ }

    /// Validate event integrity hash
    pub fn validate_integrity(&self) -> bool { /* ... */ }

    /// Export event in CEF format for SIEM integration
    pub fn to_cef_format(&self) -> String { /* ... */ }

    /// Export event in JSON Lines format for log aggregation
    pub fn to_jsonl_format(&self) -> String { /* ... */ }
}
```

#### Audit Event Types

```rust
/// Comprehensive audit event type taxonomy
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum AuditEventType {
    /// COBOL copybook parsing operations
    CopybookParse,

    /// Data validation and verification operations
    DataValidation,

    /// Data transformation (encode/decode) operations
    DataTransformation,

    /// Performance measurement and baseline validation
    PerformanceMeasurement,

    /// Regulatory compliance validation checks
    ComplianceCheck,

    /// Security-relevant events and access monitoring
    SecurityEvent,

    /// Data lineage and transformation tracking
    LineageTracking,

    /// Error and exception events
    ErrorEvent,

    /// Configuration and system changes
    ConfigurationChange,

    /// User access and authentication events
    AccessEvent,
}

impl AuditEventType {
    /// Convert to CEF signature for SIEM integration
    pub fn to_cef_signature(&self) -> &'static str {
        match self {
            AuditEventType::CopybookParse => "COPYBOOK:PARSE",
            AuditEventType::DataValidation => "DATA:VALIDATE",
            AuditEventType::DataTransformation => "DATA:TRANSFORM",
            AuditEventType::PerformanceMeasurement => "PERF:MEASURE",
            AuditEventType::ComplianceCheck => "COMPLIANCE:CHECK",
            AuditEventType::SecurityEvent => "SECURITY:EVENT",
            AuditEventType::LineageTracking => "LINEAGE:TRACK",
            AuditEventType::ErrorEvent => "ERROR:EVENT",
            AuditEventType::ConfigurationChange => "CONFIG:CHANGE",
            AuditEventType::AccessEvent => "ACCESS:EVENT",
        }
    }

    /// Get default severity level for event type
    pub fn default_severity(&self) -> EventSeverity {
        match self {
            AuditEventType::SecurityEvent => EventSeverity::High,
            AuditEventType::ComplianceCheck => EventSeverity::Medium,
            AuditEventType::ErrorEvent => EventSeverity::Medium,
            _ => EventSeverity::Low,
        }
    }
}
```

#### Event Severity Levels

```rust
/// Event severity classification for monitoring and alerting
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum EventSeverity {
    /// Informational events (normal operations)
    Info,

    /// Low priority events (minor issues or notices)
    Low,

    /// Medium priority events (important operational events)
    Medium,

    /// High priority events (security events, performance issues)
    High,

    /// Critical priority events (compliance violations, security breaches)
    Critical,
}

impl EventSeverity {
    /// Convert to numeric value for threshold comparisons
    pub fn to_numeric(&self) -> u8 {
        match self {
            EventSeverity::Info => 0,
            EventSeverity::Low => 1,
            EventSeverity::Medium => 2,
            EventSeverity::High => 3,
            EventSeverity::Critical => 4,
        }
    }

    /// Convert to syslog severity level
    pub fn to_syslog_level(&self) -> u8 {
        match self {
            EventSeverity::Info => 6,      // Informational
            EventSeverity::Low => 5,       // Notice
            EventSeverity::Medium => 4,    // Warning
            EventSeverity::High => 3,      // Error
            EventSeverity::Critical => 2,  // Critical
        }
    }
}
```

#### Audit Context Schema

```rust
/// Comprehensive audit context for enterprise operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditContext {
    /// Unique operation identifier for correlation
    pub operation_id: String,

    /// Parent operation ID for nested operations
    pub parent_operation_id: Option<String>,

    /// User context (username, service account, etc.)
    pub user: Option<String>,

    /// Operation start timestamp
    pub started_at: SystemTime,

    /// System and environment information
    pub environment: EnvironmentContext,

    /// Security classification and controls
    pub security: SecurityContext,

    /// Active compliance profiles
    pub compliance_profiles: HashSet<ComplianceProfile>,

    /// Processing configuration parameters
    pub processing_config: ProcessingConfig,

    /// Custom metadata key-value pairs
    pub metadata: HashMap<String, String>,
}

impl AuditContext {
    /// Factory methods for fluent API construction
    pub fn new() -> Self { /* ... */ }
    pub fn with_operation_id(mut self, id: impl Into<String>) -> Self { /* ... */ }
    pub fn with_user(mut self, user: impl Into<String>) -> Self { /* ... */ }
    pub fn with_compliance_profile(mut self, profile: ComplianceProfile) -> Self { /* ... */ }
    pub fn with_security_classification(mut self, classification: SecurityClassification) -> Self { /* ... */ }
    pub fn with_metadata(mut self, key: impl Into<String>, value: impl Into<String>) -> Self { /* ... */ }

    /// Create child context for nested operations
    pub fn create_child_context(&self, operation_id: impl Into<String>) -> Self { /* ... */ }

    /// Check if specific compliance profile is required
    pub fn requires_compliance(&self, profile: ComplianceProfile) -> bool { /* ... */ }

    /// Calculate effective security level based on classification and compliance
    pub fn effective_security_level(&self) -> SecurityLevel { /* ... */ }

    /// Generate correlation ID for distributed tracing
    pub fn correlation_id(&self) -> String { /* ... */ }
}
```

#### Environment Context Schema

```rust
/// System and environment information for audit context
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnvironmentContext {
    /// System hostname or container identifier
    pub hostname: String,

    /// Process ID for operation tracking
    pub process_id: u32,

    /// System architecture (x86_64, aarch64, etc.)
    pub system_arch: String,

    /// copybook-rs version information
    pub version: String,

    /// Command line invocation details
    pub command_line: Vec<String>,

    /// Environment variables (filtered for security)
    pub environment_variables: HashMap<String, String>,

    /// Resource constraints and limits
    pub resource_limits: ResourceLimits,

    /// Network and connectivity information
    pub network_context: NetworkContext,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceLimits {
    /// Maximum memory limit (bytes)
    pub max_memory_bytes: Option<u64>,

    /// CPU limit (cores)
    pub cpu_limit: Option<f64>,

    /// I/O bandwidth limit (bytes/sec)
    pub io_bandwidth_limit: Option<u64>,

    /// Processing timeout (seconds)
    pub processing_timeout_secs: Option<u64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkContext {
    /// Source IP address (if network operation)
    pub source_ip: Option<String>,

    /// Destination systems or services
    pub destination_systems: Vec<String>,

    /// Network security context
    pub security_context: Option<String>,
}
```

#### Security Context Schema

```rust
/// Security classification and access control information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityContext {
    /// Data classification level
    pub classification: SecurityClassification,

    /// Access control permissions
    pub permissions: Vec<Permission>,

    /// Authentication method used
    pub authentication_method: Option<String>,

    /// Security clearance level (if applicable)
    pub clearance_level: Option<String>,

    /// Encryption requirements
    pub encryption_requirements: EncryptionRequirements,

    /// Access pattern analysis data
    pub access_pattern: AccessPattern,
}

/// Enterprise security classification levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum SecurityClassification {
    /// Public information (no restrictions)
    Public,

    /// Internal use only
    Internal,

    /// Confidential information
    Confidential,

    /// Material transaction data (SOX compliance)
    MaterialTransaction,

    /// Protected Health Information (HIPAA compliance)
    PHI,

    /// Payment card data (PCI DSS compliance)
    PaymentCard,
}

/// Security levels derived from classification and compliance requirements
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum SecurityLevel {
    Low,
    Medium,
    High,
    Critical,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EncryptionRequirements {
    /// Encryption at rest required
    pub at_rest: bool,

    /// Encryption in transit required
    pub in_transit: bool,

    /// Minimum encryption strength (bits)
    pub minimum_key_size: u32,

    /// Required encryption algorithms
    pub algorithms: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AccessPattern {
    /// Access frequency analysis
    pub frequency: AccessFrequency,

    /// Time-based access patterns
    pub time_patterns: Vec<TimePattern>,

    /// Geographic access patterns
    pub geographic_patterns: Vec<GeographicPattern>,
}
```

### Audit Payload Schemas

#### Event-Specific Payload Types

```rust
/// Event-specific audit payload data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AuditPayload {
    /// COBOL copybook parsing payload
    CopybookParse {
        copybook_path: String,
        schema_fingerprint: String,
        parse_result: ParseResult,
        parsing_duration_ms: u64,
        field_count: u32,
        level_88_count: u32,
        error_count: u32,
        warnings: Vec<String>,
        schema_metadata: SchemaMetadata,
    },

    /// Data validation payload
    DataValidation {
        input_file: String,
        validation_result: ValidationResult,
        validation_duration_ms: u64,
        records_validated: u64,
        errors_found: u32,
        error_details: Vec<ValidationError>,
        validation_rules: Vec<String>,
        quality_metrics: DataQualityMetrics,
    },

    /// Data transformation payload
    DataTransformation {
        source_format: String,
        target_format: String,
        transformation_type: TransformationType,
        transformation_duration_ms: u64,
        records_processed: u64,
        bytes_processed: u64,
        transformation_rules: Vec<String>,
        quality_impact: QualityImpact,
    },

    /// Performance measurement payload
    PerformanceMeasurement {
        operation_name: String,
        duration_ms: u64,
        throughput_mbps: f64,
        memory_used_mb: u64,
        cpu_usage_percent: f64,
        io_operations: u64,
        baseline_comparison: Option<BaselineComparison>,
        resource_utilization: ResourceUtilization,
    },

    /// Compliance check payload
    ComplianceCheck {
        compliance_profiles: Vec<ComplianceProfile>,
        validation_result: ComplianceResult,
        validation_duration_ms: u64,
        violations_found: u32,
        violations: Vec<ComplianceViolation>,
        recommendations: Vec<ComplianceRecommendation>,
        remediation_actions: Vec<RemediationAction>,
    },

    /// Security event payload
    SecurityEvent {
        security_event_type: SecurityEventType,
        severity: String,
        affected_resources: Vec<String>,
        threat_indicators: Vec<ThreatIndicator>,
        remediation_actions: Vec<String>,
        incident_id: Option<String>,
        investigation_status: Option<String>,
    },

    /// Lineage tracking payload
    LineageTracking {
        source_fields: Vec<FieldLineage>,
        target_fields: Vec<FieldLineage>,
        transformations: Vec<TransformationRule>,
        lineage_confidence: f64,
        impact_analysis: Option<ImpactAnalysis>,
        quality_metrics: DataQualityMetrics,
    },

    /// Error event payload
    ErrorEvent {
        error_code: String,
        error_message: String,
        error_category: String,
        stack_trace: Option<String>,
        recovery_actions: Vec<String>,
        business_impact: BusinessImpact,
        technical_details: TechnicalDetails,
    },

    /// Configuration change payload
    ConfigurationChange {
        change_type: ConfigurationChangeType,
        changed_settings: Vec<ConfigurationSetting>,
        change_reason: String,
        change_approver: Option<String>,
        rollback_plan: Option<String>,
        business_justification: String,
    },

    /// Access event payload
    AccessEvent {
        access_type: AccessType,
        resource_accessed: String,
        access_result: AccessResult,
        authentication_method: String,
        session_id: Option<String>,
        risk_score: f64,
        anomaly_indicators: Vec<AnomalyIndicator>,
    },
}
```

### Compliance Engine API

#### Compliance Profile Schema

```rust
/// Regulatory compliance profile definitions
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ComplianceProfile {
    /// Sarbanes-Oxley Act (Financial industry)
    SOX,

    /// Health Insurance Portability and Accountability Act
    HIPAA,

    /// General Data Protection Regulation (EU)
    GDPR,

    /// Payment Card Industry Data Security Standard
    PciDss,

    /// California Consumer Privacy Act
    CCPA,

    /// Federal Information Security Management Act
    FISMA,

    /// Custom compliance profile
    Custom(String),
}

impl ComplianceProfile {
    /// Get human-readable name for compliance profile
    pub fn display_name(&self) -> &str {
        match self {
            ComplianceProfile::SOX => "Sarbanes-Oxley Act",
            ComplianceProfile::HIPAA => "Health Insurance Portability and Accountability Act",
            ComplianceProfile::GDPR => "General Data Protection Regulation",
            ComplianceProfile::PciDss => "Payment Card Industry Data Security Standard",
            ComplianceProfile::CCPA => "California Consumer Privacy Act",
            ComplianceProfile::FISMA => "Federal Information Security Management Act",
            ComplianceProfile::Custom(name) => name,
        }
    }

    /// Get regulatory requirements for profile
    pub fn requirements(&self) -> Vec<ComplianceRequirement> { /* ... */ }

    /// Check if profile applies to data classification
    pub fn applies_to_classification(&self, classification: SecurityClassification) -> bool { /* ... */ }
}
```

#### Compliance Result Schema

```rust
/// Comprehensive compliance validation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceResult {
    /// Validation result identifier
    pub result_id: String,

    /// Operation that was validated
    pub operation_id: String,

    /// Compliance profiles that were validated
    pub validated_profiles: Vec<ComplianceProfile>,

    /// Overall compliance status
    pub is_compliant: bool,

    /// Compliance violations found
    pub violations: Vec<ComplianceViolation>,

    /// Compliance warnings (non-blocking issues)
    pub warnings: Vec<ComplianceWarning>,

    /// Recommendations for improvement
    pub recommendations: Vec<ComplianceRecommendation>,

    /// Validation timestamp
    pub validated_at: SystemTime,

    /// Validation duration
    pub validation_duration_ms: u64,

    /// Compliance score (0.0 - 1.0)
    pub compliance_score: f64,

    /// Risk assessment
    pub risk_assessment: RiskAssessment,
}

impl ComplianceResult {
    /// Create new compliance result
    pub fn new() -> Self { /* ... */ }

    /// Add compliance violation
    pub fn add_violation(&mut self, violation: ComplianceViolation) { /* ... */ }

    /// Add compliance warning
    pub fn add_warning(&mut self, warning: ComplianceWarning) { /* ... */ }

    /// Add recommendation
    pub fn add_recommendation(&mut self, recommendation: ComplianceRecommendation) { /* ... */ }

    /// Merge results from multiple compliance profiles
    pub fn merge(&mut self, other: ComplianceResult) { /* ... */ }

    /// Calculate overall compliance score
    pub fn calculate_score(&self) -> f64 { /* ... */ }
}
```

#### Compliance Violation Schema

```rust
/// Detailed compliance violation information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceViolation {
    /// Unique violation identifier
    pub violation_id: String,

    /// Regulatory framework and section
    pub regulation: String,

    /// Violation title/summary
    pub title: String,

    /// Detailed violation description
    pub description: String,

    /// Violation severity level
    pub severity: ViolationSeverity,

    /// Affected data or operations
    pub affected_scope: Vec<String>,

    /// Business impact assessment
    pub business_impact: BusinessImpact,

    /// Recommended remediation steps
    pub remediation: String,

    /// Estimated remediation effort
    pub remediation_effort: RemediationEffort,

    /// Violation detection timestamp
    pub detected_at: SystemTime,

    /// Due date for remediation
    pub remediation_due: Option<SystemTime>,

    /// Associated risk level
    pub risk_level: RiskLevel,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ViolationSeverity {
    Low,
    Medium,
    High,
    Critical,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BusinessImpact {
    Minimal,
    Low,
    Medium,
    High,
    Severe,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RemediationEffort {
    /// Estimated time to remediate
    pub estimated_hours: u32,

    /// Required resources or skills
    pub required_resources: Vec<String>,

    /// Implementation complexity
    pub complexity: ComplexityLevel,

    /// Business approval required
    pub requires_approval: bool,
}
```

### Performance Metrics API

#### Performance Baseline Schema

```rust
/// Performance baseline definition and tracking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceBaseline {
    /// Baseline identifier and version
    pub baseline_id: String,

    /// Baseline creation timestamp
    pub created_at: SystemTime,

    /// Baseline version number
    pub version: String,

    /// System configuration snapshot
    pub system_config: SystemConfiguration,

    /// Throughput performance metrics
    pub throughput: ThroughputBaseline,

    /// Resource utilization baselines
    pub resources: ResourceBaseline,

    /// Quality and accuracy metrics
    pub quality: QualityBaseline,

    /// Statistical confidence intervals
    pub confidence_intervals: ConfidenceIntervals,

    /// Baseline validation status
    pub validation_status: BaselineStatus,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ThroughputBaseline {
    /// DISPLAY field processing throughput (bytes/sec)
    pub display_throughput_bps: u64,

    /// COMP-3 field processing throughput (bytes/sec)
    pub comp3_throughput_bps: u64,

    /// Overall record processing rate (records/sec)
    pub record_rate_per_sec: u64,

    /// File I/O throughput (bytes/sec)
    pub io_throughput_bps: u64,

    /// Network throughput (if applicable)
    pub network_throughput_bps: Option<u64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceBaseline {
    /// Peak memory usage (bytes)
    pub peak_memory_bytes: u64,

    /// Average memory usage (bytes)
    pub avg_memory_bytes: u64,

    /// CPU utilization percentage
    pub cpu_utilization_percent: f64,

    /// I/O operations per second
    pub io_ops_per_sec: u64,

    /// Network connections used
    pub network_connections: u32,

    /// Temporary file space used (bytes)
    pub temp_space_bytes: u64,
}
```

#### Performance Metrics Schema

```rust
/// Real-time performance measurement results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceMetrics {
    /// Operation name being measured
    pub operation_name: String,

    /// Measurement timestamp
    pub measured_at: SystemTime,

    /// Operation duration (milliseconds)
    pub duration_ms: u64,

    /// Data throughput (MiB/sec)
    pub throughput_mbps: f64,

    /// Memory usage (MiB)
    pub memory_used_mb: u64,

    /// CPU utilization percentage
    pub cpu_usage_percent: f64,

    /// I/O operations performed
    pub io_operations: u64,

    /// Network operations (if applicable)
    pub network_operations: Option<u64>,

    /// Quality metrics
    pub quality_metrics: QualityMetrics,

    /// Resource utilization details
    pub resource_utilization: ResourceUtilization,

    /// Performance anomalies detected
    pub anomalies: Vec<PerformanceAnomaly>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityMetrics {
    /// Data accuracy percentage
    pub accuracy_percent: f64,

    /// Processing error rate
    pub error_rate: f64,

    /// Data completeness percentage
    pub completeness_percent: f64,

    /// Validation success rate
    pub validation_success_rate: f64,

    /// Data integrity score
    pub integrity_score: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceUtilization {
    /// Memory allocation efficiency
    pub memory_efficiency: f64,

    /// CPU efficiency score
    pub cpu_efficiency: f64,

    /// I/O efficiency rating
    pub io_efficiency: f64,

    /// Cache hit ratios
    pub cache_hit_ratio: f64,

    /// Resource contention indicators
    pub contention_indicators: Vec<ContentionIndicator>,
}
```

### Data Lineage API

#### Field Lineage Schema

```rust
/// Field-level data lineage tracking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldLineage {
    /// Unique field identifier
    pub field_id: String,

    /// Field path in schema (e.g., "CUSTOMER_RECORD.ACCOUNT_INFO.BALANCE")
    pub field_path: String,

    /// Source system identifier
    pub system_id: String,

    /// Schema version or fingerprint
    pub schema_version: String,

    /// Field data type information
    pub data_type: FieldDataType,

    /// Field constraints and validation rules
    pub constraints: Vec<FieldConstraint>,

    /// Business metadata
    pub business_metadata: FieldBusinessMetadata,

    /// Technical metadata
    pub technical_metadata: FieldTechnicalMetadata,

    /// Data quality assessments
    pub quality_assessments: Vec<QualityAssessment>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldDataType {
    /// COBOL data type (DISPLAY, COMP-3, etc.)
    pub cobol_type: String,

    /// Logical data type (string, decimal, integer, etc.)
    pub logical_type: String,

    /// Field length/precision
    pub length: u32,

    /// Decimal places (for numeric types)
    pub decimal_places: Option<u32>,

    /// Character encoding
    pub encoding: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldBusinessMetadata {
    /// Business field name
    pub business_name: String,

    /// Field description
    pub description: String,

    /// Business owner/steward
    pub owner: Option<String>,

    /// Data classification
    pub classification: DataClassification,

    /// Business rules
    pub business_rules: Vec<String>,

    /// Regulatory requirements
    pub regulatory_requirements: Vec<String>,
}
```

#### Transformation Tracking Schema

```rust
/// Data transformation lineage record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransformationRecord {
    /// Transformation identifier
    pub transformation_id: String,

    /// Source field lineage
    pub source_fields: Vec<FieldLineage>,

    /// Target field lineage
    pub target_fields: Vec<FieldLineage>,

    /// Transformation type and rules
    pub transformation: TransformationDefinition,

    /// Transformation quality assessment
    pub quality_impact: QualityImpact,

    /// Transformation timestamp
    pub transformed_at: SystemTime,

    /// Processing context
    pub context: AuditContext,

    /// Validation results
    pub validation_results: TransformationValidation,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransformationDefinition {
    /// Type of transformation applied
    pub transformation_type: TransformationType,

    /// Transformation rules or logic
    pub rules: Vec<TransformationRule>,

    /// Transformation parameters
    pub parameters: HashMap<String, String>,

    /// Error handling strategy
    pub error_handling: ErrorHandlingStrategy,

    /// Rollback capability
    pub rollback_supported: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TransformationType {
    /// Direct field mapping (no transformation)
    DirectMapping,

    /// Data type conversion
    TypeConversion,

    /// Data format conversion
    FormatConversion,

    /// Data aggregation
    Aggregation,

    /// Data filtering
    Filtering,

    /// Data enrichment
    Enrichment,

    /// Custom transformation
    Custom(String),
}
```

### Enterprise Integration APIs

#### SIEM Integration Schema

```rust
/// SIEM integration configuration and formatting
#[derive(Debug, Clone)]
pub struct SiemIntegration {
    /// SIEM system configuration
    pub config: SiemConfig,

    /// Event formatters for different SIEM systems
    pub formatters: HashMap<SiemType, Box<dyn SiemFormatter>>,

    /// Transport mechanisms (HTTP, TCP, UDP, etc.)
    pub transporters: HashMap<TransportType, Box<dyn SiemTransporter>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SiemConfig {
    /// SIEM system type
    pub siem_type: SiemType,

    /// Connection endpoints
    pub endpoints: Vec<SiemEndpoint>,

    /// Authentication configuration
    pub auth_config: AuthenticationConfig,

    /// Event filtering rules
    pub event_filters: Vec<EventFilter>,

    /// Rate limiting configuration
    pub rate_limits: RateLimitConfig,

    /// Retry and failure handling
    pub failure_handling: FailureHandlingConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SiemType {
    /// Splunk Enterprise Security
    Splunk,

    /// IBM QRadar
    QRadar,

    /// Microsoft Sentinel
    Sentinel,

    /// Elasticsearch/Elastic Security
    Elastic,

    /// Generic CEF-compatible SIEM
    GenericCef,

    /// Custom SIEM integration
    Custom(String),
}

/// Common Event Format (CEF) message structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CefMessage {
    /// CEF version
    pub version: u8,

    /// Device vendor
    pub device_vendor: String,

    /// Device product
    pub device_product: String,

    /// Device version
    pub device_version: String,

    /// Signature ID
    pub signature_id: String,

    /// Event name
    pub name: String,

    /// Severity level
    pub severity: u8,

    /// CEF extensions
    pub extensions: HashMap<String, String>,
}

impl CefMessage {
    /// Create CEF message from audit event
    pub fn from_audit_event(event: &AuditEvent) -> Self { /* ... */ }

    /// Format as CEF string
    pub fn to_cef_string(&self) -> String { /* ... */ }

    /// Validate CEF format compliance
    pub fn validate(&self) -> Result<(), CefValidationError> { /* ... */ }
}
```

#### Monitoring Integration Schema

```rust
/// Enterprise monitoring system integration
#[derive(Debug, Clone)]
pub struct MonitoringIntegration {
    /// Prometheus metrics client
    pub prometheus: Option<PrometheusClient>,

    /// Datadog metrics client
    pub datadog: Option<DatadogClient>,

    /// Custom webhook clients
    pub webhooks: Vec<WebhookClient>,

    /// Alerting configuration
    pub alerting: AlertingConfig,
}

/// Prometheus metrics definitions
#[derive(Debug, Clone)]
pub struct PrometheusMetrics {
    /// Processing duration histogram
    pub processing_duration: Histogram,

    /// Throughput gauge
    pub throughput_gauge: Gauge,

    /// Memory usage gauge
    pub memory_usage: Gauge,

    /// Compliance violations counter
    pub compliance_violations: Counter,

    /// Security events counter
    pub security_events: Counter,

    /// Error rate counter
    pub error_rate: Counter,
}

impl PrometheusMetrics {
    /// Record processing performance metrics
    pub fn record_performance(&self, metrics: &PerformanceMetrics) { /* ... */ }

    /// Record compliance violation
    pub fn record_violation(&self, violation: &ComplianceViolation) { /* ... */ }

    /// Record security event
    pub fn record_security_event(&self, event: &SecurityEvent) { /* ... */ }
}
```

### API Error Handling

#### Comprehensive Error Taxonomy

```rust
/// Enterprise audit system error taxonomy
#[derive(Debug, thiserror::Error)]
pub enum AuditApiError {
    /// Configuration errors
    #[error("Configuration error: {message}")]
    Configuration { message: String },

    /// Authentication/authorization errors
    #[error("Access denied: {message}")]
    AccessDenied { message: String },

    /// Validation errors
    #[error("Validation failed: {errors:?}")]
    Validation { errors: Vec<ValidationError> },

    /// Compliance-related errors
    #[error("Compliance violation: {violation}")]
    ComplianceViolation { violation: ComplianceViolation },

    /// Performance-related errors
    #[error("Performance regression: {deviation:.2}% above threshold")]
    PerformanceRegression { deviation: f64 },

    /// Security-related errors
    #[error("Security validation failed: {message}")]
    SecurityValidation { message: String },

    /// Data quality errors
    #[error("Data quality issue: {issue}")]
    DataQuality { issue: DataQualityIssue },

    /// Integration errors
    #[error("Integration failure: {system} - {message}")]
    Integration { system: String, message: String },

    /// I/O and persistence errors
    #[error("I/O error: {source}")]
    Io {
        #[from]
        source: std::io::Error,
    },

    /// Serialization/deserialization errors
    #[error("Serialization error: {source}")]
    Serialization {
        #[from]
        source: serde_json::Error,
    },

    /// Network/transport errors
    #[error("Network error: {source}")]
    Network {
        #[from]
        source: reqwest::Error,
    },

    /// Database/persistence errors
    #[error("Database error: {message}")]
    Database { message: String },

    /// Rate limiting errors
    #[error("Rate limit exceeded: {limit} requests per {window_secs} seconds")]
    RateLimit { limit: u32, window_secs: u32 },

    /// Timeout errors
    #[error("Operation timeout: {operation} exceeded {timeout_secs} seconds")]
    Timeout { operation: String, timeout_secs: u32 },
}

/// API result type with comprehensive error handling
pub type AuditApiResult<T> = Result<T, AuditApiError>;
```

### Performance Specifications

#### API Performance Requirements

```rust
/// API performance requirements and SLA definitions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiPerformanceRequirements {
    /// Maximum API response time (milliseconds)
    pub max_response_time_ms: u64,

    /// Target throughput (requests/second)
    pub target_throughput_rps: u64,

    /// Maximum memory overhead (MB)
    pub max_memory_overhead_mb: u64,

    /// Audit event creation time limit (microseconds)
    pub audit_event_creation_limit_us: u64,

    /// Compliance validation time limit (milliseconds)
    pub compliance_validation_limit_ms: u64,

    /// Performance regression threshold (percentage)
    pub performance_regression_threshold: f64,
}

impl Default for ApiPerformanceRequirements {
    fn default() -> Self {
        Self {
            max_response_time_ms: 100,          // 100ms max response time
            target_throughput_rps: 1000,        // 1000 requests/second
            max_memory_overhead_mb: 50,         // 50MB max overhead
            audit_event_creation_limit_us: 50,  // 50μs event creation
            compliance_validation_limit_ms: 10, // 10ms compliance check
            performance_regression_threshold: 5.0, // 5% regression threshold
        }
    }
}
```

### API Usage Examples

#### Audit Event Creation Example

```rust
use copybook_core::audit::*;

// Create audit context
let context = AuditContext::new()
    .with_operation_id("enterprise_processing_001")
    .with_user("data_processor")
    .with_compliance_profile(ComplianceProfile::SOX)
    .with_security_classification(SecurityClassification::MaterialTransaction)
    .with_metadata("business_unit", "financial_operations");

// Create audit event payload
let payload = AuditPayload::CopybookParse {
    copybook_path: "enterprise_financials.cpy".to_string(),
    schema_fingerprint: "sha256:abc123...".to_string(),
    parse_result: ParseResult::Success,
    parsing_duration_ms: 150,
    field_count: 247,
    level_88_count: 34,
    error_count: 0,
    warnings: vec![],
    schema_metadata: SchemaMetadata::default(),
};

// Create audit event
let event = AuditEvent::new(
    AuditEventType::CopybookParse,
    context,
    payload,
);

// Log event with cryptographic integrity
let logger = AuditLogger::new(AuditLoggerConfig::default())?;
logger.log_event(event).await?;
```

#### Compliance Validation Example

```rust
use copybook_core::audit::{ComplianceEngine, ComplianceProfile};

// Initialize compliance engine
let compliance_engine = ComplianceEngine::new()
    .with_profiles(&[
        ComplianceProfile::SOX,
        ComplianceProfile::GDPR,
    ]);

// Run compliance validation
let result = compliance_engine
    .validate_processing_operation(&audit_context)
    .await?;

// Check compliance status
if !result.is_compliant() {
    for violation in &result.violations {
        eprintln!("Compliance violation: {} - {}",
                  violation.violation_id,
                  violation.description);
    }

    // Generate remediation plan
    let remediation = compliance_engine
        .generate_remediation_plan(&result.violations)
        .await?;

    println!("Remediation plan created: {}", remediation.plan_id);
}
```

This comprehensive API specification provides the foundation for implementing the Enterprise Audit System with full enterprise integration capabilities, regulatory compliance, and high-performance operation within the copybook-rs ecosystem.

### Summary

The Enterprise Audit System API contracts provide:

1. **Comprehensive Event Schema**: Flexible, extensible audit event structure with cryptographic integrity
2. **Multi-Framework Compliance**: Support for SOX, HIPAA, GDPR, PCI DSS, and custom frameworks
3. **Performance Integration**: Low-overhead performance monitoring with baseline management
4. **Enterprise Integration**: SIEM, monitoring, and alerting system compatibility
5. **Security Architecture**: Cryptographic integrity, access control, and anomaly detection
6. **Data Lineage**: Field-level transformation tracking with impact analysis
7. **Error Handling**: Comprehensive error taxonomy with recovery guidance
8. **Performance Specifications**: Sub-5% overhead with enterprise SLA compliance

All APIs are designed for seamless integration with existing copybook-rs workflows while providing comprehensive enterprise audit capabilities.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
