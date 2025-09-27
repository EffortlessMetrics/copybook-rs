# ADR-001: Audit Event Schema Design for Enterprise Audit System

## Status
Accepted

## Context
The copybook-rs Enterprise Audit System requires a comprehensive audit event schema that can capture detailed information about COBOL processing operations while maintaining high performance and enterprise integration compatibility. The schema must support regulatory compliance requirements (SOX, HIPAA, GDPR, PCI DSS), cryptographic integrity, and efficient serialization for various enterprise systems.

### Key Requirements
- Support for diverse audit event types (parsing, validation, compliance, security)
- Cryptographic integrity with hash chaining for tamper-proof trails
- Performance optimization for high-volume operations (>10,000 events/second)
- Enterprise integration compatibility (SIEM, monitoring, log aggregation)
- Regulatory compliance metadata tracking
- Extensible design for future audit requirements

### Design Constraints
- Must maintain <5% performance overhead on copybook processing
- Must support zero-copy serialization where possible
- Must be backward compatible for enterprise deployment
- Must align with copybook-rs's zero unsafe code policy

## Decision
We will implement a hierarchical audit event schema with the following design principles:

### 1. Core Event Structure
```rust
pub struct AuditEvent {
    // Core identification
    pub event_id: String,
    pub timestamp: String,
    pub event_type: AuditEventType,

    // Context information
    pub context: AuditContext,

    // Event-specific data
    pub payload: AuditPayload,

    // Cryptographic integrity
    pub integrity_hash: String,
    pub previous_hash: Option<String>,
    pub digital_signature: Option<String>,

    // Metadata
    pub severity: EventSeverity,
}
```

### 2. Event Type Taxonomy
Use strongly-typed enum for event classification:
- **CopybookParse**: COBOL copybook parsing operations
- **DataValidation**: Data validation and verification
- **DataTransformation**: Encoding/decoding operations
- **ComplianceCheck**: Regulatory compliance validation
- **SecurityEvent**: Security-related operations
- **PerformanceMeasurement**: Performance metrics collection
- **LineageTracking**: Data lineage and transformation tracking
- **ErrorEvent**: Error and exception handling
- **ConfigurationChange**: System configuration changes
- **AccessEvent**: User access and authentication

### 3. Payload Design Pattern
Use tagged union (enum) for type-safe payload handling:
```rust
pub enum AuditPayload {
    CopybookParse { /* specific fields */ },
    DataValidation { /* specific fields */ },
    SecurityEvent { /* specific fields */ },
    // ... other variants
}
```

### 4. Context Architecture
Comprehensive context with nested structures:
- **EnvironmentContext**: System information
- **SecurityContext**: Classification and permissions
- **ProcessingConfig**: Operation parameters
- **Metadata**: Custom key-value pairs

### 5. Cryptographic Integrity
- SHA-256 hash chaining for tamper detection
- Optional ECDSA digital signatures for high-security environments
- Previous hash linking for chain validation

## Rationale

### Performance Considerations
- **Tagged Union Design**: Enables zero-copy deserialization for specific payload types
- **String Interning**: Event IDs and common strings use Arc<str> for memory efficiency
- **Lazy Serialization**: Expensive serialization operations deferred until needed
- **Memory Pool Integration**: Compatible with pre-allocated buffer strategies

### Enterprise Integration Benefits
- **Structured Payload**: Enables efficient SIEM field extraction
- **Standard Timestamps**: ISO 8601 format with nanosecond precision
- **Severity Classification**: Maps to standard security event severity levels
- **Metadata Extensibility**: Custom fields for organization-specific requirements

### Compliance Alignment
- **Audit Trail Completeness**: Captures all required regulatory information
- **Immutable Design**: Structure prevents accidental modification after creation
- **Integrity Verification**: Cryptographic controls for regulatory requirements
- **Data Classification**: Built-in support for data sensitivity handling

### Security Benefits
- **Tamper Detection**: Hash chaining detects unauthorized modifications
- **Access Control**: Context includes permission and classification information
- **Audit Chain**: Cryptographically linked events form verifiable trail
- **Digital Signatures**: Optional signatures for non-repudiation

## Consequences

### Positive Impacts
- **Type Safety**: Rust's type system prevents schema-related runtime errors
- **Performance**: Zero-copy operations and memory-efficient design
- **Extensibility**: New event types and payloads can be added without breaking changes
- **Integration**: Standard format simplifies enterprise system integration
- **Compliance**: Built-in support for regulatory requirements
- **Security**: Cryptographic integrity provides tamper-proof audit trails

### Negative Impacts
- **Complexity**: Rich schema requires more development effort
- **Memory Usage**: Comprehensive context increases per-event memory footprint (~2-4KB per event)
- **Serialization Cost**: Large events require more processing for serialization
- **Schema Evolution**: Changes require careful backward compatibility management

### Migration Considerations
- **Backward Compatibility**: Use versioned schemas for enterprise deployment
- **Schema Registry**: Central schema management for enterprise environments
- **Gradual Migration**: Support both old and new schemas during transition periods
- **Tool Updates**: Enterprise tools may require updates for new schema features

## Implementation Details

### Hash Chain Implementation
```rust
impl AuditEvent {
    pub fn with_previous_hash(mut self, previous_hash: String) -> Self {
        self.previous_hash = Some(previous_hash);
        self.update_integrity_hash();
        self
    }

    fn calculate_integrity_hash(&self) -> String {
        let mut event_copy = self.clone();
        event_copy.integrity_hash = String::new();
        event_copy.previous_hash = None;

        let data = serde_json::to_vec(&event_copy).unwrap();
        let mut hasher = Sha256::new();
        hasher.update(&data);

        if let Some(prev) = &self.previous_hash {
            hasher.update(prev.as_bytes());
        }

        format!("{:x}", hasher.finalize())
    }
}
```

### Efficient Serialization
```rust
impl AuditEvent {
    pub fn to_json_bytes(&self) -> Vec<u8> {
        serde_json::to_vec(self).unwrap()
    }

    pub fn to_cef_format(&self) -> String {
        // CEF format implementation for SIEM integration
    }

    pub fn to_structured_log(&self) -> StructuredLogEntry {
        // Structured logging format
    }
}
```

### Memory Pool Integration
```rust
pub struct AuditEventBuilder {
    pool: Arc<MemoryPool>,
}

impl AuditEventBuilder {
    pub fn new_event(&self, event_type: AuditEventType) -> AuditEvent {
        // Use memory pool for buffer allocation
        let buffer = self.pool.get_event_buffer();
        // Build event using pre-allocated buffer
    }
}
```

## Monitoring and Validation

### Performance Metrics
- Monitor average event creation time (<50μs target)
- Track memory usage per event (<4KB average)
- Measure serialization performance across formats
- Validate hash calculation performance (<10ms per chain validation)

### Schema Validation
- Automated schema compatibility testing
- Performance regression testing for schema changes
- Integration testing with enterprise systems
- Compliance validation for regulatory requirements

### Quality Gates
- Zero unsafe code in schema implementation
- 100% test coverage for schema operations
- Performance benchmarks within targets
- Enterprise integration validation

## Alternatives Considered

### Alternative 1: Simple Flat Schema
**Approach**: Single structure with optional fields for all event types
**Rejected Because**:
- Poor type safety
- Inefficient memory usage
- Difficult to extend
- No compile-time validation of event-specific fields

### Alternative 2: External Schema Definition
**Approach**: Use Protocol Buffers or Avro for schema definition
**Rejected Because**:
- Additional dependencies and complexity
- Less Rust-native type safety
- Performance overhead for serialization
- Reduced development velocity

### Alternative 3: Key-Value Metadata Only
**Approach**: Minimal structured data with extensive metadata
**Rejected Because**:
- Poor query performance
- Difficult SIEM integration
- No compile-time validation
- Inefficient storage and transmission

## Related Decisions
- ADR-002: Cryptographic Integrity Implementation
- ADR-003: Performance Optimization Strategy
- ADR-004: Multi-Framework Compliance Engine
- ADR-005: Enterprise Integration Patterns

## References
- [NIST SP 800-92: Guide to Computer Security Log Management](https://csrc.nist.gov/publications/detail/sp/800-92/final)
- [Common Event Format (CEF) Specification](https://www.microfocus.com/documentation/arcsight/arcsight-smartconnectors-8.3/cef-implementation-standard/)
- [ISO/IEC 27001:2013 - Information Security Management](https://www.iso.org/standard/54534.html)
- [SOX Section 404 - Management Assessment of Internal Controls](https://www.sox-online.com/act_section_404.html)