<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# ADR-002: Cryptographic Integrity Implementation for Audit System

## Status
Accepted

## Context
The copybook-rs Enterprise Audit System must provide tamper-proof audit trails to meet regulatory compliance requirements (SOX, HIPAA, GDPR, PCI DSS). Regulatory frameworks require that audit logs maintain cryptographic integrity to detect unauthorized modifications and ensure non-repudiation of audit events.

### Regulatory Requirements
- **SOX Section 404**: Requires controls to ensure accuracy and completeness of financial data processing
- **HIPAA Security Rule**: Mandates integrity controls for electronic PHI audit trails
- **GDPR Article 32**: Requires appropriate technical measures to ensure security of processing
- **PCI DSS Requirement 10**: Mandates secure storage and integrity protection of audit logs

### Technical Constraints
- Performance impact must be <5% on main processing pipeline
- Must support high-volume audit event generation (>10,000 events/second)
- Must be compatible with distributed processing and parallel operations
- Must provide both real-time integrity validation and historical verification
- Must support enterprise key management and rotation policies

### Security Considerations
- Protection against malicious tampering of audit trails
- Detection of accidental corruption or system errors
- Support for forensic investigation and compliance audits
- Integration with enterprise security infrastructure
- Future-proof cryptographic algorithm selection

## Decision
We will implement a multi-layered cryptographic integrity system using SHA-256 hash chaining with optional ECDSA digital signatures for high-security environments.

### 1. Primary Integrity Layer: SHA-256 Hash Chaining
- Each audit event contains SHA-256 hash of its content
- Hash includes reference to previous event's hash (blockchain-style chaining)
- Hash calculation includes event data, context, and previous hash
- Optional salt for additional security (enterprise configurable)

### 2. Secondary Integrity Layer: Digital Signatures (Optional)
- ECDSA-P256 signatures for high-security environments
- Signatures applied to event hash for non-repudiation
- Enterprise key management integration
- Configurable per data classification level

### 3. Chain Validation Architecture
- Real-time validation during event creation
- Batch validation for historical audit trail verification
- Tamper detection with specific error identification
- Performance-optimized validation with caching

### 4. Key Management Integration
- Support for Hardware Security Modules (HSM)
- Enterprise key rotation policies
- Separate keys for different compliance frameworks
- Key escrow and recovery procedures

## Rationale

### Algorithm Selection: SHA-256
**Chosen over alternatives because:**
- **NIST FIPS 140-2 approved** for government and enterprise use
- **Proven security record** with no known practical attacks
- **Performance optimized** with hardware acceleration on modern CPUs
- **Regulatory accepted** across SOX, HIPAA, GDPR, PCI DSS frameworks
- **Future-proof** with expected security lifetime >15 years

**Rejected alternatives:**
- **SHA-1**: Cryptographically broken, unsuitable for security applications
- **MD5**: Vulnerable to collision attacks, not acceptable for compliance
- **SHA-3**: Newer but less enterprise adoption, potential performance impact
- **Blake3**: High performance but limited regulatory recognition

### Digital Signature Selection: ECDSA-P256
**Chosen for optional high-security layer:**
- **Compact signatures** (64 bytes) vs RSA (256+ bytes)
- **Fast verification** suitable for high-volume operations
- **NIST P-256 curve** provides 128-bit security level
- **Hardware support** available in enterprise HSMs
- **Regulatory compliance** accepted across frameworks

**Rejected alternatives:**
- **RSA-2048**: Larger signature size, slower performance
- **Ed25519**: Limited enterprise HSM support
- **RSA-4096**: Excessive performance overhead for audit use case

### Hash Chaining Architecture
**Benefits:**
- **Tamper Detection**: Any modification breaks the chain
- **Ordering Integrity**: Maintains chronological sequence
- **Efficient Validation**: Can validate entire chain or segments
- **Scalable**: Parallel validation of chain segments possible

**Implementation Pattern:**
```
Event[n].integrity_hash = SHA256(Event[n].data || Event[n-1].integrity_hash || salt)
```

## Implementation Architecture

### Core Cryptographic Manager
```rust
pub struct CryptographicIntegrityManager {
    hash_algorithm: HashAlgorithm,
    salt: Option<Vec<u8>>,
    signature_provider: Option<Box<dyn SignatureProvider>>,
    key_manager: KeyManager,
    validation_cache: LruCache<String, ValidationResult>,
}

impl CryptographicIntegrityManager {
    pub async fn calculate_event_hash(
        &self,
        event: &AuditEvent,
        previous_hash: Option<&str>,
    ) -> Result<String, SecurityError> {
        // 1. Serialize event data (excluding integrity fields)
        let mut event_copy = event.clone();
        event_copy.integrity_hash = String::new();
        event_copy.previous_hash = None;
        event_copy.digital_signature = None;

        let event_data = serde_json::to_vec(&event_copy)?;

        // 2. Calculate SHA-256 hash with chaining
        let mut hasher = Sha256::new();
        hasher.update(&event_data);

        if let Some(prev_hash) = previous_hash {
            hasher.update(prev_hash.as_bytes());
        }

        if let Some(salt) = &self.salt {
            hasher.update(salt);
        }

        Ok(format!("{:x}", hasher.finalize()))
    }

    pub async fn create_digital_signature(
        &self,
        event: &AuditEvent,
    ) -> Result<Option<String>, SecurityError> {
        if let Some(signer) = &self.signature_provider {
            let signature_data = format!("{}:{}", event.event_id, event.integrity_hash);
            let signature = signer.sign(signature_data.as_bytes()).await?;
            Ok(Some(hex::encode(signature)))
        } else {
            Ok(None)
        }
    }
}
```

### High-Performance Hash Chaining
```rust
pub struct HashChainManager {
    current_hash: Arc<RwLock<Option<String>>>,
    chain_state: Arc<RwLock<ChainState>>,
    performance_cache: Arc<Mutex<LruCache<String, String>>>,
}

impl HashChainManager {
    pub async fn add_event_to_chain(
        &self,
        event: &mut AuditEvent,
    ) -> Result<(), SecurityError> {
        // Get previous hash for chaining
        let previous_hash = {
            let current = self.current_hash.read().await;
            current.clone()
        };

        // Calculate new hash with chaining
        let new_hash = self.calculate_hash_with_chain(event, previous_hash.as_deref()).await?;

        // Update event with integrity information
        event.integrity_hash = new_hash.clone();
        event.previous_hash = previous_hash;

        // Update chain state atomically
        {
            let mut current = self.current_hash.write().await;
            *current = Some(new_hash.clone());
        }

        // Update chain tracking
        {
            let mut state = self.chain_state.write().await;
            state.add_event(&event.event_id, &new_hash);
        }

        Ok(())
    }
}
```

### Validation and Tamper Detection
```rust
pub struct TamperDetectionEngine {
    integrity_manager: Arc<CryptographicIntegrityManager>,
    anomaly_detector: AnomalyDetector,
}

impl TamperDetectionEngine {
    pub async fn validate_audit_chain(
        &self,
        events: &[AuditEvent],
    ) -> Result<ChainValidationResult, SecurityError> {
        let mut result = ChainValidationResult {
            overall_valid: true,
            total_events: events.len(),
            invalid_events: Vec::new(),
            chain_breaks: Vec::new(),
            tampering_indicators: Vec::new(),
        };

        // Validate hash chain integrity
        for (i, event) in events.iter().enumerate() {
            let previous_hash = if i > 0 {
                Some(&events[i - 1].integrity_hash)
            } else {
                None
            };

            let calculated_hash = self.integrity_manager
                .calculate_event_hash(event, previous_hash)
                .await?;

            if calculated_hash != event.integrity_hash {
                result.overall_valid = false;
                result.invalid_events.push(InvalidEventResult {
                    event_id: event.event_id.clone(),
                    event_index: i,
                    expected_hash: calculated_hash,
                    actual_hash: event.integrity_hash.clone(),
                });

                // Check if this indicates chain break
                if i > 0 && event.previous_hash.as_ref() != Some(&events[i-1].integrity_hash) {
                    result.chain_breaks.push(ChainBreak {
                        break_at_index: i,
                        break_event_id: event.event_id.clone(),
                    });
                }
            }
        }

        // Advanced tamper detection
        result.tampering_indicators = self.detect_tampering_patterns(events).await?;

        Ok(result)
    }

    async fn detect_tampering_patterns(
        &self,
        events: &[AuditEvent],
    ) -> Result<Vec<TamperingIndicator>, SecurityError> {
        let mut indicators = Vec::new();

        // Check for timestamp anomalies
        indicators.extend(self.detect_timestamp_anomalies(events)?);

        // Check for hash pattern anomalies
        indicators.extend(self.detect_hash_anomalies(events)?);

        // Check for event sequence anomalies
        indicators.extend(self.detect_sequence_anomalies(events)?);

        // Check for statistical anomalies
        indicators.extend(self.anomaly_detector.analyze_patterns(events).await?);

        Ok(indicators)
    }
}
```

## Performance Optimization

### 1. Hash Calculation Performance
- Use hardware-accelerated SHA-256 implementations when available
- Batch hash calculations for multiple events
- Memory pool allocation for hash calculation buffers
- Parallel hash validation for large audit trail segments

### 2. Caching Strategy
- LRU cache for recent hash validations (1000 entries)
- Cache validation results to avoid recalculation
- Intelligent cache invalidation on chain modifications
- Memory-efficient cache with size limits

### 3. Signature Performance
- Optional signatures only for high-security classifications
- Batch signature creation for multiple events
- Asynchronous signature generation to avoid blocking
- Hardware acceleration using enterprise HSMs

### Performance Targets
- **Hash Calculation**: <1ms per event for SHA-256
- **Chain Validation**: <100ms for 10,000 event chain
- **Signature Creation**: <10ms per signature (when enabled)
- **Memory Overhead**: <256 bytes per event for integrity data

## Security Properties

### Tamper Detection Capabilities
1. **Content Modification**: Any change to event data breaks hash chain
2. **Sequence Tampering**: Reordering events detected through chain breaks
3. **Injection Attacks**: Inserted events lack proper chain linkage
4. **Deletion Attacks**: Missing events create chain gaps
5. **Timestamp Manipulation**: Statistical analysis detects time anomalies

### Cryptographic Guarantees
- **Integrity**: SHA-256 provides 2^128 collision resistance
- **Authentication**: ECDSA signatures provide non-repudiation
- **Chaining**: Previous hash linkage ensures sequence integrity
- **Immutability**: Retroactive changes detectable through chain validation

## Compliance Alignment

### SOX Compliance (Financial Industry)
- Cryptographic controls satisfy Section 404 internal control requirements
- Hash chaining provides audit trail completeness verification
- Digital signatures support non-repudiation requirements
- Key rotation aligns with change management controls

### HIPAA Compliance (Healthcare)
- Integrity controls satisfy Security Rule § 164.312(c)(1)
- Audit trail protection meets Access Control requirements
- Tamper detection supports Assigned Security Responsibility
- Key management aligns with Person or Entity Authentication

### GDPR Compliance (Data Protection)
- Technical measures satisfy Article 32 security requirements
- Audit trail integrity supports accountability principle (Article 5)
- Tamper detection enables breach detection (Article 33)
- Access controls support data subject rights (Articles 15-22)

### PCI DSS Compliance (Payment Cards)
- Cryptographic controls satisfy Requirement 3 (Protect Stored Data)
- Audit integrity meets Requirement 10 (Track and Monitor Access)
- Key management aligns with Requirement 3.6 (Cryptographic Key Management)
- Tamper detection supports Requirement 11 (Regular Security Testing)

## Key Management Integration

### Enterprise Key Management
```rust
pub trait KeyManager {
    async fn get_signing_key(&self, key_id: &str) -> Result<SigningKey, KeyError>;
    async fn get_verification_key(&self, key_id: &str) -> Result<VerificationKey, KeyError>;
    async fn rotate_keys(&self, policy: KeyRotationPolicy) -> Result<(), KeyError>;
}

pub struct HsmKeyManager {
    hsm_client: HsmClient,
    key_store: KeyStore,
    rotation_scheduler: RotationScheduler,
}

impl HsmKeyManager {
    pub async fn initialize_keys(
        &self,
        compliance_profiles: &[ComplianceProfile],
    ) -> Result<(), KeyError> {
        for profile in compliance_profiles {
            let key_spec = self.get_key_specification(profile);
            let key_id = self.hsm_client.generate_key(&key_spec).await?;
            self.key_store.register_key(profile, key_id).await?;
        }
        Ok(())
    }
}
```

### Key Rotation Strategy
- **Automated Rotation**: Keys rotated every 90 days (configurable)
- **Emergency Rotation**: Immediate rotation on security incidents
- **Overlap Period**: New and old keys valid during transition (7 days)
- **Audit Trail**: Key rotation events logged with full audit trail

## Monitoring and Alerting

### Integrity Monitoring
```rust
pub struct IntegrityMonitor {
    validation_scheduler: ValidationScheduler,
    alert_manager: AlertManager,
    metrics_collector: MetricsCollector,
}

impl IntegrityMonitor {
    pub async fn continuous_validation(&self) {
        let mut interval = tokio::time::interval(Duration::from_hours(1));

        loop {
            interval.tick().await;

            // Validate recent audit chain segments
            if let Err(e) = self.validate_recent_chains().await {
                self.alert_manager.send_alert(Alert::ChainValidationFailure {
                    error: e,
                    severity: AlertSeverity::High,
                }).await;
            }

            // Check tamper indicators
            self.check_tamper_indicators().await;

            // Update performance metrics
            self.update_integrity_metrics().await;
        }
    }
}
```

### Performance Metrics
- Monitor hash calculation performance
- Track validation success rates
- Measure signature creation times
- Alert on performance degradation

### Security Alerts
- Chain validation failures
- Tamper detection incidents
- Key rotation failures
- Performance anomalies

## Testing Strategy

### Unit Tests
- Hash calculation accuracy verification
- Chain validation logic testing
- Signature creation and verification
- Error handling and edge cases

### Integration Tests
- End-to-end audit trail creation and validation
- Performance testing under load
- Key rotation workflow testing
- Compliance framework integration

### Security Tests
- Tamper detection accuracy testing
- Cryptographic algorithm validation
- Key management security testing
- Attack simulation and response

### Performance Tests
- High-volume hash chain creation
- Large-scale chain validation
- Signature performance under load
- Memory usage optimization

## Migration and Deployment

### Deployment Strategy
1. **Phase 1**: Deploy hash chaining without signatures
2. **Phase 2**: Enable digital signatures for high-security data
3. **Phase 3**: Full enterprise integration and monitoring
4. **Phase 4**: Advanced tamper detection and analytics

### Backward Compatibility
- Support validation of existing audit trails
- Gradual migration of legacy audit events
- Compatibility mode for mixed environments
- Schema versioning for enterprise deployment

### Disaster Recovery
- Key backup and recovery procedures
- Audit trail replication and verification
- Cross-region integrity validation
- Emergency key rotation procedures

## Alternatives Considered

### Alternative 1: HMAC-Based Integrity
**Approach**: Use HMAC-SHA256 with shared secret for integrity
**Rejected Because**:
- Shared secret management complexity
- No non-repudiation capability
- Single point of failure for secret compromise
- Less suitable for distributed environments

### Alternative 2: Merkle Tree Architecture
**Approach**: Organize audit events in Merkle tree structure
**Rejected Because**:
- Complex implementation for sequential audit events
- Higher memory overhead for tree maintenance
- Overkill for audit trail use case
- Reduced performance for sequential access patterns

### Alternative 3: Blockchain Integration
**Approach**: Store audit hashes in blockchain for immutability
**Rejected Because**:
- Excessive performance overhead
- Complex enterprise integration requirements
- Regulatory uncertainty for blockchain records
- Cost and scalability concerns for high-volume operations

## Related Decisions
- ADR-001: Audit Event Schema Design
- ADR-003: Performance Optimization Strategy
- ADR-004: Multi-Framework Compliance Engine
- ADR-005: Enterprise Integration Patterns

## References
- [NIST FIPS 180-4: Secure Hash Standard (SHS)](https://csrc.nist.gov/publications/detail/fips/180/4/final)
- [NIST FIPS 186-4: Digital Signature Standard (DSS)](https://csrc.nist.gov/publications/detail/fips/186/4/final)
- [RFC 6979: Deterministic Usage of the Digital Signature Algorithm (DSA) and Elliptic Curve Digital Signature Algorithm (ECDSA)](https://tools.ietf.org/html/rfc6979)
- [NIST SP 800-57 Part 1: Key Management Recommendations](https://csrc.nist.gov/publications/detail/sp/800-57-part-1/rev-5/final)
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
