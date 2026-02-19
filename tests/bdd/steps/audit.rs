// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_core::audit::context::SecurityClassification;
use copybook_core::audit::event::{
    AccessResult as AuditAccessResult, AccessType as AuditAccessType, ComparisonResult,
    ComplianceValidationResult, ConfigurationChangeType, ParseResult as AuditParseResult,
    PerformanceMeasurementType, PerformanceMetrics, SecurityEventType, TransformationOperation,
    TransformationResult, UserImpactLevel, ValidationResult as AuditValidationResult,
};
use copybook_core::audit::{
    AuditContext, AuditEvent, AuditEventType, AuditPayload, ComplianceProfile, validate_audit_chain,
};
use copybook_core::parse_copybook;
use cucumber::{given, then, when};
use serde_json::Value;

use crate::world::CopybookWorld;

// --- Audit Given Steps ---

#[given(expr = "the audit system is enabled")]
async fn given_audit_system_enabled(world: &mut CopybookWorld) {
    world.audit_events.clear();
    world.regression_metrics = false;
}

#[given(expr = "an audit context is initialized")]
async fn given_audit_context_initialized(world: &mut CopybookWorld) {
    let context = AuditContext::new()
        .with_operation_id("bdd_test_operation")
        .with_user("bdd_test_user")
        .with_metadata("session_id", "bdd_test_session");
    world.audit_context = Some(context);
}

#[given(expr = "a financial SOX compliance copybook")]
async fn given_sox_compliance_copybook(world: &mut CopybookWorld) {
    world.copybook_text = Some(
        "01 FINANCIAL-TRANSACTION.\n\
         05 TRANSACTION-ID PIC X(20).\n\
         05 TRANSACTION-AMOUNT PIC S9(13)V99 COMP-3.\n\
         05 TRANSACTION-DATE PIC 9(8).\n\
         05 ACCOUNT-NUMBER PIC X(12).\n\
         05 TRANSACTION-TYPE PIC X(2)."
            .to_string(),
    );
    if let Some(ref mut ctx) = world.audit_context {
        let updated = ctx
            .clone()
            .with_compliance_profile(ComplianceProfile::SOX)
            .with_security_classification(SecurityClassification::MaterialTransaction);
        *ctx = updated;
    }
    world.compliance_profile = Some(ComplianceProfile::SOX);
}

#[given(expr = "a healthcare HIPAA compliance copybook")]
async fn given_hipaa_compliance_copybook(world: &mut CopybookWorld) {
    world.copybook_text = Some(
        "01 PATIENT-RECORD.\n\
         05 PATIENT-ID PIC X(20).\n\
         05 PATIENT-NAME PIC X(50).\n\
         05 DIAGNOSIS-CODE PIC X(10).\n\
         05 TREATMENT-DATE PIC 9(8).\n\
         05 PROVIDER-ID PIC X(15)."
            .to_string(),
    );
    if let Some(ref mut ctx) = world.audit_context {
        let updated = ctx
            .clone()
            .with_compliance_profile(ComplianceProfile::HIPAA)
            .with_security_classification(SecurityClassification::PHI)
            .with_metadata("data_classification", "PHI");
        *ctx = updated;
    }
    world.compliance_profile = Some(ComplianceProfile::HIPAA);
}

#[given(expr = "a GDPR personal data copybook")]
async fn given_gdpr_compliance_copybook(world: &mut CopybookWorld) {
    world.copybook_text = Some(
        "01 PERSONAL-DATA.\n\
         05 DATA-SUBJECT-ID PIC X(20).\n\
         05 FULL-NAME PIC X(50).\n\
         05 EMAIL-ADDRESS PIC X(100).\n\
         05 CONSENT-DATE PIC 9(8)."
            .to_string(),
    );
    if let Some(ref mut ctx) = world.audit_context {
        let updated = ctx
            .clone()
            .with_compliance_profile(ComplianceProfile::GDPR)
            .with_security_classification(SecurityClassification::Confidential)
            .with_metadata("legal_basis", "consent")
            .with_metadata("processing_purpose", "data_processing");
        *ctx = updated;
    }
    world.compliance_profile = Some(ComplianceProfile::GDPR);
}

#[given(expr = "a PCI DSS payment card copybook")]
async fn given_pci_dss_compliance_copybook(world: &mut CopybookWorld) {
    world.copybook_text = Some(
        "01 PAYMENT-CARD.\n\
         05 CARD-NUMBER PIC X(16).\n\
         05 EXPIRY-DATE PIC 9(4).\n\
         05 CARDHOLDER-NAME PIC X(50).\n\
         05 CVV PIC 9(3)."
            .to_string(),
    );
    if let Some(ref mut ctx) = world.audit_context {
        let updated = ctx
            .clone()
            .with_compliance_profile(ComplianceProfile::PciDss)
            .with_security_classification(SecurityClassification::Confidential)
            .with_metadata("cardholder_data", "present");
        *ctx = updated;
    }
    world.compliance_profile = Some(ComplianceProfile::PciDss);
}

#[given(expr = "a simple audit copybook")]
async fn given_simple_audit_copybook(world: &mut CopybookWorld) {
    world.copybook_text = Some(
        "01 AUDIT-RECORD.\n\
         05 AUDIT-FIELD PIC X(10)."
            .to_string(),
    );
}

#[given(expr = "compliance profile {string}")]
async fn given_compliance_profile(world: &mut CopybookWorld, profile_str: String) {
    let profile = match profile_str.as_str() {
        "SOX" => ComplianceProfile::SOX,
        "HIPAA" => ComplianceProfile::HIPAA,
        "GDPR" => ComplianceProfile::GDPR,
        "PciDss" | "PCI_DSS" | "PCI DSS" => ComplianceProfile::PciDss,
        _ => panic!("Unknown compliance profile: {}", profile_str),
    };

    if let Some(ref mut ctx) = world.audit_context {
        let updated = ctx.clone().with_compliance_profile(profile);
        *ctx = updated;
    }
}

#[given(expr = "security classification {string}")]
async fn given_security_classification(world: &mut CopybookWorld, classification_str: String) {
    let classification = match classification_str.as_str() {
        "Public" => SecurityClassification::Public,
        "Internal" => SecurityClassification::Internal,
        "Confidential" => SecurityClassification::Confidential,
        "MaterialTransaction" => SecurityClassification::MaterialTransaction,
        "PHI" => SecurityClassification::PHI,
        _ => panic!("Unknown security classification: {}", classification_str),
    };

    if let Some(ref mut ctx) = world.audit_context {
        let updated = ctx
            .clone()
            .with_security_classification(classification.clone());
        *ctx = updated;
    }
    world.security_classification = Some(classification);
}

#[given(expr = "audit metadata {string} is {string}")]
async fn given_audit_metadata(world: &mut CopybookWorld, key: String, value: String) {
    if let Some(ref mut ctx) = world.audit_context {
        let updated = ctx.clone().with_metadata(key, value);
        *ctx = updated;
    }
}

#[given(expr = "throughput metrics below baseline")]
async fn given_throughput_metrics_below_baseline(world: &mut CopybookWorld) {
    world.regression_metrics = true;
}

// --- Audit When Steps ---

#[when(expr = "the copybook is processed for financial data")]
async fn when_copybook_processed_financial(world: &mut CopybookWorld) {
    when_copybook_processed_with_audit(world).await;
}

#[when(expr = "the copybook is processed with audit")]
async fn when_copybook_processed_with_audit(world: &mut CopybookWorld) {
    if let Some(ref text) = world.copybook_text.clone() {
        match parse_copybook(text) {
            Ok(schema) => {
                let field_count = schema.all_fields().len();
                let level_88_count = schema.all_fields().iter().filter(|f| f.level == 88).count();
                let fingerprint = schema.fingerprint.clone();
                world.schema = Some(schema);

                if let Some(ref context) = world.audit_context.clone() {
                    let event = AuditEvent::new(
                        AuditEventType::CopybookParse,
                        context.clone(),
                        AuditPayload::CopybookParse {
                            copybook_path: "bdd_test.cpy".to_string(),
                            schema_fingerprint: fingerprint,
                            parse_result: AuditParseResult::Success,
                            parsing_duration_ms: 1,
                            field_count,
                            level_88_count,
                            error_count: 0,
                            warnings: vec![],
                        },
                    );
                    world.audit_events.push(event);
                }
            }
            Err(e) => {
                world.error = Some(e);
            }
        }
    }
}

#[when(expr = "a decode audit event is created")]
async fn when_decode_audit_event_created(world: &mut CopybookWorld) {
    world.ensure_schema_parsed();

    if let Some(ref context) = world.audit_context.clone() {
        let event = AuditEvent::new(
            AuditEventType::DataTransformation,
            context.clone(),
            AuditPayload::DataTransformation {
                operation: TransformationOperation::Decode,
                input_file: "test_input.bin".to_string(),
                output_file: "test_output.jsonl".to_string(),
                transformation_result: TransformationResult::Success,
                processing_duration_ms: 10,
                records_processed: 100,
                bytes_processed: 5000,
                throughput_bytes_per_sec: 500_000,
                memory_usage_mb: 16,
            },
        );
        world.audit_events.push(event);
    }
}

#[when(expr = "an encode audit event is created")]
async fn when_encode_audit_event_created(world: &mut CopybookWorld) {
    world.ensure_schema_parsed();

    if let Some(ref context) = world.audit_context.clone() {
        let event = AuditEvent::new(
            AuditEventType::DataTransformation,
            context.clone(),
            AuditPayload::DataTransformation {
                operation: TransformationOperation::Encode,
                input_file: "test_input.jsonl".to_string(),
                output_file: "test_output.bin".to_string(),
                transformation_result: TransformationResult::Success,
                processing_duration_ms: 8,
                records_processed: 50,
                bytes_processed: 2500,
                throughput_bytes_per_sec: 312_500,
                memory_usage_mb: 12,
            },
        );
        world.audit_events.push(event);
    }
}

#[when(expr = "a projection audit event is created")]
async fn when_projection_audit_event_created(world: &mut CopybookWorld) {
    world.ensure_schema_parsed();

    if let Some(ref context) = world.audit_context.clone() {
        let event = AuditEvent::new(
            AuditEventType::DataValidation,
            context.clone(),
            AuditPayload::DataValidation {
                input_file: "test_input.bin".to_string(),
                validation_result: AuditValidationResult::Valid,
                validation_duration_ms: 5,
                records_validated: 100,
                errors_found: 0,
                error_details: vec![],
                validation_rules: vec!["AUDIT-FIELD".to_string()],
            },
        );
        world.audit_events.push(event);
    }
}

#[when(expr = "a configuration change audit event is created")]
async fn when_config_change_audit_event_created(world: &mut CopybookWorld) {
    if let Some(ref context) = world.audit_context.clone() {
        let event = AuditEvent::new(
            AuditEventType::ConfigurationChange,
            context.clone(),
            AuditPayload::ConfigurationChange {
                component: "dialect".to_string(),
                change_type: ConfigurationChangeType::Update,
                old_configuration: Some("Normative".to_string()),
                new_configuration: "ZeroTolerant".to_string(),
                change_reason: "IBM Enterprise COBOL compatibility".to_string(),
                approved_by: Some("bdd_test_user".to_string()),
                rollback_available: true,
            },
        );
        world.audit_events.push(event);
    }
}

#[when(expr = "a security event is created")]
async fn when_security_event_created(world: &mut CopybookWorld) {
    if let Some(ref context) = world.audit_context.clone() {
        let event = AuditEvent::new(
            AuditEventType::SecurityEvent,
            context.clone(),
            AuditPayload::SecurityEvent {
                security_event_type: SecurityEventType::SuspiciousActivity,
                severity: "High".to_string(),
                affected_resources: vec!["copybook_data".to_string()],
                threat_indicators: vec![],
                remediation_actions: vec!["review_access".to_string()],
                incident_id: Some("SEC-001".to_string()),
            },
        );
        world.audit_events.push(event);
    }
}

#[when(expr = "an access event is created with result {string}")]
async fn when_access_event_created(world: &mut CopybookWorld, result_str: String) {
    let access_result = match result_str.as_str() {
        "Success" => AuditAccessResult::Success,
        "Denied" => AuditAccessResult::Denied,
        "Failed" => AuditAccessResult::Failed,
        _ => panic!("Unknown access result: {}", result_str),
    };

    if let Some(ref context) = world.audit_context.clone() {
        let event = AuditEvent::new(
            AuditEventType::AccessEvent,
            context.clone(),
            AuditPayload::AccessEvent {
                access_type: AuditAccessType::Read,
                resource_type: "copybook_data".to_string(),
                resource_id: "test_resource".to_string(),
                access_result,
                user_id: "bdd_test_user".to_string(),
                source_ip: Some("127.0.0.1".to_string()),
                user_agent: Some("bdd_test".to_string()),
                session_id: Some("bdd_session".to_string()),
            },
        );
        world.audit_events.push(event);
    }
}

#[when(expr = "an error audit event is created")]
async fn when_error_audit_event_created(world: &mut CopybookWorld) {
    if let Some(ref context) = world.audit_context.clone() {
        let event = AuditEvent::new(
            AuditEventType::ErrorEvent,
            context.clone(),
            AuditPayload::ErrorEvent {
                error_code: "CBKD001".to_string(),
                error_message: "Invalid decimal value in field".to_string(),
                error_category: "data_validation".to_string(),
                stack_trace: None,
                context_information: std::collections::HashMap::new(),
                recovery_actions: vec!["retry_with_defaults".to_string()],
                user_impact: UserImpactLevel::Low,
            },
        );
        world.audit_events.push(event);
    }
}

#[when(expr = "a performance measurement event is created")]
async fn when_performance_measurement_created(world: &mut CopybookWorld) {
    let regression_detected = world.regression_metrics;
    let comparison = if regression_detected {
        Some(ComparisonResult::SignificantRegression)
    } else {
        Some(ComparisonResult::WithinBaseline)
    };

    if let Some(ref context) = world.audit_context.clone() {
        let event = AuditEvent::new(
            AuditEventType::PerformanceMeasurement,
            context.clone(),
            AuditPayload::PerformanceMeasurement {
                measurement_type: PerformanceMeasurementType::Throughput,
                baseline_id: Some("baseline_001".to_string()),
                metrics: PerformanceMetrics {
                    throughput_bytes_per_sec: if regression_detected {
                        50_000_000
                    } else {
                        200_000_000
                    },
                    latency_ms: 10,
                    cpu_usage_percent: 45.0,
                    memory_usage_mb: 128,
                    io_operations: 1000,
                },
                comparison_result: comparison,
                regression_detected,
            },
        );
        world.audit_events.push(event);
    }
}

#[when(expr = "a compliance check event is created")]
async fn when_compliance_check_event_created(world: &mut CopybookWorld) {
    if let Some(ref context) = world.audit_context.clone() {
        let frameworks: Vec<String> = context
            .compliance_profiles
            .iter()
            .map(|p| format!("{:?}", p))
            .collect();
        let framework_str = frameworks.join(", ");

        let event = AuditEvent::new(
            AuditEventType::ComplianceCheck,
            context.clone(),
            AuditPayload::ComplianceCheck {
                compliance_framework: framework_str,
                validation_result: ComplianceValidationResult::Compliant,
                violations: vec![],
                remediation_required: false,
                next_review_date: Some("2026-06-01".to_string()),
            },
        );
        world.audit_events.push(event);
    }
}

#[when(expr = "a child context is created for {string}")]
async fn when_child_context_created(world: &mut CopybookWorld, child_op: String) {
    let parent_ctx = world.audit_context.as_ref().expect("Audit context not set");
    let child = parent_ctx.create_child_context(child_op);
    world.child_audit_context = Some(child);
}

// --- Audit Then Steps ---

#[then(expr = "an audit trail should be generated for SOX compliance")]
async fn then_audit_trail_generated_sox(world: &mut CopybookWorld) {
    assert!(
        !world.audit_events.is_empty(),
        "Audit events should be generated"
    );
    let event = &world.audit_events[0];
    assert_eq!(event.event_type, AuditEventType::CopybookParse);
}

#[then(expr = "the audit output should be in valid JSON format")]
async fn then_audit_output_valid_json(world: &mut CopybookWorld) {
    assert!(
        !world.audit_events.is_empty(),
        "Audit events should be generated"
    );

    let json_output = serde_json::to_string_pretty(&world.audit_events)
        .expect("Audit events should serialize to JSON");
    world.audit_output = Some(json_output.clone());

    let _: Value = serde_json::from_str(&json_output).expect("Audit output should be valid JSON");
}

#[then(expr = "the audit event type should be {string}")]
async fn then_audit_event_type(world: &mut CopybookWorld, expected_type: String) {
    let event = world.audit_events.last().expect("No audit events");
    let actual_type = format!("{:?}", event.event_type);
    assert_eq!(
        actual_type, expected_type,
        "Expected audit event type '{}', got '{}'",
        expected_type, actual_type
    );
}

#[then(expr = "the audit event severity should be {string}")]
async fn then_audit_event_severity(world: &mut CopybookWorld, expected_severity: String) {
    let event = world.audit_events.last().expect("No audit events");
    let actual_severity = format!("{:?}", event.severity);
    assert_eq!(
        actual_severity, expected_severity,
        "Expected audit event severity '{}', got '{}'",
        expected_severity, actual_severity
    );
}

#[then(expr = "the audit context should have compliance profile {string}")]
async fn then_audit_context_has_compliance_profile(
    world: &mut CopybookWorld,
    expected_profile: String,
) {
    let event = world.audit_events.last().expect("No audit events");
    let profiles: Vec<String> = event
        .context
        .compliance_profiles
        .iter()
        .map(|p| format!("{:?}", p))
        .collect();
    assert!(
        profiles.contains(&expected_profile),
        "Expected audit context to have compliance profile '{}', found: {:?}",
        expected_profile,
        profiles
    );
}

#[then(expr = "the audit context should have security classification {string}")]
async fn then_audit_context_has_security_classification(
    world: &mut CopybookWorld,
    expected_classification: String,
) {
    let event = world.audit_events.last().expect("No audit events");
    let actual = format!("{:?}", event.context.security.classification);
    assert_eq!(
        actual, expected_classification,
        "Expected security classification '{}', got '{}'",
        expected_classification, actual
    );
}

#[then(expr = "the audit context metadata should contain {string}")]
async fn then_audit_context_metadata_contains(world: &mut CopybookWorld, key: String) {
    let event = world.audit_events.last().expect("No audit events");
    assert!(
        event.context.metadata.contains_key(&key),
        "Expected audit context metadata to contain key '{}', found: {:?}",
        key,
        event.context.metadata.keys().collect::<Vec<_>>()
    );
}

#[then(expr = "the audit payload should contain {string}")]
async fn then_audit_payload_contains(world: &mut CopybookWorld, substring: String) {
    let event = world.audit_events.last().expect("No audit events");
    let payload_json =
        serde_json::to_string(&event.payload).expect("Payload should serialize to JSON");
    assert!(
        payload_json.contains(&substring),
        "Expected audit payload to contain '{}', got: {}",
        substring,
        payload_json
    );
}

#[then(expr = "the audit event integrity hash should be non-empty")]
async fn then_audit_event_integrity_hash_non_empty(world: &mut CopybookWorld) {
    let event = world.audit_events.last().expect("No audit events");
    assert!(
        !event.integrity_hash.is_empty(),
        "Audit event integrity hash should be non-empty"
    );
}

#[then(expr = "the audit event timestamp should be non-empty")]
async fn then_audit_event_timestamp_non_empty(world: &mut CopybookWorld) {
    let event = world.audit_events.last().expect("No audit events");
    assert!(
        !event.timestamp.is_empty(),
        "Audit event timestamp should be non-empty"
    );
}

#[then(expr = "the audit event user should be {string}")]
async fn then_audit_event_user(world: &mut CopybookWorld, expected_user: String) {
    let event = world.audit_events.last().expect("No audit events");
    assert_eq!(
        event.context.user.as_deref(),
        Some(expected_user.as_str()),
        "Expected audit event user '{}', got '{:?}'",
        expected_user,
        event.context.user
    );
}

#[then(expr = "the audit trail should have {int} events")]
async fn then_audit_trail_has_events(world: &mut CopybookWorld, expected_count: usize) {
    assert_eq!(
        world.audit_events.len(),
        expected_count,
        "Expected {} audit events, got {}",
        expected_count,
        world.audit_events.len()
    );
}

#[then(expr = "the audit chain should be valid")]
async fn then_audit_chain_valid(world: &mut CopybookWorld) {
    assert!(
        world.audit_events.len() >= 2,
        "Need at least 2 events for chain validation, got {}",
        world.audit_events.len()
    );

    let mut chained_events: Vec<AuditEvent> = Vec::new();
    for (i, event) in world.audit_events.iter().enumerate() {
        let mut chained = event.clone();
        if i > 0 {
            let prev_hash = chained_events[i - 1].integrity_hash.clone();
            chained = chained.with_previous_hash(prev_hash);
        }
        chained_events.push(chained);
    }

    match validate_audit_chain(&chained_events) {
        Ok(valid) => {
            assert!(valid, "Audit chain validation should return true");
        }
        Err(e) => {
            panic!("Audit chain validation failed: {}", e);
        }
    }
}

#[then(expr = "the child context should have parent operation")]
async fn then_child_context_has_parent(world: &mut CopybookWorld) {
    let child = world
        .child_audit_context
        .as_ref()
        .expect("Child audit context not set");
    assert!(
        child.parent_operation_id.is_some(),
        "Child context should have parent operation ID"
    );
}

#[then(expr = "the child context operation id should differ from parent")]
async fn then_child_context_operation_id_differs(world: &mut CopybookWorld) {
    let parent = world.audit_context.as_ref().expect("Audit context not set");
    let child = world
        .child_audit_context
        .as_ref()
        .expect("Child audit context not set");

    assert_ne!(
        parent.operation_id, child.operation_id,
        "Child context operation ID should differ from parent"
    );
    assert_eq!(
        child.parent_operation_id.as_deref(),
        Some(parent.operation_id.as_str()),
        "Child's parent_operation_id should match parent's operation_id"
    );
}

#[then(expr = "all audit events should have required fields")]
async fn then_all_audit_events_required_fields(world: &mut CopybookWorld) {
    for event in &world.audit_events {
        assert!(
            !event.event_id.is_empty(),
            "Event should have non-empty event_id"
        );
        assert!(
            !event.timestamp.is_empty(),
            "Event should have non-empty timestamp"
        );
        assert!(
            !event.integrity_hash.is_empty(),
            "Event should have non-empty integrity_hash"
        );
        assert!(
            !event.source.is_empty(),
            "Event should have non-empty source"
        );
        assert!(
            !event.event_version.is_empty(),
            "Event should have non-empty event_version"
        );
    }
}

#[then(expr = "the audit JSON should contain field {string}")]
async fn then_audit_json_contains_field(world: &mut CopybookWorld, field_name: String) {
    let event = world.audit_events.last().expect("No audit events");
    let json = serde_json::to_string(event).expect("Event should serialize to JSON");
    assert!(
        json.contains(&field_name),
        "Expected audit JSON to contain field '{}', got: {}",
        field_name,
        json
    );
}
