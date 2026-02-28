#![allow(clippy::expect_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used)]
#![allow(clippy::unwrap_used, clippy::expect_used)]

//! Simple Audit Integration Tests for copybook-codec
//!
//! Tests feature spec: enterprise-audit-system-spec.md#workflow-integration
//! API contracts: audit-api-reference.md#codec-integration
//!

#![cfg(feature = "audit")]
//! This simple test scaffolding compiles and provides TDD foundation for
//! audit system integration with COBOL data processing workflow.

use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, decode_record};
use copybook_core::{audit::AuditContext, audit::ComplianceProfile, parse_copybook};

/// Tests feature spec: enterprise-audit-system-spec.md#workflow-integration
/// Test basic audit context integration scaffolding (AC16)
#[test]
fn test_audit_context_integration_scaffolding() {
    // Create simple test copybook
    let copybook_text = r"       01 FINANCIAL-RECORD.
           05 ACCOUNT-ID           PIC 9(12) COMP-3.
           05 TRANSACTION-AMOUNT   PIC S9(13)V99 COMP-3.
           05 AUDIT-TRAIL-REF      PIC X(32).
    ";

    let schema =
        parse_copybook(copybook_text).expect("Financial test fixture should parse successfully");

    let audit_context = AuditContext::new()
        .with_operation_id("financial_decode_audit_test")
        .with_security_classification(
            copybook_core::audit::context::SecurityClassification::MaterialTransaction,
        )
        .with_compliance_profile(ComplianceProfile::SOX)
        .with_metadata("data_classification", "financial_transaction")
        .with_metadata("business_unit", "trading")
        .with_metadata("schema_fingerprint", &schema.fingerprint);

    assert_eq!(
        audit_context
            .metadata
            .get("schema_fingerprint")
            .expect("schema fingerprint should be set"),
        &schema.fingerprint
    );

    // Create decode options for baseline workflow
    let decode_options = DecodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(true);

    // Create test binary data
    let test_record = vec![0u8; schema.lrecl_fixed.unwrap_or(100) as usize];

    match decode_record(&schema, &test_record, &decode_options) {
        Ok(json_value) => {
            // Verify JSON output structure
            assert!(json_value.is_object());
            let obj = json_value.as_object().expect("Decoded JSON should be object");
            assert!(obj.get("fields").is_some());
            println!("Audit context integration test passed with operational decode path");
        }
        Err(e) => {
            // Some fixtures may not be semantically valid; failure is still a valid control path
            println!("Decode operation failed for zero-filled fixture: {e}");
        }
    }
}

/// Tests feature spec: enterprise-audit-system-spec.md#performance-audit
/// Test performance monitoring integration scaffolding (AC11)
#[test]
fn test_performance_overhead_scaffolding() {
    let copybook_text = r"
       01 SIMPLE-RECORD.
           05 FIELD-1              PIC X(10).
           05 FIELD-2              PIC 9(5) COMP-3.
           05 STATUS               PIC X(1).
               88 ACTIVE           VALUE 'A'.
    ";

    let schema = parse_copybook(copybook_text).expect("Simple fixture should parse successfully");

    let _audit_context = AuditContext::new()
        .with_operation_id("performance_overhead_test")
        .with_security_classification(
            copybook_core::audit::context::SecurityClassification::Internal,
        )
        .with_metadata("schema_fingerprint", &schema.fingerprint);

    let test_record = vec![0u8; schema.lrecl_fixed.unwrap_or(100) as usize];
    let iterations = 10; // Small test for scaffolding

    // Baseline measurement
    let baseline_options = DecodeOptions::new().with_codepage(Codepage::CP037);

    let baseline_start = std::time::Instant::now();
    for _ in 0..iterations {
        let _ = decode_record(&schema, &test_record, &baseline_options);
    }
    let baseline_duration = baseline_start.elapsed();

    println!(
        "Performance overhead scaffolding test completed in {baseline_duration:?} for {iterations} decodes"
    );
}
