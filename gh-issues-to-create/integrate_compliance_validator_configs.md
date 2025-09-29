---
title: Integrate Configuration into Compliance Validators in `copybook-core/src/audit/compliance.rs`
labels: ["feature", "incomplete", "code-quality"]
assignees: []
---

## Issue Description

Multiple compliance validator structs in `copybook-core/src/audit/compliance.rs` (`SoxValidator`, `HipaaValidator`, `GdprValidator`, and `PciDssValidator`) contain a `config` field that is marked with `#[allow(dead_code)]`. This indicates that while configuration structures (`SoxConfig`, `HipaaConfig`, etc.) are defined and initialized, they are not actively being used within the respective `validate_*` methods of these validators.

This suggests that the compliance checks are either hardcoded or not yet fully implemented to leverage external configurations, limiting the flexibility and adaptability of the auditing process. The presence of `#[allow(dead_code)]` masks potential issues and indicates incomplete functionality.

## Locations

*   `copybook-core/src/audit/compliance.rs:244` (`SoxValidator::config`)
*   `copybook-core/src/audit/compliance.rs:372` (`HipaaValidator::config`)
*   `copybook-core/src/audit/compliance.rs:499` (`GdprValidator::config`)
*   `copybook-core/src/audit/compliance.rs:616` (`PciDssValidator::config`)

## Proposed Fix

1.  **Utilize Configuration:** For each validator, modify its `validate_*` methods to actively use the parameters defined in its corresponding `config` field. The configuration should dictate the behavior and rules of the compliance checks, allowing for dynamic and configurable validation.
2.  **Remove `#[allow(dead_code)]`:** Once the `config` fields are integrated and actively used in the validation logic, remove the `#[allow(dead_code)]` attributes from their declarations.
3.  **Enhance Configuration Structures:** Review and enhance `SoxConfig`, `HipaaConfig`, `GdprConfig`, and `PciDssConfig` to include all necessary parameters that would influence their respective compliance checks.
4.  **Add Tests:** Implement or update unit and integration tests to ensure that the validators correctly apply the rules defined in their configurations.

## Example (Illustrative - for `SoxValidator`)

```rust
// Before (simplified):
// pub struct SoxValidator {
//     #[allow(dead_code)]
//     config: SoxConfig,
// }
//
// impl SoxValidator {
//     pub fn new() -> Self { /* ... */ }
//
//     fn validate_financial_data_controls(&self, context: &AuditContext) -> Vec<ComplianceViolation> {
//         let mut violations = Vec::new();
//         // ... validation logic that does not use self.config ...
//         violations
//     }
// }

// After (conceptual - using config):
// pub struct SoxValidator {
//     config: SoxConfig,
// }
//
// impl SoxValidator {
//     pub fn new() -> Self { /* ... */ }
//
//     fn validate_financial_data_controls(&self, context: &AuditContext) -> Vec<ComplianceViolation> {
//         let mut violations = Vec::new();
//
//         // Example: Use a setting from the config to determine a validation rule
//         if self.config.requires_audit_trail {
//             if !context.has_audit_trail() {
//                 violations.push(ComplianceViolation::new("SOX: Audit trail required but not found."));
//             }
//         }
//
//         // Example: Use a threshold from the config
//         if context.transaction_volume() > self.config.max_transaction_volume {
//             violations.push(ComplianceViolation::new("SOX: Transaction volume exceeds configured limit."));
//         }
//
//         violations
//     }
// }
```

This will make the compliance validators more robust, configurable, and aligned with their intended purpose.
