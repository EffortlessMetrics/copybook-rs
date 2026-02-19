<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Explanations

This directory holds **understanding-oriented** documentation.

Use these documents to understand architecture, design trade-offs, and system behavior.

## Available Explanations

- **[Benchmark Reporting Architecture](benchmark-reporting-architecture.md)**  
  Design rationale for machine-readable performance reporting and operational receipts.
- **[Panic Elimination Architecture](panic-elimination-architecture.md)**  
  Production-safe error model and panic-elimination design strategy.
- **[Panic Elimination Implementation Blueprint](panic-elimination-implementation-blueprint.md)**  
  Blueprint for implementing panic-free behavior in critical paths.
- **[Enterprise Audit Architecture](enterprise-audit-architecture.md)**  
  Architectural overview for audit and compliance surfaces.
- **[Enterprise Audit Implementation Blueprint](enterprise-audit-implementation-blueprint.md)**  
  Detailed architecture and implementation sequence.
- **[Enterprise Audit Integration Patterns](enterprise-audit-integration-patterns.md)**  
  Reference implementation patterns for enterprise telemetry and policy.
- **[Security Scanning Architecture](security-scanning-architecture.md)**  
  Security validation pipeline architecture and design constraints.
- **[Performance Regression Monitoring](performance-regression-monitoring.md)**  
  Why and how regression monitoring is structured.
- **[Test Suite Enhancement Architecture](test-suite-enhancement-architecture.md)**  
  Design for improved correctness testing and reliability signal quality.
- **[Test Suite API Contracts](test-suite-api-contracts.md)**  
  API-focused explanation of suite behavior and outputs.
- **[Panic Elimination API Contracts](panic-elimination-api-contracts.md)**  
  Contract-style detail for panic-elimination behavior.
- **[Enterprise Audit API Contracts](enterprise-audit-api-contracts.md)**  
  Contractual behavior of audit APIs in context.
- **[Enterprise Audit Performance Integration](enterprise-audit-performance-integration.md)**  
  How audit and telemetry constraints interact with throughput and latency.
- **[Enterprise Audit Security Architecture](enterprise-audit-security-architecture.md)**  
  Security model and trust boundaries for audit features.

## Reading Flow

1. Read the architecture overviews first.
2. Then read implementation blueprints.
3. Consult API-contract-style explanations for exact behavior boundaries.
4. Use issue traces or ADRs as implementation history references.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../../LICENSE).
