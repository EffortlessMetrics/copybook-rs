<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Issue #35: Enhance Dependency and Security Scanning for Rust Workspace

## Context

copybook-rs is a production-ready enterprise mainframe data processing system that handles sensitive COBOL data requiring strict security compliance (SOX, HIPAA, GDPR, PCI DSS). The current security infrastructure has cargo-deny configured in CI and a comprehensive SECURITY.md, but lacks automated vulnerability scanning, dependency update management, and security artifact retention for audit trails.

**Current State:**
- cargo-deny configured in CI (advisories, licenses, bans, sources)
- SECURITY.md documented with enterprise security practices
- Zero unsafe code policy maintained
- Missing: cargo-audit integration, Dependabot, security receipts, weekly scans, unsafe code enforcement tooling

**Enterprise Impact:**
- Regulatory compliance requires auditable security scanning artifacts with 90-day retention
- Supply chain security critical for mainframe data processing trustworthiness
- Zero unsafe code policy needs automated enforcement (current: manual verification)
- Vulnerability response timeline documented (90-day disclosure) requires proactive scanning

**Affected Components:**
- CI workflows (.github/workflows/ci.yml)
- Security infrastructure (deny.toml, new security-scan.yml, dependabot.yml)
- Artifact retention systems (security receipts as JSON with 90-day retention)
- Workspace-wide crate dependencies (copybook-core, copybook-codec, copybook-cli, copybook-gen, copybook-bench)

## User Story

As a **copybook-rs maintainer**, I want **comprehensive automated dependency and security scanning infrastructure** so that **enterprise users have confidence in supply chain security, regulatory compliance requirements are met with auditable artifacts, and vulnerabilities are detected proactively before production deployment**.

## Acceptance Criteria

**AC1**: cargo-audit integrated into CI as PR quality gate with JSON artifact retention
- CI workflow (.github/workflows/ci.yml) includes cargo-audit job that runs on every PR
- Audit job produces JSON artifact (security-audit-{date}-{sha}.json) with 90-day retention
- Job fails CI pipeline on HIGH or CRITICAL vulnerabilities (--deny warnings flag)
- Job includes all workspace features (--all-features --workspace flags)
- Audit database updated before scan (cargo audit fetch --force)

**AC2**: Weekly security scanning workflow with automatic issue tracking
- New workflow file (.github/workflows/security-scan.yml) runs on cron schedule (weekly)
- Workflow runs cargo-audit with JSON output and full workspace coverage
- On vulnerability detection: creates or updates GitHub issue titled "Security Alert: [date]"
- Issue body includes: vulnerability summary, affected crates, severity levels, remediation guidance
- Workflow produces JSON artifact with 90-day retention for compliance audit trail
- Workflow sends notification on failure (GitHub Actions native notifications)

**AC3**: Dependabot configuration for Cargo and GitHub Actions dependencies
- New .github/dependabot.yml configuration file created
- Cargo ecosystem: daily schedule, grouped updates for patch versions, "dependencies" label
- GitHub Actions ecosystem: weekly schedule, security-only updates prioritized, "ci" label
- Pull request limits configured (max 10 open PRs per ecosystem)
- Conventional commit messages enabled for changelog automation
- Assignees configured for security-sensitive updates

**AC4**: Enhanced deny.toml with stricter security policies
- advisories.vulnerability = "deny" (currently implicit, make explicit)
- advisories.yanked = "deny" (upgrade from "warn")
- advisories.unsound = "deny" (new check for soundness issues)
- advisories.notice = "warn" (informational advisories tracked)
- bans.wildcards = "deny" (upgrade from "allow" for deterministic builds)
- sources.unknown-registry = "deny" (upgrade from "warn" for supply chain security)
- sources.unknown-git = "deny" (upgrade from "warn" for supply chain security)
- Documentation comments explaining enterprise rationale for each stricter policy

**AC5**: Security receipt JSON schema with compliance-grade metadata
- JSON schema includes: timestamp, commit SHA, Rust version, tool versions (cargo-audit, cargo-deny)
- Schema includes: vulnerability count by severity, affected crates list, advisory IDs
- Schema includes: scan type (pr-gate, weekly-scan, manual), scan duration, exit status
- Schema validates via JSON Schema (jsonschema.org draft-07)
- Receipts stored in GitHub Actions artifacts with 90-day retention policy
- README documentation section explaining receipt format and retention for compliance officers

**AC6**: Optional cargo-geiger integration for unsafe code enforcement
- New CI job (optional, allow-failure: true initially) runs cargo-geiger on workspace
- Job produces metrics JSON artifact showing unsafe usage counts per crate
- Job validates zero unsafe code policy (expected: 0 unsafe in all workspace crates)
- Job provides human-readable summary in GitHub Actions summary markdown
- Documentation in SECURITY.md updated with unsafe code verification instructions
- Consideration for future: upgrade to required check after validation period (30 days)

**AC7**: CI performance budget compliance (<2 minutes additional overhead)
- Security jobs parallelized with existing CI jobs (no serial dependency)
- Cargo audit cache configured (advisory-db cached via Swatinem/rust-cache@v2)
- Benchmark CI run before and after implementation to measure actual overhead
- Documentation of measured overhead in security-scan.yml comments
- Optimization applied if overhead exceeds budget (e.g., audit-only on schedule, not every PR)

**AC8**: Documentation updates for enterprise compliance context
- SECURITY.md updated with new scanning infrastructure documentation
- README.md updated with security scanning badges (cargo-audit status, cargo-deny status)
- New docs/SECURITY_SCANNING.md with detailed compliance guide for enterprise users
- Guide includes: scanning frequency, artifact retention rationale, vulnerability response workflow
- Guide includes: integration with enterprise security teams (SIEM, ticketing systems)
- Guide explains how to interpret security receipts for audit purposes

**AC9**: Integration validation via comprehensive testing
- All new CI jobs successfully pass on feature branch before merge
- Weekly scan workflow manually triggered to validate issue creation logic
- Dependabot configuration validated by observing first update PR after merge
- deny.toml stricter policies validated by testing against known yanked/vulnerable test crates
- Security receipt JSON validated against JSON Schema with test fixtures
- cargo-geiger output validated on clean workspace (expect zero unsafe code)

**AC10**: Rollback and incident response procedures documented
- docs/SECURITY_SCANNING.md includes procedure for disabling scans in emergency
- Documentation includes steps for false positive handling and advisory ignore workflow
- Documentation includes escalation path for CRITICAL vulnerabilities requiring immediate patching
- Documentation includes procedure for generating ad-hoc security receipts for audits
- Documentation includes contact information for security team (see SECURITY.md)

## Technical Implementation Notes

### Affected Files and Workflows
- **.github/workflows/ci.yml**: Add cargo-audit job with artifact upload
- **.github/workflows/security-scan.yml**: New weekly scan workflow with issue tracking
- **.github/dependabot.yml**: New Dependabot configuration (Cargo + GitHub Actions)
- **deny.toml**: Enhanced with stricter policies and documentation comments
- **SECURITY.md**: Updated with scanning infrastructure documentation
- **README.md**: Add security scanning badges
- **docs/SECURITY_SCANNING.md**: New comprehensive compliance guide
- **docs/security-receipt-schema.json**: JSON Schema for security receipts

### Enterprise Compliance Requirements
- **Audit Trail**: 90-day JSON artifact retention for SOX/HIPAA/PCI DSS compliance
- **Vulnerability Response**: Automated detection supports documented 90-day disclosure timeline
- **Zero Unsafe Code**: Automated enforcement via cargo-geiger (copybook-rs policy)
- **Supply Chain**: Deterministic builds via stricter deny.toml (wildcards="deny")
- **Regulatory**: Security receipts provide evidence for compliance audits

### Performance Considerations
- **CI Overhead Budget**: <2 minutes additional time (parallelization, caching strategies)
- **Advisory DB Caching**: Reuse advisory-db via rust-cache to reduce network overhead
- **Artifact Retention**: 90-day retention balances compliance needs vs. storage costs
- **Weekly Scans**: Off-peak scheduling to minimize resource contention
- **Notification Overhead**: GitHub native notifications (no external integrations for MVP)

### Tool Versions and Compatibility
- **cargo-audit**: Latest stable (via cargo install or taiki-e/install-action)
- **cargo-deny**: EmbarkStudios/cargo-deny-action@v2 (upgrade from v1 if needed)
- **cargo-geiger**: Latest stable (optional job, allow-failure initially)
- **Dependabot**: Native GitHub integration (no version pinning required)
- **Rust Version**: MSRV 1.90+ maintained, security tools support validated

### Testing Strategy with TDD Integration
- **AC1**: Test cargo-audit CI job runs and produces JSON artifact on test PR
- **AC2**: Manually trigger weekly-scan workflow, validate issue creation
- **AC3**: Merge dependabot.yml, observe first update PR within expected schedule
- **AC4**: Test deny.toml with intentionally yanked crate (negative test)
- **AC5**: Validate security receipt JSON against JSON Schema with jsonschema CLI
- **AC6**: Run cargo-geiger, validate zero unsafe code count in output
- **AC7**: Measure CI duration before/after with GitHub Actions timing data
- **AC8**: Documentation review by maintainers, validate completeness
- **AC9**: Comprehensive integration test: full CI pass, weekly scan, Dependabot PR
- **AC10**: Documentation review by enterprise compliance officer (if available)

### Copybook-rs-Specific Considerations
- **Mainframe Data Processing**: Enterprise compliance critical for customer trust
- **Zero Unsafe Code Policy**: Automated enforcement via cargo-geiger (AC6)
- **Performance Targets**: Security overhead must not impact benchmark baselines
- **Workspace Coverage**: All 5 crates (core, codec, cli, gen, bench) scanned
- **COBOL Parsing Context**: Dependencies impact parsing accuracy (audit critical)
- **Enterprise Deployment**: Security receipts enable enterprise adoption (regulated industries)

### Risk Mitigation
- **False Positives**: Advisory ignore workflow documented in SECURITY_SCANNING.md
- **CI Flakiness**: Advisory DB fetch failures handled with retry logic
- **Performance Regression**: Benchmark validation before/after (AC7)
- **Dependabot Spam**: PR limits (max 10 per ecosystem) prevent overwhelming maintainers
- **Breaking Changes**: Dependabot grouped updates reduce review burden
- **Compliance Gap**: 90-day retention aligns with disclosure timeline, regulatory requirements

### Success Metrics
- **Vulnerability Detection**: Zero HIGH/CRITICAL vulnerabilities in production releases
- **Response Time**: <48 hours for security issue triage (per SECURITY.md)
- **CI Reliability**: Security jobs <2% failure rate (excluding actual vulnerabilities)
- **Dependency Freshness**: <30 days average age for transitive dependencies
- **Compliance Readiness**: Security receipts available on-demand for audits
- **Zero Unsafe Code**: 100% compliance with policy (automated verification)

### Future Enhancements (Out of Scope for Issue #35)
- Integration with SIEM systems for enterprise security monitoring
- Automated CVE scoring and prioritization based on copybook-rs usage patterns
- Supply chain transparency reports (SBOM generation via cargo-sbom)
- Fuzzing infrastructure integration with security scanning
- Private advisory database for proprietary vulnerability intelligence
- Security metrics dashboard for public transparency

## Routing Decision

**NEXT → spec-analyzer** for requirements validation, technical feasibility assessment, and enterprise compliance alignment verification.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
