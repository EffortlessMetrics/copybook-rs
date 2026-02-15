# Security Policy

## Supported Versions

copybook-rs maintains security updates for the following versions:

| Version | Supported          | Status |
| ------- | ------------------ | ------ |
| 0.4.x   | :white_check_mark: | Current stable release (Engineering Preview) |
| 0.3.x   | :x:                | No longer supported |
| < 0.3.0 | :x:                | No longer supported |

**Note**: copybook-rs is currently in Engineering Preview (v0.4.x). While the CLI and library APIs are production-ready, feature completeness is still in preview. Security patches are applied to the current 0.4.x release series. See [ROADMAP.md](docs/ROADMAP.md) for version stability timeline and v1.0.0 plans.

## Reporting a Vulnerability

We take security vulnerabilities seriously. If you discover a security vulnerability in copybook-rs, please follow responsible disclosure practices.

### How to Report

**DO NOT** create a public GitHub issue for security vulnerabilities.

Instead, please report security vulnerabilities through one of these channels:

1. **GitHub Security Advisories** (Preferred)
   - Go to the [Security tab](https://github.com/EffortlessMetrics/copybook-rs/security/advisories) in our repository
   - Click "Report a vulnerability"
   - Fill out the advisory form with details

2. **GitHub Issue with Security Label** (Alternative)
   - If you cannot use GitHub Security Advisories, create a regular GitHub issue with the `security` label
   - Include minimal details in the public issue
   - We will follow up with private communication channels for sensitive details

### What to Include

Please provide as much information as possible:

- **Description**: Clear description of the vulnerability and its potential impact
- **Reproduction Steps**: Detailed steps to reproduce the issue
- **Affected Components**: Which copybook-rs crates are affected (copybook-core, copybook-codec, copybook-cli, copybook-gen, copybook-bench)
- **Attack Scenario**: Realistic exploitation scenario demonstrating impact
- **COBOL copybook or data samples** (if relevant, but please anonymize sensitive data)
- **Suggested Fix** (Optional): Proposed mitigation or patch if you have one
- **Your contact information** for follow-up questions

### Response Timeline

We aim to respond to security reports according to the following timeline:

| Phase | Timeline | Description |
|-------|----------|-------------|
| **Initial Acknowledgment** | Within 48 hours | Confirm receipt and begin triage |
| **Triage & Assessment** | Within 7 days | Evaluate severity, impact, and affected versions |
| **Patch Development** | Depends on severity | Develop and test fix |
| **Security Release** | Within 30 days for critical<br>Within 90 days for moderate | Publish patched version |
| **Public Disclosure** | After patch release | Coordinate disclosure timing |
| **Credit** | In security advisory | Recognition in advisory (if desired) |

### Severity Classifications

- **Critical**: Remote code execution, privilege escalation, data exfiltration
- **High**: Authentication bypass, denial of service, memory corruption
- **Moderate**: Information disclosure, input validation bypass
- **Low**: Configuration issues, deprecated features

**Note**: Timelines may vary based on complexity, coordination with dependencies, and responsible disclosure practices.

### Security Considerations for copybook-rs

Given copybook-rs's role in processing enterprise mainframe data, we're particularly concerned about:

#### Data Processing Security
- **Memory safety** - No buffer overflows in COBOL data parsing
- **Input validation** - Proper bounds checking for malformed copybooks
- **Denial of service** - Protection against resource exhaustion attacks
- **Information disclosure** - Preventing sensitive data leakage in error messages

#### Dependency Security
- **Supply chain** - Regular auditing of dependencies with `cargo audit`
- **Vulnerability scanning** - Automated checks for known CVEs
- **Minimal dependencies** - Reducing attack surface

#### Enterprise Environment Concerns
- **Privilege escalation** - CLI tools running with appropriate permissions
- **File system access** - Proper validation of file paths and permissions
- **Network security** - No unexpected network communication
- **Logging security** - Avoiding sensitive data in logs

### Security Best Practices

When using copybook-rs in production:

1. **Keep updated** - Use the latest version with security patches
2. **Validate inputs** - Sanitize copybook files from untrusted sources
3. **Limit permissions** - Run with minimal required privileges
4. **Monitor dependencies** - Use `cargo audit` in your CI/CD pipeline
5. **Secure deployment** - Follow secure deployment practices for your environment

### Known Security Considerations

#### COBOL Data Processing
- **Large files** - copybook-rs processes potentially large mainframe data files
- **Memory usage** - Ensure adequate memory limits to prevent resource exhaustion
- **Error handling** - Error messages are designed to avoid leaking sensitive data

#### Character Encoding
- **EBCDIC conversion** - Proper validation of character set conversions
- **Unicode handling** - Safe handling of character encoding edge cases

## Security Measures

copybook-rs implements multiple layers of security controls to ensure safe operation in enterprise environments:

### Code Safety

- **Zero Unsafe Code**: No `unsafe` blocks in public APIs across all workspace crates
- **Memory Safety**: Rust's ownership model prevents buffer overflows, use-after-free, and data races
- **Clippy Pedantic Enforcement**: All code passes `cargo clippy --workspace -- -D warnings -W clippy::pedantic`
- **Edition 2024**: Modern Rust edition with enhanced safety guarantees
- **MSRV Policy**: Rust 1.92+ enforced at workspace level and validated in CI

### Testing & Validation

- **Comprehensive Test Suite**: 1550+ tests passing (68 skipped/ignored)
- **Golden Fixtures**: SHA-256 verification of test outputs with structural validation
- **Determinism Validation**: Byte-identical results across runs and worker configurations
- **Error Taxonomy**: Stable error codes (CBKP*, CBKS*, CBKD*, CBKE*, CBKR*) for predictable error handling
- **CI Pipeline**: Continuous validation of build, test, clippy, and security scans
- **Property-based testing**: Comprehensive testing with `proptest` for edge cases
- **Fuzzing**: Regular fuzzing of parsing logic to identify vulnerabilities

### Dependency Management

- **Automated Scanning**:
  - `cargo deny` gates every PR (no yanked crates, no wildcards, trusted sources only)
  - `cargo audit` runs on `Cargo.lock` changes and in weekly scheduled scans
  - Weekly security scans run every Monday at 09:00 UTC via `.github/workflows/security-scan.yml`
- **Automated Updates**: Dependabot configured for weekly dependency updates with security fixes prioritized
- **Supply Chain Security**: Enhanced deny.toml policies enforce strict dependency hygiene
- **Dependency Grouping**: Security updates are grouped separately from routine updates for faster triage

See [.github/workflows/security-scan.yml](.github/workflows/security-scan.yml) for the complete security scan implementation and [.github/dependabot.yml](.github/dependabot.yml) for automated update configuration.

### Operational Security

- **Bounded Memory**: Streaming I/O architecture with <256 MiB steady-state for multi-GB files
- **Input Validation**: Comprehensive COBOL parsing with error recovery and bounds checking
- **Data Isolation**: Field projection (`--select`) for selective data processing and minimization
- **Audit Support**: Optional compliance framework (SOX, HIPAA, GDPR, PCI DSS) via feature flags (experimental)

## Security Scanning Infrastructure

copybook-rs implements comprehensive dependency and security scanning to protect enterprise mainframe data processing systems.

### Automated Security Scanning

**PR Quality Gate (cargo-deny + conditional cargo-audit)**:
- Every pull request runs `cargo deny check` (blocking)
- `cargo audit` runs when `Cargo.lock` changes (or when BASE/HEAD SHAs are available); skips otherwise per policy
- Scan results uploaded as GitHub Actions artifacts (90-day retention)
- CI fails on HIGH or CRITICAL vulnerabilities when `cargo audit` executes

**Weekly Proactive Scanning**:
- Scheduled Monday 09:00 UTC via `.github/workflows/security-scan.yml`
- Automatic GitHub issue creation/update on vulnerability detection
- Manual trigger available via GitHub Actions workflow_dispatch
- Security receipts uploaded as artifacts with 90-day retention

**Dependency Automation (Dependabot)**:
- Weekly dependency update PRs for Cargo ecosystem (Monday 09:00 UTC)
- Weekly GitHub Actions version updates (Monday 09:00 UTC)
- Security updates grouped separately from routine patches for faster triage
- PR limits: 10 Cargo dependencies, 5 GitHub Actions
- Automatic rebase strategy for dependency updates

### Supply Chain Security Policies

Enhanced `deny.toml` policies enforce enterprise security requirements:

- **Yanked crates**: Automatically rejected (`yanked = "deny"`)
- **Wildcard dependencies**: Prohibited for deterministic builds (`wildcards = "deny"`)
- **Unknown registries**: Only crates.io allowed (`unknown-registry = "deny"`)
- **Unknown git sources**: Untrusted git dependencies prohibited (`unknown-git = "deny"`)

### Security Receipts & Compliance

**Artifact Retention**:
- Security scan results retained for 90 days (SOX/HIPAA compliance)
- JSON receipts include: timestamp, commit SHA, tool versions, vulnerability details
- Available in GitHub Actions artifacts: `security-receipts-<commit-sha>`

**Regulatory Compliance**:
- **SOX**: Deterministic builds, 90-day audit trail
- **HIPAA**: Continuous vulnerability monitoring
- **GDPR**: Supply chain verification
- **PCI DSS**: Proactive security scanning, yanked crate prevention

### Responding to Security Findings

**Automated Process**:
1. Vulnerability detected → CI fails (PR gate) or GitHub issue created (weekly scan)
2. Security team notified via issue assignment
3. Fix applied within 48 hours for HIGH/CRITICAL (per existing security policy)
4. Dependabot PR reviewed and merged, or manual patch applied
5. Verification scan confirms vulnerability resolution

**Manual Procedures**:
- Review security receipts in workflow artifacts
- Check "Security Alert:" prefixed issues
- Use time-boxed ignores in `deny.toml` for false positives (with expiry dates)

### Emergency Procedures

**Disable Security Scanning (Critical Vulnerabilities)**:
- Edit `.github/workflows/ci.yml`: Comment out security-audit job temporarily
- Notify security team with justification and timeline for re-enablement
- Document in pull request description

**Rollback Procedures**:
See `docs/how-to/configure-security-scanning.md` §8 for detailed rollback instructions.

## Security Scope

### In Scope

Security issues within these copybook-rs workspace crates:

- **copybook-core**: COBOL parsing (lexer, parser, AST, layout resolution)
- **copybook-codec**: Data encoding/decoding, character conversion, structural validation
- **copybook-cli**: Command-line interface and subcommands
- **copybook-gen**: Test fixture generation (development-only, lower priority)
- **copybook-bench**: Performance benchmarks (development-only, lower priority)

### Out of Scope

- **Dependency Vulnerabilities**: Issues in third-party crates (report upstream, we'll apply updates)
- **Deployment Security**: User's container/VM configuration, network security, access controls
- **Data Security**: User's data classification, encryption at rest, key management
- **COBOL Compiler Bugs**: Issues in COBOL compilers producing the copybooks
- **Mainframe Security**: Security of source mainframe systems

### Borderline Cases

- **Malformed Copybooks**: Parsing crashes or memory exhaustion may be bugs, not vulnerabilities (DoS potential evaluated case-by-case)
- **Data Validation**: Incorrect COBOL data decoding is typically a correctness bug unless it leaks sensitive data

### Disclosure Timeline

Our typical timeline for security vulnerability disclosure:

1. **Day 0** - Vulnerability reported
2. **Day 1-2** - Acknowledgment and initial triage
3. **Day 3-7** - Detailed investigation and impact assessment
4. **Day 7-30** - Development and testing of fix
5. **Day 30-60** - Release preparation and coordination
6. **Day 60+** - Public disclosure and security advisory

We aim for responsible disclosure within 90 days but may extend this timeline for complex vulnerabilities or if coordinated disclosure with other projects is required.

## Security Updates & Advisories

Security advisories and patches are published through:

- **GitHub Security Advisories**: [https://github.com/EffortlessMetrics/copybook-rs/security/advisories](https://github.com/EffortlessMetrics/copybook-rs/security/advisories)
- **Release Notes**: Security fixes noted in release changelogs
- **GitHub Issues**: Public security issues labeled with `security` (post-disclosure)

Subscribe to [GitHub repository notifications](https://github.com/EffortlessMetrics/copybook-rs/subscription) to receive security updates.

## Contact

For security-related questions that are not vulnerabilities:

- Open a [GitHub Discussion](https://github.com/EffortlessMetrics/copybook-rs/discussions)
- Create a GitHub issue with the `security` and `question` labels

For commercial licensing or enterprise security inquiries, see [README.md](README.md#license) for contact information.

---

**Last Updated**: 2026-01-12
**Security Contact**: Use GitHub Security Advisories (preferred) or GitHub Issues with `security` label

Thank you for helping keep copybook-rs and its users secure!
