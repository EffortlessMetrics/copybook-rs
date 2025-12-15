# Security Policy

## Supported Versions

copybook-rs maintains security updates for the following versions:

| Version | Supported          |
| ------- | ------------------ |
| 0.3.x   | :white_check_mark: |
| 0.2.x   | :x:                |
| < 0.2   | :x:                |

## Reporting a Vulnerability

We take security vulnerabilities seriously. If you discover a security vulnerability in copybook-rs, please follow responsible disclosure practices.

### How to Report

**DO NOT** create a public GitHub issue for security vulnerabilities.

Instead, please report security vulnerabilities through one of these channels:

1. **GitHub Security Advisories** (Preferred)
   - Go to the [Security tab](https://github.com/EffortlessMetrics/copybook-rs/security/advisories) in our repository
   - Click "Report a vulnerability"
   - Fill out the advisory form with details

2. **Email** (Alternative)
   - Send an email to: security@effortlessmetrics.com
   - Include "copybook-rs Security Vulnerability" in the subject line
   - Provide the details outlined below

### What to Include

Please provide as much information as possible:

- **Description** of the vulnerability
- **Steps to reproduce** the issue
- **Potential impact** and severity assessment
- **COBOL copybook or data samples** (if relevant, but please anonymize sensitive data)
- **Suggested fix** (if you have one)
- **Your contact information** for follow-up questions

### What to Expect

- **Acknowledgment** within 48 hours of report
- **Initial assessment** within 1 week
- **Regular updates** on progress toward a fix
- **Credit** in security advisory (if desired)

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

### Security Development Practices

copybook-rs follows these security practices:

- **Zero unsafe code** - All code maintains Rust's memory safety guarantees
- **Comprehensive testing** - Including property-based testing with `proptest`
- **Static analysis** - Regular clippy lints and security-focused reviews
- **Dependency auditing** - `cargo deny` blocks every PR; `cargo audit` runs on lockfile diffs and in scheduled scans
- **Fuzzing** - Regular fuzzing of parsing logic
- **Security reviews** - Code review process includes security considerations

## Security Scanning Infrastructure

copybook-rs implements comprehensive dependency and security scanning to protect enterprise mainframe data processing systems.

### Automated Security Scanning

**PR Quality Gate (cargo-deny + conditional cargo-audit)**:
- Every pull request runs `cargo deny check` (blocking)
- `cargo audit` runs when `Cargo.lock` changes (or when BASE/HEAD SHAs are available); skips otherwise per policy
- Scan results uploaded as GitHub Actions artifacts (90-day retention)
- CI fails on HIGH or CRITICAL vulnerabilities when `cargo audit` executes

**Weekly Proactive Scanning**:
- Scheduled Monday 03:00 UTC via `.github/workflows/security-scan.yml`
- Automatic GitHub issue creation/update on vulnerability detection
- Manual trigger available via GitHub Actions workflow_dispatch

**Dependency Automation (Dependabot)**:
- Daily dependency update PRs for Cargo ecosystem
- Weekly GitHub Actions version updates
- Security updates grouped separately from routine patches
- PR limits: 10 Cargo dependencies, 5 GitHub Actions

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

### Disclosure Timeline

Our typical timeline for security vulnerability disclosure:

1. **Day 0** - Vulnerability reported
2. **Day 1-2** - Acknowledgment and initial triage
3. **Day 3-7** - Detailed investigation and impact assessment
4. **Day 7-30** - Development and testing of fix
5. **Day 30-60** - Release preparation and coordination
6. **Day 60+** - Public disclosure and security advisory

We aim for responsible disclosure within 90 days but may extend this timeline for complex vulnerabilities or if coordinated disclosure with other projects is required.

### Security Contact

For security-related questions or concerns:
- **Security Team**: security@effortlessmetrics.com
- **Response Time**: 48 hours for security reports
- **PGP Key**: Available upon request

Thank you for helping keep copybook-rs and its users secure!
