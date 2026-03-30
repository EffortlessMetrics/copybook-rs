<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Support Policy

This document defines the support commitment for copybook-rs releases.

## Support Window

| Release Type | Support Duration | Notes |
|--------------|------------------|-------|
| **Latest minor** | 12 months | Full support including bug fixes and security patches |
| **Previous minor** | 6 months | Security patches only |
| **Older releases** | Unsupported | Upgrade to a supported version |

**Example**: When v1.2.0 is released, v1.1.x receives 6 months of security-only support, and v1.0.x becomes unsupported.

## Security Patch Policy

- Critical security vulnerabilities (CVSS >= 7.0) will be patched in all supported minor versions
- Non-critical security issues will be patched in the latest minor version only
- Security patches will be released within 14 days of disclosure (or sooner for active exploits)
- Security advisories will be published via GitHub Security Advisories

## Critical Bug Fix Policy

- Critical bugs (data corruption, crashes, security issues) will be fixed in all supported minor versions
- Non-critical bugs will be fixed in the latest minor version only
- Bug fix releases will be made available within 7 days of confirmed critical issues

## Getting Support

**Issue Reporting**
- Report bugs via [GitHub Issues](https://github.com/EffortlessMetrics/copybook-rs/issues)
- Use the provided issue templates for bug reports and feature requests
- Include minimal reproducible examples and copybook/data fixtures when possible

**Support Channels**
- **GitHub Issues**: Primary channel for bug reports and feature requests
- **GitHub Discussions**: For questions, usage help, and community discussion
- **Email**: For security issues only (see [SECURITY.md](../SECURITY.md))

**Response Time Expectations**

| Issue Priority | Response Time | Resolution Time |
|----------------|---------------|-----------------|
| **Critical** (security, data loss) | 48 hours | 7 days |
| **High** (blocking production) | 1 week | 2 weeks |
| **Normal** (bugs, feature requests) | 2 weeks | Best effort |
| **Low** (documentation, minor issues) | 1 month | Best effort |

**Note**: These are best-effort targets. Response times may vary based on maintainer availability and issue complexity.

---

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
