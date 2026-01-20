## 2026-01-14 - [Weak Hashing in Fingerprinting]
**Vulnerability:** MD5 used for schema fingerprinting in `copybook-cli`.
**Learning:** Even non-security features (like structural fingerprints) can flag security scans if they use weak algorithms like MD5.
**Prevention:** Use SHA-256 by default for all hashing needs, even if not strictly cryptographic, to avoid false positives in security audits and future-proof the code.
