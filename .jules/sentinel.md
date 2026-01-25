## 2026-01-27 - Weak Hashing Algorithms Persistence
**Vulnerability:** MD5 usage for schema fingerprinting persisted despite intent to deprecate.
**Learning:** "Safe" usages of weak algorithms (like non-cryptographic fingerprinting) can still be flagged as security debt and should be proactively upgraded to standard secure algorithms (SHA-256) to avoid confusion and potential misuse.
**Prevention:** Enforce `deny.toml` or similar policies to block `md5` crate usage globally.
