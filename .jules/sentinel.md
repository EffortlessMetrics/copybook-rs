# Sentinel's Journal

## 2026-02-14 - Denial of Service via Unlimited Copybook File Read
**Vulnerability:** `copybook-cli` read the entire copybook schema file into memory without any size limit.
**Learning:** Even for "small" configuration files like schemas, assuming they fit in memory is dangerous. A malicious user could provide a massive file (e.g., via `/dev/zero` or a large generated file) to exhaust memory and crash the application (DoS).
**Prevention:** Always enforce hard limits on input sizes, especially when reading fully into memory. Use `take(limit)` on readers to strictly enforce this at the IO level.
