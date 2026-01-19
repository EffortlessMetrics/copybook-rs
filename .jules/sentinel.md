# Sentinel's Journal

## 2026-01-26 - Enforcing Input Size Limits to Prevent DoS
**Vulnerability:** Missing size limit on copybook input files allowed potentially unbounded memory allocation via `read_to_string` in `copybook-cli`.
**Learning:** Even if documentation claims a limit exists, always verify implementation. `read_to_string` is dangerous for untrusted input.
**Prevention:** Use `reader.take(LIMIT).read_to_string()` or check metadata size (with TOCTOU awareness) before reading entire files into memory.
