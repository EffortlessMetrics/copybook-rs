## 2026-02-24 - Unbounded Input Read DoS
**Vulnerability:** `read_file_or_stdin` used `read_to_string` without a limit, allowing attackers to cause OOM by providing massive inputs via file or stdin.
**Learning:** Utility functions intended for "small" configuration or schema files are often reused for larger inputs or exposed to untrusted sources without limits being reconsidered.
**Prevention:** Always use `take(LIMIT)` or equivalent bounded readers when processing external input, especially for text-based formats like copybooks or config files. Define explicit limits for all I/O operations.
