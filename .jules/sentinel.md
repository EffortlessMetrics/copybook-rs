## 2026-01-22 - Unbounded File Reads in CLI Utilities
**Vulnerability:** The `read_file_or_stdin` utility in `copybook-cli` read the entire input into memory without size limits, exposing the application to Denial of Service (DoS) via memory exhaustion when processing large copybook files.
**Learning:** Utility functions intended for "small" inputs (like configuration or schemas) must strictly enforce those assumptions, as they are often reused in contexts where inputs might be malicious or unexpectedly large.
**Prevention:** Always use `take(limit)` or equivalent bounds when reading from external sources (files, stdin, network) into memory buffers.
