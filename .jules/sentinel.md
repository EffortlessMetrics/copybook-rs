# Sentinel Journal

## 2026-02-06 - DoS Protection in CLI Input
**Vulnerability:** Unbounded file/stdin reading in `read_file_or_stdin` allowed memory exhaustion via large inputs.
**Learning:** Utility functions reading "text" often assume small inputs but must enforce limits when exposed to user input (CLI arguments, pipes).
**Prevention:** Use `Read::take(limit)` wrapper for all external input reading to enforce strict upper bounds.
