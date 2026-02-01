# Sentinel Journal

## 2026-02-24 - Missing Input Size Limits in CLI Utilities
**Vulnerability:** The `read_file_or_stdin` utility in `copybook-cli` read entire files into memory without limits, despite documentation/memory claiming a 16 MiB limit existed.
**Learning:** Documentation and "known state" can drift from actual implementation. Trusted helpers like "read file" are prime spots for DoS vulnerabilities if not bounded.
**Prevention:** Always verify "known" security controls in code. Use bounded readers (`take()`) for all user-supplied inputs.
