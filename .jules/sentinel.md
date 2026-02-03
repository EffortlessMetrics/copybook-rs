## 2026-02-23 - Unbounded File Read in CLI
**Vulnerability:** `read_file_or_stdin` in `copybook-cli` permitted unbounded reads of copybook files, creating a Denial of Service (DoS) risk via memory exhaustion if a large file was provided.
**Learning:** Historical documentation or agent memory claimed a `MAX_COPYBOOK_SIZE` limit existed, but code inspection proved it was missing. Trust code over artifacts.
**Prevention:** Enforce explicit size limits (e.g., `take(LIMIT)`) on all user-supplied inputs read into memory, especially from stdin or unknown file paths.
