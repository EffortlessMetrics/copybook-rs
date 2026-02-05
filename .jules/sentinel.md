## 2026-02-17 - Unbounded Input Reading
**Vulnerability:** `read_file_or_stdin` in `copybook-cli` read the entire input into a string without size limits, exposing the application to OOM DoS attacks via large files or infinite stdin streams.
**Learning:** Utility functions for reading inputs often default to "read everything" for convenience, bypassing necessary resource constraints.
**Prevention:** Always use bounded readers (e.g., `take()`) or check metadata size before reading untrusted inputs into memory. Enforce explicit limits at the I/O boundary.
