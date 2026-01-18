## 2026-02-18 - [DoS Protection via Resource Limits]
**Vulnerability:** Unbounded file reading in `read_file_or_stdin` allows reading potentially infinite streams (stdin) or massive files into memory, causing OOM crashes.
**Learning:** Utility functions that read entire inputs into memory must enforce limits, especially when processing user-supplied paths or stdin.
**Prevention:** Use `take(limit)` on readers and check file metadata (with TOCTOU awareness) to enforce strict size caps.
