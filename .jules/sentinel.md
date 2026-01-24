## 2025-02-14 - DoS Protection in CLI Input Utilities
**Vulnerability:** `read_file_or_stdin` in `copybook-cli` lacked size enforcement, allowing potentially unbounded memory consumption from stdin or large files.
**Learning:** Even internal utility functions used by CLI tools need strict bounds checking, especially when "Enterprise mainframe data processing" implies potential automated pipeline usage where inputs might be untrusted or malformed (infinite streams). Relying on memory assumptions is unsafe.
**Prevention:** Always wrap `io::stdin()` with `.take(LIMIT)` when reading into memory buffers. For files, check metadata size before reading as a fail-fast mechanism, but also enforce `take()` during read to handle potential file growth or TOCTOU race conditions.
