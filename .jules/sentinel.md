## 2026-02-?? - Unbounded File Read DoS
**Vulnerability:** `read_file_or_stdin` in `copybook-cli` read entire inputs into memory without size limits using `read_to_string`.
**Learning:** Even in memory-safe languages like Rust, unbounded resource consumption (memory) is a simple but effective DoS vector. Standard library convenience functions like `fs::read_to_string` are dangerous for untrusted input.
**Prevention:** Always use `reader.take(limit)` or explicit buffer size checks when reading potentially large or untrusted inputs, especially from stdin.
