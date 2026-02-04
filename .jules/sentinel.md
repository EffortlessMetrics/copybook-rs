## 2026-02-18 - Unbounded File Reads in CLI Tools
**Vulnerability:** `copybook-cli` used `std::fs::read_to_string` and `stdin().read_to_string` without limits, exposing the application to Memory Exhaustion (DoS) attacks via large files or infinite streams (e.g., `/dev/zero`).
**Learning:** Checking `fs::metadata(path).len()` is insufficient for special files (like `/dev/zero` or named pipes) which may report 0 size but provide infinite data.
**Prevention:** Always use `take(LIMIT)` or a bounded reader wrapper when processing user-supplied files or streams. Implemented `read_with_limit` helper to enforce strict 16 MiB bounds on all input sources.
