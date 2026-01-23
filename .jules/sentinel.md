## 2026-01-12 - [DoS Vulnerability in File Reading]
**Vulnerability:** `copybook-cli`'s `read_file_or_stdin` utility was using `read_to_string` without a size limit, allowing arbitrarily large files to be read into memory, potentially causing a Denial of Service (OOM).
**Learning:** Documented security features (like the 16 MiB limit mentioned in memory) must be verified in code. "Convenience" functions like `fs::read_to_string` are dangerous for untrusted input.
**Prevention:** Always use `take(LIMIT)` when reading potentially large or untrusted input into memory. Enforce explicit limits at the I/O boundary.
