## 2026-02-23 - [DoS Vulnerability in File Reading]
**Vulnerability:** `copybook-cli`'s `read_file_or_stdin` used `std::fs::read_to_string` without size limits, allowing potential DoS via memory exhaustion if a user provided a multi-GB file as a copybook argument.
**Learning:** Shared utility functions for file reading must always enforce reasonable limits, even if they are intended for small configuration files, as they might be exposed to untrusted input.
**Prevention:** Always use `read_with_limit` (or similar bounded readers) when reading external files into memory. Enforce `MAX_COPYBOOK_SIZE` (16 MiB) for schema inputs.
