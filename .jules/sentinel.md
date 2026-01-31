# Sentinel Journal 🛡️

## 2026-02-17 - DoS via Unbounded File Read
**Vulnerability:** `copybook-cli`'s `read_file_or_stdin` utility read the entire input stream into memory using `read_to_string` without any size limit, enabling a Denial of Service (DoS) attack via large files or infinite stdin streams (CWE-400).
**Learning:** Historical documentation/memory incorrectly stated a 16 MiB limit was enforced, but the actual code lacked this check. This highlights the risk of relying on documentation or "known state" without verifying the source code.
**Prevention:** Enforce explicit size limits (`take(limit)`) on all unbounded input streams (files, network, stdin) at the point of ingestion. Verify "known" security controls against the actual codebase.
