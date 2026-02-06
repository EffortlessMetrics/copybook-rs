## 2026-02-06 - [Missing Charset Optimization]
**Learning:** Codebase state did not match memory/docs which claimed `utf8_to_ebcdic` was cached. It was rebuilding HashMaps on every call (~12µs/op).
**Action:** Implemented `OnceLock` caching, restoring performance to ~0.76µs/op. Always verify claims in documentation/memory against actual code.
