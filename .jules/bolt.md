## 2026-02-18 - [Optimized EBCDIC Encoding]
**Learning:** `std::sync::OnceLock` is highly effective for caching static lookup tables derived from other static data, providing massive speedups (42x in `utf8_to_ebcdic`) for hot-path encoding functions by avoiding O(n) map construction per call.
**Action:** Look for other repetitive initialization patterns in hot paths and consider `OnceLock` for lazy static initialization.
