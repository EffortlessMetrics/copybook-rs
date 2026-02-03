## 2026-02-27 - `utf8_to_ebcdic` Reverse Lookup Table Caching
**Learning:** The `utf8_to_ebcdic` function was rebuilding the reverse lookup `HashMap` on every call (256 insertions), causing severe performance degradation. The overhead was ~11 microseconds per call even for short strings.
**Action:** Use `std::sync::OnceLock` to cache static lookup tables derived from other static data. This reduced per-call time to ~1 microsecond (~11x speedup).
