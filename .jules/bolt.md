## 2026-02-24 - [Rebuilding Lookup Tables in Hot Paths]
**Learning:** The `utf8_to_ebcdic` function was rebuilding a `HashMap` reverse lookup table on every call. This seemingly small overhead (building a 256-entry map) dominated execution time (13µs/call vs 1.8µs/call after fix), reducing throughput by 7.5x.
**Action:** Always check loop-invariant or static data construction in hot paths. Use `std::sync::OnceLock` (or `lazy_static`) to cache immutable lookup tables, especially when they are derived from static arrays.
