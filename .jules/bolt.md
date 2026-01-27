## 2026-02-18 - Static Lookup Table Caching in Hot Paths
**Learning:** Initializing static lookup tables (like `HashMap`) inside a hot function is a massive performance killer (O(N) per call). Even with small tables (256 entries), the allocation and insertion overhead dominates.
**Action:** Use `std::sync::OnceLock` (available in MSRV 1.90.0) to cache these tables lazily. This changes per-call complexity from O(N) to O(1) amortized, yielding 10x+ speedups for codec operations. Ensure keys (like `Codepage` enum) derive `Hash`.
