## 2026-02-24 - [Packed Decimal Encoding Optimization]
**Learning:** `Vec::with_capacity` followed by `push` can be slightly faster than `vec![0; n]` for very small vectors (likely due to initialization overhead). However, avoiding allocation entirely via scratch buffers yields massive gains (33% speedup). Also, beware of digit extraction order when filling buffers from right-to-left.
**Action:** Prioritize zero-allocation APIs (scratch buffers) for hot paths over micro-optimizing vector initialization.
