## 2026-01-01 - [Charset Conversion Allocation Bottleneck]
**Learning:** Rebuilding static lookup tables (like `HashMap`s) on every function call can silently destroy performance, even for "simple" operations. In `utf8_to_ebcdic`, this overhead was ~12µs per call (vs ~1µs optimized), a 12x slowdown.
**Action:** Always check hot-path conversion functions for hidden allocations or reconstruction of static data. Use `OnceLock` or `lazy_static` for immutable lookup tables.
