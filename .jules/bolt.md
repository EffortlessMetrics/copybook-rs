## 2026-02-17 - String Allocation in Hot Paths
**Learning:** `format!` and `write!` macros, along with intermediate `String` allocations (like `to_string()`), are significant bottlenecks in high-throughput serialization paths (e.g., COMP-3 to JSON).
**Action:** Use manual integer/decimal formatting directly into the target buffer (via `push_str`/`push`) to avoid allocation and macro overhead. Implement `append_to_buffer` methods for core types.
