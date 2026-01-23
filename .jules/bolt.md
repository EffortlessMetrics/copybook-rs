## 2025-01-29 - Optimized Path Consistency Trap
**Learning:** Optimized "scratch buffer" paths can easily drift from the "standard" implementation (e.g., `to_string`), leading to subtle behavior differences like zero normalization. In this case, `format_to_scratch_buffer` was missing a zero-normalization check present in `to_string`, causing "0.00" vs "0" inconsistency.
**Action:** When optimizing a hot path by duplicating logic (manual formatting vs std::fmt), always verify strict output parity with the slow path, ideally with property-based tests or exhaustive edge case comparisons.
