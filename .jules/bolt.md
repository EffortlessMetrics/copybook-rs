# Bolt's Journal

## 2025-02-18 - Missing Optimization Artifacts
**Learning:** The project memory referenced a `PACKED_BCD_LOOKUP` table in `copybook-codec` which did not exist in the codebase. This indicates a disconnect between documentation/memory and the actual code state, possibly due to a reverted change or planned-but-not-implemented feature.
**Action:** When optimizing, always verify the existence of "known" optimizations before assuming they are in place. In this case, implementing the missing lookup table is a clear opportunity.

## 2025-02-18 - Benchmark Bottlenecks Masking Codec Improvements
**Learning:** Optimizing the raw `decode_packed_decimal` logic yielded only ~1.8% improvement on the `comp3_heavy` benchmark. Profiling analysis suggests that `SmallDecimal::to_string` and JSON serialization (in `decode_file_to_jsonl`) dominate the runtime, masking gains in the core decoding logic.
**Action:** Future optimization efforts for COMP-3 should prioritize `SmallDecimal` formatting (e.g., direct-to-buffer formatting) or reducing JSON serialization overhead to unlock higher throughput.
