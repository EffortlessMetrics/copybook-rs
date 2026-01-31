## 2026-02-24 - Integration Benchmarks vs Micro-optimizations
**Learning:** High-level integration benchmarks (like `encode_comp3` involving JSON) can completely mask significant micro-optimizations (e.g., 5.6x speedup in `total_digits`) due to overhead from other components (like `serde_json`).
**Action:** Always verify low-level arithmetic optimizations with isolated micro-benchmarks before relying on integration benchmarks.
