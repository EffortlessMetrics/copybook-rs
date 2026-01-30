## 2026-01-30 - Integer Log10 Optimization
**Learning:** `ilog10` CPU intrinsic (O(1)) is significantly faster (~1.5x) than iterative division (O(log N)) for counting digits in hot paths like `SmallDecimal` serialization.
**Action:** Replace iterative digit counting loops with `ilog10` where possible, especially in high-frequency validation logic like JSON encoding.
