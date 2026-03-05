# Performance Receipts

## Baseline (2025-09-30, commit 1fa63633)
- DISPLAY: 205 MiB/s (target: ≥80 MiB/s)
- COMP-3: 58 MiB/s (target: ≥40 MiB/s)
- Environment: WSL2, AMD Ryzen 9 9950X3D
- Methodology: tools/copybook-bench/BASELINE_METHODOLOGY.md

## CI Posture
- Advisory-only performance policy (Issues #74, #75)
- CI enforces throughput floors (DISPLAY ≥80, COMP-3 ≥40)
- No perf claims without receipts

## Receipt Requirements
- Before/after scripts/bench/perf.json
- Hardware + environment noted
- Variance acknowledged (~5% DISPLAY, ~8% COMP-3)
