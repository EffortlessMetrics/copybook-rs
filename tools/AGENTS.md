<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Tooling Guidance

This file extends the root [`AGENTS.md`](../AGENTS.md). `tools/` is the
development control plane for benchmarks, fixture generation, CI helpers, and
`xtask`; it is not shipped library API.

Keep automation deterministic, fallible, and reproducible from a clean
checkout. Checker and policy changes need accept/reject fixtures, including
malformed and missing-input cases. Keep generated artifacts stable and update
their owning policy or receipt in the same change.

Performance claims are governed by `docs/PERFORMANCE_GOVERNANCE.md` and
`scripts/bench/perf.json`. A benchmark run proves only its recorded environment
and inputs. For `xtask` changes, run focused tests plus the exact command being
changed; for CI scripts, preserve local/CI parity and explain skip behavior.
