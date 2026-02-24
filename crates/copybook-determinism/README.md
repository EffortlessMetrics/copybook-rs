<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-determinism

Determinism primitives for copybook-rs.

This crate isolates one responsibility:
compare two byte streams in a deterministic way using BLAKE3 digests and bounded byte-level diffs.

## Public API

- `DeterminismMode`
- `ByteDiff`
- `DeterminismResult`
- `compare_outputs`
- `compare_outputs_with_limit`
- `find_byte_differences`
- `find_byte_differences_with_limit`
- `blake3_hex`
