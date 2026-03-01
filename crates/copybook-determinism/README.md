<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-determinism

Determinism primitives for repeatable output validation in copybook-rs.

## Overview

This crate compares two codec output byte streams using BLAKE3 digests and reports bounded
byte-level differences. It supports decode-only, encode-only, and full round-trip determinism
checks, enabling CI pipelines and tests to verify that codec operations are repeatable.

## Usage

```rust
use copybook_determinism::{compare_outputs, DeterminismMode, blake3_hex};

let output_a = b"hello world";
let output_b = b"hello world";

let result = compare_outputs(DeterminismMode::DecodeOnly, output_a, output_b);
assert!(result.is_deterministic);

// Stable content hashing
let hash = blake3_hex(b"some data");
assert_eq!(hash.len(), 64); // hex-encoded BLAKE3
```

## Public API

- `DeterminismMode` — Decode-only, encode-only, or round-trip checking
- `DeterminismResult` — Hash comparison result with optional byte diffs
- `ByteDiff` — Single byte-level difference descriptor
- `compare_outputs` / `compare_outputs_with_limit` — Compare two byte slices
- `find_byte_differences` / `find_byte_differences_with_limit` — Collect diffs
- `blake3_hex` — Hex-encoded BLAKE3 content hash

## License

AGPL-3.0-or-later
