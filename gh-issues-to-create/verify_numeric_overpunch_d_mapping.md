---
title: Verify Numeric Overpunch 'D' Mapping in copybook-codec
labels: ["bug", "enhancement", "todo"]
assignees: []
---

## Issue Description

In `copybook-codec/src/numeric.rs`, the character 'D' is mapped to a value of -4 in the context of numeric overpunching. There are `TODO` comments at lines 701, 865, and 1415 indicating that this mapping needs verification. If this mapping is incorrect, it could lead to data corruption or misinterpretation when processing COBOL numeric fields with overpunch signs.

## Locations

*   `copybook-codec/src/numeric.rs:701`
*   `copybook-codec/src/numeric.rs:865`
*   `copybook-codec/src/numeric.rs:1415`

## Proposed Fix

1.  **Research:** Consult the COBOL standard or relevant documentation (e.g., IBM Enterprise COBOL Language Reference) to definitively determine the correct numeric overpunch mapping for the character 'D'.
2.  **Verification:** If the current mapping of `0x44` (ASCII 'D') to `(4, true)` (value 4, negative sign) is confirmed to be correct, remove the `TODO` comments.
3.  **Correction:** If the mapping is found to be incorrect, update the `match` arm in `numeric.rs` to reflect the accurate value and sign.

## Example (Illustrative - assuming current mapping is correct and just needs verification)

```rust
// Before:
// L701: 0x44 => (4, true), // 'D' = -4 (TODO: verify correct mapping)
// L865: 0x44 => (4, true), // 'D' = -4 (TODO: verify correct mapping)
// L1415: 4 => 0x44, // 'D' = -4 (TODO: verify correct ASCII overpunch mapping)

// After (if verified correct):
// L701: 0x44 => (4, true), // 'D' = -4 (Verified correct mapping for overpunch)
// L865: 0x44 => (4, true), // 'D' = -4 (Verified correct mapping for overpunch)
// L1415: 4 => 0x44, // 'D' = -4 (Verified correct ASCII overpunch mapping)
```

This issue is critical for data integrity and should be addressed to ensure full compliance with COBOL numeric representation.
