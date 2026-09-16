<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# VB / BDW design record (#953)

Required design record before implementation. Policy follows mainframe VB
(blocked variable-length) conventions; where real-world variants exist the
choice is explicit below, never inferred from one fixture.

## Wire format

A VB dataset is a sequence of blocks. Each block:

```text
BDW (4 bytes): block length including these 4 bytes, big-endian u16
               + 2 reserved bytes (must be zero)
RDW (4 bytes) + payload, repeated: record length including these 4 bytes,
               big-endian u16 + 2 reserved bytes
```

Critical convention difference: inside a VB block, RDW length **includes**
its own 4-byte header (mainframe LL convention). The bare-RDW path in
`copybook-rdw` uses payload length (header excluded). The VB reader converts
block-relative LL to payload slices; the two conventions never mix, and
misapplying one to the other is a `CBKF224` error, not a silent truncation.

## Policy decisions

- BDW shape: `u16 block_len` (header included) + `u16 reserved`. Reserved
  must be zero.
- BDW length interpretation: includes the BDW header. Length < 4 is invalid
  (`CBKF222`); length > 32760 is invalid (`CBKF222`).
- RDW length interpretation inside a block: includes the RDW header.
  Payload length = LL - 4; LL < 4 is invalid (`CBKF222`).
- Endianness: big-endian only. No little-endian option exists for VB;
  non-default endianness is never legitimate here.
- No length adjustments: the reader applies no compatibility guesses. A
  caller that needs a different wire variant converts input first.
- Maximums: block 32760 bytes; record (RDW LL) 32756 bytes. Oversize before
  allocation is `CBKF222`.
- Empty blocks: BDW length exactly 4 is a valid empty block yielding zero
  records. Zero-length input is clean EOF, not a block.
- Trailing bytes: bytes after the final complete block that do not form a
  BDW are `CBKF223` under strict, warning + stop under lenient (mirrors the
  RDW underflow policy).
- Reserved bytes: nonzero BDW reserved is a warning + continue under
  lenient, `CBKF225` under strict (mirrors `CBKR211` for RDW).
- RDW beyond block: an RDW whose LL exceeds the remaining block bytes is
  `CBKF224`, always fatal.
- Short/truncated BDW or nested RDW: `CBKF223`, always fatal.
- Reader behavior: bounded block reader state; lengths validated before any
  allocation or extraction; iteration yields payload + RDW + block position.
- Writer behavior: deterministic packing in record order; a record starts a
  new block when it would overflow 32760; reserved bytes zero; empty input
  writes zero blocks.
- Physical vs payload accounting stays distinct: input bytes consumed
  (headers included), payload bytes, block count, record count, and byte
  offsets are separate counters. A green record count never proves physical
  accounting.

## Stable error identities

| Code | Meaning |
| --- | --- |
| `CBKF222_BDW_LENGTH_INVALID` | zero/undersized/oversized block or record length |
| `CBKF223_BDW_UNDERFLOW` | truncated block, short nested RDW, strict trailing bytes |
| `CBKF224_RDW_BEYOND_BLOCK` | nested RDW escapes its containing block |
| `CBKF225_BDW_RESERVED_NONZERO` | strict-mode nonzero BDW reserved bytes |

Every failure preserves: stable identity, block index, record index where
known, physical byte offset, claimed block/record length, available bytes,
strict/lenient policy where applicable, bounded remediation. CLI exit mapping
follows the existing `CBKF` family mapping (exit 4).

## Raw and accounting contract

- Existing raw keys are unchanged. VB records reuse `RecordRDW` semantics
  (RDW header + payload per record).
- Planned (not shipped): `RawMode::RecordBlock` would capture the
  containing block once per record as `block_raw_b64` with
  `raw_capture: "record+block"`, plus per-record block provenance metadata
  (`block_index`, `block_offset`, `block_len`) in VB run summaries. Until
  implemented across the enum, codec, CLI/API documentation, JSONL schema,
  and tests, the only VB captures are `record` and `record+rdw`.
- New machine fields carry explicit beta stability until corpus
  differential proof.

## Stability classification

Both read and write paths ship **beta** in code, CLI help, support output,
docs, and scenario-ledger rows until corpus differential proof lands. No
release headline implies general VB support. Encode never rejects a
supported shape; unsupported shapes fail with the codes above, never with an
internal or panic result.

## Ownership recap

`copybook-rdw` owns schema-independent BDW/VB framing (`bdw.rs`);
`copybook-codec::file/vb.rs` owns format selection,
schema-aware dispatch, and worker execution; `copybook-core` stays out of
file-container policy; the CLI owns option parsing and rendering.
