<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Decision packet: physical-record and heterogeneous-layout breadth

Issue #1007 (scope inventory) with the runnable differential seed for #1021.
Every selection below is evidenced, not asserted: current copybook-rs
behavior was probed on this workspace, and the JRecord lanes were executed
against the pinned `JRecord_0.93.2.zip` bundle (SHA-256
`74b0196d7565bc97b4f465e82176a7f8f4f50654a98b115c18992e417e28ed5c`,
OpenJDK 21) via `scripts/differential-jrecord.sh`. Cobrix lanes are
recorded inconclusive: no Spark runtime and no pinned local artifact were
available, and per #1021 a missing comparator is never agreement.

Machine-readable inventory: `breadth-inventory.toml` in this directory
(one row per candidate capability, same field order as #1007). This note
records the executed lanes and their classifications; the TOML is
authoritative for selections.

## Executed JRecord lanes (all with `-IC ASCII -OC ASCII -D "," -Q DoubleQuote -Dialect 0`)

Conventions: `copybook-rs` invoked as
`decode <cpy> <input> --output <jsonl> --format <f> --codepage ascii`;
JRecord invoked as `Cobol2Csv -C <cpy> -I <input> -O <csv> -IFS <s>` plus
the conventions above. Fixtures live in this directory.

### Lane 1 — multi-block VB, varied record sizes (capability `vb-multi-block-varied`)

Copybook `vary.cpy` (`V-FIELD PIC X(6)`), records `vbvaried.bin`: BDW block
one holds a 6-byte record (`ABCDEF`), block two a 2-byte record (`GH`).

- copybook-rs `--format vb`: record 1 decodes (`ABCDEF`); record 2 is a
  classified per-record error,
  `CBKF221_RDW_UNDERFLOW: RDW payload too short: 2 bytes, schema requires
  6 bytes`. Framing holds across blocks; short payloads never decode
  silently.
- JRecord `-IFS Mainframe_VB_As_RECFMU`: emits both rows (`ABCDEF`,
  `GH`), accepting the short record.
- Classification: **policy/dialect difference** (strict length
  enforcement vs lenient short-record acceptance). Both original outputs
  preserved by the script run, not committed.

### Lane 2 — line-delimited fixed-width (capability `line-delimited-fixed`)

Copybook `lines.cpy` (two `PIC X(4)` fields), records `lines8.txt`
(`ABCD1234\nEFGH5678\n`).

- copybook-rs `--format fixed`: the newline is a data byte; the file ends
  mid-record and decode fails `CBKR101_FIXED_RECORD_ERROR`. No text
  framing exists.
- JRecord `-IFS Text`: strips terminators and splits both rows
  (`ABCD,1234` / `EFGH,5678`).
- Classification: **unsupported/non-overlapping** on the copybook-rs
  side; JRecord Text is the runnable oracle for the selected 0.10 work.

### Lane 3 — short final fixed block (capability `fixed-blocked-retained`)

Copybook `lines.cpy` (single 01, two `PIC X(4)` fields: one 8-byte stride
for both tools, so layout policy cannot contaminate the framing
comparison), records `fb-short-8.bin` (14 bytes: one full record plus a
6-byte tail).

- copybook-rs `--format fixed`: `CBKR101_FIXED_RECORD_ERROR`, expected 8
  bytes at record 2. Strict multiple enforcement.
- JRecord `-IFS Fixed_Length`: emits the full row plus a second row with
  the 6 tail bytes NUL-padded (`EFGH,56` + `0x0000`, verified by hexdump
  of the CSV).
- Classification: **policy/dialect difference** (strict rejection vs
  silent NUL-pad). The 0.10 selection keeps strict rejection and names
  expected/actual lengths; the JRecord pole documents the lenient
  alternative that is explicitly rejected.

### Lane 4 — generic 2-byte length prefix (capability `length-prefix-124`)

Copybook `vary.cpy`, records `prefix2.bin` (BE u16 length + payload,
no reserved bytes).

- copybook-rs `--format rdw`: record error (4-byte RDW framing assumed).
  No generic-prefix framing exists.
- JRecord `-IFS Binary`: does not parse prefixes; prefix bytes land in
  the field (`\0\4ABCD`). External limitation, not an oracle.
- Classification: **unsupported/non-overlapping** (copybook-rs) plus
  **external limitation** (JRecord). No independent oracle exists yet;
  selection is 0.11 pending format-reference and hostile families.

### Lane 5 — multiple 01-level layouts (capability `multi-01-layouts`)

Copybook `multi01.cpy` (`A-REC` with `A-FIELD X(4)`, `B-REC` with
`B-FIELD 9(4)`), records `fb-short-8.bin` (14 bytes: 3.5 four-byte rows
at the overlaid stride).

- copybook-rs: concatenates both 01s into one 8-byte layout (inspect
  reports `Fixed LRECL: 8 bytes`).
- JRecord `-IFS Fixed_Length`: strides by the first 01 (4 bytes) and
  overlays both fields at offset 0 (`A_FIELD,B_FIELD` with identical
  values per row).
- Classification: **policy/dialect difference** (concatenation vs
  overlay); neither side selects a layout per record. Any 0.11
  discriminator design must first resolve this divergence; until then
  the documented behavior is concatenation.

## What was not executed

- In-record field lengths, discriminator-mapped lengths, discriminator
  selection (capabilities `length-from-field`,
  `discriminator-mapped-lengths`, `discriminator-layout-selection`,
  `discriminator-length-layout`): no framing or selection surface exists
  on the copybook-rs side to probe, and the single-record `Cobol2Csv`
  entry point selects nothing. JRecord's multi-record path is cited in
  the TOML as documentation-level prior art, not executed evidence.
- Parent/child multisegment assembly: no assembly surface exists;
  Cobrix (the named comparator) is unavailable without Spark.
- Custom framing seam: internal design surface; evidenced by code
  inspection (`RecordFormat` is a closed three-variant enum), not by
  execution.
