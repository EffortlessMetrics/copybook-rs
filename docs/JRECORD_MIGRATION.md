<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Migrating from JRecord

Guide for teams moving COBOL extract pipelines from JRecord (Java) to
copybook-rs. Every equivalence claim below is pinned to the differential
slice in [differential-jrecord-0.93.2.md](evidence/differential-jrecord-0.93.2.md);
anything not in that slice is stated as a boundary, not a claim.

## Pinned evidence

- JRecord 0.93.2 (`Cobol2Csv.jar`), bundle SHA-256
  `74b0196d7565bc97b4f465e82176a7f8f4f50654a98b115c18992e417e28ed5c`,
  run under OpenJDK 21.
- copybook-rs side: `copybook decode` over the same input files.
- Three agreement cases, zero copybook-rs defects, zero inconclusive runs.
- JRecord is treated as an independent implementation, never as infallible
  ground truth.

## What agrees

| # | Case | JRecord invocation | copybook-rs invocation |
|---|------|--------------------|------------------------|
| 1 | Fixed ASCII mini records (`fixtures/corpus/mini.cpy` + `mini_fixed.bin`) | `-IFS Fixed_Length -D "," -Q DoubleQuote -IC ASCII -OC ASCII -Dialect 0` | `decode --format fixed --codepage ascii` |
| 2 | VB with BDW (`mini_vb.bin`) | `-IFS Mainframe_VB_As_RECFMU`, same flags | `decode --format vb --codepage ascii` |
| 3 | EBCDIC COMP-3 (`fixtures/copybooks/simple.cpy` + `fixtures/data/simple.bin`) | `-IFS Fixed_Length -IC CP037 -OC ASCII -Dialect 1` | `decode --format fixed --codepage cp037` |

Agreement means: same records, same fields, same semantics after the
normalization policy below. Field-for-field agreement includes the COMP-3
amount in case 3.

### Normalization policy (what "same" means)

- Record identity, field identity, offsets, widths, and record lengths
  match exactly.
- Alphanumeric trailing spaces and unsigned zoned leading zeros are
  trimmed before comparison: they are projection policies of each tool's
  CSV/JSON output, not record semantics. Both tools decode the same
  payload bytes.

## Boundary: bare RDW

JRecord `Binary` and `Text` file structures have no bare-RDW mode matching
`fixtures/corpus/mini_rdw.bin` (4-byte big-endian RDW headers, no BDW);
both misframe the stream by construction. This is recorded as
non-overlapping, not a defect on either side. If your extracts are bare
RDW, copybook-rs decodes them per `format.rdw.basic` while JRecord has no
matching mode — plan for copybook-rs-side validation (`verify`,
`determinism`) instead of output diffing.

## Migration workflow

1. **Diagnose.** `copybook doctor <COPYBOOK> <DATA>` stages parse,
   framing, codepage, and trial decode, and hands you the exact decode
   command. Start here whenever an extract misbehaves.
2. **Decode and diff.** Run the equivalent commands from the table above
   and compare under the normalization policy (trim trailing spaces on
   alphanumeric fields, leading zeros on unsigned zoned numerics).
3. **Lock determinism.** `copybook determinism round-trip <COPYBOOK>
   <DATA>` proves byte-identical output across runs.
4. **Gate regressions.** `copybook compat base.cpy head.cpy` fails CI on
   breaking copybook changes; `copybook explain <CODE>` teaches every
   failure the pipeline surfaces.

## Explicit non-claims

- No byte-identical output claim: padding and sign representations differ
  by tool policy (see the normalization policy above).
- No throughput claim on this page; performance policy, receipts, and
  floors live in [PERFORMANCE_GOVERNANCE.md](PERFORMANCE_GOVERNANCE.md).
- No Java toolchain appears anywhere in the product manifests or CI
  build lanes (grep the workspace `Cargo.toml` files and
  `.github/workflows/`), so dropping the JVM is a dependency-graph fact,
  not a benchmark. Install and distribution options are covered by the
  release process.
