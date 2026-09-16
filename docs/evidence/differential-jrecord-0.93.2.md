<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Differential slice: JRecord 0.93.2 vs copybook-rs 0.7.0

Issue #952 Lane C (0.7C cut). One pinned differential slice over a genuinely
overlapping subset. JRecord is treated as an independent implementation, never
as infallible ground truth.

## Pinned external implementation

- Tool: JRecord `Cobol2Csv.jar` from `JRecord_0.93.2.zip`
- Distribution: `https://sourceforge.net/projects/jrecord/files/jrecord/Version_0.93.2/JRecord_0.93.2.zip/download`
- Bundle SHA-256: `74b0196d7565bc97b4f465e82176a7f8f4f50654a98b115c18992e417e28ed5c`
- Runtime: OpenJDK 21
- copybook-rs side: workspace 0.7.0 (`copybook decode`), same input files

## Overlapping contract

Fixed-length and VB (BDW + RDW) record containers with zoned-decimal,
alphanumeric, and COMP-3 fields; ASCII and EBCDIC CP037 codepages.
Out of scope by policy: bare-RDW framing (no matching JRecord file
structure), encoded-bytes comparison (padding/sign representation policies
differ), and anything beyond the fixtures below.

## Normalization policy

- Record identity, field identity, offsets, widths, and record lengths must
  match exactly.
- Semantic field values agree after trimming trailing spaces on
  alphanumeric fields and leading zeros on unsigned zoned numerics. Rationale:
  space padding and zero padding are representation policies of each tool's
  CSV/JSON projection, not record semantics; both tools decode the same
  payload bytes.
- A mismatch preserves both original outputs and this decision.

## Results

Outcome classes: agreement, copybook-rs defect, external limitation,
dialect/policy difference, normalization gap, unsupported/non-overlapping,
inconclusive/tool failure. "Inconclusive" is never reported as agreement.

### 1. mini-fixed-ascii — agreement

Copybook `fixtures/corpus/mini.cpy`, records
`fixtures/corpus/mini_fixed.bin` (14-byte fixed ASCII, 2 records).

JRecord (`-IFS Fixed_Length -D "," -Q DoubleQuote -IC ASCII -OC ASCII
-Dialect 0`):

```text
REC_ID,REC_NAME
1,ALPHA
2,BETA
```

copybook-rs (`decode --format fixed --codepage ascii`):

```text
REC-ID=0001 REC-NAME="ALPHA     "
REC-ID=0002 REC-NAME="BETA      "
```

Same records, same fields, same semantics; numeric and space padding
normalize per policy. Outcome: **agreement**.

### 2. mini-vb-ascii — agreement

Same copybook, records `fixtures/corpus/mini_vb.bin` (BDW blocks with RDW
records).

JRecord (`-IFS Mainframe_VB_As_RECFMU`, other flags as above):

```text
REC_ID,REC_NAME
1,ALPHA
2,BETA
```

copybook-rs (`decode --format vb --codepage ascii`): identical semantics to
case 1. JRecord independently frames our BDW + RDW blocks. Outcome:
**agreement**. This is the first external proof of the 0.7 VB/BDW framing.

### 3. EBCDIC COMP-3 record — agreement

Copybook `fixtures/copybooks/simple.cpy`, records
`fixtures/data/simple.bin` (EBCDIC CP037, zoned + COMP-3).

JRecord (`-IFS Fixed_Length -IC CP037 -OC ASCII -Dialect 1`):

```text
CUSTOMER_ID,CUSTOMER_NAME,ACCOUNT_BALANCE,LAST_ACTIVITY_DATE,STATUS_CODE
123456,John Smith,12345.67,20230915,A
```

copybook-rs (`decode --format fixed --codepage cp037`):

```text
CUSTOMER-ID=123456 CUSTOMER-NAME="John Smith                    " ACCOUNT-BALANCE=12345.67 LAST-ACTIVITY-DATE=20230915 STATUS-CODE=A
```

(The 30-character name field is space-padded in the copybook-rs JSON
projection; JRecord trims it. Same payload bytes, normalization policy
applies.)

Field-for-field agreement including the COMP-3 amount. Outcome:
**agreement**.

### 4. bare RDW framing — unsupported/non-overlapping

Records `fixtures/corpus/mini_rdw.bin` (4-byte big-endian RDW headers, no
BDW). JRecord `Binary` and `Text` file structures have no bare-RDW mode
matching this framing; both misframe the stream (headers decoded as data):

```text
REC_ID,REC_NAME
,0001ALPHA
```

copybook-rs decodes it per `format.rdw.basic`. No defect on either side:
the overlap contract does not cover this framing. Outcome:
**unsupported/non-overlapping**. JRecord-side limitation, not a
copybook-rs defect; no regression fixture owed.

### 5. VB read as BDW-less Mainframe_VB — non-comparable policy

`mini_vb.bin` under `-IFS Mainframe_VB` (which assumes no BDW) misframes by
construction. This run served only to confirm the mode assumption; it is not
evidence about either implementation. Outcome: **non-comparable**
(dialect/policy difference in file-structure selection, classified at
collection time, never upgraded to agreement or defect).

## Conclusion

Three agreement cases (fixed ASCII, VB with BDW, EBCDIC COMP-3), zero
copybook-rs defects, zero inconclusive runs. No minimized regression
fixtures are owed because no confirmed defect was found. The bare-RDW gap is
recorded above as non-overlapping with rationale; a future slice may extend
the overlap to JRecord RDW dialects if a matching file structure is
identified — product behavior will not change merely to match.
