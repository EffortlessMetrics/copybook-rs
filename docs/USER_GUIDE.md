# ODO & REDEFINES – Practical Guide

## ODO (OCCURS DEPENDING ON)
- Place the counter **before** the dependent array.
- Keep the array's maximum explicit (e.g., `OCCURS 0 TO 10 DEPENDING ON CNT`).

**Strict**: invalid or missing `CNT` → error.
**Lenient**: out-of-range counters are clamped; warnings surface in diagnostics.

## REDEFINES
- At the **same level**, multiple views share the same bytes.
- Encoder selects a single, unambiguous view. Ambiguity is an error (strict) or warning (lenient).

## Edited PIC
Formats like `ZZ9.99`, trailing signs (`999.99-`), or `CR`/`DB` are currently unsupported.
They return `CBKP051_UNSUPPORTED_EDITED_PIC` with a message containing "edited PIC".

## Fixed-form Notes
- Columns 1–6: sequence area (ignored)
- Column 7: indicator (`*` comment, `-` continuation)
- Columns 8–72: code
- Columns 73–80: identification (ignored)

The parser ignores trailing tokens after the terminating `.` on a line.