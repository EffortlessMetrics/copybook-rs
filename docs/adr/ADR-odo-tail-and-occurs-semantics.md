# ADR: ODO Tail Validation & OCCURS Semantics

## Decision
Validate ODO "tail" structurally (last storage sibling). Children under ODO do not count as "after".

## Rationale
Removes false positives from offset-based checks; matches COBOL structure.

## Error Taxonomy
- **CBKS301** (count>max): Enforced equally in decode/verify
- **CBKS302** (count<min): Enforced equally in decode/verify
- **CBKD301** (insufficient payload): Enforced equally in decode/verify

## Dialect Consideration
`OCCURS n TIMES DEPENDING ON …` lower bound is compiler-dependent; current behavior = min=max=n. Make configurable in future work.

## Status
Accepted - Implemented in v0.3.0

## Consequences
- **Positive**: Eliminates structural false positives, cleaner validation logic
- **Negative**: May need dialect configuration for edge cases
- **Neutral**: Maintains strict error taxonomy consistency