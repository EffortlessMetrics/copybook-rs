# copybook-pic

Standalone microcrate for parsing COBOL PIC clauses and computing display metadata
(width, scale, sign-editing characteristics).

This crate is extracted from `copybook-core` so PIC parsing logic can be reused by
other crates without pulling the full parser stack.
