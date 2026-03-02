# copybook-rdw-header

`copybook-rdw-header` provides focused RDW header primitives:

- header constants (`RDW_HEADER_LEN`, `RDW_MAX_PAYLOAD_LEN`),
- `RdwHeader` parse/build helpers,
- low-level `BufRead` helpers for peeking lengths and slicing payload bytes.

Higher-level record reading/writing remains in `copybook-rdw`.
