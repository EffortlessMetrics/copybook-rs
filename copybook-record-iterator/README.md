# copybook-record-iterator

Streaming raw-record iteration primitives for fixed-length and RDW-framed data.

This microcrate focuses on a single responsibility:
- read sequential records from a `Read` source
- maintain record position / EOF state
- return raw record bytes without decode concerns

Higher-level JSON decode/encode behavior remains in `copybook-codec`.
