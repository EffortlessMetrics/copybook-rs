# copybook-scratch

Reusable scratch buffers for allocation-efficient COBOL codec processing.

This crate provides:
- `DigitBuffer`: stack-first digit storage for numeric decoding paths
- `ScratchBuffers`: reusable byte/string/digit buffers for hot loops
