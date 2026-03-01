# copybook-codec-memory

Core memory management utilities for `copybook-codec`, extracted as a dedicated crate.

## Overview

Provides performance-critical memory patterns for high-throughput COBOL data processing:
reusable scratch buffers to eliminate hot-path allocations, a deterministic worker pool for
parallel record processing, and a streaming processor that enforces bounded memory usage
(<256 MiB) for multi-GB files.

## Usage

```rust
use copybook_codec_memory::ScratchBuffers;

// Reuse buffers across records to avoid allocation
let mut scratch = ScratchBuffers::new();
scratch.digit_buffer.push(5);
scratch.byte_buffer.extend_from_slice(b"data");
scratch.string_buffer.push_str("text");
scratch.clear(); // clears without deallocating
```

## Public API

- `ScratchBuffers` — Reusable byte/digit/string buffers for hot-path processing
- `WorkerPool` — Parallel record processing with deterministic output ordering
- `StreamingProcessor` — Memory-bounded streaming with pressure tracking
- `SequenceRing` / `SequencedRecord` — Re-exported from `copybook-sequence-ring`

## License

AGPL-3.0-or-later
