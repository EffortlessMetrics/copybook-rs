# copybook-sequence-ring

Deterministic sequence reordering primitive for parallel pipelines.

## Overview

`SequenceRing` accepts potentially out-of-order records tagged with a sequence ID and emits
them in strict order. This enables deterministic output when processing COBOL records across
multiple worker threads, which is critical for maintaining data integrity in parallel pipelines.

## Usage

```rust
use copybook_sequence_ring::{SequenceRing, SequencedRecord};

let mut ring = SequenceRing::new(100, 50); // channel capacity, max reorder window
let sender = ring.sender();

// Workers send results out of order
sender.send(SequencedRecord::new(2, "second")).unwrap();
sender.send(SequencedRecord::new(1, "first")).unwrap();
sender.send(SequencedRecord::new(3, "third")).unwrap();

// Consumer receives in strict sequence order
assert_eq!(ring.recv_ordered().unwrap(), Some("first"));
assert_eq!(ring.recv_ordered().unwrap(), Some("second"));
assert_eq!(ring.recv_ordered().unwrap(), Some("third"));
```

## Public API

- `SequencedRecord<T>` — Record wrapper with a monotonic sequence ID
- `SequenceRing<T>` — Bounded channel with reorder buffer for in-order emission
- `SequenceRingStats` — Operational statistics (buffer size, sequence progress)

## License

AGPL-3.0-or-later
