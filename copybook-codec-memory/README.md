# copybook-codec-memory

Core memory management utilities for `copybook-codec`, extracted as a dedicated crate.

`SequenceRing` and related sequence-ordering types are re-exported from
`copybook-sequence-ring`.

## Public API

- `copybook_codec_memory::ScratchBuffers`
- `copybook_codec_memory::SequenceRing`
- `copybook_codec_memory::SequencedRecord`
- `copybook_codec_memory::WorkerPool`
- `copybook_codec_memory::StreamingProcessor`
