// SPDX-License-Identifier: AGPL-3.0-or-later
//! Compatibility coverage for the pre-0.6 `copybook_codec::memory` path.

use copybook_codec::memory::{ScratchBuffers, SequenceRing, SequencedRecord, WorkerPool};

#[test]
fn memory_alias_forwards_runtime_types() {
    let mut scratch = ScratchBuffers::new();
    scratch.byte_buffer.extend_from_slice(b"runtime");
    assert_eq!(scratch.byte_buffer, b"runtime");

    let mut ring = SequenceRing::new(2, 1);
    ring.sender()
        .send(SequencedRecord::new(1, 7))
        .expect("compatibility ring send");
    assert_eq!(
        ring.recv_ordered().expect("compatibility ring receive"),
        Some(7)
    );

    let mut pool = WorkerPool::new(1, 2, 1, |value: u8, _| value + 1);
    pool.submit(41).expect("compatibility worker submit");
    assert_eq!(
        pool.recv_ordered().expect("compatibility worker receive"),
        Some(42)
    );
    pool.shutdown().expect("compatibility worker shutdown");
}
